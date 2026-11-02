#------------------------------------------------------------------------------
# test_supply_vs_nsece_local.R -- local half of 2 November supply comparison
# (methodology memo open items: wages, staffing, CCDF). Writes diagnostics only,
# nothing simulator or build reads.
#
# 1. Model 2019 wages (config/state/supply/<ST>.yaml) by Census region, PEP
#    2019 children 0-4 weights -- compared w/ NSECE regional wages in
#    test_supply_vs_nsece_cluster.R.
# 2. Legal centre children per adult by region x year of age (harmonic mean
#    over states, PEP 0-4 weights) -- input to cluster half's observed vs
#    legal comparison. National: NSECE enrolment-weighted over ages
#    (shared_nsece_ages.R counts). Legal home capacity by region.
# 3. CCDF ACF-801 FY2019: licensed share of subsidised home-based children
#    0-4 (primary setting = most hours in child-month, child weight), by
#    state; correlation w/ selected home capacity.
#
# Usage: Rscript state_data/tests/test_supply_vs_nsece_local.R
#   writes state_data/out/supply_vs_nsece_local_2019.txt,
#   state_data/out/legal_centre_children_per_staff_by_region_age_2019.csv,
#   state_data/out/ccdf_home_licensed_share_2019.csv
# Exit status 1 on failed check.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr); library(yaml); library(tidyr)})
options(width = 200)
source('src/shared_functions/constants.R')
source('state_data/code/09_ccdf_subsidy_children.R')   # load_ccdf(); main() guarded
source('state_data/code/shared_nsece_ages.R')          # NSECE centre enrolment by age

OUT <- 'state_data/out'
POP_FILE <- 'resources/census_population/US_PEP__population_children_age_0_4_by_county__2019.csv'
RATIOS <- 'resources/supply/licensing_ratios_reviewed_2019.csv'
# Census region (NSECE CB9 / HH9 _REGION: 1 Northeast, 2 Midwest, 3 South, 4 West)
REG <- c(CT = 1, ME = 1, MA = 1, NH = 1, RI = 1, VT = 1, NJ = 1, NY = 1, PA = 1,
         IL = 2, IN = 2, MI = 2, OH = 2, WI = 2, IA = 2, KS = 2, MN = 2, MO = 2, NE = 2, ND = 2, SD = 2,
         DE = 3, FL = 3, GA = 3, MD = 3, NC = 3, SC = 3, VA = 3, DC = 3, WV = 3, AL = 3, KY = 3, MS = 3,
         TN = 3, AR = 3, LA = 3, OK = 3, TX = 3,
         AZ = 4, CO = 4, ID = 4, MT = 4, NV = 4, NM = 4, UT = 4, WY = 4, AK = 4, CA = 4, HI = 4, OR = 4, WA = 4)
# CCDF CARETYPE digits 2-3 (ICPSR 38677 codebook): home-based settings, licensed/regulated settings
CCDF_HOME <- c('02', '03', '07', '08', '09', '10')
CCDF_LICENSED <- c('01', '02', '03', '04')

fails <- 0
check <- function(ok, msg) {
  cat(if (ok) '  [PASS] ' else '  [FAIL] ', msg, '\n', sep = '')
  if (!ok) fails <<- fails + 1
}
log_lines <- character()
say <- function(...) { x <- capture.output(...); log_lines <<- c(log_lines, x); cat(x, sep = '\n') }



pop <- read_csv(POP_FILE, col_types = cols(.default = 'c')) %>%
  mutate(state = names(STATE_POSTAL_TO_FIPS)[match(as.integer(STATE), STATE_POSTAL_TO_FIPS)]) %>%
  group_by(state) %>% summarise(pop = sum(as.numeric(TOT_POP)), .groups = 'drop') %>%
  filter(state %in% names(REG)) %>% mutate(region = REG[state])
check(nrow(pop) == 51, '50 states + DC have PEP children 0-4')

# 1. Model wages by region
wg <- bind_rows(lapply(names(REG), function(g) {
  y <- read_yaml(file.path('config', 'state', 'supply', paste0(g, '.yaml')))
  tibble(state = g, no_ba = y$wages$no_ba, ba = y$wages$ba)
})) %>% left_join(pop, by = 'state')
check(all(wg$no_ba > 0 & wg$ba > 0), 'every state wage positive')
say(cat('== model 2019 wages by region, PEP 0-4 weighted (region 0 = 50 states + DC)\n'))
wr <- bind_rows(wg %>% group_by(region) %>% summarise(no_ba = weighted.mean(no_ba, pop), ba = weighted.mean(ba, pop)),
                wg %>% summarise(no_ba = weighted.mean(no_ba, pop), ba = weighted.mean(ba, pop)) %>% mutate(region = 0))
say(print(as.data.frame(wr %>% mutate(across(c(no_ba, ba), ~ round(.x, 3)))), row.names = FALSE))

# 2. Legal centre and home ratios
rt <- read_csv(RATIOS, show_col_types = FALSE) %>% filter(state %in% names(REG)) %>% left_join(pop, by = 'state')
cen <- rt %>% filter(setting == 'center')
check(setequal(unique(cen$state), names(REG)), 'centre ratios for 50 states + DC')
harm <- function(d) d %>% summarise(legal_cps = 1 / weighted.mean(1 / children_per_staff, pop), .groups = 'drop')
leg <- cen %>% group_by(region, age = age_group) %>% harm()
check(nrow(leg) == 20 && all(leg$legal_cps > 0), 'legal centre ratio in every region x age cell')
write_csv(leg, file.path(OUT, 'legal_centre_children_per_staff_by_region_age_2019.csv'))
nw <- NSECE_CENTRE_ENROLLED
s <- cen %>% group_by(state, pop, region) %>%
  summarise(sp = sum(nw[age_group] / children_per_staff) / sum(nw[age_group]), .groups = 'drop')
hm <- rt %>% filter(setting == 'home')
say(cat('\n== legal centre children per adult, NSECE enrolment-weighted over ages: national',
        round(1 / weighted.mean(s$sp, s$pop), 2), '\n'))
say(cat('== legal home capacity (one adult, infant present), harmonic, PEP weights: national',
        round(harm(hm)$legal_cps, 2), '\n'))
say(print(as.data.frame(hm %>% group_by(region) %>% harm() %>% mutate(legal_cps = round(legal_cps, 2))), row.names = FALSE))

# 3. CCDF licensed share of subsidised home-based children 0-4
se <- load_ccdf('setting_records'); ch <- load_ccdf('child_records')
ym <- function(x) (x %/% 100) * 12 + x %% 100
kids <- tibble(STATE = ch$STATE, FAMID = ch$FAMID, CHID = ch$CHID, month = ch$CREPORT, w = ch$A_FCHWT,
               age_m = ym(ch$CREPORT) - ym(ch$CH_DOB)) %>% filter(STATE %in% names(REG), age_m >= 0, age_m < 60)
sett <- tibble(STATE = se$STATE, FAMID = se$FAMID, CHID = se$CHID, month = se$REPORT, hours = se$HOURS,
               code = substr(as.character(se$CARETYPE), 2, 3)) %>%
  group_by(STATE, FAMID, CHID, month) %>% arrange(desc(hours), .by_group = TRUE) %>%
  summarise(code = first(code), .groups = 'drop')
cm <- kids %>% inner_join(sett, by = c('STATE', 'FAMID', 'CHID', 'month')) %>%
  mutate(home = code %in% CCDF_HOME, lic = code %in% CCDF_LICENSED)
res <- cm %>% filter(home) %>% group_by(state = STATE) %>%
  summarise(home_children_weighted = sum(w), licensed_share = sum(w * lic) / sum(w), records = n(), .groups = 'drop') %>%
  left_join(hm %>% select(state, home_capacity = children_per_staff), by = 'state')
check(all(res$licensed_share >= 0 & res$licensed_share <= 1), 'CCDF licensed shares in [0, 1]')
write_csv(res, file.path(OUT, 'ccdf_home_licensed_share_2019.csv'))
say(cat('\n== CCDF FY2019 subsidised home-based children 0-4: licensed share (child weight), national',
        round(with(cm[cm$home, ], sum(w * lic) / sum(w)), 3), '\n'))
say(cat('== correlation of state licensed share w/ selected home capacity (', nrow(res), ' states): ',
        round(cor(res$licensed_share, res$home_capacity, use = 'complete.obs'), 3), '\n', sep = ''))

writeLines(c(paste('script: state_data/tests/test_supply_vs_nsece_local.R; run at', format(Sys.time())), log_lines),
           file.path(OUT, 'supply_vs_nsece_local_2019.txt'))
cat('\n', fails, ' failure(s)\n', sep = '')
if (fails > 0) quit(status = 1)
