#------------------------------------------------------------------------------
# 02_nsece_constants.R -- derives every NSECE constant 00_main.R holds.
#
# From the NSECE 2019 household file, w/ the classification
# src/1_processing/1a_nsece_processing.R uses, by Census region and nationally:
# formal share, centre-home split, informal mix, weekly hours -- each w/ its
# unweighted record count. Full precision to out/nsece_universe.csv.
#
# TWO UNIVERSES, and the build uses A:
#
#   universe A   every child 0-4 w/ a regular non-parental arrangement --
#                NSECE's own screener, five hours. It reproduces NSECE's
#                published figures exactly, which validates the code. THE
#                SIMULATOR'S OWN UNIVERSE: 1a_nsece_processing.R keeps every
#                NSECE child w/ positive hours, adds no threshold of its own
#                -- screener's five-hour floor still applies -- so build
#                describes population simulator holds.
#   universe B   the same children whose TOTAL weekly hours across regular
#                non-parental arrangements reach 10 -- the NSCH k6q20
#                threshold. Kept because NSCH can only measure B, so
#                `children_all_over_10plus` carries step 1 from B up to A.
#
# The restriction is on the child's total, since NSCH asks about all
# non-parental care. School (K-8) is excluded from both.
#
# WHERE THE DATA IS. NSECE public-use is ICPSR 37941, licensed, not in this
# repository. It lives on the cluster beside the simulator's own inputs. Pass
# --nsece <dir> or set NSECE_ROOT; the file read is 2019/37941-0009-Data.tsv.
# Submit w/ sbatch -- the file is 117 MB and several thousand columns wide.
#
# Usage:
#   Rscript state_data/code/02_nsece_constants.R --nsece <dir> [--out <file>]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tidyr); library(stringr)
  library(tibble); library(readr)
})

# Census region codes as HH9_REGION carries them.
REGIONS <- c('1' = 'northeast', '2' = 'midwest', '3' = 'south', '4' = 'west')

# Centre price split, the boundary 1a_nsece_processing.R uses.
PRICE_BOUNDARY <- 4.75

# Weekly hours below which the simulator calls an arrangement part-time.
# src/shared_functions/constants.R sets pt = 10 hrs/wk and ft = 40;
# src/1_processing/1a_nsece_processing.R cuts between them at `hours < 20`.
PT_FT_CUT <- 20

# Primary-arrangement ranking. Same order and same reason as the simulator:
# paid before unpaid, centre before home, hours only as a tie-break.
CATEGORY_PRIORITY <- c('K-8', 'High-Priced Center-Based',
                       'Low-Priced Center-Based', 'Unpaid Center-Based',
                       'Paid Home-Based', 'Other Paid', 'Unpaid Home-Based',
                       'Other Unpaid')

FORMAL <- c('High-Priced Center-Based', 'Low-Priced Center-Based',
            'Unpaid Center-Based', 'Paid Home-Based')
INFORMAL <- c('Other Unpaid', 'Other Paid', 'Unpaid Home-Based')

# NSECE's published regional formal shares -- universe A must reproduce them.
PUBLISHED_FORMAL <- c(northeast = 0.5653, midwest = 0.5292,
                    south = 0.5316, west = 0.4835)



arg <- function(flag, default = NA_character_) {

  #----------------------------------------------------------------------------
  # Value following a CLI flag.
  #
  # Params:
  #   - flag (chr): flag name, e.g. '--nsece'
  #   - default (chr): returned when flag absent
  #
  # Returns: (chr) arg after flag, or default
  #----------------------------------------------------------------------------

  a <- commandArgs(trailingOnly = TRUE)
  i <- which(a == flag)
  if (length(i) == 0) return(default)
  a[i + 1]
}



read_household <- function(root) {

  #----------------------------------------------------------------------------
  # Reads the columns this measurement needs, and no others. The file is 117MB
  # and several thousand columns wide; selecting first keeps it small enough to
  # hold twice.
  #
  # Params:
  #   - root (chr): NSECE root holding 2019/
  #
  # Returns: (data.table)
  #----------------------------------------------------------------------------

  f <- file.path(root, '2019', '37941-0009-Data.tsv')
  if (!file.exists(f)) stop('missing NSECE household file: ', f)

  hdr <- names(fread(f, nrows = 0))

  # Hours arrive as PROV<provider>_<child> in some releases and
  # PROV_<child>_<provider> in others. Take both spellings and normalise after.
  want <- c('HH9_METH_CASEID', 'HH9_REGION',
    grep('^HHC9_AGE_[1-9]$',                             hdr, value = TRUE),
    grep('^HHC9_METH_WEIGHT_[1-9]$',                     hdr, value = TRUE),
    grep('^HH9_CHILDREN_[1-9]_(?:[1-9]|1[0-4])$',        hdr, value = TRUE),
    grep('^HH9_REGCARE_[1-9]_(?:[1-9]|1[0-4])$',         hdr, value = TRUE),
    grep('^HH9_TYPEOFCARE_AGG_[1-9]_(?:[1-9]|1[0-4])$',  hdr, value = TRUE),
    grep('^HH9_CARELOC_[1-9]_(?:[1-9]|1[0-4])$',         hdr, value = TRUE),
    grep('^HH9_WEEKLY_PAYMENT_TC_[1-9]_(?:[1-9]|1[0-4])$', hdr, value = TRUE),
    grep('^HH9_PRIORREL_(?:[1-9]|1[0-4])$',              hdr, value = TRUE),
    grep('^HH9_HRS_WEEK_PROV[0-9_]+$',                   hdr, value = TRUE),
    grep('^HH9_HOURS_WEEK_PROV[0-9_]+$',                 hdr, value = TRUE))

  d <- fread(f, select = unique(want))
  setnames(d,
    old = grep('^HH9_HRS_WEEK_PROV[0-9]+_[0-9]+$', names(d), value = TRUE),
    new = str_replace(grep('^HH9_HRS_WEEK_PROV[0-9]+_[0-9]+$', names(d),
                           value = TRUE),
                      '^HH9_HRS_WEEK_PROV([0-9]+)_([0-9]+)$',
                      'HH9_HRS_WEEK_PROV_\\2_\\1'))
  setnames(d,
    old = grep('^HH9_HOURS_WEEK_PROV[0-9]+_[0-9]+$', names(d), value = TRUE),
    new = str_replace(grep('^HH9_HOURS_WEEK_PROV[0-9]+_[0-9]+$', names(d),
                           value = TRUE),
                      '^HH9_HOURS_WEEK_PROV([0-9]+)_([0-9]+)$',
                      'HH9_HRS_WEEK_PROV_\\2_\\1'))
  d
}



build_children <- function(d) {

  #----------------------------------------------------------------------------
  # One row per child: age, weight, region. Ages 0-4 only, matching the build.
  #
  # Params:
  #   - d (data.table)
  #
  # Returns: (tibble) hh_id, child_id, age, child_weight, region
  #----------------------------------------------------------------------------

  age_cols <- grep('^HHC9_AGE_[1-9]$',         names(d), value = TRUE)
  wt_cols  <- grep('^HHC9_METH_WEIGHT_[1-9]$', names(d), value = TRUE)

  long <- as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID, region = HH9_REGION,
           all_of(age_cols), all_of(wt_cols)) %>%
    pivot_longer(c(all_of(age_cols), all_of(wt_cols)),
                 names_to = 'variable', values_to = 'value') %>%
    mutate(name     = if_else(str_detect(variable, 'WEIGHT'),
                              'child_weight', 'age_months'),
           child_id = as.integer(str_sub(variable, start = -1))) %>%
    select(-variable) %>%
    pivot_wider()

  long %>%
    filter(!is.na(age_months), age_months >= 0) %>%
    mutate(age = floor(age_months / 12)) %>%
    filter(age <= 4, !is.na(child_weight), child_weight > 0) %>%
    mutate(region = REGIONS[as.character(region)]) %>%
    select(hh_id, child_id, age, child_weight, region)
}



build_arrangements <- function(d) {

  #----------------------------------------------------------------------------
  # One row per child x provider, classified into the simulator's care types.
  #
  # Classification is 1a_nsece_processing.R's, kept deliberately identical --
  # a different rule here would measure a different thing and the universe
  # difference would not be readable.
  #
  # Params:
  #   - d (data.table)
  #
  # Returns: (tibble) hh_id, child_id, provider_id, ecec_choice, hours
  #----------------------------------------------------------------------------

  xy <- function(p) grep(p, names(d), value = TRUE)

  prior <- as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID, all_of(xy('^HH9_PRIORREL_(?:[1-9]|1[0-4])$'))) %>%
    pivot_longer(-hh_id, names_to = 'provider_id',
                 names_pattern = 'HH9_PRIORREL_([0-9]+)$',
                 values_to = 'priorrel') %>%
    mutate(provider_id = as.integer(provider_id))

  as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID,
           all_of(xy('^HH9_CHILDREN_[1-9]_(?:[1-9]|1[0-4])$')),
           all_of(xy('^HH9_REGCARE_[1-9]_(?:[1-9]|1[0-4])$')),
           all_of(xy('^HH9_TYPEOFCARE_AGG_[1-9]_(?:[1-9]|1[0-4])$')),
           all_of(xy('^HH9_CARELOC_[1-9]_(?:[1-9]|1[0-4])$')),
           all_of(xy('^HH9_WEEKLY_PAYMENT_TC_[1-9]_(?:[1-9]|1[0-4])$')),
           all_of(xy('^HH9_HRS_WEEK_PROV_[1-9]_(?:[1-9]|1[0-4])$'))) %>%
    pivot_longer(-hh_id, names_to = c('name', 'child_id', 'provider_id'),
                 names_pattern = '(HH9_[A-Z0-9_]+)_([1-9])_([0-9]+)$',
                 values_to = 'value') %>%
    mutate(child_id = as.integer(child_id),
           provider_id = as.integer(provider_id)) %>%
    pivot_wider() %>%
    filter(HH9_CHILDREN == 1) %>%
    left_join(prior, by = c('hh_id', 'provider_id')) %>%
    transmute(hh_id, child_id, provider_id,
              regular  = HH9_REGCARE,
              type_raw = HH9_TYPEOFCARE_AGG,
              careloc  = HH9_CARELOC,
              priorrel,
              hours    = HH9_HRS_WEEK_PROV,
              cost     = HH9_WEEKLY_PAYMENT_TC) %>%
    mutate(
      hours = if_else(!is.na(hours) & hours >= 0, hours, NA_real_),
      cost  = if_else(!is.na(cost)  & cost  >= 0, cost,  NA_real_),
      is_paid      = !is.na(cost) & cost > 0,
      hourly       = if_else(!is.na(hours) & hours > 0, cost / hours, NA_real_),
      is_k8        = type_raw == 6,
      is_center    = type_raw == 4,
      is_home      = type_raw %in% c(1, 2, 3),
      in_child_home = !is.na(careloc) & careloc == 1,
      has_prior_rel = !is.na(priorrel) & priorrel == 1,
      is_home_based = is_home & !in_child_home & !has_prior_rel,
      is_other_kind = (!is_center & !is_k8) &
                        (!is_home | in_child_home | has_prior_rel),
      is_low  = is_center & is_paid & !is.na(hourly) & hourly <  PRICE_BOUNDARY,
      is_high = is_center & is_paid & !is.na(hourly) & hourly >= PRICE_BOUNDARY,
      ecec_choice = case_when(
        is_k8                              ~ 'K-8',
        is_center & !is_paid               ~ 'Unpaid Center-Based',
        is_center & is_paid & is_low       ~ 'Low-Priced Center-Based',
        is_center & is_paid & is_high      ~ 'High-Priced Center-Based',
        is_center & is_paid                ~ NA_character_,
        is_home_based & !is_paid           ~ 'Unpaid Home-Based',
        is_home_based &  is_paid           ~ 'Paid Home-Based',
        is_other_kind & !is_paid           ~ 'Other Unpaid',
        is_other_kind &  is_paid           ~ 'Other Paid',
        TRUE                               ~ NA_character_)) %>%
    filter(!is.na(regular), regular == 1, !is.na(ecec_choice),
           !is.na(hours), hours > 0) %>%
    select(hh_id, child_id, provider_id, ecec_choice, hours)
}



primary_arrangement <- function(arr) {

  #----------------------------------------------------------------------------
  # One row per child: primary care type, its hours, child's TOTAL
  # non-parental hours.
  #
  # K-8 dropped first. NSCH's question asks about care from someone other than
  # parent -- names centres, preschool, Head Start, family child care,
  # nannies, babysitters, and relatives, doesn't name school. Counting school
  # hours would put every school-age child over ten-hour line for wrong
  # reason.
  #
  # Params:
  #   - arr (tibble) from build_arrangements()
  #
  # Returns: (tibble) hh_id, child_id, ecec_choice, hours, total_hours
  #----------------------------------------------------------------------------

  arr %>%
    filter(ecec_choice != 'K-8') %>%
    group_by(hh_id, child_id, ecec_choice) %>%
    summarise(hours = sum(hours), .groups = 'drop') %>%
    group_by(hh_id, child_id) %>%
    mutate(total_hours = sum(hours)) %>%
    arrange(match(ecec_choice, CATEGORY_PRIORITY), desc(hours),
            .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
}



summarise_universe <- function(kids, label, all_children = NULL) {

  #----------------------------------------------------------------------------
  # The NSECE constants, on whatever universe `kids` holds.
  #
  # Params:
  #   - kids (tibble): child rows w/ ecec_choice, hours, child_weight, region
  #   - label (chr)
  #   - all_children (tibble|NULL): every child 0-4, in care or not. The centre
  #       price-band shares are shares of THAT denominator, not of children in
  #       care, which is how 00_main.R states them.
  #
  # Returns: (list) formal, mix, hours, bands (tibbles)
  #----------------------------------------------------------------------------

  w <- function(x, wt) sum(x * wt) / sum(wt)

  formal <- kids %>%
    filter(!is.na(region)) %>%
    group_by(region) %>%
    summarise(universe = label,
              children = n(),
              formal_share = w(ecec_choice %in% FORMAL, child_weight),
              .groups = 'drop')

  mix <- kids %>%
    filter(ecec_choice %in% INFORMAL) %>%
    group_by(ecec_choice) %>%
    summarise(universe = label, children = n(), wt = sum(child_weight),
              .groups = 'drop') %>%
    mutate(share = wt / sum(wt)) %>%
    select(universe, ecec_choice, children, share)

  hrs <- kids %>%
    group_by(ecec_choice) %>%
    summarise(universe = label,
              children = n(),
              weekly = w(hours, child_weight),
              .groups = 'drop') %>%
    mutate(annual = weekly * 50)

  # Centre price bands. Only the RATIO of the two enters the blended
  # Center-Based hours, so the denominator cancels there -- but 00_main.R and
  # the memo both print the shares themselves, so compute them as stated.
  bands <- NULL
  if (!is.null(all_children)) {
    denom <- sum(all_children$child_weight)
    bands <- kids %>%
      filter(ecec_choice %in% c('High-Priced Center-Based',
                                'Low-Priced Center-Based')) %>%
      group_by(ecec_choice) %>%
      summarise(universe = label, children = n(),
                share = sum(child_weight) / denom, .groups = 'drop')
  }

  # Unpaid Center-Based share of formal care, by region -- step 3. Weighted
  # children whose primary type is Unpaid Center-Based over weighted children
  # whose primary type is formal. Printed cross-check only -- step 3 uses
  # program counts, not this share.
  unpaid <- kids %>%
    filter(!is.na(region), ecec_choice %in% FORMAL) %>%
    group_by(region) %>%
    summarise(universe = label,
              children = n(),
              n_unpaid = sum(ecec_choice == 'Unpaid Center-Based'),
              unpaid_share = w(ecec_choice == 'Unpaid Center-Based', child_weight),
              .groups = 'drop')

  # Centre-home split of paid formal care, by region -- divides step 4's other
  # formal. Paid centres (both price bands) over paid centres + Paid
  # Home-Based. Unpaid Center-Based excluded: own share, step 3.
  PAID_CENTRE <- c('High-Priced Center-Based', 'Low-Priced Center-Based')
  split <- kids %>%
    filter(!is.na(region), ecec_choice %in% c(PAID_CENTRE, 'Paid Home-Based')) %>%
    group_by(region) %>%
    summarise(universe = label,
              n_centre = sum(ecec_choice %in% PAID_CENTRE),
              n_home   = sum(ecec_choice == 'Paid Home-Based'),
              centre_share = w(ecec_choice %in% PAID_CENTRE, child_weight),
              .groups = 'drop')

  # Part-time / full-time split, by region and type. THE SIMULATOR'S OWN CUT:
  # src/1_processing/1a_nsece_processing.R reads hours < PT_FT_CUT as part-time
  # over the primary arrangement's hours, so the build classifies the same way
  # and the two agree child for child.
  ptft_nat <- kids %>%
    mutate(ecec_choice = dplyr::if_else(ecec_choice %in% PAID_CENTRE,
                                        'Center-Based', ecec_choice),
           part_time = hours < PT_FT_CUT) %>%
    group_by(ecec_choice) %>%
    summarise(universe = label, children = n(),
              pt_share = w(part_time, child_weight), .groups = 'drop')

  ptft <- kids %>%
    filter(!is.na(region)) %>%
    mutate(ecec_choice = dplyr::if_else(ecec_choice %in% PAID_CENTRE,
                                        'Center-Based', ecec_choice),
           part_time = hours < PT_FT_CUT) %>%
    group_by(region, ecec_choice) %>%
    summarise(universe = label, children = n(),
              n_pt = sum(part_time),
              pt_share = w(part_time, child_weight), .groups = 'drop')

  # Regional versions of the informal mix and weekly hours -- same definitions
  # as above, grouped by Census region.
  mix_r <- kids %>%
    filter(!is.na(region), ecec_choice %in% INFORMAL) %>%
    group_by(region, ecec_choice) %>%
    summarise(universe = label, children = n(), wt = sum(child_weight),
              .groups = 'drop') %>%
    group_by(region) %>%
    mutate(share = wt / sum(wt)) %>%
    ungroup() %>%
    select(universe, region, ecec_choice, children, share)

  # Paid centres pooled into one Center-Based row -- no price-band blend.
  hrs_r <- kids %>%
    filter(!is.na(region)) %>%
    mutate(ecec_choice = dplyr::if_else(ecec_choice %in% PAID_CENTRE,
                                        'Center-Based', ecec_choice)) %>%
    group_by(region, ecec_choice) %>%
    summarise(universe = label, children = n(),
              weekly = w(hours, child_weight), .groups = 'drop')

  list(formal = formal, mix = mix, hours = hrs, bands = bands, split = split,
       mix_r = mix_r, hours_r = hrs_r, unpaid = unpaid, ptft = ptft,
       ptft_nat = ptft_nat)
}



main <- function() {

  #----------------------------------------------------------------------------
  # Entry point -- reads NSECE household file, builds children, arrangements,
  # primary arrangement, prints every regional constant 00_main.R carries,
  # writes long CSV of quantities by universe.
  #
  # Params: none -- reads --nsece <dir> (or env NSECE_ROOT; stops if neither),
  #   --out <file> (default nsece_universe.csv) from CLI
  #
  # Returns: (tibble) written quantities, invisibly
  #----------------------------------------------------------------------------

  root <- arg('--nsece', Sys.getenv('NSECE_ROOT'))
  if (!nzchar(root)) stop('pass --nsece <dir> or set NSECE_ROOT')
  out <- arg('--out', 'nsece_universe.csv')

  cat('reading ', root, '\n', sep = '')
  d <- read_household(root)
  cat('  households ', nrow(d), '\n', sep = '')

  kids <- build_children(d)
  cat('  children aged 0-4 ', nrow(kids), '\n', sep = '')

  arr <- build_arrangements(d)
  cat('  child x provider arrangements ', nrow(arr), '\n', sep = '')

  prim <- primary_arrangement(arr) %>% inner_join(kids, by = c('hh_id', 'child_id'))
  cat('  children 0-4 in non-parental care ', nrow(prim), '\n', sep = '')

  a <- summarise_universe(prim, 'A: NSECE, any regular care', kids)
  b <- summarise_universe(dplyr::filter(prim, total_hours >= 10),
                          'B: NSCH-matched, 10+ hrs/week', kids)

  cat('\n== children reaching the ten-hour line\n')
  cat(sprintf('  universe A %d children, universe B %d (%.1f%% kept)\n',
              nrow(prim), sum(prim$total_hours >= 10),
              100 * mean(prim$total_hours >= 10)))

  # THE TARGETS ARE DENOMINATED IN HOURS, not children, so report the share of
  # HOURS below the ten-hour line as well. Simulator's baseline includes
  # every regular arrangement on NSECE screener (five-hour floor), adds no
  # threshold of its own.
  hA <- sum(prim$child_weight * prim$hours)
  keep <- prim$total_hours >= 10
  hB <- sum(prim$child_weight[keep] * prim$hours[keep])
  cat(sprintf('  weighted primary-arrangement hours: A %.0f, B %.0f (%.1f%% kept)\n',
              hA, hB, 100 * hB / hA))
  cat(sprintf('  sub-ten-hour group: %.1f%% of children, %.1f%% of HOURS\n',
              100 * (1 - mean(keep)), 100 * (1 - hB / hA)))

  cat('\n== REGIONAL FORMAL SHARE (step 2)\n')
  cmp <- a$formal %>%
    select(region, n_A = children, A = formal_share) %>%
    left_join(b$formal %>% select(region, n_B = children, B = formal_share),
              by = 'region') %>%
    mutate(published = PUBLISHED_FORMAL[region],
           `A vs published` = A - published,
           `B - A` = B - A)
  print(as.data.frame(cmp %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
        row.names = FALSE)

  cat('\n== INFORMAL MIX -- divides the residual\n')
  print(as.data.frame(
    a$mix %>% select(ecec_choice, A = share) %>%
      left_join(b$mix %>% select(ecec_choice, B = share), by = 'ecec_choice') %>%
      mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)

  cat('\n== MEAN WEEKLY HOURS IN THE PRIMARY ARRANGEMENT\n')
  print(as.data.frame(
    a$hours %>% select(ecec_choice, n_A = children, weekly_A = weekly) %>%
      left_join(b$hours %>% select(ecec_choice, n_B = children, weekly_B = weekly),
                by = 'ecec_choice') %>%
      mutate(annual_A = weekly_A * 50, annual_B = weekly_B * 50,
             pct = 100 * (weekly_B / weekly_A - 1)) %>%
      mutate(across(where(is.numeric), ~round(.x, 2)))), row.names = FALSE)

  cat('\n== CENTRE PRICE-BAND SHARES -- weights for the blended centre hours\n')
  cat('   (shares of ALL children 0-4; NSECE publishes 0.07008 high, 0.06556 low)\n')
  bands <- a$bands %>% select(ecec_choice, n_A = children, A = share) %>%
    left_join(b$bands %>% select(ecec_choice, n_B = children, B = share),
              by = 'ecec_choice')
  print(as.data.frame(bands %>% mutate(across(where(is.numeric), ~round(.x, 5)))),
        row.names = FALSE)

  blend <- function(tb, col) {
    h <- setNames(a$hours$weekly * 50, a$hours$ecec_choice)
    if (col == 'B') h <- setNames(b$hours$weekly * 50, b$hours$ecec_choice)
    s <- setNames(tb[[col]], tb$ecec_choice)
    (s[['High-Priced Center-Based']] * h[['High-Priced Center-Based']] +
     s[['Low-Priced Center-Based']]  * h[['Low-Priced Center-Based']]) /
      (s[['High-Priced Center-Based']] + s[['Low-Priced Center-Based']])
  }
  cat(sprintf('   blended Center-Based annual hours: A %.1f   B %.1f  (%+.1f%%)\n',
              blend(bands, 'A'), blend(bands, 'B'),
              100 * (blend(bands, 'B') / blend(bands, 'A') - 1)))

  cat('\n== CENTRE-HOME SPLIT OF PAID FORMAL CARE, by region (step 4, universe A)\n')
  print(as.data.frame(
    a$split %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
    row.names = FALSE)

  cat('\n== PART-TIME SHARE (hours < ', PT_FT_CUT, '), by region and type\n', sep = '')
  print(as.data.frame(
    a$ptft %>% select(region, ecec_choice, n_A = children, pt_A = n_pt, A = pt_share) %>%
      left_join(b$ptft %>% select(region, ecec_choice, n_B = children, B = pt_share),
                by = c('region', 'ecec_choice')) %>%
      mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)

  cat('\n== UNPAID CENTER-BASED SHARE OF FORMAL CARE, by region (step 3)\n')
  print(as.data.frame(
    a$unpaid %>% select(region, n_A = children, unpaid_A = n_unpaid, A = unpaid_share) %>%
      left_join(b$unpaid %>% select(region, n_B = children, unpaid_B = n_unpaid,
                                    B = unpaid_share), by = 'region') %>%
      mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)

  cat('\n== INFORMAL MIX, by region (universe A)\n')
  print(as.data.frame(a$mix_r %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
        row.names = FALSE)
  cat('\n== WEEKLY HOURS, by region (universe A)\n')
  print(as.data.frame(a$hours_r %>% mutate(across(where(is.numeric), ~round(.x, 2)))),
        row.names = FALSE)

  b_kids <- dplyr::filter(prim, total_hours >= 10)

  # POOLED NATIONAL SHARES, for BOTH universes. Reference rows only -- build
  # reads regional cells.
  pooled_national <- function(kids, label) {
    paid_formal <- dplyr::filter(kids, ecec_choice %in%
                                 c('High-Priced Center-Based',
                                   'Low-Priced Center-Based', 'Paid Home-Based'))
    formal_kids <- dplyr::filter(kids, ecec_choice %in% FORMAL)
    tibble::tibble(
      universe = label,
      quantity = c('formal_share', 'centre_home_split', 'unpaid_centre_share'),
      key = 'national',
      value = c(sum(kids$child_weight[kids$ecec_choice %in% FORMAL]) /
                  sum(kids$child_weight),
                sum(paid_formal$child_weight[paid_formal$ecec_choice %in%
                       c('High-Priced Center-Based', 'Low-Priced Center-Based')]) /
                  sum(paid_formal$child_weight),
                sum(formal_kids$child_weight[formal_kids$ecec_choice ==
                       'Unpaid Center-Based']) / sum(formal_kids$child_weight)),
      n = c(nrow(kids), nrow(paid_formal), nrow(formal_kids)))
  }

  national_structure <- dplyr::bind_rows(
    pooled_national(prim,   'A: NSECE, any regular care'),
    pooled_national(b_kids, 'B: NSCH-matched, 10+ hrs/week'))

  # THE UNIVERSE RATIO. NSCH k6q20 counts children at 10+ hrs/week, but the
  # simulator's baseline holds every regular arrangement on NSECE screener
  # (five-hour floor).
  # Step 1 scales the NSCH control total by this, so the build targets the
  # simulator's own population.
  universe_ratio <- tibble::tibble(
    universe = 'A: NSECE, any regular care',
    quantity = 'children_all_over_10plus',
    key = 'national',
    value = sum(prim$child_weight) / sum(b_kids$child_weight),
    n = nrow(prim))

  res <- bind_rows(
    national_structure,
    universe_ratio,
    a$ptft_nat %>% transmute(universe, quantity = 'part_time_share',
                             key = ecec_choice, value = pt_share, n = children),
    b$ptft_nat %>% transmute(universe, quantity = 'part_time_share',
                             key = ecec_choice, value = pt_share, n = children),
    a$ptft %>% transmute(universe, quantity = 'part_time_share_region',
                         key = paste(region, ecec_choice, sep = ':'),
                         value = pt_share, n = children),
    b$ptft %>% transmute(universe, quantity = 'part_time_share_region',
                         key = paste(region, ecec_choice, sep = ':'),
                         value = pt_share, n = children),
    a$mix_r  %>% transmute(universe, quantity = 'informal_mix_region',
                           key = paste(region, ecec_choice, sep = ':'),
                           value = share, n = children),
    a$hours_r %>% transmute(universe, quantity = 'weekly_hours_region',
                            key = paste(region, ecec_choice, sep = ':'),
                            value = weekly, n = children),
    b$mix_r  %>% transmute(universe, quantity = 'informal_mix_region',
                           key = paste(region, ecec_choice, sep = ':'),
                           value = share, n = children),
    b$hours_r %>% transmute(universe, quantity = 'weekly_hours_region',
                            key = paste(region, ecec_choice, sep = ':'),
                            value = weekly, n = children),
    a$split  %>% transmute(universe, quantity = 'centre_home_split',
                           key = region, value = centre_share,
                           n = n_centre + n_home),
    b$split  %>% transmute(universe, quantity = 'centre_home_split',
                           key = region, value = centre_share,
                           n = n_centre + n_home),
    a$unpaid %>% transmute(universe, quantity = 'unpaid_centre_share',
                           key = region, value = unpaid_share, n = children),
    b$unpaid %>% transmute(universe, quantity = 'unpaid_centre_share',
                           key = region, value = unpaid_share, n = children),
    a$bands  %>% transmute(universe, quantity = 'price_band_share',
                           key = ecec_choice, value = share, n = children),
    b$bands  %>% transmute(universe, quantity = 'price_band_share',
                           key = ecec_choice, value = share, n = children),
    a$formal %>% transmute(universe, quantity = 'formal_share',
                           key = region, value = formal_share, n = children),
    b$formal %>% transmute(universe, quantity = 'formal_share',
                           key = region, value = formal_share, n = children),
    a$mix    %>% transmute(universe, quantity = 'informal_mix',
                           key = ecec_choice, value = share, n = children),
    b$mix    %>% transmute(universe, quantity = 'informal_mix',
                           key = ecec_choice, value = share, n = children),
    a$hours  %>% transmute(universe, quantity = 'weekly_hours',
                           key = ecec_choice, value = weekly, n = children),
    b$hours  %>% transmute(universe, quantity = 'weekly_hours',
                           key = ecec_choice, value = weekly, n = children))

  write_csv(res, out)
  cat('\nwrote ', out, '\n', sep = '')
  invisible(res)
}


if (sys.nframe() == 0) main()
