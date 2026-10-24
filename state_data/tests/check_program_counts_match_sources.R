#-----------------------------------------------------------------------------
# Do the step 4 program counts in states.R match their public sources?
#
# Every count is hand-transcribed into a spec file -- a typo or stale snapshot
# would reach build, memo unchallenged. One source per program, compared
# directly:
#
#   public pre-K       = CCD 2018-19 pre-K membership, summed from the county
#                        file (NYC: the five boroughs). CA has no CCD rows, MI
#                        reports none -- specs cite NIEER, reported, not checked.
#   Head Start         = NIEER State of Preschool 2019, Appendix B: federal +
#                        AIAN + Migrant & Seasonal funded slots, ages 3 and 4.
#                        NIEER rounds each cell, so +/- 2 slots is allowed.
#   Early Head Start   = NIEER State(s) of Head Start and Early Head Start
#                        (2023), state profile, funded-enrollment chart,
#                        2018-2019 bar -- smaller of the two bars.
#   NYC shares         = NYC's share of New York's OPEN funded slots in the
#                        Head Start service-location file (states.R), HS types
#                        and EHS types separately.
#
# Every source is a public file in raw/US/.
#
#   Rscript state_data/tests/check_program_counts_match_sources.R
#
# Exit status is 1 on any mismatch or a source row it cannot find.
#-----------------------------------------------------------------------------
suppressPackageStartupMessages({library(readr); library(dplyr)})

# main() is guarded; this only loads
source('state_data/code/00_build_demand_targets.R')

DOC      <- file.path('resources', 'nieer')
NIEER    <- file.path(DOC, 'US_NIEER__state_of_preschool_yearbook__2019.pdf')
NIEER_HS <- file.path(DOC, 'US_NIEER__states_of_head_start_and_early_head_start__2023.pdf')
CCD      <- raw_path('US_CCD__public_school_prekindergarten_children_by_county__2018-19.csv')
HSSL     <- raw_path('US_HSSL__head_start_funded_slots_by_service_location__2026-09-23.csv')

HS_TYPES  <- c('1', '3', '5')    # Head Start types (as states.R)
EHS_TYPES <- c('2', '4', '6')
NYC_COUNTIES <- paste0(load_spec('NYC')$counties, ' County')   # states.R
NIEER_TOL <- 2
BAR_POINTS <- 25                 # bar label within this many points of axis label
STATE_NAME <- c(setNames(state.name, state.abb), DC = 'District of Columbia')

for (f in c(NIEER, NIEER_HS, CCD, HSSL)) {
  if (!file.exists(f)) { cat('MISSING SOURCE: ', f, '\n', sep = ''); quit(status = 1) }
}



pdf_row <- function(pages, state) {

  #----------------------------------------------------------------------------
  # Integers on state's row of a PDF table, after its first dollar figure,
  # percent cells dropped. Row matched on full name at line start --
  # 'Virginia' never takes 'West Virginia'.
  #
  # Params: pages (chr): text per page, state (chr): full name
  # Returns: (num) NULL if no row
  #----------------------------------------------------------------------------

  l <- unlist(strsplit(pages, '\n'))
  hit <- grep(paste0('^\\s*', state, '\\s+\\$'), l, value = TRUE)
  if (length(hit) != 1) return(NULL)
  cells <- strsplit(trimws(sub(paste0('^\\s*', state, '\\s+'), '', hit)), '\\s{2,}|\\s+(?=\\$)',
                    perl = TRUE)[[1]]
  cells <- cells[-1]                                  # the first dollar figure
  cells <- cells[!grepl('%$', cells) & !grepl('^\\$', cells)]
  as.numeric(gsub(',', '', cells))
}



ehs_bar <- function(state) {

  #----------------------------------------------------------------------------
  # Early Head Start 2018-19 funded enrollment, read off state profile's
  # funded-enrollment bar chart by word position: labels above '2018-2019'
  # axis tick, between chart title and axis. Two bars per year -- Head Start
  # always larger, so EHS = smaller label.
  #
  # Params: state (chr): full name. Returns: (num) NA if page or bar not found
  #----------------------------------------------------------------------------

  key <- paste0('STATEPROFILE:', toupper(gsub(' ', '', state)), '2020-2021')
  i <- which(startsWith(hs_hdr, key))[1]
  if (is.na(i)) return(NA)
  d <- hs_dat[[i]]
  k <- which(d$text == 'Funded' & c(d$text[-1], '') == 'Enrollment')[1]
  if (is.na(k)) return(NA)
  ax <- d[d$text == '2018-2019' & d$y > d$y[k], ]
  if (nrow(ax) == 0) return(NA)
  ax <- ax[order(ax$y, ax$x), ][1, ]
  n <- d[grepl('^[0-9][0-9,]*$', d$text) & d$y > d$y[k] & d$y < ax$y, ]
  n <- n[abs(n$x + n$width / 2 - (ax$x + ax$width / 2)) < BAR_POINTS, ]
  v <- as.numeric(gsub(',', '', n$text))
  if (length(v) != 2) return(NA)
  min(v)
}


nieer <- pdftools::pdf_text(NIEER)
nieer_p <- nieer[grepl('FEDERAL HEAD START', nieer) &
                 grepl('Funded enrollment', nieer)]
hs_txt <- pdftools::pdf_text(NIEER_HS)
hs_dat <- pdftools::pdf_data(NIEER_HS)
hs_hdr <- gsub('\\s', '', substr(hs_txt, 1, 200))   # 'S TAT E P R O F I L E' spaced out

ccd <- read_chr(CCD)
ccd$n <- num(ccd$pk_enrollment)

hssl <- read_chr(HSSL)
hssl <- hssl[hssl$status == 'Open' & hssl$state == 'NY', ]
hssl$slots <- num(hssl$funded_slots)



# OPEN funded NY slots of program_type `types`: c(city = NYC counties, state)
share_nyc <- function(types) {
  h <- hssl[hssl$program_type %in% types, ]
  c(city = sum(h$slots[h$county %in% NYC_COUNTIES]), state = sum(h$slots))
}



# One [ok]/[FAIL] line, spec count `have` vs source `want`; failure bumps `fails`
say <- function(geo, what, have, want, ok = have == want) {
  cat(sprintf('  %s %-4s %-18s spec %9s   source %9s\n', if (ok) '[ok]  ' else '[FAIL]',
              geo, what, format(have, big.mark = ','), format(want, big.mark = ',')))
  if (!ok) fails <<- fails + 1
}

fails <- 0
for (g in GEOGRAPHIES) {
  s <- load_spec(g)
  p <- setNames(s$programs$n_children, s$programs$program)
  prek <- unname(p[grep('pre-K', names(p))])

  if (!is.null(s$counties)) {
    # NYC -- county-defined, shares of New York's counts
    want <- sum(ccd$n[ccd$state == 'NY' & ccd$county_name %in% NYC_COUNTIES])
    say(g, 'public pre-K', prek, want)
    ny <- load_spec('NY')
    nyp <- setNames(ny$programs$n_children, ny$programs$program)
    hs  <- share_nyc(HS_TYPES);  ehs <- share_nyc(EHS_TYPES)
    want <- round(nyp[['Head Start']] * hs[['city']] / hs[['state']])
    say(g, 'Head Start', p[['Head Start']], want)
    want <- round(nyp[['Early Head Start']] * ehs[['city']] / ehs[['state']])
    say(g, 'Early Head Start', p[['Early Head Start']], want)
    next
  }

  # CA has no CCD rows; MI's rows all read 0 ('Not reported') -- both specs
  # cite NIEER state pre-K, so both reported, not checked.
  if (sum(ccd$n[ccd$state == g], na.rm = TRUE) > 0) {
    say(g, 'public pre-K', prek, sum(ccd$n[ccd$state == g]))
  } else {
    cat(sprintf('  [--]   %-4s %-18s spec %9s   no CCD count, spec cites NIEER\n',
                g, 'public pre-K', format(prek, big.mark = ',')))
  }

  nm <- STATE_NAME[[g]]
  nr <- pdf_row(nieer_p, nm)
  if (is.null(nr) || length(nr) < 2) {
    cat('  [FAIL]', g, 'row not found in NIEER Appendix B\n')
    fails <- fails + 1
  } else {
    say(g, 'Head Start', p[['Head Start']], sum(nr), ok = abs(p[['Head Start']] - sum(nr)) <= NIEER_TOL)
  }

  want <- ehs_bar(nm)
  if (is.na(want)) {
    cat('  [FAIL]', g, '2018-19 bar not found in NIEER State(s) of Head Start\n')
    fails <- fails + 1
  } else {
    say(g, 'Early Head Start', p[['Early Head Start']], want)
  }
}

if (fails == 0) {
  cat('\nevery program count matches its public source\n')
} else {
  cat('\n', fails, ' program count(s) do not match their source.\n', sep = '')
  quit(status = 1)
}
