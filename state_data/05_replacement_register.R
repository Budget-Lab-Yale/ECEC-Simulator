#------------------------------------------------------------------------------
# 05_replacement_register.R -- what NSECE supplies, and what could replace it.
#
# NSECE is 2019-only, licensed, and carries no state identifier. Every
# behavioural share in the build is therefore regional. This enumerates the
# job: one row per constant the build reads, what it feeds, the geography it
# has today, and the state of the search for a state-level replacement.
#
# THE CONSTANT LIST IS DERIVED, NOT TRANSCRIBED. It comes from universe A of
# out/nsece_universe.csv -- the same file 00_main.R's constants are checked
# against by tests/check_constants_match_derivation.R. A constant added to the
# build appears here automatically, with no candidate, and the script says so.
#
# STATUS VALUES:
#   tested-no       tested against the source; cannot carry this constant
#   tested-partial  tested; carries the constant's type but not its quantity
#   untested        candidate named, not yet tested
#   blocked         source identified, retrieval blocked
#
# Usage:
#   Rscript state_data/05_replacement_register.R
#   Rscript state_data/05_replacement_register.R --check
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble)
})

STATE_DATA_ROOT <- 'state_data'
CSV <- file.path(STATE_DATA_ROOT, 'out', 'nsece_universe.csv')
OUT <- file.path(STATE_DATA_ROOT, 'out', 'replacement_register.csv')
UNIVERSE_A <- 'A: NSECE, any regular care'

# THE SIPP EVIDENCE. Distinct children 0-4 by state and care type, pooled over
# the 2018-2023 panels, counted on the cluster from
# shared/raw_data/SIPP/<year>/pu<year>.csv. Held here because SIPP is public;
# the count is the thing that decides whether a state-level split is possible.
SIPP_CELLS <- file.path(STATE_DATA_ROOT, 'raw',
                        'US_SIPP__children_0_4_by_care_type_and_state__2018-2023.csv')
CELLS_OUT  <- file.path(STATE_DATA_ROOT, 'out', 'replacement_register_cells.csv')

# SIPP's care flags against the build's six types. SIPP splits centre care into
# a child care centre and a nursery/preschool, and has no paid/unpaid
# distinction per arrangement, so this is a mapping of CONVENIENCE for counting
# records -- not a proposed replacement rule.
SIPP_TO_TYPE <- c(RDAYCARE = 'Center-Based',
                  RNUR     = 'Center-Based',
                  RHEADST  = 'Unpaid Center-Based',
                  RFAM     = 'Paid Home-Based',
                  RNREL    = 'Other Paid',
                  RGRAN    = 'Other Unpaid',
                  RSIB15   = 'Other Unpaid',
                  ROTHR    = 'Other Unpaid',
                  RPROG    = 'Other Paid')



# WHAT EACH QUANTITY FEEDS, and where the replacement search stands. One row
# per quantity in universe A. `constant` names the object in 00_main.R.
#
# Every `status` below records a test that was run, or says plainly that none
# was. Nothing here is an expectation.
REGISTER <- tibble::tribble(
  ~quantity,                  ~constant,                   ~feeds,

  'children_all_over_10plus', 'NSECE_UNIVERSE_RATIO',      'step 1 -- carries the NSCH rate from the 10-hour universe up to any regular care',
  'formal_share',             'NSECE_FORMAL_SHARE',        'step 2 -- splits children into formal and informal',
  'unpaid_centre_share',      'NSECE_UNPAID_CENTRE_SHARE', 'step 3 -- cross-check only; the build uses programme counts',
  'centre_home_split',        'NSECE_CENTRE_HOME_SPLIT',   'step 4 -- splits the rest of formal care into centres and homes',
  'informal_mix',             'NSECE_RESIDUAL_MIX',        'step 5 -- national fallback for a thin regional cell',
  'informal_mix_region',      'NSECE_MIX_REGION',          'step 5 -- splits informal care three ways',
  'weekly_hours',             'NSECE_ANNUAL_HOURS',        'step 6 -- national fallback hours per child',
  'weekly_hours_region',      'NSECE_HOURS_REGION',        'step 6 -- reference hours; superseded by the grid for the target',
  'part_time_share',          'NSECE_PART_TIME',           'step 6 -- national fallback part-time share',
  'part_time_share_region',   'NSECE_PART_TIME_REGION',    'step 6 -- places each child at 10 or 40 hrs/week on the simulator grid',
  'price_band_share',         'NSECE_ANNUAL_HOURS',        'step 6 -- blends the two paid-centre price bands into one Center-Based row')


# THE SEARCH. Keyed on what the constant IS, not on which quantity it is, so
# the same evidence is not restated per row.
#
#   share   a split of children between care types
#   hours   a quantity of time per child
#   ratio   a universe correction
KIND <- c(children_all_over_10plus = 'ratio',
          formal_share             = 'share',
          unpaid_centre_share      = 'share',
          centre_home_split        = 'share',
          informal_mix             = 'share',
          informal_mix_region      = 'share',
          weekly_hours             = 'hours',
          weekly_hours_region      = 'hours',
          part_time_share          = 'hours',
          part_time_share_region   = 'hours',
          price_band_share         = 'share')

CANDIDATE <- c(
  share = 'SIPP 2018-2023 (Bouchet: shared/raw_data/SIPP)',
  hours = 'none identified',
  ratio = 'SIPP 2018-2023, or NHES-ECPP')

CANDIDATE_GEO <- c(share = 'state, all 51',
                   hours = '--',
                   ratio = 'state, all 51')

STATUS <- c(share = 'tested-partial',
            hours = 'tested-no',
            ratio = 'untested')

NOTE <- c(
  share = 'SIPP carries care type per child and identifies every state. It cannot carry payment per arrangement (EPAY is one household answer), so a paid/unpaid split is approximate. Cell sizes bind: see out/replacement_register_cells.csv.',
  hours = 'SIPP has NO hours variable. Its whole care battery is yes/no flags; the only quantity is TPAYWK, a weekly amount the reference parent pays for all children together. NSCH k6q20 is binary at 10 hrs/week. NHES-ECPP has hours but state only in restricted-use.',
  ratio = 'Needs an hours variable to define the 10-hour cut, so it inherits the hours problem. Left open.')



main <- function() {

  #----------------------------------------------------------------------------
  # Builds the register and writes it. --check compares the quantity list in
  # the register to universe A and fails on any difference either way.
  #
  # Returns: (tibble) invisibly
  #----------------------------------------------------------------------------

  check_only <- '--check' %in% commandArgs(TRUE)

  d <- read_csv(CSV, col_types = cols(.default = col_character()))
  d$value <- as.numeric(d$value)
  d$n     <- as.numeric(d$n)
  a <- d[d$universe == UNIVERSE_A, ]

  # What the derivation actually holds, per quantity. GEOGRAPHY COMES FROM THE
  # KEYS, not from the quantity name -- `formal_share` is keyed by region and
  # carries no suffix saying so, and `informal_mix` is keyed by care type and
  # is national. Naming would get both wrong.
  REGIONS <- c('northeast', 'midwest', 'south', 'west')

  geography_of <- function(keys) {
    if (all(grepl(':', keys, fixed = TRUE))) return('Census region x type')
    if (all(keys %in% REGIONS))              return('Census region')
    if (identical(keys, 'national'))         return('national')
    'national, by care type'
  }

  shape <- a %>%
    group_by(quantity) %>%
    summarise(cells = n(),
              min_records = min(n),
              max_records = max(n),
              geography_now = geography_of(key),
              .groups = 'drop')

  missing <- setdiff(shape$quantity, REGISTER$quantity)
  extra   <- setdiff(REGISTER$quantity, shape$quantity)

  if (length(missing) || length(extra)) {
    if (length(missing))
      cat('QUANTITY IN THE DERIVATION WITH NO REGISTER ROW: ',
          paste(missing, collapse = ', '), '\n', sep = '')
    if (length(extra))
      cat('REGISTER ROW FOR A QUANTITY THE DERIVATION DOES NOT HOLD: ',
          paste(extra, collapse = ', '), '\n', sep = '')
    if (check_only) quit(status = 1)
  }

  reg <- REGISTER %>%
    left_join(shape, by = 'quantity') %>%
    mutate(kind            = unname(KIND[quantity]),
           candidate       = unname(CANDIDATE[kind]),
           candidate_geography = unname(CANDIDATE_GEO[kind]),
           status          = unname(STATUS[kind]),
           note            = unname(NOTE[kind])) %>%
    select(quantity, constant, feeds, geography_now, cells, min_records,
           max_records, kind, candidate, candidate_geography, status, note)

  if (check_only) {
    if (length(missing) || length(extra)) quit(status = 1)
    cat('register covers every universe-A quantity (', nrow(reg), ' rows)\n',
        sep = '')
    return(invisible(reg))
  }

  write_csv(reg, OUT)

  cat('== REPLACEMENT REGISTER --', nrow(reg), 'NSECE constants\n\n')
  print(as.data.frame(reg %>% select(quantity, geography_now, cells,
                                     min_records, kind, status)),
        row.names = FALSE)

  cat('\n== by status\n')
  print(as.data.frame(reg %>% count(kind, status, candidate)), row.names = FALSE)

  cat('\nwrote', OUT, '\n')

  sipp_cells()

  invisible(reg)
}



sipp_cells <- function() {

  #----------------------------------------------------------------------------
  # Could SIPP carry a state-level care-type split? Counts distinct SIPP
  # children per build geography and care type, and compares each cell to
  # NSECE_MIN_RECORDS -- the same threshold the build already applies to a thin
  # regional NSECE cell. A source cannot replace NSECE at state level on cells
  # NSECE itself would reject.
  #
  # Writes out/replacement_register_cells.csv.
  #
  # Returns: (tibble) invisibly
  #----------------------------------------------------------------------------

  if (!file.exists(SIPP_CELLS)) {
    cat('\n(no SIPP evidence at', SIPP_CELLS, '-- skipping cell sizes)\n')
    return(invisible(NULL))
  }

  source(file.path(STATE_DATA_ROOT, '00_main.R'))   # main() is guarded

  # The whole states the build writes. NYC is county-defined, and SIPP carries
  # no county, so it cannot appear.
  geos <- setdiff(GEOGRAPHIES, 'NYC')

  s <- read_csv(SIPP_CELLS, col_types = cols(state = col_character(),
                                             care_flag = col_character(),
                                             distinct_children = col_double()))

  cells <- s %>%
    filter(state %in% geos, care_flag %in% names(SIPP_TO_TYPE)) %>%
    mutate(ecec_type = unname(SIPP_TO_TYPE[care_flag])) %>%
    group_by(state, ecec_type) %>%
    summarise(children = sum(distinct_children), .groups = 'drop') %>%
    tidyr::complete(state = geos, ecec_type = ECEC_TYPES,
                    fill = list(children = 0)) %>%
    mutate(meets_min = children >= NSECE_MIN_RECORDS)

  write_csv(cells, CELLS_OUT)

  wide <- cells %>%
    select(state, ecec_type, children) %>%
    tidyr::pivot_wider(names_from = ecec_type, values_from = children)

  cat('\n== SIPP distinct children 0-4, 2018-2023, by build geography\n')
  cat('   the build rejects an NSECE cell under', NSECE_MIN_RECORDS,
      'records; same bar here\n')

  # A type no SIPP flag maps to reads zero. That is a gap in what SIPP ASKS,
  # not a count of children, and the two must not be confused.
  unmapped <- setdiff(ECEC_TYPES, unname(SIPP_TO_TYPE))
  if (length(unmapped)) {
    cat('   NOTE: ', paste(unmapped, collapse = ', '),
        ' reads 0 because SIPP has no flag for it --\n',
        '   SIPP does not ask whether home-based care is paid.\n', sep = '')
  }
  cat('\n')
  print(as.data.frame(wide), row.names = FALSE)

  pass <- cells %>%
    group_by(state) %>%
    summarise(types_at_or_above_min = sum(meets_min), .groups = 'drop') %>%
    arrange(types_at_or_above_min)

  cat('\n== care types per state at or above', NSECE_MIN_RECORDS, 'records',
      '(of', length(ECEC_TYPES), ')\n\n')
  print(as.data.frame(pass), row.names = FALSE)

  cat('\n', sum(pass$types_at_or_above_min), ' of ',
      nrow(pass) * length(ECEC_TYPES),
      ' state x type cells clear the bar.\n', sep = '')
  cat('wrote', CELLS_OUT, '\n')
  invisible(cells)
}



if (sys.nframe() == 0) main()
