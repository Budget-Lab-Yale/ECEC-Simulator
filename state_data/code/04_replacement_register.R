#------------------------------------------------------------------------------
# 04_replacement_register.R -- what NSECE supplies, and what could replace it.
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
#   tested-no          tested against the source; cannot carry this constant
#   tested-no-at-state tested; the source carries the quantity, but its finest
#                      geography is the Census region, so it does not replace
#                      NSECE where NSECE actually fails
#   tested-partial     tested; carries the constant's type but not its quantity
#   untested           candidate named, not yet tested
#   blocked            source identified, retrieval blocked
#
# Usage:
#   Rscript state_data/code/04_replacement_register.R
#   Rscript state_data/code/04_replacement_register.R --check
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

# THE NHES EVIDENCE. Regional part-time shares and hours universes, derived on
# the cluster. check_nsece_against_nhes.R asserts the universe ratio against
# it; hours_note() fills the register's own figures from it, so the register
# cannot quote a number the file does not hold.
NHES <- file.path(STATE_DATA_ROOT, 'raw',
                  'US_NHES__ecpp_children_0_4_by_hours_universe__2019.csv')

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



# WHAT EACH QUANTITY FEEDS, where replacement search stands. One row per
# quantity in universe A. `constant` names object in 00_main.R.
#
# Every `status` below records test run, or says plainly none was. Nothing
# here an expectation.
REGISTER <- tibble::tribble(
  ~quantity,                  ~constant,                   ~feeds,

  'children_all_over_10plus', 'NSECE_UNIVERSE_RATIO',      'step 1 -- carries the NSCH rate from the 10-hour universe up to any regular care',
  'formal_share',             'NSECE_FORMAL_SHARE',        'step 2 -- splits children into formal and informal',
  'unpaid_centre_share',      'NSECE_UNPAID_CENTRE_SHARE', 'step 4 -- cross-check only; the build uses program counts',
  'centre_home_split',        'NSECE_CENTRE_HOME_SPLIT',   'step 5 -- splits the rest of formal care into centres and homes',
  'informal_mix',             'NSECE_RESIDUAL_MIX',        'step 3 -- national fallback for a thin regional cell',
  'informal_mix_region',      'NSECE_MIX_REGION',          'step 3 -- splits informal care three ways',
  'weekly_hours',             'NSECE_ANNUAL_HOURS',        'step 6 -- national fallback hours per child',
  'weekly_hours_region',      'NSECE_HOURS_REGION',        'step 6 -- reference hours; superseded by the grid for the target',
  'part_time_share',          'NSECE_PART_TIME',           'step 6 -- national fallback part-time share',
  'part_time_share_region',   'NSECE_PART_TIME_REGION',    'step 6 -- places each child at 10 or 40 hrs/week on the simulator grid',
  'price_band_share',         'NSECE_ANNUAL_HOURS',        'step 6 -- blends the two paid-centre price bands into one Center-Based row')


# THE PART-TIME / FULL-TIME MARKER. Simulator places every child at 10 or
# 40 hrs/week, nothing between -- build states its target on that grid.
# Replacement unable to say which side of cut a child sits on can't feed
# grid, whatever else it supplies:
#
#   - HOURS source w/out marker gives a mean, a mean implies wrong child
#     count once put on two-point grid;
#   - PRICE source w/out marker prices part-time child at full-time rate,
#     or reverse, error runs straight into equilibrium -- price drives it.
#
# NSECE supplies marker: 1a_nsece_processing.R cuts at 20 hrs/week, build
# uses same cut. Any candidate checked against this first.
NEEDS_PT_FT <- c(children_all_over_10plus = FALSE,
                 formal_share             = FALSE,
                 unpaid_centre_share      = FALSE,
                 centre_home_split        = FALSE,
                 informal_mix             = FALSE,
                 informal_mix_region      = FALSE,
                 weekly_hours             = TRUE,
                 weekly_hours_region      = TRUE,
                 part_time_share          = TRUE,
                 part_time_share_region   = TRUE,
                 price_band_share         = TRUE)


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
  hours = 'NHES-ECPP 2019 -- Census region only; state needs the IES restricted-use licence',
  ratio = 'NHES-ECPP 2019 (has the hours the cut needs)')

CANDIDATE_GEO <- c(share = 'state, all 51',
                   hours = 'Census region (public-use); state restricted',
                   ratio = 'Census region (public-use); state restricted')

STATUS <- c(share = 'tested-partial',
            hours = 'tested-no-at-state',
            ratio = 'tested-no-at-state')

NOTE <- c(
  share = 'SIPP carries care type per child and identifies every state. It cannot carry payment per arrangement (EPAY is one household answer), so a paid/unpaid split is approximate. Cell sizes bind: see out/replacement_register_cells.csv.',
  # The part-time shares in this note are FILLED FROM DISK by hours_note(),
  # not typed. They were typed once, which made the register a third place the
  # same four numbers had to be edited, and the only one nothing checked.
  hours = 'NHES-ECPP 2019 public-use has real hours per arrangement (CAREHOURX, CPHRS, RCHRS, NCHRS) for %d children, and gives a part-time share at the build\'s own 20 hrs/week cut: %s. But its finest geography is CENREG, four Census regions -- the SAME wall as NSECE. State exists only in the IES restricted-use file, which needs a licence. SIPP has no hours at all; NSCH k6q20 is binary at 10 hrs/week; CCDF Table 14 is state x hours but covers subsidy children only and acf.gov is bot-blocked (202, empty body) from both Windows and Bouchet.',
  ratio = 'Needs an hours variable to define the 10-hour cut. NHES-ECPP has one, but only at Census region, so the ratio cannot be made state-specific either.')


# PER-QUANTITY OVERRIDE. A constant whose search differs from its kind's takes
# its row from here. price_band_share is the only one so far: it is a `share`
# by shape, but the thing that would replace it is a PRICE source, and the
# price evidence is its own.
OVERRIDE <- tibble::tribble(
  ~quantity, ~candidate, ~candidate_geography, ~status, ~note,

  'price_band_share',
  'DOL Women\'s Bureau National Database of Childcare Prices (NDCP), 2008-2022',
  'county, 47 states',
  'tested-partial',
  paste(
    'NDCP gives median and 75th-percentile prices by county, provider type',
    '(center / family) and age, and is fetchable from Bouchet (www.dol.gov is',
    '403 from Windows, 200 there). BUT every price is a FULL-TIME WEEKLY price',
    'and there is NO part-time series, so the grid\'s 10 hrs/week point has no',
    'NDCP price. Worse for a state build, "full-time" is each state\'s own',
    'definition and is not a fixed number of hours: ME reports 21-30 hrs most',
    'frequent, IN 35, MI 45, NJ 51.7, and several states let each provider',
    'decide. Using NDCP across states would import variation in the DEFINITION',
    'of full-time as if it were variation in price. Hourly-reporting states are',
    'converted at x40 hrs/week, which matches the simulator\'s full-time point',
    'only by coincidence. Technical report in raw/documentation/.'))



hours_note <- function() {

  #----------------------------------------------------------------------------
  # The `hours` note, with its figures read from the NHES derivation rather
  # than typed into this file.
  #
  # The same regional part-time shares appear in the memo and in state_data/README.md.
  # Typing them here too made this the third copy, and the only one no check
  # compared against the file. Now the register cannot disagree with the
  # evidence it cites.
  #
  # Returns: (chr)
  #----------------------------------------------------------------------------

  if (!file.exists(NHES)) {
    stop('missing ', NHES, ' -- the register quotes figures it must read')
  }

  n <- read_csv(NHES, col_types = cols(
    universe = col_character(), universe_label = col_character(),
    region = col_character(), n = col_double(),
    weighted_children = col_double(), mean_weekly_hours = col_double(),
    share_under_20_hrs = col_double()))

  # The five-hour cut is NSECE universe A's analogue, which is what the build
  # screens on.
  a <- n[n$universe == 'ge5' & n$region != 'national', ]
  a <- a[order(match(a$region, c('northeast', 'south', 'midwest', 'west'))), ]
  tot <- sum(a$n)

  abb <- c(northeast = 'NE', south = 'S', midwest = 'MW', west = 'W')
  shares <- paste(sprintf('%.3f %s', a$share_under_20_hrs,
                          unname(abb[a$region])), collapse = ', ')
  sprintf(NOTE[['hours']], tot, shares)
}



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
    # The regions PLUS a pooled national row. 02_nsece_constants.R emits that
    # national row as reference; build itself reads four regional cells.
    if (setequal(keys, c(REGIONS, 'national')))
      return('Census region + national pool')
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
           needs_pt_ft_marker = unname(NEEDS_PT_FT[quantity]),
           candidate       = unname(CANDIDATE[kind]),
           candidate_geography = unname(CANDIDATE_GEO[kind]),
           status          = unname(STATUS[kind]),
           note            = unname(NOTE[kind])) %>%
    mutate(note = if_else(kind == 'hours', hours_note(), note))

  # A quantity in OVERRIDE takes its search from there, not from its kind.
  stray <- setdiff(OVERRIDE$quantity, reg$quantity)
  if (length(stray)) {
    cat('OVERRIDE NAMES A QUANTITY THE REGISTER DOES NOT HOLD: ',
        paste(stray, collapse = ', '), '\n', sep = '')
    if (check_only) quit(status = 1)
  }
  for (i in seq_len(nrow(OVERRIDE))) {
    j <- which(reg$quantity == OVERRIDE$quantity[i])
    if (!length(j)) next
    reg$candidate[j]           <- OVERRIDE$candidate[i]
    reg$candidate_geography[j] <- OVERRIDE$candidate_geography[i]
    reg$status[j]              <- OVERRIDE$status[i]
    reg$note[j]                <- OVERRIDE$note[i]
  }

  reg <- reg %>%
    select(quantity, constant, feeds, geography_now, cells, min_records,
           max_records, kind, needs_pt_ft_marker, candidate,
           candidate_geography, status, note)

  # A quantity with no entry in NEEDS_PT_FT would carry NA and pass unnoticed.
  if (any(is.na(reg$needs_pt_ft_marker))) {
    cat('QUANTITY WITH NO PART-TIME / FULL-TIME RULING: ',
        paste(reg$quantity[is.na(reg$needs_pt_ft_marker)], collapse = ', '),
        '\n', sep = '')
    if (check_only) quit(status = 1)
  }

  if (check_only) {
    if (length(missing) || length(extra)) quit(status = 1)
    cat('register covers every universe-A quantity (', nrow(reg), ' rows)\n',
        sep = '')
    return(invisible(reg))
  }

  write_csv(reg, OUT)

  cat('== REPLACEMENT REGISTER --', nrow(reg), 'NSECE constants\n\n')
  print(as.data.frame(reg %>% select(quantity, geography_now, cells,
                                     min_records, kind, needs_pt_ft_marker,
                                     status)),
        row.names = FALSE)

  cat('\n== by status\n')
  print(as.data.frame(reg %>% count(kind, status, candidate)), row.names = FALSE)

  cat('\n== part-time / full-time marker\n')
  cat('   The simulator holds a child at 10 or 40 hrs/week. A replacement that\n')
  cat('   cannot place a child on that cut cannot feed the grid.\n\n')
  print(as.data.frame(reg %>%
                        count(needs_pt_ft_marker, kind, status) %>%
                        arrange(desc(needs_pt_ft_marker))),
        row.names = FALSE)
  cat('\n  ', sum(reg$needs_pt_ft_marker), ' of ', nrow(reg),
      ' constants require the marker.\n', sep = '')

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

  source(file.path(STATE_DATA_ROOT, 'code', '00_main.R'))   # main() is guarded

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
