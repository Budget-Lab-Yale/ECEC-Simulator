#------------------------------------------------------------------------------
# shared_setup.R -- names every state_data script shares. Light: no packages,
# no data, no simulator source -- 01 and 02 (cluster, licensed NSECE) source it
# standalone; 00_build_demand_targets.R sources it for itself, 05, 07. Run
# from repo root.
#------------------------------------------------------------------------------

STATE_DATA_ROOT <- 'state_data'
source('src/shared_functions/get_raw_data_path.R')

# NSECE HH9_REGION codes -> Census region keys, spelled as
# 00_build_demand_targets.R's NSECE_* tables and out/nsece_constants.csv spell
# them.
REGIONS <- c('1' = 'northeast', '2' = 'midwest', '3' = 'south', '4' = 'west')

# Universe labels in out/nsece_constants.csv -- 02 writes, 00 and tests read.
# Build uses A.
UNIVERSE_A <- 'A: NSECE, any regular care'
UNIVERSE_B <- 'B: NSCH-matched, 10+ hrs/week'

# Formal care types, one Center-Based row (00 build, 02 constants)
FORMAL_TYPES <- c('Center-Based', 'Unpaid Center-Based', 'Paid Home-Based')



# Null default. Returns: (any) x unless NULL, else y
`%||%` <- function(x, y) if (is.null(x)) y else x



# Value after a CLI flag.
#   - flag (chr); default (chr): returned when flag absent; args (chr)
# Returns: (chr)
cli_value <- function(flag, default = NA_character_,
                      args = commandArgs(trailingOnly = TRUE)) {
  i <- which(args == flag)
  if (length(i) == 0) default else args[i + 1]
}



# Prints a table w/out row names. Returns: (NULL) invisibly
show_df <- function(x) {
  print(as.data.frame(x), row.names = FALSE)
  invisible(NULL)
}



# NHES-ECPP 2019 per-day hours answers. Some parents give hours per DAY in
# weekly-hours items RCHRS / NCHRS / CPHRS: heaps at 6, 8, 9 hrs/week, many on
# 4-5 days (center: 8 hrs on 5 days, 111 records, children 0-4) -- under 2
# hrs/day not plausible for a regular arrangement. Such answer -> hours x days
# (RCDAYS / NCDAYS / CPDAYS). 2-3 day short weeks kept -- part-day preschool
# runs that way. Totals of further arrangements (*TLHR) carry no days, kept.
# Children 0-4 w/ arrangement fixed (FEWT): 13.0% center, 12.9% relative, 9.0%
# non-relative (324, 168, 61 records). Answers under 5 hrs/week can become
# regular care (5+). Center part-time 37.0% -> 27.3% (NSECE 24.5%).
# Checked: questionnaire asks days, then "hours each week" (Q47-48), no hours
# unit item (*UNIT = cost unit); fixed answers reported, not imputed (8 of 324
# center), same rate every MODECOMP; work-coverage answer (*CVRWK) matches
# full-time children, not short weeks -- README NHES-ECPP. (2026-10-13)
NHES_PER_DAY_MAX_HOURS <- 2
NHES_PER_DAY_MIN_DAYS  <- 4

#   - d (df): NHES rows, any column types
# Returns: (df) d, *HRS rewritten where answer per day, same column types;
#   attr 'per_day_fixed' = rows fixed per item
nhes_fix_per_day_hours <- function(d) {
  fixed <- c()
  for (p in c('RC', 'NC', 'CP')) {
    hv <- paste0(p, 'HRS')
    h  <- suppressWarnings(as.numeric(d[[hv]]))
    dy <- suppressWarnings(as.numeric(d[[paste0(p, 'DAYS')]]))
    bad <- !is.na(h) & !is.na(dy) & h > 0 & dy >= NHES_PER_DAY_MIN_DAYS &
           h / dy < NHES_PER_DAY_MAX_HOURS
    new <- ifelse(bad, h * dy, h)
    d[[hv]] <- if (is.character(d[[hv]])) ifelse(bad, as.character(new), d[[hv]]) else new
    fixed[hv] <- sum(bad)
  }
  attr(d, 'per_day_fixed') <- fixed
  d
}
