#------------------------------------------------------------------------------
# shared_setup.R -- names every state_data script shares. Light: no packages,
# no data, no simulator source -- 01 and 02 (cluster, licensed NSECE) source it
# standalone; 00_build_demand_targets.R sources it for itself and 03-04. Run
# from repo root.
#------------------------------------------------------------------------------

STATE_DATA_ROOT <- 'state_data'

# NSECE HH9_REGION codes -> Census region keys, spelled as
# 00_build_demand_targets.R's NSECE_* tables and out/nsece_constants.csv spell
# them.
REGIONS <- c('1' = 'northeast', '2' = 'midwest', '3' = 'south', '4' = 'west')

# Universe labels in out/nsece_constants.csv -- 02 writes, 00 and tests read.
# Build uses A.
UNIVERSE_A <- 'A: NSECE, any regular care'
UNIVERSE_B <- 'B: NSCH-matched, 10+ hrs/week'



# raw/ subfolder of a raw or documentation file: scope prefix of its name
# (US_, KY_, NJ_, NY_, NYC_, VA_), NYS_ filed under NY.
#   - f (chr): file name, e.g. 'KY_CCAOA__licensed_providers_by_county__2018-09.csv'
# Returns: (chr) folder name, e.g. 'KY'
raw_scope <- function(f) {
  s <- sub('_.*$', '', basename(f))
  if (s == 'NYS') 'NY' else s
}



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
