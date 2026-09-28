#-----------------------------------------------------------------------------
# Does the memo print the numbers the build produced?
#
# Reads the hours table out of docs/SUMMARY_method.tex and compares it to
# out/*.csv. Nothing is hardcoded here, so this cannot pass by
# agreeing with a stale copy of itself.
#
#   Rscript state_data/tests/check_memo_matches_build.R
#
# Exit status is 1 on any mismatch.
#-----------------------------------------------------------------------------
suppressPackageStartupMessages(library(readr))

TEX <- 'state_data/docs/SUMMARY_method.tex'
OUT <- 'state_data/out'
GEO <- c('KY', 'NJ', 'NY', 'NYC', 'VA')

# THE CARE TYPES COME FROM THE BUILD, not from a copy kept here. This file used
# to hold its own transcription of the constants, which made it a second place
# to edit whenever they moved -- and a checker that silently disagrees with the
# thing it checks is worse than no checker. Sourcing 00_main.R is safe: main()
# is guarded, so nothing runs.
source('state_data/code/00_main.R')
TYPES <- ECEC_TYPES

tex <- readLines(TEX, warn = FALSE)



row_of <- function(label, after) {

  #----------------------------------------------------------------------------
  # Numeric cells of first memo table row starting with `label` after line
  # `after` (strictly after -- code tests i > after). Strips \\, whitespace,
  # commas, \%.
  #
  # Params:
  #   - label (chr): row label before first '&' (regex)
  #   - after (int): line index in `tex` to search past
  #
  # Returns: (num) cells after label, NULL if no such row
  #----------------------------------------------------------------------------

  i <- grep(sprintf('^%s\\s*&', label), tex)
  i <- i[i > after][1]
  if (is.na(i)) return(NULL)
  cells <- strsplit(tex[i], '&')[[1]][-1]
  cells <- gsub('\\\\\\\\|\\s|,|\\\\%', '', cells)
  as.numeric(cells[cells != ''])
}

# THE TITLE is a spanning row inside the rule, not a caption. Keep the
# anchor exact -- a looser one would catch another table and compare the wrong
# cells.
hours_hdr <- grep('Annual hours by care type', tex, fixed = TRUE)
stopifnot(length(hours_hdr) == 1)

build_hours <- sapply(GEO, function(g) {
  x <- read_csv(file.path(OUT, paste0(g, '.csv')), show_col_types = FALSE)
  round(setNames(x$annual_hours, x$ecec_type)[TYPES] / 1e6, 1)
})

fails <- 0
for (t in TYPES) {
  m <- row_of(t, hours_hdr)
  if (is.null(m) || length(m) != 5) {
    cat('  [FAIL] hours: no row for', t, '\n'); fails <- fails + 1; next
  }
  d <- abs(m - build_hours[t, ])
  if (any(d > 0.1)) {
    cat('  [FAIL] hours', t, ': memo', paste(m, collapse = ' '),
        '| build', paste(build_hours[t, ], collapse = ' '), '\n')
    fails <- fails + 1
  }
}

if (fails == 0) {
  cat('memo matches the build: 6 hours rows across 5 geographies\n')
} else {
  cat('\n', fails, 'mismatch(es)\n'); quit(status = 1)
}
