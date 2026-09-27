#-----------------------------------------------------------------------------
# Do the docs describe the check suite correctly?
#
# The README states how many checks there are and which are read-only. This
# derives the suite from disk and fails if that prose drifts:
#
#   tests/check_*.R, check_*.py     the read-only checks
#   01_nsch_rate.R --check          derivations that also verify themselves
#   03_head_start_in_schools.R --check
#
# 00_main.R --check is NOT counted. It runs the build's own invariants while
# writing out/, so it is the thing being checked, not a check.
#
#   Rscript state_data/tests/check_docs_describe_the_checks.R
#
# Exit status is 1 on any mismatch.
#-----------------------------------------------------------------------------

ROOT <- 'state_data'

DOCS <- c('README.md',
          file.path(ROOT, 'README.md'),
          file.path(ROOT, 'docs', 'SUMMARY_method.tex'))

WORDS <- c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight',
           'nine', 'ten', 'eleven', 'twelve')



writes_output <- function(path) {

  #----------------------------------------------------------------------------
  # Does the script write a file? A check that rewrites its own input is not
  # read-only, whatever its name says.
  #
  # Params:
  #   - path (chr)
  #
  # Returns: (logical)
  #----------------------------------------------------------------------------

  # BUILT FROM FRAGMENTS so the pattern never appears in this file as a
  # literal. Spelled out, it matched this script's own source and reported
  # that the checker writes output.
  verbs <- paste(c('write', '_csv'), collapse = '')
  pat <- paste(c(verbs, paste0('write', '.csv'), paste0('write', 'Lines'),
                 paste0('save', 'RDS'), paste0('file', '.create')),
               collapse = '|')

  src <- readLines(path, warn = FALSE)
  src <- src[!grepl('^\\s*#', src)]          # ignore comments
  any(grepl(pat, src, fixed = FALSE))
}


## what is actually on disk --------------------------------------------------
checks <- sort(c(list.files(file.path(ROOT, 'tests'), pattern = '^check_.*[.](R|py)$',
                            full.names = TRUE)))
# Two derivations verify their own committed constants w/ --check.
derivations <- file.path(ROOT, c('01_nsch_rate.R', '03_head_start_in_schools.R'))
stopifnot(all(file.exists(derivations)))

all_checks <- c(checks, derivations)
n_total    <- length(all_checks)
n_readonly <- sum(!vapply(all_checks, writes_output, logical(1)))

cat('found ', n_total, ' checks on disk, ', n_readonly, ' of them read-only\n\n',
    sep = '')
for (f in all_checks) {
  cat(sprintf('  %-38s %s\n', basename(f),
              if (writes_output(f)) 'writes output' else 'read-only'))
}


## what the docs claim -------------------------------------------------------
want_total    <- WORDS[n_total]
want_readonly <- WORDS[n_readonly]

cat('\nchecking the three docs say "', want_total, '" checks',
    ' and do not call them all read-only\n\n', sep = '')

fails <- 0
for (d in DOCS) {
  txt <- tolower(paste(readLines(d, warn = FALSE), collapse = ' '))

  # A claim about the suite is a number word next to "check" or "script" --
  # but only where the neighbourhood is describing the suite. Without that
  # second condition an ordinary sentence like "derived by one script" reads
  # as a miscount.
  pat <- sprintf('(%s)\\s+(read-only\\s+)?(scripts?|checks)',
                 paste(WORDS, collapse = '|'))
  hits <- gregexpr(pat, txt)[[1]]
  claims <- character(0)
  if (hits[1] != -1) {
    for (k in seq_along(hits)) {
      at  <- hits[k]
      ctx <- substr(txt, max(1, at - 60), min(nchar(txt), at + 160))
      if (grepl('exit|read-only|failure|fails', ctx)) {
        claims <- c(claims,
                    substr(txt, at, at + attr(hits, 'match.length')[k] - 1))
      }
    }
  }

  if (length(claims) == 0) {
    cat(sprintf('  [--]   %-34s makes no claim about the suite\n', d)); next
  }

  bad <- claims[!startsWith(claims, want_total)]
  wrong_ro <- claims[grepl('read-only', claims) &
                       !startsWith(claims, want_readonly)]

  if (length(bad) == 0 && length(wrong_ro) == 0) {
    cat(sprintf('  [ok]   %-34s %s\n', d, paste(unique(claims), collapse = '; ')))
  } else {
    cat(sprintf('  [FAIL] %-34s %s\n', d, paste(unique(c(bad, wrong_ro)),
                                                collapse = '; ')))
    if (length(bad))      cat('           the suite has ', n_total, '\n', sep = '')
    if (length(wrong_ro)) cat('           only ', n_readonly,
                              ' are read-only\n', sep = '')
    fails <- fails + 1
  }
}

if (fails == 0) {
  cat('\nthe docs describe the check suite correctly\n')
} else {
  cat('\n', fails, ' doc(s) describe the suite wrongly. The count and the\n',
      '"read-only" claim both drift whenever a check is added.\n', sep = '')
  quit(status = 1)
}
