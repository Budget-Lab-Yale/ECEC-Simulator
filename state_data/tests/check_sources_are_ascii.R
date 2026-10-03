#-----------------------------------------------------------------------------
# Does any build source hold a byte outside ASCII?
#
# R source is UTF-8 only where machine's native encoding is. On a legacy
# codepage source() reads non-ASCII literal as different characters,
# comparison against it silently fails, build becomes locale-dependent --
# nothing errors. So code stays ASCII.
#
#   Rscript state_data/tests/check_sources_are_ascii.R
#
# Exit status is 1 on any non-ASCII byte.
#-----------------------------------------------------------------------------

ROOT  <- 'state_data'
FILES <- c(list.files(ROOT, pattern = '[.]R$', full.names = TRUE),
           list.files(file.path(ROOT, c('code', 'states')), pattern = '[.]R$',
                      full.names = TRUE),
           list.files(file.path(ROOT, c('cluster', 'tests')), pattern = '[.](R|py)$',
                      full.names = TRUE))

cat('scanning ', length(FILES), ' build sources for non-ASCII bytes\n\n',
    sep = '')

fails <- 0
for (f in FILES) {
  b   <- readBin(f, 'raw', file.size(f))
  hit <- which(b > as.raw(0x7f))
  if (length(hit) == 0) next

  # Line, not byte offset -- makes fix obvious
  line <- sum(b[seq_len(hit[1])] == as.raw(0x0a)) + 1
  cat(sprintf('  [FAIL] %s: %d non-ASCII byte(s), first on line %d\n',
              f, length(hit), line))
  fails <- fails + 1
}

if (fails == 0) {
  cat('CLEAN -- every build source is ASCII, so no literal depends on the locale\n')
} else {
  cat('\n', fails, ' file(s) hold a byte the locale can change. Parse the ',
      'value instead of\nmatching the character.\n', sep = '')
  quit(status = 1)
}
