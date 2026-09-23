#-----------------------------------------------------------------------------
# Does the memo name a repository file?
#
# IT SHOULD NOT. The memo is written for a reader outside this repository, so
# it describes what a step does, never which script does it. A file name in it
# is a reference that reader cannot follow and that would rot when the file
# moves.
#
# A URL may end in a file name, because it is a retrieval route rather than a
# path into this repository. Every \href target is stripped before the scan.
#
#   Rscript state_data/tests/check_memo_names_real_files.R
#
# Exit status is 1 on any file name found.
#-----------------------------------------------------------------------------

TEX <- 'state_data/docs/SUMMARY_method.tex'

tex <- paste(readLines(TEX, warn = FALSE), collapse = '\n')

# Drop the URL argument of every \href, and the preamble, which names packages
# and is not prose. Everything left is what a reader reads.
body <- sub('(?s)^.*?\\\\begin\\{document\\}', '', tex, perl = TRUE)
body <- gsub('\\\\href\\{[^}]*\\}', '\\\\href{}', body, perl = TRUE)

# A file name is a token carrying an extension this project uses. The leading
# boundary keeps "0--4" and "2018--19" out; the extension list keeps ordinary
# prose out.
EXT <- 'R|r|py|csv|tsv|pdf|md|yaml|yml|tex|rds|dta|do'
pat <- sprintf('(?<![A-Za-z0-9_./-])[A-Za-z0-9_][A-Za-z0-9_./-]*\\.(?:%s)\\b',
               EXT)

hits <- regmatches(body, gregexpr(pat, body, perl = TRUE))[[1]]
hits <- unique(trimws(gsub('\\\\_', '_', hits)))

cat('scanning the memo for repository file names\n\n')

if (length(hits) == 0) {
  cat('CLEAN -- the memo names no file, so no reference in it can go stale\n')
} else {
  for (h in hits) cat(sprintf('  [FAIL] %s\n', h))
  cat('\n', length(hits), ' file name(s) in the memo. Say what the step does,\n',
      'not which script does it.\n', sep = '')
  quit(status = 1)
}
