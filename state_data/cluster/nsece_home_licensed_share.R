#------------------------------------------------------------------------------
# nsece_home_licensed_share.R -- licensed share of children in paid home-based
# care, NSECE 2019 home-based provider survey (ICPSR 37941 DS7, public use).
#
# Feeds home staffing index blend (resources/supply/state_supply_params.R):
# licensing capacity binds licensed homes only -- home index = s x licensing
# index + (1 - s), s = this share.
#
# Licensed = listed sample (HB9_METH_SAMPLE_SOURCE 2: providers on state /
# national lists -- licensed, registered, regulated). Unlisted = sample source
# 1, paid (HB9_PRGM_PAID 1). Weights HB9_METH_WEIGHT. Counts below zero
# (don't know, suppressed, not reported) dropped per measure.
#
# Measures (main = children_0_5, model age scope; others sensitivity):
#   children_0_5  HB9_ENRL_NUMCH_UNDER3 + HB9_ENRL_NUMCH_3TO5 (regular care,
#                 5+ hours/week, not in kindergarten)
#   paid_children HB9_ENRL_PAID_NUMCH (any age)
#   all_children  HB9_ENRL_TOTAL (any age)
#
# Usage (repo root, cluster; raw NSECE):
#   Rscript state_data/cluster/nsece_home_licensed_share.R --nsece <dir> --out <csv>
#   writes <csv>, <csv>.provenance.txt
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(data.table)})

args <- commandArgs(trailingOnly = TRUE)
arg_val <- function(k) { i <- match(k, args); if (is.na(i)) stop('Missing ', k) else args[i + 1] }
path <- file.path(arg_val('--nsece'), '37941-0007-Data.tsv')
out  <- arg_val('--out')
if (!file.exists(path)) stop('MISSING INPUT: ', path)

d <- fread(path, select = c('HB9_METH_WEIGHT', 'HB9_METH_SAMPLE_SOURCE', 'HB9_PRGM_PAID',
                            'HB9_ENRL_NUMCH_UNDER3', 'HB9_ENRL_NUMCH_3TO5',
                            'HB9_ENRL_PAID_NUMCH', 'HB9_ENRL_TOTAL'))
d <- d[HB9_METH_SAMPLE_SOURCE == 2 | HB9_PRGM_PAID == 1]
d[, licensed := HB9_METH_SAMPLE_SOURCE == 2]

measures <- list(
  children_0_5  = d[HB9_ENRL_NUMCH_UNDER3 >= 0 & HB9_ENRL_NUMCH_3TO5 >= 0,
                    .(licensed, w = HB9_METH_WEIGHT, v = HB9_ENRL_NUMCH_UNDER3 + HB9_ENRL_NUMCH_3TO5)],
  paid_children = d[HB9_ENRL_PAID_NUMCH >= 0, .(licensed, w = HB9_METH_WEIGHT, v = HB9_ENRL_PAID_NUMCH)],
  all_children  = d[HB9_ENRL_TOTAL >= 0, .(licensed, w = HB9_METH_WEIGHT, v = HB9_ENRL_TOTAL)])

res <- rbindlist(lapply(names(measures), function(m) {
  x <- measures[[m]]
  r <- x[, .(n = .N, providers = sum(w), children = sum(w * v)), by = licensed]
  data.table(measure = m,
             licensed_share = r[licensed == TRUE, children] / sum(r$children),
             licensed_n = r[licensed == TRUE, n], unlicensed_n = r[licensed == FALSE, n],
             licensed_providers = r[licensed == TRUE, providers],
             unlicensed_providers = r[licensed == FALSE, providers],
             licensed_children = r[licensed == TRUE, children],
             unlicensed_children = r[licensed == FALSE, children])
}))

fwrite(res, out)
writeLines(c(paste('input:', path), paste('md5:', unname(tools::md5sum(path))),
             paste('date:', format(Sys.time())), paste('R:', R.version.string)),
           paste0(out, '.provenance.txt'))
print(res)
