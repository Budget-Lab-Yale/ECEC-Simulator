#------------------------------------------------------------------------------
# nsece_wage_region.R -- NSECE 2019 centre staff hourly wage by Census region x
# education (no BA, BA), w/ design-based standard errors. Compared w/ model
# state wages (OEWS ratios x NSECE national) in
# state_data/code/analyze_regional_wages.R.
#
# Wage: 1a recipe -- workforce file, roles 1-2 (lead + assistant teachers),
#   WF9_WORK_WAGE > 0, worker weight WF9_METH_WEIGHT; BA = WF9_CHAR_EDUC 7-8,
#   no BA = 1, 3-6. Region: worker's centre (CB9_REGION).
# SE: Taylor linearisation of weighted mean, strata x PSU of worker's centre
#   (CB9_METH_VSTRATUMPU, CB9_METH_VPSUPU), w/ replacement; single-PSU strata
#   add 0. Same estimator as nsece_price_region_density.R.
# Check: national cells reproduce 1a wages (12.06219 no BA, 19.695 BA).
#
# Usage (repo root, cluster):
#   Rscript state_data/cluster/nsece_wage_region.R --nsece <dir> --out <csv>
#   writes <csv>, <csv>.provenance.txt
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(data.table)})

args <- commandArgs(trailingOnly = TRUE)
arg_val <- function(k) { i <- match(k, args); if (is.na(i)) stop('Missing ', k) else args[i + 1] }
NSECE <- arg_val('--nsece'); OUT <- arg_val('--out')
NAT_1A <- c(no_ba = 12.06219, ba = 19.695)
TOL <- 1e-4



wmean_se <- function(y, w, strata, psu) {

  #----------------------------------------------------------------------------
  # Weighted mean, linearised SE (strata x PSU, w/ replacement).
  #
  # Params: y, w (num): values, weights; strata, psu: design ids
  # Returns: (num) c(est, se, n, df) -- df = PSUs - strata
  #----------------------------------------------------------------------------

  R <- sum(w * y) / sum(w)
  z <- data.table(h = strata, p = psu, z = w * (y - R) / sum(w))[, .(z = sum(z)), by = .(h, p)]
  v <- z[, .(n = .N, ss = if (.N > 1) .N / (.N - 1) * sum((z - mean(z))^2) else 0), by = h]
  c(est = R, se = sqrt(sum(v$ss)), n = length(y), df = sum(v$n) - nrow(v))
}



cb <- fread(file.path(NSECE, '37941-0006-Data.tsv'),
            select = c('CB9_METH_CASEID', 'CB9_REGION', 'CB9_METH_VSTRATUMPU', 'CB9_METH_VPSUPU'))
wf <- fread(file.path(NSECE, '37941-0005-Data.tsv'),
            select = c('CB9_METH_CASEID', 'WF9_METH_WEIGHT', 'WF9_WORK_WAGE', 'WF9_WORK_ROLE', 'WF9_CHAR_EDUC'))
w <- merge(wf, cb, by = 'CB9_METH_CASEID')
w <- w[WF9_WORK_WAGE > 0 & WF9_WORK_ROLE %in% 1:2 & WF9_CHAR_EDUC %in% c(1, 3:8),
       .(wage = WF9_WORK_WAGE, wt = WF9_METH_WEIGHT, region = CB9_REGION,
         educ = ifelse(WF9_CHAR_EDUC %in% 7:8, 'ba', 'no_ba'),
         strata = CB9_METH_VSTRATUMPU, psu = CB9_METH_VPSUPU)]

one <- function(s) as.list(wmean_se(s$wage, s$wt, s$strata, s$psu))
res <- rbind(w[, one(.SD), by = .(region, educ)],
             w[, one(.SD), by = educ][, region := 0L], use.names = TRUE)
res[, rse := se / est]
setorder(res, educ, region)

nat <- setNames(res[region == 0, est], res[region == 0, educ])
ok <- all(abs(nat[names(NAT_1A)] - NAT_1A) < TOL)
cat(if (ok) '  [PASS] ' else '  [FAIL] ', 'national cells reproduce 1a wages\n', sep = '')
fwrite(res, OUT)
writeLines(c('script: state_data/cluster/nsece_wage_region.R', paste('run at:', format(Sys.time())),
             paste('nsece:', NSECE), paste('R:', R.version.string), paste('workers:', nrow(w))),
           paste0(OUT, '.provenance.txt'))
options(width = 200)
print(as.data.frame(res[, .(educ, region, n, df, est = round(est, 3), se = round(se, 3), rse = round(rse, 3))]),
      row.names = FALSE)
if (!ok) quit(status = 1)
