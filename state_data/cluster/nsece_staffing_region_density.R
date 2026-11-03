#------------------------------------------------------------------------------
# nsece_staffing_region_density.R -- NSECE 2019 observed staff per child by
# Census region x community urban density (urban_density.R codes 1 high, 2
# moderate, 3 rural), centre + paid home, w/ design-based standard errors.
# Same cells, reliability rule, fallback as nsece_price_region_density.R.
#
# Observed staffing index for state supply inputs: geo value = cell values at
# geo's child mix by density (density_mix(), ndcp_local_prices_2019.R).
#
# Centre: classroom module (CB9_F3_*, classroom weight CB9_WEIGHT_CLSM) --
#   teachers + assistants, enrolment, age of youngest child (< 60 months).
#   Indirectly age-standardised: est = sum w staff / sum w enrol x n_a, n_a =
#   national staff per enrolled child at classroom's year of age -- 1 =
#   national staffing at same ages. Cells need no age split.
# Home: listed home-based providers (HB9_METH_SAMPLE_SOURCE 2), staff = 1
#   + helpers, children = total enrolment, provider weight; est = staff per
#   child, no age standardisation.
# SE: Taylor linearisation of ratio, strata x PSU (_METH_VSTRATUMPU,
#   _METH_VPSUPU), w/ replacement; single-PSU strata add 0.
# Reliability: relative SE <= 0.30 and design df >= 1, else national density
#   value x region / national value.
#
# Usage (repo root, cluster):
#   Rscript state_data/cluster/nsece_staffing_region_density.R --nsece <dir> --out <csv>
#   writes <csv>, <csv>.provenance.txt
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(data.table)})

args <- commandArgs(trailingOnly = TRUE)
arg_val <- function(k) { i <- match(k, args); if (is.na(i)) stop('Missing ', k) else args[i + 1] }
NSECE <- arg_val('--nsece'); OUT <- arg_val('--out')
RSE_MAX <- 0.30



ratio_se <- function(y, x, w, strata, psu) {

  #----------------------------------------------------------------------------
  # Weighted ratio sum w y / sum w x, linearised SE (strata x PSU, w/
  # replacement).
  #
  # Params: y, x, w (num): numerator, denominator, weights; strata, psu
  # Returns: (num) c(est, se, n, df) -- df = PSUs - strata
  #----------------------------------------------------------------------------

  R <- sum(w * y) / sum(w * x)
  z <- data.table(h = strata, p = psu, z = w * (y - R * x) / sum(w * x))[, .(z = sum(z)), by = .(h, p)]
  v <- z[, .(n = .N, ss = if (.N > 1) .N / (.N - 1) * sum((z - mean(z))^2) else 0), by = h]
  c(est = R, se = sqrt(sum(v$ss)), n = length(y), df = sum(v$n) - nrow(v))
}



need_cols <- function(d, cols, file) {
  miss <- setdiff(cols, names(d))
  if (length(miss)) stop(file, ' lacks ', paste(miss, collapse = ', '))
}

# Centre classrooms
cb_cols <- c('CB9_WEIGHT_CLSM', 'CB9_F3_ENROLL_TC', 'CB9_F3_TEACH_TC', 'CB9_F3_ASSIST_TC',
             'CB9_F3_YOUNGMONTHS_R', 'CB9_REGION', 'CB9_COMM_URBAN_DENSITY',
             'CB9_METH_VSTRATUMPU', 'CB9_METH_VPSUPU')
cb <- fread(file.path(NSECE, '37941-0006-Data.tsv'), select = cb_cols)
need_cols(cb, cb_cols, 'centre file')
cls <- cb[CB9_F3_TEACH_TC >= 0 & CB9_F3_ASSIST_TC >= 0,
          .(w = CB9_WEIGHT_CLSM, enroll = CB9_F3_ENROLL_TC, staff = CB9_F3_TEACH_TC + CB9_F3_ASSIST_TC,
            young = CB9_F3_YOUNGMONTHS_R, region = CB9_REGION, density = CB9_COMM_URBAN_DENSITY,
            strata = CB9_METH_VSTRATUMPU, psu = CB9_METH_VPSUPU)]
cls <- cls[w > 0 & enroll > 0 & staff > 0 & young >= 0 & young < 60 & density %in% 1:3]
cls[, age := floor(young / 12)]
norm <- cls[, .(n_a = sum(w * staff) / sum(w * enroll)), by = age]
cls <- merge(cls, norm, by = 'age')
cls[, x := enroll * n_a]

# Listed homes
hb_cols <- c('HB9_METH_WEIGHT', 'HB9_METH_SAMPLE_SOURCE', 'HB9_ENRL_TOTAL', 'HB9_H2_NUMHELP', 'HB9_REGION',
             'HB9_COMM_URBAN_DENSITY', 'HB9_METH_VSTRATUMPU', 'HB9_METH_VPSUPU')
hb <- fread(file.path(NSECE, '37941-0007-Data.tsv'), select = hb_cols)
need_cols(hb, hb_cols, 'home file')
home <- hb[HB9_METH_SAMPLE_SOURCE == 2 & HB9_ENRL_TOTAL > 0 & HB9_H2_NUMHELP > -4 & HB9_COMM_URBAN_DENSITY %in% 1:3,
           .(w = HB9_METH_WEIGHT, x = HB9_ENRL_TOTAL, staff = 1 + ifelse(HB9_H2_NUMHELP == -2, 0, HB9_H2_NUMHELP),
             region = HB9_REGION, density = HB9_COMM_URBAN_DENSITY,
             strata = HB9_METH_VSTRATUMPU, psu = HB9_METH_VPSUPU)]
home <- home[staff > 0]

cells <- function(d, setting) {
  one <- function(s) as.list(ratio_se(s$staff, s$x, s$w, s$strata, s$psu))
  rbind(d[, one(.SD), by = .(region, density)],
        d[, one(.SD), by = region][, density := 0L],
        d[, one(.SD), by = density][, region := 0L],
        d[, one(.SD)][, `:=`(region = 0L, density = 0L)], use.names = TRUE)[, setting := setting]
}
res <- rbind(cells(cls, 'center'), cells(home, 'home'))
res[, rse := se / est]
res[, reliable := rse <= RSE_MAX & df >= 1]

# Fallback: national density value x region / national
res[, est_used := est][, rule := 'cell']
for (s in unique(res$setting)) {
  nat_all <- res[setting == s & region == 0 & density == 0, est]
  for (i in which(res$setting == s & res$region > 0 & res$density > 0 & !res$reliable)) {
    r <- res$region[i]; d <- res$density[i]
    nd <- res[setting == s & region == 0 & density == d]; ra <- res[setting == s & region == r & density == 0]
    if (!nd$reliable || !ra$reliable) stop(s, ': fallback inputs unreliable for region ', r, ' density ', d)
    res$est_used[i] <- nd$est * ra$est / nat_all
    res$rule[i] <- 'national_density_x_region'
  }
}
setorder(res, setting, region, density)
fwrite(res, OUT)
writeLines(c('script: state_data/cluster/nsece_staffing_region_density.R', paste('run at:', format(Sys.time())),
             paste('nsece:', NSECE), paste('RSE_MAX:', RSE_MAX), paste('R:', R.version.string),
             paste('centre classrooms:', nrow(cls), ' listed homes:', nrow(home))),
           paste0(OUT, '.provenance.txt'))
options(width = 200)
cat('national centre staff per enrolled child by age (n_a):\n'); print(norm[order(age)])
print(as.data.frame(res[, .(setting, region, density, n, df, est = round(est, 3), se = round(se, 3), rse = round(rse, 3),
                            est_used = round(est_used, 3), rule)]), row.names = FALSE)
