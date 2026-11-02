#------------------------------------------------------------------------------
# nsece_price_region_density.R -- NSECE 2019 paid care price per child-hour by
# Census region x community urban density (urban_density.R codes 1 high, 2
# moderate, 3 rural), paid centre (low + high tier pooled) and paid home, w/
# design-based standard errors.
#
# Fallback local price for geographies w/out NDCP price (resources/supply/
# ndcp_local_prices_2019.R: IN, NM; MS home): geo price = cell prices at geo's
# child mix by density.
#
# Centre: posted weekly rate / 40 (1a rules: age bins 0-1, 2-3, 4-5, FTE
#   enrolment = FT + 0.5 PT), price > 0, weight = centre weight x FTE
#   enrolment. Home: household-side net hourly cost, Paid Home-Based, age < 5,
#   child weight (1a home price). Density: CB9_COMM_URBAN_DENSITY /
#   HH9_COMM_URBAN_DENSITY.
# SE: Taylor linearisation of weighted mean, strata x PSU (CB9 / HH9
#   _METH_VSTRATUMPU, _METH_VPSUPU), w/ replacement; single-PSU strata add 0.
# Reliability: cell used if relative SE (SE / price) <= 0.30 (NCHS rule) and
#   design df (PSUs - strata) >= 1 -- else SE not estimable (one PSU per
#   stratum reads SE 0). Unreliable cell: national density price x region /
#   national price (both reliable).
#
# Usage (repo root, cluster):
#   Rscript state_data/cluster/nsece_price_region_density.R --nsece <dir> --est <interface>/estimation/data --out <csv>
#   writes <csv>, <csv>.provenance.txt
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(data.table)})

args <- commandArgs(trailingOnly = TRUE)
arg_val <- function(k) { i <- match(k, args); if (is.na(i)) stop('Missing ', k) else args[i + 1] }
NSECE <- arg_val('--nsece'); EST <- arg_val('--est'); OUT <- arg_val('--out')
RSE_MAX <- 0.30



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



# Centres
cb <- fread(file.path(NSECE, '37941-0006-Data.tsv'))
age_of <- function(a) fcase(a %in% c('INF', '1YR'), '0-1', a %in% c('2YR', '3YR'), '2-3', a %in% c('4YR', '5YR'), '4-5')
ages <- c('INF', '1YR', '2YR', '3YR', '4YR', '5YR')
long <- rbindlist(lapply(ages, function(a) {
  pc <- paste0('CB9_STDRATE_WKLY_', a); gc <- paste0('CB9_B1_3A_AGERATE_', a, '_R')
  cb[, .(id = CB9_METH_CASEID, w = CB9_METH_WEIGHT, region = CB9_REGION, density = CB9_COMM_URBAN_DENSITY,
         strata = CB9_METH_VSTRATUMPU, psu = CB9_METH_VPSUPU, age_bin = age_of(a),
         enrl = get(paste0('CB9_ENRL_', a)), ft = get(paste0('CB9_ENRL_FT_', a)),
         rate = if (pc %in% names(cb)) get(pc) else NA_real_, flag = if (gc %in% names(cb)) get(gc) else NA_real_)]
}))
long[, price := fcase(is.na(rate) | enrl == 0, NA_real_, flag == -8, 0, flag == 2, NA_real_,
                      rate %in% c(-1, -2), NA_real_, default = rate / 40)]
cen <- long[, .(w = mean(w), region = first(region), density = first(density), strata = first(strata), psu = first(psu),
                enrollment = sum(enrl),
                share_ft = if (all(ft < 0)) 0 else if (any(ft < 0)) mean(ft[ft >= 0]) / 100 else mean(ft) / 100,
                price = if (all(is.na(price))) NA_real_ else mean(price, na.rm = TRUE)),
            by = .(id, age_bin)]
cen[, fte := enrollment * share_ft + 0.5 * enrollment * (1 - share_ft)]
cen <- cen[price > 0 & fte > 0]

# Homes
kids <- fread(file.path(EST, 'nsece_children_2019.csv'))
prov <- fread(file.path(EST, 'nsece_providers_2019.csv'))
hh   <- fread(file.path(EST, 'nsece_households_2019.csv'))[, .(hh_id, region, density)]
hhd  <- fread(file.path(NSECE, '37941-0009-Data.tsv'),
              select = c('HH9_METH_CASEID', 'HH9_METH_VSTRATUMPU', 'HH9_METH_VPSUPU'))
setnames(hhd, c('hh_id', 'strata', 'psu'))
home <- merge(kids, prov, by = c('hh_id', 'child_id'))[ecec_choice == 'Paid Home-Based' & age < 5 &
                                                        net_hourly_cost_status == 'OK']
home <- merge(merge(home, hh, by = 'hh_id'), hhd, by = 'hh_id')

cells <- function(d, y, w, setting) {
  one <- function(x) as.list(wmean_se(x[[y]], x[[w]], x$strata, x$psu))
  rbind(d[, one(.SD), by = .(region, density)],
        d[, one(.SD), by = region][, density := 0L],
        d[, one(.SD), by = density][, region := 0L],
        d[, one(.SD)][, `:=`(region = 0L, density = 0L)], use.names = TRUE)[, setting := setting]
}
cen[, wt := w * fte]
res <- rbind(cells(cen, 'price', 'wt', 'center'), cells(home, 'net_hourly_cost', 'child_weight', 'home'))
res[, rse := se / est]
res[, reliable := rse <= RSE_MAX & df >= 1]

# Fallback: national density price x region / national
res[, price_used := est][, rule := 'cell']
for (s in unique(res$setting)) {
  nat_all <- res[setting == s & region == 0 & density == 0, est]
  for (i in which(res$setting == s & res$region > 0 & res$density > 0 & !res$reliable)) {
    r <- res$region[i]; d <- res$density[i]
    nd <- res[setting == s & region == 0 & density == d]; ra <- res[setting == s & region == r & density == 0]
    if (!nd$reliable || !ra$reliable) stop(s, ': fallback inputs unreliable for region ', r, ' density ', d)
    res$price_used[i] <- nd$est * ra$est / nat_all
    res$rule[i] <- 'national_density_x_region'
  }
}
setorder(res, setting, region, density)
fwrite(res, OUT)
writeLines(c('script: state_data/cluster/nsece_price_region_density.R', paste('run at:', format(Sys.time())),
             paste('nsece:', NSECE), paste('interface data:', EST), paste('RSE_MAX:', RSE_MAX), paste('R:', R.version.string)),
           paste0(OUT, '.provenance.txt'))
options(width = 200)
print(as.data.frame(res[, .(setting, region, density, n, df, est = round(est, 3), se = round(se, 3), rse = round(rse, 3),
                            price_used = round(price_used, 3), rule)]), row.names = FALSE)
