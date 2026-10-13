#------------------------------------------------------------------------------
# 12_participation_bias.R -- why p0 Parent Only sits above demand targets
# (model - targets, Parent Only, ~+3 pts every income-rank scheme).
#
# DIAGNOSTIC ONLY -- prints, writes nothing. Splits national non-parental
# share (1 - Parent Only), children 0-4, into:
#   NSECE direct   universe A, kindergarten out (out/nsece_constants.csv --
#                  licensed microdata on cluster, constants only here)
#   model          p0 over ACS families, kindergarten out (P0_BY_AGE_FILE)
#   targets        PEP 0-4 x NSCH 10+ rate x five-hour ratio (build_of())
# then measures each definition gap NSCH / NHES microdata allow locally:
#   NSCH  k6q20 by single year, item nonresponse, age mix, 2016-19 pool
#   NHES  five-to-ten-hour ratio on NSECE's own construction (5+ per
#         arrangement screen, 10+ on SUMMED screened hours) and variants
#
# Every share child-weighted (NSCH fwc, NHES FEWT, ACS child weight, NSECE
# child weight). Record counts unweighted.
#
# Usage: Rscript state_data/code/12_participation_bias.R
#------------------------------------------------------------------------------

# build_of(), nhes NHES_FILE, p0_by_age(), acs_age_mix(), read_chr(), num(),
# GEOGRAPHIES; main() guarded
source('state_data/code/07_compare_nhes.R')

NSCH_POOL <- 2016:2019             # 01_'s BASE_WAVE



nsch_pool <- function() {

  #----------------------------------------------------------------------------
  # NSCH 2016-19 records, ages 0-5, k6q20 kept w/ NA (item nonresponse).
  # Waves stacked as is -- each wave's fwc sums to population, pooled share =
  # mean of wave shares, as 01_.
  #
  # Returns: (data.frame) year, age, k6q20, fwc
  #----------------------------------------------------------------------------

  do.call(rbind, lapply(NSCH_POOL, function(y) {
    f <- file.path(STATE_DATA_ROOT, 'raw', 'US',
                   sprintf('US_NSCH__child_health_survey_topical__%d.dta', y))
    d <- haven::read_dta(f)
    names(d) <- tolower(names(d))
    a <- as.numeric(d$sc_age_years)
    keep <- !is.na(a) & a <= 5
    data.frame(year = y, age = a[keep], k6q20 = as.numeric(d$k6q20)[keep],
               fwc = as.numeric(d$fwc)[keep])
  }))
}



nhes_hours <- function() {

  #----------------------------------------------------------------------------
  # NHES children 0-4 (AGE2018), six arrangement slots' weekly hours: detailed
  # + further arrangements of relative, non-relative, center care. Slot
  # counts only when flag = 1 AND weekly (RCWEEK / NCWEEK / CPWEEKX = 1 for
  # detailed; further slots carry total hours only). Hours -1 = valid skip
  # (not weekly) -> 0. NHES imputes item nonresponse -- no missing hours.
  #
  # Returns: (list) h (matrix n x 6, weekly hours, 0 = none), age, w (FEWT)
  #----------------------------------------------------------------------------

  d <- read_chr(NHES_FILE)
  d <- d[num(d$AGE2018) <= 4, ]
  v <- function(x) num(d[[x]])
  slot <- function(flag, h) ifelse(v(flag) == 1 & v(h) > 0, v(h), 0)
  h <- cbind(rc  = slot('RCNOW', 'RCHRS'),   rc2 = slot('RCOTHC', 'RCTLHR'),
             nc  = slot('NCNOW', 'NCHRS'),   nc2 = slot('NCOTHC', 'NCTLHR'),
             cp  = slot('CPNNOWX', 'CPHRS'), cp2 = slot('CPOTHC', 'CPTLHR'))
  list(h = h, age = num(d$AGE2018), w = num(d$FEWT))
}



nhes_universes <- function(x) {

  #----------------------------------------------------------------------------
  # Indicator per child for each universe definition.
  #   any_weekly  any weekly arrangement, any hours (NHES's own universe)
  #   A5          any arrangement >= 5 hrs (NSECE screen, universe A)
  #   A5_total    every weekly arrangement summed >= 5 (README's 1.194 --
  #               CAREHOURX >= 5 over >= 10, no per-arrangement screen)
  #   B10_sum     screened (>= 5) arrangements' SUMMED hours >= 10 (NSECE
  #               universe B -- 02_ sums regular arrangements only)
  #   B10_all     every weekly arrangement summed >= 10 (k6q20 reading:
  #               total care, no per-arrangement floor)
  #   B10_one     one arrangement >= 10 (no summing)
  #
  # Params:
  #   - x (list): nhes_hours()
  #
  # Returns: (data.frame) one logical column per universe
  #----------------------------------------------------------------------------

  h  <- x$h
  h5 <- h * (h >= 5)
  data.frame(any_weekly = rowSums(h) > 0,
             A5         = rowSums(h5) > 0,
             A5_total   = rowSums(h) >= 5,
             B10_sum    = rowSums(h5) >= 10,
             B10_all    = rowSums(h) >= 10,
             B10_one    = apply(h, 1, max) >= 10)
}



wshare <- function(flag, w) sum(w[flag]) / sum(w)



main_bias <- function() {

  #----------------------------------------------------------------------------
  # Prints national decomposition, NSCH age / nonresponse checks, NHES ratio
  # variants.
  #
  # Returns: (NULL) invisibly
  #----------------------------------------------------------------------------

  pct <- function(x) sprintf('%6.2f', 100 * x)
  acs_mix <- acs_age_mix()

  ## 1. national levels ------------------------------------------------------
  st <- names(STATE_POSTAL_TO_FIPS)
  b  <- lapply(st, function(g) { utils::capture.output(r <- build_of(g)); r })
  pop  <- sum(vapply(b, function(r) r$participation$children_0_4, 0))
  n10  <- sum(vapply(b, function(r) r$participation$nsch_children, 0))
  nall <- sum(vapply(b, function(r) sum(r$care$children), 0))
  p_nat <- n10 / pop
  r_nat <- nall / n10

  p0 <- p0_by_age()
  model_np <- 1 - sum(p0$children[p0$ecec_type == 'Parent Only']) / sum(p0$children)
  model_np_age <- tapply(p0$children * (p0$ecec_type != 'Parent Only'), p0$age, sum) /
    tapply(p0$children, p0$age, sum)

  k <- read_chr(file.path(STATE_DATA_ROOT, 'out', 'nsece_constants.csv'))
  kv <- function(q, key) num(k$value[k$quantity == q & k$key == key])
  nsece_A <- 1 - kv('share_of_children_region', 'US:Parent Only')
  nsece_ratio <- kv('children_all_over_10plus', 'national')
  nsece_B <- nsece_A / nsece_ratio

  cat('\n== 1. NATIONAL NON-PARENTAL SHARE, children 0-4 (weighted, %)\n')
  cat('  NSECE direct, universe A (5+ screen), K out        ', pct(nsece_A), '\n')
  cat('  NSECE universe B (summed 10+), = A / ', sprintf('%.4f', nsece_ratio),
      '          ', pct(nsece_B), '\n', sep = '')
  cat('  model p0 on ACS, K out                              ', pct(model_np), '\n')
  cat('  targets: NSCH 10+ (pop-wtd state rates)             ', pct(p_nat), '\n')
  cat('  targets: x five-hour ratio ', sprintf('%.4f', r_nat),
      ' = in care / PEP 0-4  ', pct(nall / pop), '\n', sep = '')
  cat('  gap, model - targets, Parent Only (pts)             ',
      sprintf('%6.2f', 100 * (nall / pop - model_np)), '\n')
  cat('  model by age 0-4: ', paste(pct(model_np_age), collapse = ' '), '\n')

  ## 2. NSCH: ages, nonresponse ---------------------------------------------
  n <- nsch_pool()
  resp <- n$k6q20 %in% c(1, 2)
  cat('\n== 2. NSCH k6q20, 2016-19 pool (fwc-weighted %; records unweighted)\n')
  by_age <- sapply(0:5, function(a) {
    s <- n$age == a & resp
    c(rate = wshare(n$k6q20[s] == 1, n$fwc[s]),
      nonresp = 1 - sum(n$fwc[n$age == a & resp]) / sum(n$fwc[n$age == a]),
      records = sum(n$age == a))
  })
  colnames(by_age) <- paste('age', 0:5)
  print(round(rbind(rate = 100 * by_age['rate', ], nonresp = 100 * by_age['nonresp', ],
                    records = by_age['records', ]), 2))
  s04 <- n$age <= 4
  r04 <- wshare(n$k6q20[s04 & resp] == 1, n$fwc[s04 & resp])
  nr  <- 1 - sum(n$fwc[s04 & resp]) / sum(n$fwc[s04])
  r_acs <- sum(by_age['rate', 1:5] * acs_mix[as.character(0:4)])
  nsch_mix <- tapply(n$fwc[s04 & resp], n$age[s04 & resp], sum)
  nsch_mix <- nsch_mix / sum(nsch_mix)
  cat('  0-4 rate, respondents (national, NSCH age mix)      ', pct(r04), '\n')
  cat('  0-4 rate on ACS single-year age mix                 ', pct(r_acs), '\n')
  cat('  0-4 item nonresponse (weighted)                     ', pct(nr), '\n')
  cat('  bound: nonresponse all No / all Yes                 ',
      pct(r04 * (1 - nr)), pct(r04 * (1 - nr) + nr), '\n')
  cat('  NSCH age mix 0-4: ', paste(pct(nsch_mix), collapse = ' '), '\n')
  cat('  ACS  age mix 0-4: ', paste(pct(acs_mix), collapse = ' '), '\n')

  ## 3. NHES five-to-ten ratios ---------------------------------------------
  x <- nhes_hours()
  u <- nhes_universes(x)
  w_acs <- x$w * acs_mix[as.character(x$age)] /
    (tapply(x$w, x$age, sum) / sum(x$w))[as.character(x$age)]
  sh <- function(w) vapply(u, wshare, 0, w = w)
  s_raw <- sh(x$w); s_acs <- sh(w_acs)
  cat('\n== 3. NHES-ECPP 2019, children 0-4 not in K (FEWT-weighted %; ',
      nrow(u), ' records)\n', sep = '')
  print(round(rbind(share_raw = 100 * s_raw, share_acs_mix = 100 * s_acs,
                    records = colSums(u)), 2))
  rat <- function(s) c('A5 / B10_sum (NSECE construction)' = s[['A5']] / s[['B10_sum']],
                       'A5_total / B10_all (README 1.194)'  = s[['A5_total']] / s[['B10_all']],
                       'A5 / B10_all'                       = s[['A5']] / s[['B10_all']],
                       'A5 / B10_one'                       = s[['A5']] / s[['B10_one']],
                       'any_weekly / B10_one'               = s[['any_weekly']] / s[['B10_one']],
                       'any_weekly / B10_all'               = s[['any_weekly']] / s[['B10_all']])
  cat('\n  ratio                               NHES raw   NHES ACS mix   NSECE\n')
  rr <- rat(s_raw); ra <- rat(s_acs)
  for (i in names(rr)) {
    cat(sprintf('  %-34s %8.4f %12.4f   %s\n', i, rr[[i]], ra[[i]],
                if (i == names(rr)[1]) sprintf('%.4f', nsece_ratio) else ''))
  }

  ## 4. implied targets under NHES ratio ------------------------------------
  cat('\n== 4. targets non-parental share w/ aligned NHES ratio (weighted %)\n')
  cat('  NSCH 10+ x NSECE ratio (build)                      ', pct(nall / pop), '\n')
  cat('  NSCH 10+ x NHES ratio, NSECE construction, ACS mix  ',
      pct(p_nat * r_nat / nsece_ratio * ra[[1]]), '\n')
  cat('  NHES A5 direct, ACS mix                             ', pct(s_acs[['A5']]), '\n')
  cat('  NHES B10_sum, ACS mix  vs  NSCH 10+                 ',
      pct(s_acs[['B10_sum']]), pct(p_nat), '\n')

  invisible(NULL)
}


if (sys.nframe() == 0) main_bias()
