#------------------------------------------------------------------------------
# 05_compare_nsece_shares.R -- production demand build against regional NSECE
# shares.
#
# COMPARISON ONLY -- writes nothing simulator reads. Production
# (00_build_demand_targets.R) divides children in care left after program
# counts by simulator p0 over geography's own ACS households. Here same total,
# same program count, divided by regional NSECE constants instead -- steps in
# number order:
#   2 formal = total x regional formal share; informal = rest
#   3 informal split by regional informal mix
#   4 programs carved out of formal
#   5 rest of formal split center / home by regional split
#   6 part-time share regional
# Thin regional cell (under NSECE_MIN_RECORDS unweighted children) takes
# national value.
#
# NSECE CONSTANTS. NSECE 2019 household file (ICPSR 37941, dataset 0009),
# child-weighted, universe A (every child 0-4 w/ a regular non-parental
# arrangement). 02_derive_nsece_constants.R derives every one; full precision,
# record counts in out/nsece_constants.csv;
# tests/check_constants_match_derivation.R compares them back.
#
# Usage: Rscript state_data/code/05_compare_nsece_shares.R [--geo KY[,NJ] | --all]
#   prints table, writes out/nsece_share_comparison.csv; short_share_split()
#   writes out/short_share_sensitivity.csv (step 1 w/ regional, type short
#   shares)
#------------------------------------------------------------------------------

# build_of(), grid_annual_hours(), GEOGRAPHIES, ECEC_TYPES, WEEKS_PER_YEAR;
# main() guarded
source('state_data/code/00_build_demand_targets.R')



#==== NSECE CONSTANTS =========================================================

# NATIONAL annual hours per child -- weekly mean in primary arrangement x
# WEEKS_PER_YEAR (50, constants.R). Fallback for thin regional cell.
# Center-Based = one paid-centre row. Constants below from 02_ run on cluster
# 2026-10-14 under NSECE label rules back to pre-2026-10-12 (one label draw,
# regular care = REGCARE 1 w/ positive reference-week hours, TOC 8 counts
# Other); record counts = unweighted children.
NSECE_ANNUAL_HOURS <- c(
  'Center-Based'        = WEEKS_PER_YEAR * 33.840936175175059,
  'Unpaid Center-Based' = WEEKS_PER_YEAR * 28.733041207982051,
  'Paid Home-Based'     = WEEKS_PER_YEAR * 38.389082189644242,
  'Other Unpaid'        = WEEKS_PER_YEAR * 38.587712836110917,
  'Other Paid'          = WEEKS_PER_YEAR * 31.085307060488336,
  'Unpaid Home-Based'   = WEEKS_PER_YEAR * 42.447909360764569)

# Step 2. Share of children in regular care whose primary type is formal
# (FORMAL_TYPES), by Census region. Records: NE 456, MW 539, S 1,062, W 757.
NSECE_FORMAL_SHARE <- c(northeast = 0.56526531752316456,
                        midwest   = 0.52922849008705364,
                        south     = 0.53156573832827003,
                        west      = 0.48328153589694967)

# NATIONAL informal mix. Fallback for thin regional cell.
NSECE_RESIDUAL_MIX <- local({
  w <- c('Other Unpaid'      = 0.60414352783601066,
         'Other Paid'        = 0.3607068983264482,
         'Unpaid Home-Based' = 0.035149573837541151)
  w / sum(w)
})

# Regional cell under this many unweighted NSECE children ("thin") takes
# national value. NOT from a reliability rule -- none cited anywhere in build.
# Exact number inconsequential: every real cell either <=14 (four Unpaid
# Home-Based cells -- 5, 8, 11, 14 records) or >=33, so any threshold 15 to 33
# makes same fallback decisions; 30 sits in gap.
NSECE_MIN_RECORDS <- 30

# Step 4 cross-check. Weighted children w/ primary type Unpaid Center-Based
# over those w/ formal primary type, by region. Records w/ Unpaid Center-Based
# primary type: NE 245, MW 269, S 507, W 299.
NSECE_UNPAID_CENTRE_SHARE <- c(northeast = 0.29858157955075193,
                               midwest   = 0.30548525748105915,
                               south     = 0.30277851159431718,
                               west      = 0.27074481379107573)

# Step 5. Paid centers (both price bands) over paid centers + Paid Home-Based.
# Records: NE 154, MW 175, S 337, W 187.
NSECE_CENTRE_HOME_SPLIT <- c(northeast = 0.87993883317805333,
                             midwest   = 0.69717000392022277,
                             south     = 0.81634871993532276,
                             west      = 0.81017363509609863)

# Step 3. Share within informal care, w/ unweighted records.
NSECE_MIX_REGION <- tibble::tribble(
  ~region,     ~type,                 ~share,               ~n,
  'midwest',   'Other Paid',          0.38719871480736601, 100,
  'midwest',   'Other Unpaid',        0.55903149493096027, 159,
  'midwest',   'Unpaid Home-Based',   0.053769790261673722,   11,
  'northeast', 'Other Paid',          0.29552600016602532, 63,
  'northeast', 'Other Unpaid',        0.6244886593258383, 140,
  'northeast', 'Unpaid Home-Based',   0.07998534050813641, 8,
  'south',     'Other Paid',          0.40564007560413312, 221,
  'south',     'Other Unpaid',        0.58323004377712584, 329,
  'south',     'Unpaid Home-Based',   0.011129880618741055,   5,
  'west',      'Other Paid',          0.31282122991262645, 152,
  'west',      'Other Unpaid',        0.66362639205927221, 292,
  'west',      'Unpaid Home-Based',   0.023552378028101305,  14)

# Step 6 cross-check. Mean weekly hours in primary arrangement, paid centers
# pooled into one Center-Based row (no price-band blend).
NSECE_HOURS_REGION <- tibble::tribble(
  ~region,     ~type,                 ~weekly,              ~n,
  'midwest',   'Center-Based',        30.056770015436204, 108,
  'midwest',   'Unpaid Center-Based', 30.44888583275668,  94,
  'midwest',   'Paid Home-Based',     38.889637633062954,     67,
  'midwest',   'Other Unpaid',        30.626601346026423, 159,
  'midwest',   'Other Paid',          26.706524100014814, 100,
  'midwest',   'Unpaid Home-Based',   28.317133199873044,      11,
  'northeast', 'Center-Based',        32.382220313348391, 121,
  'northeast', 'Unpaid Center-Based', 25.846815562406483,  91,
  'northeast', 'Paid Home-Based',     36.652663067839313, 33,
  'northeast', 'Other Unpaid',        49.338755267291035, 140,
  'northeast', 'Other Paid',          30.587391167993445, 63,
  'northeast', 'Unpaid Home-Based',   37.524592804705385,   8,
  'south',     'Center-Based',        36.736110919401, 282,
  'south',     'Unpaid Center-Based', 31.038765111462236, 170,
  'south',     'Paid Home-Based',     39.765481785223919,    55,
  'south',     'Other Unpaid',        38.513523650777842, 329,
  'south',     'Other Paid',          34.826621780664269, 221,
  'south',     'Unpaid Home-Based',   86.487033556365958,     5,
  'west',      'Center-Based',        33.206642538858546, 144,
  'west',      'Unpaid Center-Based', 24.877819112834029, 112,
  'west',      'Paid Home-Based',     36.140299365834373,     43,
  'west',      'Other Unpaid',        36.862572646348276, 292,
  'west',      'Other Paid',          28.251129470021411,  152,
  'west',      'Unpaid Home-Based',   49.797052589825938,    14)

# NATIONAL part-time share. Fallback for thin regional cell -- Unpaid
# Home-Based rests on 6 to 19 children per region.
NSECE_PART_TIME <- c(
  'Center-Based'        = 0.23845679514677057,
  'Unpaid Center-Based' = 0.32374404913138011,
  'Paid Home-Based'     = 0.077045563749482557,
  'Other Unpaid'        = 0.29237982925281691,
  'Other Paid'          = 0.28619765711509221,
  'Unpaid Home-Based'   = 0.11530630069636505)

# Share of type's children whose primary arrangement runs under 20 hrs/week --
# 1a_nsece_processing.R's cut, so build, simulator call same children
# part-time.
NSECE_PART_TIME_REGION <- tibble::tribble(
  ~region,     ~type,                 ~pt_share,            ~n,
  'midwest',   'Center-Based',        0.36621291696208463, 108,
  'midwest',   'Unpaid Center-Based', 0.35141495654904287, 94,
  'midwest',   'Paid Home-Based',     0.11617270708924447,    67,
  'midwest',   'Other Unpaid',        0.37785895560814797, 159,
  'midwest',   'Other Paid',          0.41808597522294039, 100,
  'midwest',   'Unpaid Home-Based',   0.28106570943939091,    11,
  'northeast', 'Center-Based',        0.25109310206151403, 121,
  'northeast', 'Unpaid Center-Based', 0.33255851125278985, 91,
  'northeast', 'Paid Home-Based',     0.14580574806693766, 33,
  'northeast', 'Other Unpaid',        0.15614414994326181, 140,
  'northeast', 'Other Paid',          0.202731670977534, 63,
  'northeast', 'Unpaid Home-Based',   0,                 8,
  'south',     'Center-Based',        0.19456310771704727, 282,
  'south',     'Unpaid Center-Based', 0.27279750254859914, 170,
  'south',     'Paid Home-Based',     0.041364009479663379,    55,
  'south',     'Other Unpaid',        0.26191405725939559, 329,
  'south',     'Other Paid',          0.22327817599035985, 221,
  'south',     'Unpaid Home-Based',   0.21655714153438207,    5,
  'west',      'Center-Based',        0.19650704782041981, 144,
  'west',      'Unpaid Center-Based', 0.39448633165072772, 112,
  'west',      'Paid Home-Based',     0.034311770992339015,  43,
  'west',      'Other Unpaid',        0.37178280597886115, 292,
  'west',      'Other Paid',          0.33648929621069606, 152,
  'west',      'Unpaid Home-Based',   0,   14)



regional_or_national <- function(tbl, region, value, national) {

  #----------------------------------------------------------------------------
  # One region's value per care type; thin cell takes national value.
  #
  # Params:
  #   - tbl (tibble): region, type, n, `value` columns
  #   - region (chr); value (chr): column name; national (named num) by type
  #
  # Returns: (named num) by care type, ECEC_TYPES order
  #----------------------------------------------------------------------------

  r <- tbl[tbl$region == region, ]
  v <- setNames(r[[value]], r$type)
  thin <- r$n < NSECE_MIN_RECORDS
  v[thin] <- national[r$type[thin]]
  v[ECEC_TYPES]
}



regional_mix <- function(region) {

  #----------------------------------------------------------------------------
  # Informal mix for one region. Thin cell takes national share; other cells
  # keep regional ratio, rescaled so the three sum to 1.
  #
  # Params:
  #   - region (chr)
  #
  # Returns: (named num) share by informal type
  #----------------------------------------------------------------------------

  r <- NSECE_MIX_REGION[NSECE_MIX_REGION$region == region, ]
  s <- setNames(r$share, r$type)
  thin <- r$n < NSECE_MIN_RECORDS
  if (!any(thin)) return(s / sum(s))
  fixed <- NSECE_RESIDUAL_MIX[names(s)[thin]]
  rest  <- s[!thin] / sum(s[!thin]) * (1 - sum(fixed))
  c(rest, fixed)[names(NSECE_RESIDUAL_MIX)]
}



nsece_allocation <- function(region, np_total, program) {

  #----------------------------------------------------------------------------
  # Divides one geography's children in care by regional NSECE shares, steps
  # 2-6 in number order. Same total and program count as production.
  #
  # Params:
  #   - region (chr): lower-case Census region
  #   - np_total (num): children in regular care (production step 1)
  #   - program (num): net program children (production step 4)
  #
  # Returns: (tibble) ecec_type, children, annual_hours (grid), raw_hours
  #          (NSECE mean hours); NA children when programs fill formal total
  #----------------------------------------------------------------------------

  formal <- NSECE_FORMAL_SHARE[[region]] * np_total
  if (program >= formal) {
    return(tibble::tibble(ecec_type = ECEC_TYPES, children = NA_real_,
                          annual_hours = NA_real_, raw_hours = NA_real_))
  }
  split <- NSECE_CENTRE_HOME_SPLIT[[region]]
  other <- formal - program
  kids  <- c('Center-Based'        = other * split,
             'Unpaid Center-Based' = program,
             'Paid Home-Based'     = other * (1 - split),
             (np_total - formal) * regional_mix(region))[ECEC_TYPES]

  pt   <- regional_or_national(NSECE_PART_TIME_REGION, region, 'pt_share', NSECE_PART_TIME)
  grid <- grid_annual_hours(pt)
  raw  <- regional_or_national(dplyr::mutate(NSECE_HOURS_REGION, annual = WEEKS_PER_YEAR * weekly),
                               region, 'annual', NSECE_ANNUAL_HOURS)
  tibble::tibble(ecec_type    = ECEC_TYPES,
                 children     = unname(kids),
                 annual_hours = unname(kids * grid),
                 raw_hours    = unname(kids * raw))
}



compare_shares_geo <- function(geo) {

  #----------------------------------------------------------------------------
  # Production build vs NSECE-share build, one geography.
  #
  # Params:
  #   - geo (chr)
  #
  # Returns: (tibble) one row per care type
  #----------------------------------------------------------------------------

  res     <- build_of(geo)
  np      <- res$participation$nonparental_total
  program <- res$programs$total
  alt <- nsece_allocation(res$region, np, program)

  formal_nsece <- NSECE_FORMAL_SHARE[[res$region]] * np
  tibble::tibble(geo = geo, region = res$region, ecec_type = ECEC_TYPES,
                 children_p0     = res$care$children,
                 children_nsece  = alt$children,
                 hours_p0        = res$care$annual_hours,
                 hours_nsece     = alt$annual_hours,
                 hours_nsece_raw = alt$raw_hours,
                 programs_over_nsece_unpaid =
                   program / (formal_nsece * NSECE_UNPAID_CENTRE_SHARE[[res$region]]))
}



short_share_split <- function() {

  #----------------------------------------------------------------------------
  # Step 1 w/ NSECE short share by region or by primary care type in place of
  # national NSECE_SHORT_SHARE_OF_PT. Children in care scale w/ relative
  # factor = geography's predicted ratio / 50 states + DC ratio, so change
  # here = change in children in care. Writes out/short_share_sensitivity.csv.
  #
  # Returns: (tibble) geo, region_pct, type_pct (change vs national share, %)
  #----------------------------------------------------------------------------

  k <- read_chr(file.path(STATE_DATA_ROOT, 'out', 'nsece_constants.csv'))
  # Thin cell (under NSECE_MIN_RECORDS) takes national share, as rest of 05_
  sh <- function(q) {
    r <- k[k$quantity == q, ]
    setNames(ifelse(num(r$n) < NSECE_MIN_RECORDS, NSECE_SHORT_SHARE_OF_PT, num(r$value)), r$key)
  }
  s_reg <- sh('short_share_of_part_time_region')
  s_typ <- sh('short_share_of_part_time_type')
  if (!length(s_reg) || !length(s_typ)) stop('nsece_constants.csv lacks split short shares -- rerun 02_')

  # In care, short children of one geography under a per-row short share
  parts <- function(g, s_of) {
    d <- p0_rows(g)
    ic <- sum(num(d$children[d$ecec_type != 'Parent Only']))
    pt <- d[d$hours_choice == 'pt', ]
    c(ic = ic, short = sum(num(pt$children) * s_of(g, pt$ecec_type)))
  }
  nat <- function(g, t) rep(NSECE_SHORT_SHARE_OF_PT, length(t))
  reg <- function(g, t) rep(s_reg[[get_spec(parent_geo(g))$region]], length(t))
  typ <- function(g, t) unname(s_typ[P0_TYPE_MAP[t]])
  rel <- function(s_of) {
    p <- sapply(GEOGRAPHIES, parts, s_of = s_of)
    n <- rowSums(p[, names(STATE_POSTAL_TO_FIPS)])
    (p['ic', ] / (p['ic', ] - p['short', ])) / (n[['ic']] / (n[['ic']] - n[['short']]))
  }
  base <- rel(nat)
  out <- tibble::tibble(geo = GEOGRAPHIES,
                        region_pct = 100 * (rel(reg) / base - 1),
                        type_pct   = 100 * (rel(typ) / base - 1))
  cat(sprintf('\nShort share by region / type: children in care change at most %.2f%% / %.2f%%\n',
              max(abs(out$region_pct)), max(abs(out$type_pct))))
  readr::write_csv(out, file.path(STATE_DATA_ROOT, 'out', 'short_share_sensitivity.csv'))
  invisible(out)
}



main_compare_shares <- function(args = commandArgs(trailingOnly = TRUE)) {

  #----------------------------------------------------------------------------
  # Every requested geography: prints children by type under both shares,
  # writes out/nsece_share_comparison.csv.
  #
  # Params:
  #   - args (chr): --geo KY[,NJ] | --all (default --all)
  #
  # Returns: (tibble) invisibly
  #----------------------------------------------------------------------------

  geos <- if ('--geo' %in% args) {
    strsplit(cli_value('--geo', args = args), ',')[[1]]
  } else {
    GEOGRAPHIES
  }

  out <- dplyr::bind_rows(lapply(geos, compare_shares_geo))

  cat('Production (p0) vs regional NSECE shares: children 0-4 (thousands)\n\n')
  show_df(out %>%
            dplyr::transmute(geo, ecec_type,
                             p0    = round(children_p0 / 1000, 1),
                             nsece = round(children_nsece / 1000, 1)))
  cat('\nnsece NA: programs fill the regional formal total\n')

  f <- file.path(STATE_DATA_ROOT, 'out', 'nsece_share_comparison.csv')
  readr::write_csv(out, f)
  cat('wrote', f, '\n')
  short_share_split()
  invisible(out)
}


if (sys.nframe() == 0) main_compare_shares()
