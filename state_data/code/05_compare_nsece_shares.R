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
# Center-Based = one paid-centre row. 02_ run 2026-10-12 -- unknown payment
# drawn paid status on every unknown row (as 1a); regular care per
# resolve_regular_care() (zero-hour regular providers imputed hours, REGCARE
# -1 / -2 regular when TOC 1-5). Hours, part-time tables: imputed hours drawn
# from cell's weighted donor hours (keyed; cell weighted median before).
# TOC 7 / 8 never regular (NSECE regular care = TOC 1-5; TOC 8 counted
# Other before) -- universe A 3,462 -> 3,427 children. Label draws averaged
# over NSECE_MI_DRAWS = 50 (nsece_mi_draws.R, 2026-10-12) -- record counts
# below = mean per draw, may be fractional.
NSECE_ANNUAL_HOURS <- c(
  'Center-Based'        = WEEKS_PER_YEAR * 33.75893415200879,
  'Unpaid Center-Based' = WEEKS_PER_YEAR * 29.175548621735036,
  'Paid Home-Based'     = WEEKS_PER_YEAR * 37.77749862719917,
  'Other Unpaid'        = WEEKS_PER_YEAR * 43.41002195705766,
  'Other Paid'          = WEEKS_PER_YEAR * 32.896281600626324,
  'Unpaid Home-Based'   = WEEKS_PER_YEAR * 41.41512678974394)

# Step 2. Share of children in regular care whose primary type is formal
# (FORMAL_TYPES), by Census region. Records: NE 540, MW 654, S 1,263, W 970.
NSECE_FORMAL_SHARE <- c(northeast = 0.5411036844553323,
                        midwest   = 0.45966512852533986,
                        south     = 0.4901581223944361,
                        west      = 0.4272777402156575)

# NATIONAL informal mix. Fallback for thin regional cell.
NSECE_RESIDUAL_MIX <- local({
  w <- c('Other Unpaid'      = 0.6217837919880265,
         'Other Paid'        = 0.362584539667172,
         'Unpaid Home-Based' = 0.015631668344801536)
  w / sum(w)
})

# Regional cell under this many unweighted NSECE children ("thin") takes
# national value. NOT from a reliability rule -- none cited anywhere in build.
# Exact number inconsequential: every real cell either <=19 (four Unpaid
# Home-Based cells -- 6, 6, 7, 19 records) or >=40, so any threshold 20 to 40
# makes same fallback decisions; 30 sits in gap.
NSECE_MIN_RECORDS <- 30

# Step 4 cross-check. Weighted children w/ primary type Unpaid Center-Based
# over those w/ formal primary type, by region. Formal records: NE 269.62, MW
# 300, S 551, W 330 (Unpaid Center-Based 89.16, 97.22, 154.62, 105.12).
NSECE_UNPAID_CENTRE_SHARE <- c(northeast = 0.25137990444390057,
                               midwest   = 0.27243648838996287,
                               south     = 0.23564578015766885,
                               west      = 0.2135218139866267)

# Step 5. Paid centers (both price bands) over paid centers + Paid Home-Based.
# Records: NE 180.46, MW 202.78, S 396.38, W 224.88.
NSECE_CENTRE_HOME_SPLIT <- c(northeast = 0.8637412679802096,
                             midwest   = 0.6685571184246332,
                             south     = 0.8226720949604892,
                             west      = 0.8248516058000019)

# Step 3. Share within informal care, w/ unweighted records.
NSECE_MIX_REGION <- tibble::tribble(
  ~region,     ~type,                 ~share,               ~n,
  'midwest',   'Other Paid',          0.34372910790294786, 128.98,
  'midwest',   'Other Unpaid',        0.6490111667469648, 219.02,
  'midwest',   'Unpaid Home-Based',   0.007259725350087429,   6,
  'northeast', 'Other Paid',          0.29513932520751407, 87.26,
  'northeast', 'Other Unpaid',        0.6579617423062342, 177.74,
  'northeast', 'Unpaid Home-Based',   0.046898932486251786, 5.38,
  'south',     'Other Paid',          0.4211813241925467, 289.04,
  'south',     'Other Unpaid',        0.5728478737905269, 415.96,
  'south',     'Unpaid Home-Based',   0.005970802016926425,   7,
  'west',      'Other Paid',          0.3365998444062576, 241.18,
  'west',      'Other Unpaid',        0.6457316216202467, 379.82,
  'west',      'Unpaid Home-Based',   0.017668533973495652,  19)

# Step 6 cross-check. Mean weekly hours in primary arrangement, paid centers
# pooled into one Center-Based row (no price-band blend).
NSECE_HOURS_REGION <- tibble::tribble(
  ~region,     ~type,                 ~weekly,              ~n,
  'midwest',   'Center-Based',        30.341162291698772, 121.78,
  'midwest',   'Unpaid Center-Based', 30.41135342419286,  97.22,
  'midwest',   'Paid Home-Based',     37.12185490505811,     81,
  'midwest',   'Other Unpaid',        37.347726821099975, 219.02,
  'midwest',   'Other Paid',          28.013073792841798, 128.98,
  'midwest',   'Unpaid Home-Based',   41.49039374373457,      6,
  'northeast', 'Center-Based',        31.780220620611612, 139.84,
  'northeast', 'Unpaid Center-Based', 27.95670890699668,  89.16,
  'northeast', 'Paid Home-Based',     37.063522280642864, 40.62,
  'northeast', 'Other Unpaid',        52.04813848616243, 177.74,
  'northeast', 'Other Paid',          31.128140382910612, 87.26,
  'northeast', 'Unpaid Home-Based',   40.08323532897601,   5.38,
  'south',     'Center-Based',        36.89108017243702, 329.38,
  'south',     'Unpaid Center-Based', 29.745532565490652, 154.62,
  'south',     'Paid Home-Based',     39.133961353218396,    67,
  'south',     'Other Unpaid',        45.04692123420868, 415.96,
  'south',     'Other Paid',          36.264342117082805, 289.04,
  'south',     'Unpaid Home-Based',   52.175666482670835,     7,
  'west',      'Center-Based',        32.70702949850111, 174.88,
  'west',      'Unpaid Center-Based', 27.720194625526045, 105.12,
  'west',      'Paid Home-Based',     36.98632658852082,     50,
  'west',      'Other Unpaid',        41.26988444609215, 379.82,
  'west',      'Other Paid',          32.3371974351913,  241.18,
  'west',      'Unpaid Home-Based',   38.223165692480336,    19)

# NATIONAL part-time share. Fallback for thin regional cell -- Unpaid
# Home-Based rests on 6 to 19 children per region.
NSECE_PART_TIME <- c(
  'Center-Based'        = 0.24472582731000103,
  'Unpaid Center-Based' = 0.3128599785835478,
  'Paid Home-Based'     = 0.10299640506681328,
  'Other Unpaid'        = 0.2701702045320909,
  'Other Paid'          = 0.2961192234237463,
  'Unpaid Home-Based'   = 0.06748315729195549)

# Share of type's children whose primary arrangement runs under 20 hrs/week --
# 1a_nsece_processing.R's cut, so build, simulator call same children
# part-time.
NSECE_PART_TIME_REGION <- tibble::tribble(
  ~region,     ~type,                 ~pt_share,            ~n,
  'midwest',   'Center-Based',        0.3480211620150312, 121.78,
  'midwest',   'Unpaid Center-Based', 0.36165824010589226, 97.22,
  'midwest',   'Paid Home-Based',     0.1547124732991101,    81,
  'midwest',   'Other Unpaid',        0.3120250647142747, 219.02,
  'midwest',   'Other Paid',          0.3957723546553799, 128.98,
  'midwest',   'Unpaid Home-Based',   0.04635660441158015,    6,
  'northeast', 'Center-Based',        0.2766457581938724, 139.84,
  'northeast', 'Unpaid Center-Based', 0.25833619321943724, 89.16,
  'northeast', 'Paid Home-Based',     0.1172632377150076, 40.62,
  'northeast', 'Other Unpaid',        0.19011259457010213, 177.74,
  'northeast', 'Other Paid',          0.21847048852370984, 87.26,
  'northeast', 'Unpaid Home-Based',   0.0,                 5.38,
  'south',     'Center-Based',        0.1997696000676197, 329.38,
  'south',     'Unpaid Center-Based', 0.2846602690863041, 154.62,
  'south',     'Paid Home-Based',     0.0755038620951855,    67,
  'south',     'Other Unpaid',        0.23073442705685568, 415.96,
  'south',     'Other Paid',          0.2639640969004947, 289.04,
  'south',     'Unpaid Home-Based',   0.09950403723102844,    7,
  'west',      'Center-Based',        0.21170843977265288, 174.88,
  'west',      'Unpaid Center-Based', 0.37206355499682225, 105.12,
  'west',      'Paid Home-Based',     0.045633309492679845,  50,
  'west',      'Other Unpaid',        0.336305585449333, 379.82,
  'west',      'Other Paid',          0.30406105518629495, 241.18,
  'west',      'Unpaid Home-Based',   0.17732985648227523,   19)



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
