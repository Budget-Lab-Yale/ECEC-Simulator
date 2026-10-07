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
#   prints table, writes out/nsece_share_comparison.csv
#------------------------------------------------------------------------------

# build_of(), grid_annual_hours(), GEOGRAPHIES, ECEC_TYPES, WEEKS_PER_YEAR;
# main() guarded
source('state_data/code/00_build_demand_targets.R')



#==== NSECE CONSTANTS =========================================================

# NATIONAL annual hours per child -- weekly mean in primary arrangement x
# WEEKS_PER_YEAR (50, constants.R). Fallback for thin regional cell. Center-Based = mean over both paid-center
# price bands.
NSECE_ANNUAL_HOURS <- c(
  'Center-Based'        = (0.07008002232267292 * WEEKS_PER_YEAR * 27.454813402111718 +
                           0.06556113476684644 * WEEKS_PER_YEAR * 40.405040897598035) /
                          (0.07008002232267292 + 0.06556113476684644),
  'Unpaid Center-Based' = WEEKS_PER_YEAR * 28.733041207982050,
  'Paid Home-Based'     = WEEKS_PER_YEAR * 38.389082189644240,
  'Other Unpaid'        = WEEKS_PER_YEAR * 38.587712836110920,
  'Other Paid'          = WEEKS_PER_YEAR * 31.085307060488336,
  'Unpaid Home-Based'   = WEEKS_PER_YEAR * 42.447909360764570)

# Step 2. Share of children in regular care whose primary type is formal
# (FORMAL_TYPES), by Census region. Records: NE 456, MW 539, S 1,062, W 757.
NSECE_FORMAL_SHARE <- c(northeast = 0.5652653175231646,
                        midwest   = 0.5292284900870536,
                        south     = 0.53156573832827,
                        west      = 0.48328153589694967)

# NATIONAL informal mix. Fallback for thin regional cell.
NSECE_RESIDUAL_MIX <- local({
  w <- c('Other Unpaid'      = 0.6041435278360107,
         'Other Paid'        = 0.3607068983264482,
         'Unpaid Home-Based' = 0.03514957383754115)
  w / sum(w)
})

# Regional cell under this many unweighted NSECE children ("thin") takes
# national value. NOT from a reliability rule -- none cited anywhere in build.
# Exact number inconsequential: every real cell either <=14 (four Unpaid
# Home-Based cells -- 5, 8, 11, 14 records) or >=33, so any threshold 15 to 33
# makes same fallback decisions; 30 sits in gap.
NSECE_MIN_RECORDS <- 30

# Step 4 cross-check. Weighted children w/ primary type Unpaid Center-Based
# over those w/ formal primary type, by region. Formal records: NE 235, MW
# 254, S 482, W 279 (Unpaid Center-Based 84, 93, 163, 106).
NSECE_UNPAID_CENTRE_SHARE <- c(northeast = 0.29858157955075193,
                               midwest   = 0.30548525748105915,
                               south     = 0.3027785115943172,
                               west      = 0.27074481379107573)

# Step 5. Paid centers (both price bands) over paid centers + Paid Home-Based.
# Records: NE 154, MW 175, S 337, W 187.
NSECE_CENTRE_HOME_SPLIT <- c(northeast = 0.8799388331780533,
                             midwest   = 0.6971700039202228,
                             south     = 0.8163487199353228,
                             west      = 0.8101736350960986)

# Step 3. Share within informal care, w/ unweighted records.
NSECE_MIX_REGION <- tibble::tribble(
  ~region,     ~type,               ~share,             ~n,
  'midwest',   'Other Paid',        0.3871987148073660, 100,
  'midwest',   'Other Unpaid',      0.5590314949309603, 159,
  'midwest',   'Unpaid Home-Based', 0.0537697902616737,  11,
  'northeast', 'Other Paid',        0.2955260001660253,  63,
  'northeast', 'Other Unpaid',      0.6244886593258383, 140,
  'northeast', 'Unpaid Home-Based', 0.0799853405081364,   8,
  'south',     'Other Paid',        0.4056400756041331, 221,
  'south',     'Other Unpaid',      0.5832300437771258, 329,
  'south',     'Unpaid Home-Based', 0.0111298806187411,   5,
  'west',      'Other Paid',        0.3128212299126265, 152,
  'west',      'Other Unpaid',      0.6636263920592722, 292,
  'west',      'Unpaid Home-Based', 0.0235523780281013,  14)

# Step 6 cross-check. Mean weekly hours in primary arrangement, paid centers
# pooled into one Center-Based row (no price-band blend).
NSECE_HOURS_REGION <- tibble::tribble(
  ~region,     ~type,                 ~weekly,             ~n,
  'midwest',   'Center-Based',         29.9188971275273020, 108,
  'midwest',   'Unpaid Center-Based',  30.4488858327566800,  94,
  'midwest',   'Paid Home-Based',      38.8896376330629536,  67,
  'midwest',   'Other Unpaid',         30.6266013460264230, 159,
  'midwest',   'Other Paid',           26.7065241000148140, 100,
  'midwest',   'Unpaid Home-Based',    28.3171331998730444,  11,
  'northeast', 'Center-Based',         31.9334570995736900, 121,
  'northeast', 'Unpaid Center-Based',  25.8468155624064830,  91,
  'northeast', 'Paid Home-Based',      36.6526630678393133,  33,
  'northeast', 'Other Unpaid',         49.3387552672910350, 140,
  'northeast', 'Other Paid',           30.5873911679934450,  63,
  'northeast', 'Unpaid Home-Based',    37.5245928047053852,   8,
  'south',     'Center-Based',         36.7361109194010000, 282,
  'south',     'Unpaid Center-Based',  31.0387651114622360, 170,
  'south',     'Paid Home-Based',      39.7654817852239191,  55,
  'south',     'Other Unpaid',         38.5135236507778400, 329,
  'south',     'Other Paid',           34.8266217806642700, 221,
  'south',     'Unpaid Home-Based',    86.4870335563659600,   5,
  'west',      'Center-Based',         33.2066425388585460, 144,
  'west',      'Unpaid Center-Based',  24.8778191128340300, 112,
  'west',      'Paid Home-Based',      36.1402993658343732,  43,
  'west',      'Other Unpaid',         36.8625726463482760, 292,
  'west',      'Other Paid',           28.2511294700214100, 152,
  'west',      'Unpaid Home-Based',    49.7970525898259382,  14)

# NATIONAL part-time share. Fallback for thin regional cell -- Unpaid
# Home-Based rests on 5 to 14 children per region.
NSECE_PART_TIME <- c(
  'Center-Based'        = 0.24156066533388276,
  'Unpaid Center-Based' = 0.32374404913138010,
  'Paid Home-Based'     = 0.07704556374948256,
  'Other Unpaid'        = 0.29237982925281690,
  'Other Paid'          = 0.28619765711509220,
  'Unpaid Home-Based'   = 0.11530630069636505)

# Share of type's children whose primary arrangement runs under 20 hrs/week --
# 1a_nsece_processing.R's cut, so build, simulator call same children
# part-time.
NSECE_PART_TIME_REGION <- tibble::tribble(
  ~region,     ~type,                 ~pt_share,           ~n,
  'midwest',   'Center-Based',         0.3672560769094304, 108,
  'midwest',   'Unpaid Center-Based',  0.3514149565490429,  94,
  'midwest',   'Paid Home-Based',      0.1161727070892445,  67,
  'midwest',   'Other Unpaid',         0.3778589556081480, 159,
  'midwest',   'Other Paid',           0.4180859752229404, 100,
  'midwest',   'Unpaid Home-Based',    0.2810657094393909,  11,
  'northeast', 'Center-Based',         0.2639149081693626, 121,
  'northeast', 'Unpaid Center-Based',  0.3325585112527899,  91,
  'northeast', 'Paid Home-Based',      0.1458057480669377,  33,
  'northeast', 'Other Unpaid',         0.1561441499432618, 140,
  'northeast', 'Other Paid',           0.2027316709775340,  63,
  'northeast', 'Unpaid Home-Based',    0.0000000000000000,   8,
  'south',     'Center-Based',         0.1945631077170473, 282,
  'south',     'Unpaid Center-Based',  0.2727975025485991, 170,
  'south',     'Paid Home-Based',      0.0413640094796634,  55,
  'south',     'Other Unpaid',         0.2619140572593956, 329,
  'south',     'Other Paid',           0.2232781759903599, 221,
  'south',     'Unpaid Home-Based',    0.2165571415343821,   5,
  'west',      'Center-Based',         0.1965070478204198, 144,
  'west',      'Unpaid Center-Based',  0.3944863316507277, 112,
  'west',      'Paid Home-Based',      0.0343117709923390,  43,
  'west',      'Other Unpaid',         0.3717828059788612, 292,
  'west',      'Other Paid',           0.3364892962106961, 152,
  'west',      'Unpaid Home-Based',    0.0000000000000000,  14)



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
  invisible(out)
}


if (sys.nframe() == 0) main_compare_shares()
