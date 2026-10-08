# In-memory cache of epop growth paths -- one entry per epop file x base year
# x base-year population weights (see get_employment_rate_growth_factor())
EPOP_GROWTH_CACHE <- new.env(parent = emptyenv())



get_employment_rate_growth_factor <- function(macro_projections, base_year, target_year,
                                             column = NULL, epop_path = NULL) {

  #----------------------------------------------------------------------------
  # Calculates employment rate growth factors by sex x age bin from base_year
  # to target_year.
  #
  # Uses CBO epop projections (or a state override) aggregated to broad age
  # bins (EMPLOYMENT_AGE_BINS) with base-year population weights from macro
  # projections: aggregate fine-bin rates first, then divide target-year
  # aggregate by base-year aggregate. File read + growth paths for every year
  # built once per epop file x base year x weights, cached in
  # EPOP_GROWTH_CACHE; later calls look up target_year row.
  #
  # Input contract -- stop() if: epop file missing, required column missing,
  # base or target year row missing, needed rate NA or non-positive, base-year
  # macro row missing, married_/unmarried_ age column missing or NA, fine-bin
  # weight non-positive. target_year <= base_year returns 1.0 for all groups
  # without reading anything.
  #
  # Params:
  #   - macro_projections (df): Macro projections table (for population weights;
  #       married_<age>, unmarried_<age> for ages 16-59 in base_year row)
  #   - base_year (int): Base year (e.g., 2019 for NSECE)
  #   - target_year (int): Target year to project to
  #   - column (chr): Unused, for backwards compatibility
  #   - epop_path (chr or NULL): Optional override for the epop projections
  #       CSV (state-level runs pass config/state/employment/<ST>.csv; NULL
  #       uses the national CBO file)
  #
  # Returns:
  #   Named numeric vector with 8 growth factors (EMPLOYMENT_TARGETING_GROUPS):
  #   c(F_u25, M_u25, F_25_35, M_25_35, F_35_45, M_35_45, F_45plus, M_45plus)
  #----------------------------------------------------------------------------

  group_names <- EMPLOYMENT_TARGETING_GROUPS

  # Return 1.0 for all groups if target year <= base year
  if (target_year <= base_year) {
    return(setNames(rep(1.0, length(group_names)), group_names))
  }

  # National CBO file unless state override given
  epop_path <- epop_path %||% 'resources/epop/epop_projections.csv'
  if (!file.exists(epop_path)) {
    stop('get_employment_rate_growth_factor: epop projections not found at ',
         epop_path, ' -- build it (resources/epop/emp_projections.R national, ',
         'resources/epop/state_epop_projections.R per state)')
  }

  # Fine-bin population weights, base year -- cheap, recomputed per call so
  # cache key reflects exactly the macro table passed. State file carrying
  # pop_<bin> columns (state_epop_projections.R) -> its own base-year
  # population in place of national macro weights
  weights <- get_epop_file_weights(epop_path, base_year) %||%
    get_epop_fine_bin_weights(macro_projections, base_year)

  # Cache key: normalized path, file mtime, base year, weights to full precision
  path_norm <- normalizePath(epop_path, winslash = '/', mustWork = TRUE)
  key <- paste(path_norm,
               format(file.info(path_norm)$mtime, '%Y%m%d%H%M%OS6'),
               base_year,
               paste(sprintf('%.17g', weights), collapse = ','),
               sep = '|')

  paths <- EPOP_GROWTH_CACHE[[key]]
  if (is.null(paths)) {
    paths <- build_epop_growth_paths(epop_path, base_year, weights)
    assign(key, paths, envir = EPOP_GROWTH_CACHE)
  }

  # Look up target year
  row <- match(target_year, paths$year)
  if (is.na(row)) {
    stop('get_employment_rate_growth_factor: no year ', target_year, ' in ',
         epop_path, ' (covers ', min(paths$year), '-', max(paths$year), ')')
  }
  growth <- paths$growth[row, group_names]
  if (any(!is.finite(growth))) {
    stop('get_employment_rate_growth_factor: NA or non-positive epop rate in ',
         epop_path, ' for year ', target_year, ', group(s) ',
         paste(group_names[!is.finite(growth)], collapse = ', '))
  }

  return(growth)
}



get_epop_fine_bin_weights <- function(macro_projections, base_year) {

  #----------------------------------------------------------------------------
  # Base-year population per fine epop age bin (EMPLOYMENT_AGE_BINS), summed
  # over married_<age> + unmarried_<age>. Stops on missing row, column, NA,
  # or non-positive bin total.
  #
  # Params:
  #   - macro_projections (df): Macro projections table
  #   - base_year (int): Year whose population weights the bins
  #
  # Returns:
  #   Named numeric vector, one weight per fine bin ('16_17', ..., '55_59')
  #----------------------------------------------------------------------------

  fine_bins <- unlist(EMPLOYMENT_AGE_BINS, use.names = FALSE)

  pop_row <- macro_projections %>% filter(year == !!base_year)
  if (nrow(pop_row) != 1) {
    stop('get_employment_rate_growth_factor: macro projections have ',
         nrow(pop_row), ' rows for base year ', base_year, ' (need exactly 1)')
  }

  sapply(fine_bins, function(bin) {
    ages <- as.integer(strsplit(bin, '_')[[1]])
    cols <- paste0(rep(c('married_', 'unmarried_'), each = ages[2] - ages[1] + 1),
                   ages[1]:ages[2])
    missing <- setdiff(cols, names(pop_row))
    if (length(missing)) {
      stop('get_employment_rate_growth_factor: macro projections lack ',
           paste(missing, collapse = ', '))
    }
    vals <- unlist(pop_row[, cols], use.names = FALSE)
    if (anyNA(vals)) {
      stop('get_employment_rate_growth_factor: NA population in macro ',
           'projections for base year ', base_year, ', bin ', bin)
    }
    total <- sum(vals)
    if (!(total > 0)) {
      stop('get_employment_rate_growth_factor: non-positive population weight ',
           'for base year ', base_year, ', bin ', bin)
    }
    total
  })
}



get_epop_file_weights <- function(epop_path, base_year) {

  #----------------------------------------------------------------------------
  # Fine-bin population weights from epop file's own pop_<bin> columns, base
  # year row. NULL when file has no pop_ columns (national file). Stops on
  # partial set, missing base year, NA, or non-positive weight.
  #
  # Params:
  #   - epop_path (chr): Epop projections CSV
  #   - base_year (int): Base year
  #
  # Returns:
  #   Named numeric vector, one weight per fine bin, or NULL
  #----------------------------------------------------------------------------

  fine_bins <- unlist(EMPLOYMENT_AGE_BINS, use.names = FALSE)
  cols <- paste0('pop_', fine_bins)
  hdr <- names(read_csv(epop_path, n_max = 0, show_col_types = FALSE))
  if (!any(cols %in% hdr)) return(NULL)
  if (!all(cols %in% hdr)) {
    stop('get_employment_rate_growth_factor: ', epop_path, ' has only some pop_ columns -- lacks ',
         paste(setdiff(cols, hdr), collapse = ', '))
  }
  epop <- read_csv(epop_path, show_col_types = FALSE)
  row <- epop[epop$year == base_year, cols]
  if (nrow(row) != 1) {
    stop('get_employment_rate_growth_factor: no base year ', base_year, ' row in ', epop_path)
  }
  w <- setNames(unlist(row, use.names = FALSE), fine_bins)
  if (any(!is.finite(w) | w <= 0)) {
    stop('get_employment_rate_growth_factor: NA or non-positive pop_ weight in ', epop_path,
         ' for base year ', base_year)
  }
  w
}



build_epop_growth_paths <- function(epop_path, base_year, weights) {

  #----------------------------------------------------------------------------
  # Reads epop file once; per sex x broad bin, population-weighted mean of
  # fine-bin rates each year, divided by base-year mean. Base-year rates must
  # all be positive; a year with any NA or non-positive needed rate gets NA
  # growth (caller stops if that year requested).
  #
  # Params:
  #   - epop_path (chr): Epop projections CSV (year + epop_<m|f>_<bin> cols)
  #   - base_year (int): Base year
  #   - weights (num): Named fine-bin weights (get_epop_fine_bin_weights())
  #
  # Returns:
  #   list(year = int vector, growth = matrix years x EMPLOYMENT_TARGETING_GROUPS)
  #----------------------------------------------------------------------------

  epop <- read_csv(epop_path, show_col_types = FALSE)

  fine_bins <- unlist(EMPLOYMENT_AGE_BINS, use.names = FALSE)
  required <- c('year', as.vector(outer(c('epop_f_', 'epop_m_'), fine_bins, paste0)))
  missing <- setdiff(required, names(epop))
  if (length(missing)) {
    stop('get_employment_rate_growth_factor: ', epop_path, ' lacks column(s) ',
         paste(missing, collapse = ', '))
  }
  if (anyDuplicated(epop$year)) {
    stop('get_employment_rate_growth_factor: duplicate years in ', epop_path)
  }

  base_idx <- match(base_year, epop$year)
  if (is.na(base_idx)) {
    stop('get_employment_rate_growth_factor: no base year ', base_year, ' in ', epop_path)
  }

  growth <- matrix(NA_real_, nrow = nrow(epop), ncol = length(EMPLOYMENT_TARGETING_GROUPS),
                   dimnames = list(NULL, EMPLOYMENT_TARGETING_GROUPS))

  for (sex in c('F', 'M')) {
    sex_prefix <- if (sex == 'F') 'epop_f_' else 'epop_m_'

    for (bin_name in names(EMPLOYMENT_AGE_BINS)) {
      bins <- EMPLOYMENT_AGE_BINS[[bin_name]]
      rates <- as.matrix(epop[, paste0(sex_prefix, bins)])
      w <- weights[bins]

      base_vals <- rates[base_idx, ]
      if (any(is.na(base_vals) | base_vals <= 0)) {
        stop('get_employment_rate_growth_factor: NA or non-positive base-year (',
             base_year, ') rate in ', epop_path, ' for ', sex, '_', bin_name)
      }
      base_epop <- weighted.mean(base_vals, w)

      # Aggregate first, then ratio -- not mean of fine-cell growth factors
      agg <- apply(rates, 1, function(r) {
        if (any(is.na(r) | r <= 0)) NA_real_ else weighted.mean(r, w)
      })
      growth[, paste0(sex, '_', bin_name)] <- agg / base_epop
    }
  }

  list(year = as.integer(epop$year), growth = growth)
}
