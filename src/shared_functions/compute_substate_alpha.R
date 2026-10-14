#------------------------------------------------------------------------------
# compute_substate_alpha.R
#
# Sub-state geography ranked twice (SUBSTATE_GEOGRAPHIES, constants.R; NYC
# only). National uses + parent-state run (NY) keep geo's households ranked
# inside their state -- national alpha_<type>_2019.rds. Geo's own run (NYC)
# takes alpha_<type>_2019_<geo>.rds: same households, same national beta /
# rho, but p0 from geo's own income ranks (substate_rank_inputs(),
# income_rank.R):
#   - p0: 1b forest prediction on geo rank inputs ->
#     acs_enrollment[_joint]_2019_<geo>.csv
#   - V: geo's own price wedge rows -- all four wedges from price forests at
#     geo's own income percentile (price_wedge_income_pctiles(geo),
#     compute_price_wedge_cache.R), geo's own percentile draw (main:
#     set.seed + sample over geo's units)
#   - alpha = log p0 - V, normalised to choice 1, as 2a compute_alpha_matrix()
# Scheme national: versions identical, nothing written, national alphas used.
# Alpha rds record urban-density setting (urban_density.R) --
# load_substate_alpha() stops when run's differs.
#
# Called from: 2a (run_calibration, end -- compute_substate_alphas()), 3a
# (initialize_simulation -- load_substate_alpha()), 1b (with_rng_preserved()),
# state_data/cluster/acs_p0_shares*.R (substate_alpha_path())
#------------------------------------------------------------------------------



with_rng_preserved <- function(expr) {

  #----------------------------------------------------------------------------
  # Evaluates expr, then puts global RNG state back -- sub-state extras run
  # own seeds w/out moving any draw after them.
  #
  # Params:
  #   - expr (expression): evaluated lazily in caller's frame
  #
  # Returns: value of expr
  #----------------------------------------------------------------------------

  had_seed <- exists('.Random.seed', envir = globalenv(), inherits = FALSE)
  if (had_seed) old_seed <- get('.Random.seed', envir = globalenv())
  on.exit({
    if (had_seed) {
      assign('.Random.seed', old_seed, envir = globalenv())
    } else if (exists('.Random.seed', envir = globalenv(), inherits = FALSE)) {
      rm('.Random.seed', envir = globalenv())
    }
  })
  expr
}



substate_alpha_path <- function(estimation_dir, pu_type, year, geo) {

  #----------------------------------------------------------------------------
  # Path of geo's own alpha rds, beside national alpha_<type>_<year>.rds.
  #
  # Params:
  #   - estimation_dir (chr): <output root>/estimation
  #   - pu_type (chr): 'c1' or 'c2plus'
  #   - year (int): calibration year
  #   - geo (chr): sub-state code, e.g. 'NYC'
  #
  # Returns: (chr) path alpha_<type>_<year>_<geo>.rds
  #----------------------------------------------------------------------------

  file.path(estimation_dir, paste0('alpha_', pu_type, '_', year, '_', geo, '.rds'))
}



compute_substate_alphas <- function(parent_units_wide, acs_tables,
                                    build_type_data, compute_V, supply_prices,
                                    other_paid_base_price, wage_perturbation,
                                    tax_max, data_dir, estimation_dir,
                                    year = 2019, price_qrf_models = NULL) {

  #----------------------------------------------------------------------------
  # Writes alpha_<type>_<year>_<geo>.rds for every sub-state geography, after
  # national demand estimation. Geo's rows of calibration data rebuilt w/ geo
  # p0 (1b sub-state enrollment files), geo's own price wedge (quintile from
  # geo's own income ranks); V w/ national beta / rho. Skips geo, logged,
  # when scheme leaves versions identical, geo enrollment files missing, or
  # households lack countyfip. No RNG draws.
  #
  # Params:
  #   - parent_units_wide (df): 2a wide parent units (w/ national price_wedge.*)
  #   - acs_tables (list): 2a processed_acs_2019 (households used: hh_id,
  #       income, region, statefip, countyfip; parent_units, children for
  #       geo's wedges)
  #   - build_type_data (fn): 2a build_calibration_type_data()
  #   - compute_V (fn): 2a compute_V_crra()
  #   - supply_prices (num vec): baseline supply prices by sector
  #   - other_paid_base_price (dbl): Other Paid base price
  #   - wage_perturbation (dbl): 2a wage multiplier (1.01)
  #   - tax_max (dbl): OASI taxable earnings cap
  #   - data_dir (chr): estimation/data (1b enrollment files)
  #   - estimation_dir (chr): estimation/ -- reads national alpha rds (beta,
  #       rho at full precision, rows to align), writes geo rds beside it
  #   - year (int): calibration year
  #   - price_qrf_models (list): price forests (1a, price_qrf_models.rds) --
  #       geo's wedges recomputed on own income ranks; NULL (wedge off) keeps
  #       parent_units_wide wedges (1.0)
  #
  # Returns: (chr vec, invisible) geos written
  #----------------------------------------------------------------------------

  written <- character(0)

  for (geo in names(SUBSTATE_GEOGRAPHIES)) {
    cat('Sub-state alpha (', geo, ', own income ranks):\n', sep = '')

    enr_path   <- file.path(data_dir, paste0('acs_enrollment_', year, '_', geo, '.csv'))
    joint_path <- file.path(data_dir, paste0('acs_enrollment_joint_', year, '_', geo, '.csv'))
    if (!file.exists(enr_path)) {
      cat('  No ', basename(enr_path), ' (1b wrote none: scheme ', get_income_rank_scheme(),
          ' or no countyfip) -- ', geo, ' runs on national alphas\n', sep = '')
      next
    }

    geo_ids <- tryCatch(
      select_geo_hh_ids(acs_tables$households, geo, SUBSTATE_GEOGRAPHIES[[geo]]$statefip),
      error = function(e) {
        cat('  Skipped --', conditionMessage(e), '\n')
        NULL
      }
    )
    if (is.null(geo_ids)) next

    # Scheme w/ identical versions (national) -> national alphas, even if
    # stale geo enrollment files exist
    hh <- acs_tables$households
    if (is.null(substate_rank_inputs(hh$income, hh$region, hh$statefip,
                                     hh$hh_id %in% geo_ids, geo))) {
      cat('  ', geo, ' runs on national alphas\n', sep = '')
      next
    }

    # Geo rows: own price wedge -- forest wedge at geo's own income ranks
    # (center_low, other_paid differ from national); wedge off -> national
    # 1.0 wedges
    pu_geo <- parent_units_wide %>% filter(hh_id %in% geo_ids)
    if (!is.null(price_qrf_models)) {
      geo_pctiles <- price_wedge_income_pctiles(acs_tables$parent_units, acs_tables$households, geo)
      geo_wedge <- compute_price_wedge_cache(
        parent_units     = acs_tables$parent_units %>% filter(hh_id %in% geo_ids),
        children         = acs_tables$children %>% filter(hh_id %in% geo_ids),
        households       = acs_tables$households %>% filter(hh_id %in% geo_ids),
        price_qrf_models = price_qrf_models,
        income_pctiles   = geo_pctiles
      )
      wedge_cols <- paste0('price_wedge.', PRICE_WEDGE_TYPES)
      pu_geo <- pu_geo %>%
        select(-all_of(wedge_cols)) %>%
        left_join(geo_wedge, by = c('hh_id', 'parent_unit_id')) %>%
        mutate(across(all_of(wedge_cols), ~ coalesce(.x, 1.0)))
      cat('  ', geo, ' price wedge rows on own income ranks\n', sep = '')
    }

    enr <- data.table::fread(enr_path)
    enr_joint <- if (file.exists(joint_path)) data.table::fread(joint_path) else NULL

    for (i in seq_along(PARENT_UNIT_NAMES)) {
      pu_type <- PARENT_UNIT_NAMES[i]
      d <- build_type_data(
        pu_type               = pu_type,
        n_children            = PARENT_UNIT_N_CHILDREN[i],
        child_cat             = PARENT_UNIT_CHILD_CATEGORY[i],
        parent_units_wide     = pu_geo,
        enrollment            = enr,
        enrollment_joint      = enr_joint,
        supply_prices         = supply_prices,
        other_paid_base_price = other_paid_base_price,
        wage_perturbation     = wage_perturbation,
        tax_max               = tax_max
      )
      if (nrow(d) == 0) next

      n_choices <- nrow(get_choice_catalog(PARENT_UNIT_N_CHILDREN[i]))
      Y  <- as.matrix(d[, paste0('Y.', 1:n_choices)])
      C  <- as.matrix(d[, paste0('C.', 1:n_choices)])
      p0 <- as.matrix(d[, paste0('p.', 1:n_choices)])

      # National rds: beta / rho at full precision (yaml rounds), rows to
      # align against
      nat_path <- file.path(estimation_dir, paste0('alpha_', pu_type, '_', year, '.rds'))
      if (!file.exists(nat_path)) {
        stop('compute_substate_alphas: ', nat_path, ' missing. Run national demand ',
             'estimation (run_demand_estimation()) before sub-state alphas.')
      }
      nat <- readRDS(nat_path)
      beta <- nat$beta
      rho  <- nat$rho
      V <- compute_V(Y - C, beta, rho)

      # alpha = log p0 - V, normalised so alpha_1 = 0, first column dropped --
      # as 2a compute_alpha_matrix()
      raw_alpha <- log(pmax(p0, 1e-12)) - V
      alpha <- (raw_alpha - raw_alpha[, 1])[, -1, drop = FALSE]
      dimnames(alpha) <- NULL
      dimnames(p0) <- NULL

      row_id_cols <- intersect(c('hh_id', 'parent_unit_id', 'pseudofamily_id'), names(d))
      weight_cols <- intersect(c('child_weight.1', 'child_weight.2'), names(d))
      row_ids <- as.data.frame(d[, row_id_cols])
      child_weights <- as.data.frame(d[, weight_cols])

      # Alignment: geo rows of national rds, same order, same child weights
      in_geo <- nat$row_ids$hh_id %in% geo_ids
      nat_ids <- nat$row_ids[in_geo, row_id_cols, drop = FALSE]
      nat_w <- nat$child_weights[in_geo, weight_cols, drop = FALSE]
      rm(nat)
      # NA matches NA only; ids exact, weights to fwrite / fread round-off
      same_id <- function(a, b) {
        if (is.numeric(a) && is.numeric(b)) {
          a <- as.numeric(a); b <- as.numeric(b)
        } else {
          a <- as.character(a); b <- as.character(b)
        }
        all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
      }
      same_wt <- function(a, b) {
        all((is.na(a) & is.na(b)) |
              (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-8 * pmax(1, abs(a))))
      }
      same_ids <- nrow(nat_ids) == nrow(row_ids) &&
        all(vapply(row_id_cols, function(k) same_id(nat_ids[[k]], row_ids[[k]]), logical(1)))
      same_w <- same_ids &&
        all(vapply(weight_cols, function(k) same_wt(nat_w[[k]], child_weights[[k]]), logical(1)))
      if (!same_ids || !same_w) {
        stop('compute_substate_alphas: ', geo, ' ', pu_type, ' rows (', nrow(row_ids),
             ') do not match ', geo, ' rows of ', basename(nat_path), ' (', nrow(nat_ids),
             ') in order / child weights. Check parent_units_wide filter.')
      }

      p_rec <- compute_probs_from_alpha(alpha, V)
      max_diff <- max(abs(p_rec - p0))
      if (max_diff > 1e-6) {
        warning('Sub-state alpha recovery error for ', geo, ' ', pu_type, ': ',
                sprintf('%.2e', max_diff))
      }

      out_path <- substate_alpha_path(estimation_dir, pu_type, year, geo)
      saveRDS(list(
        alpha              = alpha,
        p0                 = p0,
        child_weights      = child_weights,
        row_ids            = row_ids,
        calibration_year   = year,
        beta               = beta,
        rho                = rho,
        geo                = geo,
        income_rank_scheme = get_income_rank_scheme(),
        urban_density      = get_urban_density_setting()
      ), out_path)
      cat('  ', pu_type, ': ', nrow(alpha), ' units -> ', basename(out_path), '\n', sep = '')
    }
    written <- c(written, geo)
  }

  invisible(written)
}



load_substate_alpha <- function(geo, estimation_dir, calibration_year) {

  #----------------------------------------------------------------------------
  # Geo's own alpha data for its run, per pu type. NULL (logged) -> caller
  # uses national alphas: geo not sub-state, scheme national (versions
  # identical), or rds missing (interface predates sub-state alphas). Rds
  # built under other scheme than this run's stops -- incl. unlabeled rds
  # (pre-scheme, built within-state) + removed schemes 'state' / 'region':
  # recalibrate.
  #
  # Params:
  #   - geo (chr): run geography code
  #   - estimation_dir (chr): <output root>/estimation
  #   - calibration_year (int): calibration year
  #
  # Returns: (list) per pu type, rds contents (alpha, p0, child_weights,
  #   row_ids, ...); NULL as above
  #----------------------------------------------------------------------------

  if (is.null(SUBSTATE_GEOGRAPHIES[[geo]])) return(NULL)

  scheme <- get_income_rank_scheme()
  if (scheme == 'national') {
    cat('  Sub-state ', geo, ': scheme ', scheme, ' ranks ', geo,
        ' same as its state -- national alphas\n', sep = '')
    return(NULL)
  }

  paths <- setNames(vapply(PARENT_UNIT_NAMES, function(t) {
    substate_alpha_path(estimation_dir, t, calibration_year, geo)
  }, character(1)), PARENT_UNIT_NAMES)
  if (!all(file.exists(paths))) {
    cat('  Sub-state ', geo, ': no ', paste(basename(paths[!file.exists(paths)]), collapse = ', '),
        ' -- interface predates sub-state alphas; national alphas, ', geo,
        ' ranked inside ', SUBSTATE_GEOGRAPHIES[[geo]]$parent, '\n', sep = '')
    return(NULL)
  }

  out <- lapply(paths, readRDS)
  for (t in names(out)) {
    built <- out[[t]]$income_rank_scheme
    if (is.null(built) || !built %in% INCOME_RANK_SCHEMES) {
      stop('Sub-state alpha ', paths[[t]], ' built under income-rank scheme "',
           if (is.null(built)) 'unlabeled (pre-scheme, = state)' else built,
           '" -- within-state / within-region ranks, removed 2026-10-12. It can no ',
           'longer be used. Recalibrate this interface (2a) under ECEC_INCOME_RANK=',
           scheme, '.')
    }
    if (built != scheme) {
      stop('Sub-state alpha ', paths[[t]], ' built under income-rank scheme "', built,
           '", this run uses "', scheme, '". Set ECEC_INCOME_RANK=', built,
           ' or recalibrate.')
    }
    check_urban_density_built(out[[t]]$urban_density, paste0('Sub-state alpha ', paths[[t]]), '2a')
  }
  cat('  Sub-state ', geo, ': own alphas (', paste(basename(paths), collapse = ', '),
      ', scheme ', scheme, ')\n', sep = '')
  out
}
