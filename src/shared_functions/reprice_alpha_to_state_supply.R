#------------------------------------------------------------------------------
# reprice_alpha_to_state_supply.R
#
# State runs (env var ECEC_STATE_ALPHA_PRICE, read in config.R; default 'state';
# 'national' = off). Rebuilds a state's stored alpha at
# state's own 2019 supply prices before demand re-anchoring.
#
# Problem: calibration (2a) builds alpha = log p0 - V at NATIONAL supply
# prices (national NSECE yaml). State run prices care at state wages +
# staffing (config/state/supply/<ST>.yaml), and demand re-anchoring
# (adjust_alpha_for_state.R) shifts stored alpha using p0 only, so state's
# base-year model sits at national prices while state supply sits at state
# prices -- first simulated year jumps by state-vs-national price gap.
#
# Fix: alpha_state = log p0 - V_state, V_state = V at state base-year supply
# prices (state L_req %*% wages + residual; at base year labor = L_0, so
# equilibrium price = supply price). Wedge, Y, p0, beta, rho unchanged;
# only care cost of sectors 2-4 changes (cost = wedge x price x hours; Other
# Paid base price national). Stored alpha is normalised to choice 1, so
#   alpha_state[, j] = alpha[, j] + dV[, j] - dV[, 1],  dV = V_nat - V_state.
#
# Y comes from calibration data (estimation/data/calibration_<type>_<year>.csv,
# Y.k). Check: Y - C_nat rebuilt here must equal calibration's net_income.k
# (skipped for sub-state geographies: own wedges). Identity check:
# softmax(alpha_state + V_state) = p0. Frozen-base-year check
# (check_state_base_year()): after re-anchoring, sector hours at state V equal
# demand adjustment's sector hours -- base-year labor L_0.
#
# Called from: 3a (initialize_simulation, state contexts)
#------------------------------------------------------------------------------



get_state_alpha_price_mode <- function() {

  #----------------------------------------------------------------------------
  # Active mode -- global state_alpha_price_mode (config.R), else env var
  # ECEC_STATE_ALPHA_PRICE, else 'state'.
  #
  # Returns: (chr) 'state' (default) | 'national' (off: alpha at national prices)
  #----------------------------------------------------------------------------

  m <- if (exists('state_alpha_price_mode', envir = globalenv())) {
    get('state_alpha_price_mode', envir = globalenv())
  } else {
    Sys.getenv('ECEC_STATE_ALPHA_PRICE', 'state')
  }
  if (identical(m, '')) m <- 'state'
  if (!m %in% c('national', 'state')) {
    stop('ECEC_STATE_ALPHA_PRICE = "', m, '" unknown. Use state (default) or national.')
  }
  m
}



state_base_supply_prices <- function(supply_params) {

  #----------------------------------------------------------------------------
  # Base-year supply prices by sector, as calibration (2a) builds them:
  # unit cost (L_req %*% w) + per-unit residual (delta_0); sector 1 (unpaid
  # center) = 0.
  #
  # Params:
  #   - supply_params (list): L_req (4 x 2), w (2), delta_0 (4, NA = no
  #       residual) -- parse_supply_params_yaml() format, 3a
  #
  # Returns: (num vec) length 4, SECTOR_MAPPING order
  #----------------------------------------------------------------------------

  unit_costs <- as.vector(supply_params$L_req %*% supply_params$w)
  delta_0 <- supply_params$delta_0
  delta_0[is.na(delta_0)] <- 0
  P <- unit_costs + delta_0
  P[1] <- 0
  P
}



compute_care_cost_matrix <- function(catalog, n_children, wedge, supply_prices,
                                     other_paid_base_price) {

  #----------------------------------------------------------------------------
  # Annual care cost by unit x choice: wedge x price x hours per child, summed
  # over children. Same arithmetic as 2a build_calibration_type_data().
  #
  # Params:
  #   - catalog (df): get_choice_catalog(n_children)
  #   - n_children (int): 1 or 2
  #   - wedge (list): price_wedge vectors (length n_units) -- center_low,
  #       center_high, home, other_paid
  #   - supply_prices (num vec): length 4, sector 1 = 0
  #   - other_paid_base_price (dbl or NULL): Other Paid base price
  #
  # Returns: (matrix) n_units x n_choices
  #----------------------------------------------------------------------------

  n_units <- length(wedge$center_low)
  n_choices <- nrow(catalog)
  wedge_by_sector <- list(`1` = rep(1, n_units), `2` = wedge$center_low,
                          `3` = wedge$center_high, `4` = wedge$home)

  one_child <- function(child) {
    sector <- catalog[[paste0('child', child, '_market_sector_id')]]
    hours  <- unname(HOURS_ANNUAL[catalog[[paste0('child', child, '_hours_choice')]]])
    ecec_type <- catalog[[paste0('child', child, '_ecec_type')]]
    M <- matrix(0, nrow = n_units, ncol = n_choices)
    for (sector_id in 1:4) {
      P_sector <- supply_prices[sector_id]
      if (is.na(P_sector) || P_sector == 0) next
      cols <- which(sector == sector_id & hours > 0)
      if (length(cols) == 0) next
      M[, cols] <- tcrossprod(wedge_by_sector[[as.character(sector_id)]] * P_sector, hours[cols])
    }
    if (!is.null(other_paid_base_price) && !is.na(other_paid_base_price) &&
        other_paid_base_price > 0) {
      w_op <- wedge$other_paid
      if (is.null(w_op)) w_op <- rep(1, n_units)
      op_cols <- which(is.na(sector) & ecec_type == 'Other Paid' & hours > 0)
      if (length(op_cols) > 0) {
        M[, op_cols] <- tcrossprod(w_op * other_paid_base_price, hours[op_cols])
      }
    }
    M
  }

  C <- one_child(1)
  if (n_children == 2) C <- C + one_child(2)
  C
}



reprice_alpha_to_supply <- function(alpha_data_by_type, state_hh_ids, calibration_data_dir,
                                    wedge_rows, P_nat, P_state, other_paid_base_price,
                                    calibration_year = 2019, check_net_income = TRUE,
                                    state_postal = '', v_fn = compute_V_crra) {

  #----------------------------------------------------------------------------
  # Rebuilds one geography's alpha at its own base-year supply prices (see
  # header). Rows of the geography only.
  #
  # Params:
  #   - alpha_data_by_type (list): per pu type, alpha rds contents (alpha, p0,
  #       child_weights, row_ids, beta, rho)
  #   - state_hh_ids (vec): geography's household ids
  #   - calibration_data_dir (chr): estimation/data (calibration_<type>_<year>.csv)
  #   - wedge_rows (df): geography's price wedge rows (hh_id, parent_unit_id,
  #       price_wedge.center_low / center_high / home / other_paid); NA -> 1
  #   - P_nat (num vec): national supply prices, length 4, calibration's
  #   - P_state (num vec): state supply prices, length 4
  #   - other_paid_base_price (dbl): national Other Paid base price
  #   - calibration_year (int): calibration year
  #   - check_net_income (lgl): compare rebuilt Y - C_nat with calibration's
  #       net_income (FALSE for sub-state geographies on own wedges)
  #   - state_postal (chr): label for messages
  #   - v_fn (fn): V = beta * u(NI), compute_V_crra
  #
  # Returns: (list) alpha_data_by_type (geography rows, repriced alpha),
  #   V_by_type (V at state prices), diagnostics (df per type)
  #----------------------------------------------------------------------------

  out <- list()
  V_by_type <- list()
  diag <- list()
  wedge_key <- paste(wedge_rows$hh_id, wedge_rows$parent_unit_id, sep = '|')

  for (pu_type in names(alpha_data_by_type)) {
    ad <- alpha_data_by_type[[pu_type]]
    n_children <- PARENT_UNIT_N_CHILDREN[match(pu_type, PARENT_UNIT_NAMES)]
    catalog <- get_choice_catalog(n_children)
    K <- nrow(catalog)
    in_state <- ad$row_ids$hh_id %in% state_hh_ids
    if (!any(in_state)) next
    key_cols <- names(ad$row_ids)
    rid <- ad$row_ids[in_state, , drop = FALSE]
    a_key <- do.call(paste, c(lapply(rid, as.character), sep = '|'))

    # Y (and calibration's net income, for the check) of the geography's rows
    csv_path <- file.path(calibration_data_dir, paste0('calibration_', pu_type, '_', calibration_year, '.csv'))
    if (!file.exists(csv_path)) {
      stop('ECEC_STATE_ALPHA_PRICE=state: ', csv_path, ' missing. The state-price alpha needs the ',
           'calibration data (Y by choice) of the interface.')
    }
    y_cols <- paste0('Y.', seq_len(K)); ni_cols <- paste0('net_income.', seq_len(K))
    hdr <- names(data.table::fread(csv_path, nrows = 0))
    want <- c(key_cols, y_cols, if (check_net_income) ni_cols)
    miss <- setdiff(want, hdr)
    if (length(miss) > 0) {
      stop('ECEC_STATE_ALPHA_PRICE=state: ', basename(csv_path), ' lacks columns ',
           paste(head(miss, 5), collapse = ', '), '.')
    }
    cal <- data.table::fread(csv_path, select = want)
    cal <- cal[cal$hh_id %in% state_hh_ids, ]
    c_key <- do.call(paste, c(lapply(as.data.frame(cal)[key_cols], as.character), sep = '|'))
    idx <- match(a_key, c_key)
    if (anyNA(idx)) {
      stop('State ', state_postal, ' ', pu_type, ': ', sum(is.na(idx)), ' alpha rows missing from ',
           basename(csv_path), '.')
    }
    Y <- as.matrix(as.data.frame(cal)[idx, y_cols])

    # Wedges of these rows (NA / absent -> 1.0, as calibration)
    wi <- match(paste(rid$hh_id, rid$parent_unit_id, sep = '|'), wedge_key)
    wedge <- lapply(c(center_low = 'price_wedge.center_low', center_high = 'price_wedge.center_high',
                      home = 'price_wedge.home', other_paid = 'price_wedge.other_paid'), function(col) {
      v <- if (col %in% names(wedge_rows)) wedge_rows[[col]][wi] else rep(NA_real_, length(wi))
      v[is.na(v)] <- 1.0
      v
    })

    C_nat   <- compute_care_cost_matrix(catalog, n_children, wedge, P_nat, other_paid_base_price)
    C_state <- compute_care_cost_matrix(catalog, n_children, wedge, P_state, other_paid_base_price)

    ni_err <- NA_real_
    if (check_net_income) {
      NI_cal <- as.matrix(as.data.frame(cal)[idx, ni_cols])
      ni_err <- max(abs((Y - C_nat) - NI_cal) / pmax(1, abs(NI_cal)))
      if (!is.finite(ni_err) || ni_err > 1e-6) {
        stop('State ', state_postal, ' ', pu_type, ': net income rebuilt at national prices differs ',
             'from calibration (max relative gap ', signif(ni_err, 3), '). Wedge rows or national ',
             'supply prices do not match the interface.')
      }
    }

    V_nat   <- v_fn(Y - C_nat, ad$beta, ad$rho)
    V_state <- v_fn(Y - C_state, ad$beta, ad$rho)
    dV <- V_nat - V_state
    alpha_new <- ad$alpha[in_state, , drop = FALSE] + dV[, -1, drop = FALSE] - dV[, 1]

    p0 <- ad$p0[in_state, , drop = FALSE]
    id_err <- max(abs(compute_probs_from_alpha(alpha_new, V_state) - p0))
    if (id_err > 1e-6) {
      stop('State ', state_postal, ' ', pu_type, ': repriced alpha does not reproduce p0 at state ',
           'prices (max gap ', signif(id_err, 3), ').')
    }

    out[[pu_type]] <- list(alpha = alpha_new, p0 = p0,
                           child_weights = ad$child_weights[in_state, , drop = FALSE],
                           row_ids = rid, beta = ad$beta, rho = ad$rho)
    V_by_type[[pu_type]] <- V_state
    diag[[pu_type]] <- data.frame(type = pu_type, units = nrow(alpha_new),
                                  max_abs_dV = max(abs(dV)), mean_abs_dV = mean(abs(dV)),
                                  ni_check_error = ni_err, identity_error = id_err)
  }

  list(alpha_data_by_type = out, V_by_type = V_by_type, diagnostics = do.call(rbind, diag))
}



check_state_base_year <- function(alpha_by_type, V_by_type, alpha_data_by_type,
                                  expected_sector_hours, state_postal = '', tol = 1e-6) {

  #----------------------------------------------------------------------------
  # Frozen-base-year check after re-anchoring: base-year choice probabilities
  # at state V and re-anchored alpha give sector hours equal to the demand
  # adjustment's sector hours (the base-year labor L_0 uses). Stops on a gap.
  #
  # Params:
  #   - alpha_by_type (list): re-anchored alpha (adjust_alpha_for_state())
  #   - V_by_type (list): V at state prices (reprice_alpha_to_supply())
  #   - alpha_data_by_type (list): repriced alpha data (child weights)
  #   - expected_sector_hours (num vec): demand adjustment's sector_hours
  #   - state_postal (chr): label
  #   - tol (dbl): relative tolerance
  #
  # Returns: (num) max relative gap over sectors, invisibly
  #----------------------------------------------------------------------------

  hours <- rep(0, length(expected_sector_hours))
  for (pu_type in names(V_by_type)) {
    n_children <- PARENT_UNIT_N_CHILDREN[match(pu_type, PARENT_UNIT_NAMES)]
    sh <- get_sector_hours_matrices(n_children)
    P <- compute_probs_from_alpha(alpha_by_type[[pu_type]], V_by_type[[pu_type]])
    w <- alpha_data_by_type[[pu_type]]$child_weights
    h <- crossprod(w[['child_weight.1']], P) %*% sh$H1
    if (n_children == 2) h <- h + crossprod(w[['child_weight.2']], P) %*% sh$H2
    hours <- hours + as.vector(h)
  }
  gap <- max(abs(hours - expected_sector_hours) / pmax(1, abs(expected_sector_hours)))
  cat(sprintf('  Frozen base year (%s): sector hours at state prices vs re-anchored hours, max relative gap %.2e\n',
              state_postal, gap))
  if (!is.finite(gap) || gap > tol) {
    stop('State ', state_postal, ': base-year sector hours at state prices differ from the ',
         're-anchored hours (max relative gap ', signif(gap, 3), ').')
  }
  invisible(gap)
}
