get_labor_cost_matrix <- function(supply_params) {

  #----------------------------------------------------------------------------
  # Employer labor cost per hour of care, by sector and staff type, in hours of
  # pay: staffing ratio (L_req, worker hours) x labor cost multiplier (M,
  # Stanford STANFORD_WAGE_MULTIPLIERS -- benefits, payroll tax, owner
  # discount). Unit labor cost = this %*% w. Labor demand and payroll
  # subsidies use L_req alone -- worker hours, as in Stanford white paper
  # sec. 2.2 (multipliers enter price only). M absent (supply files before
  # 2026-11-23, or --no-stanford-wedges) -> all 1.
  #
  # Params:
  #   - supply_params (list): Supply parameters w/ L_req (4 x 2) and optional M (4 x 2)
  #
  # Returns: (num matrix) 4 x 2, L_req * M
  #----------------------------------------------------------------------------

  M <- supply_params$M
  if (is.null(M)) return(supply_params$L_req)
  if (!identical(dim(M), dim(supply_params$L_req))) {
    stop('get_labor_cost_matrix: M is ', paste(dim(M), collapse = ' x '), ', L_req is ',
         paste(dim(supply_params$L_req), collapse = ' x '), '. Rebuild supply parameters (run 1a).')
  }
  supply_params$L_req * M
}
