#------------------------------------------------------------------------------
# report_figures.R
#
# Generates report tables and figures from report_runs output.
#
# Outputs:
#   1. Budget cost table: nominal ($B) and share of GDP by decade
#   2. Fiscal NPV table: cost, return, and share recovered at 3% discount rate
#   3. Enrollment figures: stacked + faceted by care type (2030)
#   4. Employment figure: PT/FT stacked, faceted by sex (2030)
#   5. Distributional figures: welfare income change in $ and % of AGI by quintile
#   6. Child earnings figures: earnings at 27 change in $ and % by quintile
#
# Usage:
#   Rscript test/report_figures.R <timestamp>
#   e.g., Rscript test/report_figures.R 202602101200
#------------------------------------------------------------------------------

library(tidyverse)
library(yaml)
library(flextable)


# ---- Configuration ----------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop('Usage: Rscript test/report_figures.R <timestamp>')
}

TIMESTAMP <- args[1]

# Load paths from default config
default_paths <- read_yaml('./config/default_paths.yaml')
OUTPUT_ROOT <- file.path(default_paths$roots$output, TIMESTAMP)
REPORT_DIR  <- file.path(OUTPUT_ROOT, 'figures')
dir.create(REPORT_DIR, showWarnings = FALSE, recursive = TRUE)
MACRO_PATH  <- file.path(default_paths$roots$input, default_paths$dependencies$`Macro-Projections`)

# All scenarios including baseline
ALL_SCENARIOS <- c('baseline', 'universal', 'means_tested', 'arpa_cdctc', 'wage_subsidy', 'child_ubi_1k')
COUNTERFACTUALS <- ALL_SCENARIOS[ALL_SCENARIOS != 'baseline']

# Display labels for scenarios
SCENARIO_LABELS <- c(
  'baseline'     = 'Baseline',
  'universal'    = 'Universal Subsidy',
  'means_tested' = 'Income-Limited Subsidy',
  'arpa_cdctc'   = 'Expanded Care Credit',
  'wage_subsidy' = 'Employer Tax Credit',
  'child_ubi_1k' = 'Child Allowance'
)

# Care type aggregation mapping
CARE_TYPE_MAP <- c(
  'Parent Only'               = 'Parent Only',
  'High-Priced Center-Based'  = 'Center-Based',
  'Low-Priced Center-Based'   = 'Center-Based',
  'Unpaid Center-Based'       = 'Center-Based',
  'Paid Home-Based'           = 'Home-Based',
  'Unpaid Home-Based'         = 'Home-Based',
  'Other Paid'                = 'Other',
  'Other Unpaid'              = 'Other'
)

# Display order for stacked bar (bottom to top)
CARE_TYPE_ORDER <- c('Other', 'Home-Based', 'Center-Based', 'Parent Only')
CARE_TYPE_COLORS <- c(
  'Parent Only'  = '#8C564B',
  'Center-Based' = '#17BECF',
  'Home-Based'   = '#BCBD22',
  'Other'        = '#E377C2'
)

# Scenario colors (all scenarios including baseline)
SCENARIO_COLORS <- c(
  'Baseline'                  = '#333333',
  'Universal Subsidy'         = '#F28E2B',
  'Income-Limited Subsidy'    = '#E15759',
  'Expanded Care Credit'      = '#76B7B2',
  'Employer Tax Credit'       = '#59A14F',
  'Child Allowance'           = '#B07AA1'
)
SCENARIO_COLORS_CF <- SCENARIO_COLORS[SCENARIO_LABELS[COUNTERFACTUALS]]

# Snapshot year for enrollment and employment figures
ENROLLMENT_YEAR <- 2030

# Decade boundaries (fiscal years)
DECADE_NAMES <- c('Budget Window', 'Second Decade', 'Third Decade')
DECADE_YEARS <- list(2026:2035, 2036:2045, 2046:2055)
names(DECADE_YEARS) <- DECADE_NAMES


# ---- Helpers ----------------------------------------------------------------

lighten_color <- function(color, amount = 0.4) {

  #----------------------------------------------------------------------------
  # Lighten a hex color toward white by blending with (1,1,1).
  #
  # Params:
  #   - color (chr): hex color string (e.g. '#FF0000')
  #   - amount (dbl): blend fraction toward white, 0 = unchanged, 1 = white
  #
  # Returns: (chr) hex color string
  #----------------------------------------------------------------------------

  col_rgb <- col2rgb(color) / 255
  lighter <- col_rgb + (1 - col_rgb) * amount
  rgb(lighter[1], lighter[2], lighter[3])
}

format_dollar_label <- function(x) {

  #----------------------------------------------------------------------------
  # Format numeric values as rounded dollar labels for chart annotations.
  # Values <10 become '<$10'; <100 round to nearest 10; <1000 round to
  # nearest 50; larger values display as '$X.XK'.
  #
  # Params:
  #   - x (num vec): numeric values to format
  #
  # Returns: (chr vec) formatted dollar label strings
  #----------------------------------------------------------------------------

  sapply(x, function(v) {
    if (is.na(v)) return('')
    av <- abs(v)
    sign_str <- if (v < 0) '-' else ''
    if (av < 10) {
      paste0(sign_str, '<$10')
    } else if (av < 100) {
      paste0(sign_str, '$', round(av / 10) * 10)
    } else if (av < 1000) {
      paste0(sign_str, '$', round(av / 50) * 50)
    } else {
      paste0(sign_str, '$', formatC(round(av / 100) * 100 / 1000, format = 'f', digits = 1), 'K')
    }
  })
}

# Shared theme for bar charts
chart_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(face = 'bold'),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(face = 'bold', size = 12)
  )


# ---- Load macro projections for GDP ----------------------------------------

cat('Loading macro projections from:', MACRO_PATH, '\n')
macro <- bind_rows(
  read_csv(file.path(MACRO_PATH, 'historical.csv'),  show_col_types = FALSE),
  read_csv(file.path(MACRO_PATH, 'projections.csv'), show_col_types = FALSE)
)

if (!'gdp' %in% names(macro)) {
  stop('Macro projections missing "gdp" column. Available columns: ',
       paste(names(macro), collapse = ', '))
}

# Apply fiscal year conversion to GDP (FY(t) = 0.25 * CY(t-1) + 0.75 * CY(t))
gdp_cy <- macro %>%
  filter(year >= 2025 & year <= 2055) %>%
  select(year, gdp) %>%
  arrange(year)

gdp_fy <- tibble(
  fiscal_year = 2026:2055,
  gdp = sapply(2026:2055, function(fy) {
    cy_prior <- gdp_cy$gdp[gdp_cy$year == fy - 1]
    cy_curr  <- gdp_cy$gdp[gdp_cy$year == fy]
    if (length(cy_prior) == 0 || length(cy_curr) == 0) return(NA_real_)
    0.25 * cy_prior + 0.75 * cy_curr
  })
)

# Sum GDP by decade
gdp_by_decade <- sapply(DECADE_YEARS, function(years) {
  sum(gdp_fy$gdp[gdp_fy$fiscal_year %in% years], na.rm = TRUE)
})


# ---- Load budget effects for each scenario ----------------------------------

cat('Loading budget effects from:', OUTPUT_ROOT, '\n\n')

# Accumulate: scenario -> decade costs (billions) and raw annual data
cost_by_decade   <- list()
direct_by_decade <- list()
tax_by_decade    <- list()
budget_raw       <- list()

for (scenario in COUNTERFACTUALS) {
  budget_path <- file.path(OUTPUT_ROOT, 'simulation', scenario, 'totals', 'deltas', 'budget_effect.csv')

  if (!file.exists(budget_path)) {
    cat('  WARNING: Missing budget_effect.csv for', scenario, '\n')
    cat('    Expected:', budget_path, '\n')
    cost_by_decade[[scenario]]   <- rep(NA_real_, 3)
    direct_by_decade[[scenario]] <- rep(NA_real_, 3)
    tax_by_decade[[scenario]]    <- rep(NA_real_, 3)
    next
  }

  budget <- read_csv(budget_path, show_col_types = FALSE)
  budget_raw[[scenario]] <- budget

  # Net cost = -budget_effect (positive = cost to government)
  cost_by_decade[[scenario]] <- sapply(DECADE_YEARS, function(years) {
    sum(-budget$total_budget_effect[budget$fiscal_year %in% years], na.rm = TRUE)
  })

  # Direct costs = demand + supply + employer subsidies
  direct_by_decade[[scenario]] <- sapply(DECADE_YEARS, function(years) {
    b_sub <- budget[budget$fiscal_year %in% years, ]
    sum(b_sub$total_demand_subsidy + b_sub$total_supply_subsidy + b_sub$total_employer_subsidy, na.rm = TRUE)
  })

  # Tax effects = cdctc cost - tax revenue change (negative = reduces net cost)
  tax_by_decade[[scenario]] <- sapply(DECADE_YEARS, function(years) {
    b_sub <- budget[budget$fiscal_year %in% years, ]
    sum(b_sub$total_cdctc_cost - b_sub$total_tax_change, na.rm = TRUE)
  })
}

# Flip signs: negative = cost to government, positive = revenue
for (scenario in COUNTERFACTUALS) {
  cost_by_decade[[scenario]]   <- -cost_by_decade[[scenario]]
  direct_by_decade[[scenario]] <- -direct_by_decade[[scenario]]
  tax_by_decade[[scenario]]    <- -tax_by_decade[[scenario]]
}

# Reclassify Child Allowance: treat as direct cash transfer, not tax effect
direct_by_decade[['child_ubi_1k']] <- cost_by_decade[['child_ubi_1k']]
tax_by_decade[['child_ubi_1k']]    <- rep(0, length(DECADE_NAMES))


# ---- Print unified table with multi-level headers ---------------------------

fmt_dollar <- function(val, suffix = ' B') {

  #----------------------------------------------------------------------------
  # Format a dollar value with proper sign placement for table display.
  # Returns 'N/A' for missing values and '0.0' for near-zero values.
  #
  # Params:
  #   - val (dbl): dollar value to format
  #   - suffix (chr): suffix to append (default ' B' for billions)
  #
  # Returns: (chr) formatted string like '-1.2 B'
  #----------------------------------------------------------------------------

  if (is.na(val)) return('N/A')
  if (abs(val) < 0.05) return(paste0('0.0', suffix))
  sprintf('%s%.1f%s', ifelse(val < 0, '-', ''), abs(val), suffix)
}

fmt_gdp <- function(val) {

  #----------------------------------------------------------------------------
  # Format a GDP share value as a signed percentage string for table display.
  # Returns 'N/A' for missing values and '0.000%' for near-zero values.
  #
  # Params:
  #   - val (dbl): GDP share value (already multiplied by 100)
  #
  # Returns: (chr) formatted string like '-0.034%'
  #----------------------------------------------------------------------------

  if (is.na(val)) return('N/A')
  if (abs(val) < 0.0005) return('0.000%')
  sprintf('%s%.3f%%', ifelse(val < 0, '-', ''), abs(val))
}

nom_w <- 14
gdp_w <- 12
decade_w <- nom_w + gdp_w
scenario_w <- 30
n_decades <- length(DECADE_NAMES)
table_width <- scenario_w + decade_w * n_decades

# Top-level group headers (one per decade)
cat(paste(rep('=', table_width), collapse = ''), '\n')
cat(sprintf('%-*s', scenario_w, ''))
for (decade in DECADE_NAMES) {
  cat(sprintf('%-*s', decade_w, paste0('    ', decade)))
}
cat('\n')

# Sub-headers (Nominal + GDP% under each decade)
cat(sprintf('%-*s', scenario_w, 'Scenario'))
for (i in seq_len(n_decades)) {
  cat(sprintf('%*s%*s', nom_w, 'Nominal', gdp_w, 'GDP%'))
}
cat('\n')
cat(paste(rep('-', table_width), collapse = ''), '\n')

# Data rows (with Direct Subsidy Costs / Effects on Tax Revenue sub-rows)
for (scenario in COUNTERFACTUALS) {
  costs   <- cost_by_decade[[scenario]]
  directs <- direct_by_decade[[scenario]]
  taxes   <- tax_by_decade[[scenario]]
  shares  <- costs / gdp_by_decade * 100

  # Main scenario row
  cat(sprintf('%-*s', scenario_w, SCENARIO_LABELS[scenario]))
  for (j in seq_len(n_decades)) {
    cat(sprintf('%*s%*s', nom_w, fmt_dollar(costs[j]), gdp_w, fmt_gdp(shares[j])))
  }
  cat('\n')

  # Direct Subsidy Costs sub-row
  direct_shares <- directs / gdp_by_decade * 100
  cat(sprintf('%-*s', scenario_w, '  Direct Subsidy Costs'))
  for (j in seq_len(n_decades)) {
    cat(sprintf('%*s%*s', nom_w, fmt_dollar(directs[j]), gdp_w, fmt_gdp(direct_shares[j])))
  }
  cat('\n')

  # Effects on Tax Revenue sub-row
  tax_shares <- taxes / gdp_by_decade * 100
  cat(sprintf('%-*s', scenario_w, '  Effects on Tax Revenue'))
  for (j in seq_len(n_decades)) {
    cat(sprintf('%*s%*s', nom_w, fmt_dollar(taxes[j]), gdp_w, fmt_gdp(tax_shares[j])))
  }
  cat('\n')
}

cat(paste(rep('=', table_width), collapse = ''), '\n')
cat('\nNote: Negative = net cost to government | Positive = net revenue\n')
cat('      GDP share = nominal decade cost / nominal decade GDP\n')

# Save budget cost table as CSV
budget_csv <- tibble(
  scenario = SCENARIO_LABELS[COUNTERFACTUALS],
  nominal_budget_window_B = sapply(COUNTERFACTUALS, function(s) cost_by_decade[[s]][1]),
  nominal_second_decade_B = sapply(COUNTERFACTUALS, function(s) cost_by_decade[[s]][2]),
  nominal_third_decade_B  = sapply(COUNTERFACTUALS, function(s) cost_by_decade[[s]][3]),
  direct_budget_window_B  = sapply(COUNTERFACTUALS, function(s) direct_by_decade[[s]][1]),
  direct_second_decade_B  = sapply(COUNTERFACTUALS, function(s) direct_by_decade[[s]][2]),
  direct_third_decade_B   = sapply(COUNTERFACTUALS, function(s) direct_by_decade[[s]][3]),
  tax_budget_window_B     = sapply(COUNTERFACTUALS, function(s) tax_by_decade[[s]][1]),
  tax_second_decade_B     = sapply(COUNTERFACTUALS, function(s) tax_by_decade[[s]][2]),
  tax_third_decade_B      = sapply(COUNTERFACTUALS, function(s) tax_by_decade[[s]][3]),
  gdp_share_budget_window = sapply(COUNTERFACTUALS, function(s) cost_by_decade[[s]][1] / gdp_by_decade[1]),
  gdp_share_second_decade = sapply(COUNTERFACTUALS, function(s) cost_by_decade[[s]][2] / gdp_by_decade[2]),
  gdp_share_third_decade  = sapply(COUNTERFACTUALS, function(s) cost_by_decade[[s]][3] / gdp_by_decade[3])
)
budget_csv_path <- file.path(REPORT_DIR, 'budget_cost.csv')
write_csv(budget_csv, budget_csv_path)
cat('Saved budget cost table to:', budget_csv_path, '\n')

# Budget cost table image (3 rows per scenario: net, direct, tax)
# Column order: grouped by decade (nom, gdp% for each decade)
budget_rows <- list()
for (s in COUNTERFACTUALS) {
  # Net cost row
  net_row <- data.frame(Scenario = SCENARIO_LABELS[s], row_type = 'net',
                        stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_along(DECADE_NAMES)) {
    net_row[[paste0('nom_', i)]] <- cost_by_decade[[s]][i]
    net_row[[paste0('gdp_', i)]] <- cost_by_decade[[s]][i] / gdp_by_decade[i] * 100
  }
  # Direct Subsidy Costs sub-row
  direct_row <- data.frame(Scenario = '  Direct Subsidy Costs', row_type = 'sub',
                           stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_along(DECADE_NAMES)) {
    direct_row[[paste0('nom_', i)]] <- direct_by_decade[[s]][i]
    direct_row[[paste0('gdp_', i)]] <- direct_by_decade[[s]][i] / gdp_by_decade[i] * 100
  }
  # Effects on Tax Revenue sub-row
  tax_row <- data.frame(Scenario = '  Effects on Tax Revenue', row_type = 'sub',
                        stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_along(DECADE_NAMES)) {
    tax_row[[paste0('nom_', i)]] <- tax_by_decade[[s]][i]
    tax_row[[paste0('gdp_', i)]] <- tax_by_decade[[s]][i] / gdp_by_decade[i] * 100
  }
  budget_rows <- c(budget_rows, list(net_row, direct_row, tax_row))
}
budget_tbl <- bind_rows(budget_rows)
sub_row_indices <- which(budget_tbl$row_type == 'sub')
budget_tbl$row_type <- NULL

# Reorder columns: Scenario, then (nom_i, gdp_i) pairs grouped by decade
budget_tbl <- budget_tbl[, c('Scenario',
  'nom_1', 'gdp_1', 'nom_2', 'gdp_2', 'nom_3', 'gdp_3')]

budget_ft <- flextable(budget_tbl) %>%
  set_header_labels(
    nom_1 = 'Nominal ($B)', gdp_1 = 'GDP%',
    nom_2 = 'Nominal ($B)', gdp_2 = 'GDP%',
    nom_3 = 'Nominal ($B)', gdp_3 = 'GDP%'
  ) %>%
  add_header_row(
    values = c('', DECADE_NAMES[1], DECADE_NAMES[2], DECADE_NAMES[3]),
    colwidths = c(1, 2, 2, 2)
  ) %>%
  colformat_double(j = c('nom_1', 'nom_2', 'nom_3'), digits = 0, big.mark = ',') %>%
  colformat_double(j = c('gdp_1', 'gdp_2', 'gdp_3'), digits = 2, suffix = '%') %>%
  italic(i = sub_row_indices, part = 'body') %>%
  fontsize(i = sub_row_indices, size = 9, part = 'body') %>%
  color(i = sub_row_indices, color = 'grey40', part = 'body') %>%
  align(j = 2:7, align = 'center', part = 'all') %>%
  align(j = 1, align = 'left', part = 'all') %>%
  bold(part = 'header') %>%
  fontsize(size = 10, part = 'all') %>%
  fontsize(size = 11, part = 'header') %>%
  fontsize(i = sub_row_indices, size = 9, part = 'body') %>%
  padding(padding = 4, part = 'all') %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 2), part = 'header') %>%
  hline(i = 1, border = fp_border_default(width = 1), part = 'header') %>%
  hline_bottom(border = fp_border_default(width = 2), part = 'body') %>%
  hline_top(border = fp_border_default(width = 1), part = 'body') %>%
  autofit()

budget_tbl_path <- file.path(REPORT_DIR, 'budget_cost_table.png')
save_as_image(budget_ft, path = budget_tbl_path, res = 200)
cat('Saved budget cost table image to:', budget_tbl_path, '\n')

# ---- Table: 10-Year Annual Budget Cost ----------------------------------------

BUDGET_WINDOW_YEARS <- DECADE_YEARS[['Budget Window']]

cat('\n\n')
annual_col_w <- 12
annual_scenario_w <- 30
annual_table_width <- annual_scenario_w + annual_col_w * length(BUDGET_WINDOW_YEARS)

cat(paste(rep('=', annual_table_width), collapse = ''), '\n')
cat(sprintf('%-*s', annual_scenario_w, 'Scenario (nominal $B)'))
for (yr in BUDGET_WINDOW_YEARS) {
  cat(sprintf('%*s', annual_col_w, yr))
}
cat('\n')
cat(paste(rep('-', annual_table_width), collapse = ''), '\n')

annual_cost_list   <- list()
annual_direct_list <- list()
annual_tax_list    <- list()
for (scenario in COUNTERFACTUALS) {
  if (is.null(budget_raw[[scenario]])) next
  b <- budget_raw[[scenario]]
  annual_costs <- sapply(BUDGET_WINDOW_YEARS, function(yr) {
    val <- -b$total_budget_effect[b$fiscal_year == yr]
    if (length(val) == 0) NA_real_ else val
  })
  annual_directs <- sapply(BUDGET_WINDOW_YEARS, function(yr) {
    row <- b[b$fiscal_year == yr, ]
    if (nrow(row) == 0) return(NA_real_)
    sum(row$total_demand_subsidy + row$total_supply_subsidy + row$total_employer_subsidy, na.rm = TRUE)
  })
  annual_taxes <- sapply(BUDGET_WINDOW_YEARS, function(yr) {
    row <- b[b$fiscal_year == yr, ]
    if (nrow(row) == 0) return(NA_real_)
    sum(row$total_cdctc_cost - row$total_tax_change, na.rm = TRUE)
  })
  # Flip signs: negative = cost, positive = revenue
  annual_cost_list[[scenario]]   <- -annual_costs
  annual_direct_list[[scenario]] <- -annual_directs
  annual_tax_list[[scenario]]    <- -annual_taxes

  # Reclassify Child Allowance: treat as direct cash transfer, not tax effect
  if (scenario == 'child_ubi_1k') {
    annual_direct_list[[scenario]] <- annual_cost_list[[scenario]]
    annual_tax_list[[scenario]]    <- rep(0, length(BUDGET_WINDOW_YEARS))
  }

  # Main scenario row (use flipped values from lists)
  cat(sprintf('%-*s', annual_scenario_w, SCENARIO_LABELS[scenario]))
  for (cost in annual_cost_list[[scenario]]) {
    if (is.na(cost)) {
      cat(sprintf('%*s', annual_col_w, 'N/A'))
    } else {
      cat(sprintf('%*s', annual_col_w, fmt_dollar(cost, '')))
    }
  }
  cat('\n')

  # Direct Subsidy Costs sub-row
  cat(sprintf('%-*s', annual_scenario_w, '  Direct Subsidy Costs'))
  for (val in annual_direct_list[[scenario]]) {
    if (is.na(val)) {
      cat(sprintf('%*s', annual_col_w, ''))
    } else {
      cat(sprintf('%*s', annual_col_w, fmt_dollar(val, '')))
    }
  }
  cat('\n')

  # Effects on Tax Revenue sub-row
  cat(sprintf('%-*s', annual_scenario_w, '  Effects on Tax Revenue'))
  for (val in annual_tax_list[[scenario]]) {
    if (is.na(val)) {
      cat(sprintf('%*s', annual_col_w, ''))
    } else {
      cat(sprintf('%*s', annual_col_w, fmt_dollar(val, '')))
    }
  }
  cat('\n')
}
cat(paste(rep('=', annual_table_width), collapse = ''), '\n')

# Save annual cost CSV (net cost, direct costs, tax effects per scenario)
annual_cost_df <- tibble(fiscal_year = BUDGET_WINDOW_YEARS)
for (scenario in COUNTERFACTUALS) {
  if (!is.null(annual_cost_list[[scenario]])) {
    lbl <- SCENARIO_LABELS[scenario]
    annual_cost_df[[lbl]]                       <- annual_cost_list[[scenario]]
    annual_cost_df[[paste0(lbl, ' (Direct)')]]  <- annual_direct_list[[scenario]]
    annual_cost_df[[paste0(lbl, ' (Tax)')]]     <- annual_tax_list[[scenario]]
  }
}
annual_csv_path <- file.path(REPORT_DIR, 'annual_budget_cost.csv')
write_csv(annual_cost_df, annual_csv_path)
cat('Saved annual budget cost table to:', annual_csv_path, '\n')

# Annual budget cost table image (3 rows per scenario: net, direct, tax)
annual_rows <- list()
for (s in COUNTERFACTUALS) {
  if (is.null(annual_cost_list[[s]])) next
  # Net cost row
  net_row <- data.frame(Scenario = SCENARIO_LABELS[s], row_type = 'net',
                        stringsAsFactors = FALSE, check.names = FALSE)
  for (yr in BUDGET_WINDOW_YEARS) {
    net_row[[as.character(yr)]] <- annual_cost_list[[s]][which(BUDGET_WINDOW_YEARS == yr)]
  }
  # Direct Subsidy Costs sub-row
  direct_row <- data.frame(Scenario = '  Direct Subsidy Costs', row_type = 'sub',
                           stringsAsFactors = FALSE, check.names = FALSE)
  for (yr in BUDGET_WINDOW_YEARS) {
    direct_row[[as.character(yr)]] <- annual_direct_list[[s]][which(BUDGET_WINDOW_YEARS == yr)]
  }
  # Effects on Tax Revenue sub-row
  tax_row <- data.frame(Scenario = '  Effects on Tax Revenue', row_type = 'sub',
                        stringsAsFactors = FALSE, check.names = FALSE)
  for (yr in BUDGET_WINDOW_YEARS) {
    tax_row[[as.character(yr)]] <- annual_tax_list[[s]][which(BUDGET_WINDOW_YEARS == yr)]
  }
  annual_rows <- c(annual_rows, list(net_row, direct_row, tax_row))
}
annual_tbl <- bind_rows(annual_rows)

if (length(annual_rows) > 0 && ncol(annual_tbl) > 0) {
  annual_sub_indices <- which(annual_tbl$row_type == 'sub')
  annual_tbl$row_type <- NULL

  yr_cols <- as.character(BUDGET_WINDOW_YEARS)
  annual_ft <- flextable(annual_tbl) %>%
    colformat_double(j = yr_cols, digits = 0, big.mark = ',', suffix = 'B') %>%
    italic(i = annual_sub_indices, part = 'body') %>%
    fontsize(i = annual_sub_indices, size = 8, part = 'body') %>%
    color(i = annual_sub_indices, color = 'grey40', part = 'body') %>%
    align(j = yr_cols, align = 'right', part = 'all') %>%
    align(j = 1, align = 'left', part = 'all') %>%
    bold(part = 'header') %>%
    fontsize(size = 9, part = 'all') %>%
    fontsize(size = 10, part = 'header') %>%
    fontsize(i = annual_sub_indices, size = 8, part = 'body') %>%
    padding(padding = 3, part = 'all') %>%
    border_remove() %>%
    hline_top(border = fp_border_default(width = 2), part = 'header') %>%
    hline_bottom(border = fp_border_default(width = 2), part = 'body') %>%
    hline_top(border = fp_border_default(width = 1), part = 'body') %>%
    autofit()

  annual_tbl_path <- file.path(REPORT_DIR, 'annual_budget_cost_table.png')
  save_as_image(annual_ft, path = annual_tbl_path, res = 200)
  cat('Saved annual budget cost table image to:', annual_tbl_path, '\n')
} else {
  cat('Skipping annual budget cost table image: no budget data available.\n')
}


# ---- Table: Fiscal NPV at 3% Discount Rate ----------------------------------

cat('\n\nLoading fiscal NPV data...\n')

npv_data <- list()

for (scenario in COUNTERFACTUALS) {
  npv_path <- file.path(OUTPUT_ROOT, 'simulation', scenario, 'totals', 'deltas', 'fiscal_npv.csv')

  if (!file.exists(npv_path)) {
    cat('  WARNING: Missing fiscal_npv.csv for', scenario, '\n')
    npv_data[[scenario]] <- list(cost_npv = NA_real_, return_npv = NA_real_, share_recovered = NA_real_)
    next
  }

  npv <- read_csv(npv_path, show_col_types = FALSE) %>%
    filter(discount_rate == 0.03)

  if (nrow(npv) == 0) {
    cat('  WARNING: No 3% discount rate row for', scenario, '\n')
    npv_data[[scenario]] <- list(cost_npv = NA_real_, return_npv = NA_real_, share_recovered = NA_real_)
    next
  }

  npv_data[[scenario]] <- list(
    cost_npv       = npv$cost_npv[1],
    return_npv     = npv$return_npv[1],
    share_recovered = npv$share_recovered[1]
  )
}

# Print table
npv_col_w <- 20
npv_scenario_w <- 30
npv_table_width <- npv_scenario_w + npv_col_w * 3

cat(paste(rep('=', npv_table_width), collapse = ''), '\n')
cat(sprintf('%-*s%*s%*s%*s\n',
            npv_scenario_w, 'Scenario',
            npv_col_w, 'Upfront Net Cost ($B)',
            npv_col_w, 'Long-Run Returns ($B)',
            npv_col_w, 'Fiscal Recovery Ratio'))
cat(paste(rep('-', npv_table_width), collapse = ''), '\n')

for (scenario in COUNTERFACTUALS) {
  d <- npv_data[[scenario]]
  cat(sprintf('%-*s', npv_scenario_w, SCENARIO_LABELS[scenario]))

  if (is.na(d$cost_npv)) {
    cat(sprintf('%*s%*s%*s', npv_col_w, 'N/A', npv_col_w, 'N/A', npv_col_w, 'N/A'))
  } else {
    cat(sprintf('%*s%*s%*s',
                npv_col_w, sprintf('$%.2f B', d$cost_npv),
                npv_col_w, sprintf('$%.2f B', d$return_npv),
                npv_col_w, sprintf('%.1f%%', d$share_recovered * 100)))
  }
  cat('\n')
}

cat(paste(rep('=', npv_table_width), collapse = ''), '\n')
cat('Note: All values at 3% discount rate (Hendren reference rate)\n')
cat('      Upfront Net Cost = NPV of net government expenditure on ECEC cohort\n')
cat('      Long-Run Returns = NPV of lifetime tax revenue from increased child earnings\n')

# Save fiscal NPV table as CSV
npv_csv <- tibble(
  scenario        = SCENARIO_LABELS[COUNTERFACTUALS],
  cost_npv_B      = sapply(COUNTERFACTUALS, function(s) npv_data[[s]]$cost_npv),
  return_npv_B    = sapply(COUNTERFACTUALS, function(s) npv_data[[s]]$return_npv),
  share_recovered = sapply(COUNTERFACTUALS, function(s) npv_data[[s]]$share_recovered)
)
npv_csv_path <- file.path(REPORT_DIR, 'fiscal_npv.csv')
write_csv(npv_csv, npv_csv_path)
cat('Saved fiscal NPV table to:', npv_csv_path, '\n')

# Fiscal NPV table image
npv_tbl <- data.frame(
  Scenario = SCENARIO_LABELS[COUNTERFACTUALS],
  cost_npv = sapply(COUNTERFACTUALS, function(s) npv_data[[s]]$cost_npv),
  return_npv = sapply(COUNTERFACTUALS, function(s) npv_data[[s]]$return_npv),
  share_recovered = sapply(COUNTERFACTUALS, function(s) npv_data[[s]]$share_recovered * 100),
  stringsAsFactors = FALSE, check.names = FALSE
)

npv_ft <- flextable(npv_tbl) %>%
  set_header_labels(
    cost_npv = 'Upfront Net Cost ($B)',
    return_npv = 'Long-Run Returns ($B)',
    share_recovered = 'Fiscal Recovery Ratio'
  ) %>%
  colformat_double(j = c('cost_npv', 'return_npv'), digits = 0, prefix = '$', big.mark = ',') %>%
  colformat_double(j = 'share_recovered', digits = 1, suffix = '%') %>%
  align(j = 2:4, align = 'right', part = 'all') %>%
  align(j = 1, align = 'left', part = 'all') %>%
  bold(part = 'header') %>%
  fontsize(size = 10, part = 'all') %>%
  fontsize(size = 11, part = 'header') %>%
  padding(padding = 4, part = 'all') %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 2), part = 'header') %>%
  hline_bottom(border = fp_border_default(width = 2), part = 'body') %>%
  hline_top(border = fp_border_default(width = 1), part = 'body') %>%
  add_footer_lines('Upfront Net Cost = NPV of net government expenditure. Long-Run Returns = NPV of lifetime tax revenue from increased child earnings. 3% discount rate.') %>%
  fontsize(size = 8, part = 'footer') %>%
  autofit()

npv_tbl_path <- file.path(REPORT_DIR, 'fiscal_npv_table.png')
save_as_image(npv_ft, path = npv_tbl_path, res = 200)
cat('Saved fiscal NPV table image to:', npv_tbl_path, '\n')


# ---- Figure: Enrollment by Care Type (2030) ---------------------------------

cat('\n\nGenerating enrollment figure for 2030...\n')

# Load allocation levels for all scenarios (including baseline)
enrollment_data <- list()

for (scenario in ALL_SCENARIOS) {
  alloc_path <- file.path(OUTPUT_ROOT, 'simulation', scenario, 'totals', 'levels', 'allocation.csv')

  if (!file.exists(alloc_path)) {
    cat('  WARNING: Missing allocation.csv for', scenario, '\n')
    next
  }

  alloc <- read_csv(alloc_path, show_col_types = FALSE) %>%
    filter(year == ENROLLMENT_YEAR)

  if (nrow(alloc) == 0) {
    cat('  WARNING: No data for year', ENROLLMENT_YEAR, 'in', scenario, '\n')
    next
  }

  # Aggregate care types and sum across all ages (total column)
  alloc_agg <- alloc %>%
    mutate(care_type = CARE_TYPE_MAP[ecec_choice]) %>%
    group_by(care_type) %>%
    summarise(children = sum(total), .groups = 'drop') %>%
    mutate(scenario = scenario)

  enrollment_data[[scenario]] <- alloc_agg
}

enrollment_df <- bind_rows(enrollment_data) %>%
  group_by(scenario) %>%
  mutate(share_pp = children / sum(children) * 100) %>%
  ungroup()

# Compute pp change from baseline
baseline_shares <- enrollment_df %>%
  filter(scenario == 'baseline') %>%
  select(care_type, baseline_pp = share_pp)

enrollment_df <- enrollment_df %>%
  left_join(baseline_shares, by = 'care_type') %>%
  mutate(
    pp_delta = share_pp - baseline_pp,
    label = if_else(
      scenario == 'baseline',
      sprintf('%.1f', share_pp),
      sprintf('%.1f (%+.1f)', share_pp, pp_delta)
    ),
    scenario = factor(scenario, levels = ALL_SCENARIOS, labels = SCENARIO_LABELS[ALL_SCENARIOS]),
    care_type = factor(care_type, levels = CARE_TYPE_ORDER)
  )

# Stacked bar chart
fig_enrollment <- ggplot(enrollment_df, aes(x = scenario, y = share_pp, fill = care_type)) +
  geom_bar(stat = 'identity', width = 0.7) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 2.8,
    color = 'white',
    fontface = 'bold'
  ) +
  scale_fill_manual(values = CARE_TYPE_COLORS, name = 'Care Type') +
  labs(
    title = paste0('Child Care Enrollment by Type (', ENROLLMENT_YEAR, ')'),
    subtitle = 'Share of children ages 0-4 (pp), with pp change from baseline',
    x = NULL,
    y = 'Share of Children (pp)'
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = 'bottom',
    plot.title = element_text(face = 'bold')
  )

# Save figure
fig_path <- file.path(REPORT_DIR, 'enrollment_by_care_type_2030.png')
ggsave(fig_path, fig_enrollment, width = 10, height = 6, dpi = 300)
cat('Saved enrollment figure to:', fig_path, '\n')

# Grouped bar version: care types on x, scenarios dodged
fig_enrollment_faceted <- ggplot(enrollment_df, aes(x = care_type, y = share_pp, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 2.2
  ) +
  scale_fill_manual(values = SCENARIO_COLORS, name = 'Scenario') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = paste0('Child Care Enrollment by Type (', ENROLLMENT_YEAR, ')'),
    subtitle = 'Share of children ages 0-4 (pp), with pp change from baseline',
    x = NULL,
    y = 'Share of Children (pp)'
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(face = 'bold'),
    axis.text.x = element_text(size = 11)
  ) +
  guides(fill = guide_legend(nrow = 1))

fig_path_faceted <- file.path(REPORT_DIR, 'enrollment_by_care_type_2030_faceted.png')
ggsave(fig_path_faceted, fig_enrollment_faceted, width = 14, height = 6, dpi = 300)
cat('Saved faceted enrollment figure to:', fig_path_faceted, '\n')


# ---- Figure: Employment Rates by Sex (2030) ----------------------------------

cat('\nGenerating employment rate figure for 2030...\n')

# Build FT (full) and PT (lighter) color palette keyed by 'Scenario.emp_type'
emp_fill_colors <- c()
for (s in names(SCENARIO_COLORS)) {
  emp_fill_colors[paste0(s, '.ft')] <- SCENARIO_COLORS[s]
  emp_fill_colors[paste0(s, '.pt')] <- lighten_color(SCENARIO_COLORS[s], 0.45)
}

# Load employment levels for all scenarios
emp_data <- list()

for (scenario in ALL_SCENARIOS) {
  emp_path <- file.path(OUTPUT_ROOT, 'simulation', scenario, 'totals', 'levels', 'parental_employment.csv')

  if (!file.exists(emp_path)) {
    cat('  WARNING: Missing parental_employment.csv for', scenario, '\n')
    next
  }

  emp <- read_csv(emp_path, show_col_types = FALSE) %>%
    filter(year == ENROLLMENT_YEAR, sex == 'f')

  if (nrow(emp) == 0) {
    cat('  WARNING: No data for year', ENROLLMENT_YEAR, 'in', scenario, '\n')
    next
  }

  emp_long <- emp %>%
    select(sex, pt_share, ft_share) %>%
    pivot_longer(cols = c(pt_share, ft_share), names_to = 'emp_type', values_to = 'rate') %>%
    mutate(
      emp_type = sub('_share', '', emp_type),
      scenario = scenario,
      rate_pct = rate * 100
    )

  emp_data[[scenario]] <- emp_long
}

emp_df <- bind_rows(emp_data) %>%
  mutate(
    scenario_label = factor(
      SCENARIO_LABELS[scenario],
      levels = SCENARIO_LABELS[ALL_SCENARIOS]
    ),
    sex_label = factor(
      ifelse(sex == 'f', 'Mothers', 'Fathers'),
      levels = c('Mothers', 'Fathers')
    ),
    emp_type = factor(emp_type, levels = c('pt', 'ft')),
    fill_key = paste0(scenario_label, '.', emp_type)
  )

# Totals for labels above bars
emp_totals <- emp_df %>%
  group_by(sex_label, scenario_label) %>%
  summarise(total_rate = sum(rate_pct), .groups = 'drop')

# Faceted chart: one panel per sex, x = scenario, stacked PT/FT
fig_employment <- ggplot(emp_df, aes(x = scenario_label, y = rate_pct, fill = fill_key)) +
  geom_col(width = 0.7) +
  # PT/FT labels inside stacked segments
  geom_text(
    aes(label = sprintf('%.1f', rate_pct)),
    position = position_stack(vjust = 0.5),
    size = 2.5,
    color = 'white',
    fontface = 'bold'
  ) +
  # Total employment rate above bars
  geom_text(
    data = emp_totals,
    aes(x = scenario_label, y = total_rate, label = sprintf('%.1f%%', total_rate)),
    inherit.aes = FALSE,
    vjust = -0.4,
    size = 3,
    fontface = 'bold'
  ) +
  scale_fill_manual(values = emp_fill_colors, guide = 'none') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = paste0('Maternal Employment Rates (', ENROLLMENT_YEAR, ')'),
    subtitle = 'Part-time (lighter) + Full-time (darker) = Total Employment Rate',
    x = NULL,
    y = 'Employment Rate (%)'
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = 'bold'),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

fig_emp_path <- file.path(REPORT_DIR, 'maternal_employment_rates_2030.png')
ggsave(fig_emp_path, fig_employment, width = 10, height = 6, dpi = 300)
cat('Saved employment rate figure to:', fig_emp_path, '\n')


# ---- Figure: Care Worker Hourly Wages (2030) --------------------------------

cat('\nGenerating care worker wage figure...\n')

# CPI deflator: convert 2030 nominal to 2026 dollars
cpi_2026 <- macro %>% filter(year == 2026) %>% pull(cpiu)
cpi_2030 <- macro %>% filter(year == 2030) %>% pull(cpiu)
deflator_2030_to_2026 <- cpi_2026 / cpi_2030

# Read equilibrium wages from solver_results text files
wage_data <- list()

for (scenario in ALL_SCENARIOS) {
  solver_path <- file.path(OUTPUT_ROOT, 'simulation', scenario,
                           'models', 'equilibrium',
                           sprintf('solver_results_%d.txt', ENROLLMENT_YEAR))
  if (!file.exists(solver_path)) {
    cat('  WARNING: Missing solver results for', scenario, '\n')
    next
  }

  solver_lines <- readLines(solver_path)

  # Find the WAGES section and parse equilibrium wages
  wages_idx <- which(grepl('^WAGES\\s*$', solver_lines))
  no_ba_line <- solver_lines[wages_idx + 4]  # "No BA" line is 4 lines after header
  ba_line    <- solver_lines[wages_idx + 5]   # "BA+" line is 5 lines after header

  # Extract equilibrium wage (last number on each line, trimming trailing spaces)
  no_ba_wage <- as.numeric(trimws(sub('.*\\s+(\\S+)\\s*$', '\\1', no_ba_line)))
  ba_wage    <- as.numeric(trimws(sub('.*\\s+(\\S+)\\s*$', '\\1', ba_line)))

  wage_data[[scenario]] <- tibble(
    scenario = scenario,
    education = c('No Bachelor\'s Degree', 'Bachelor\'s Degree or Higher'),
    wage_nominal = c(no_ba_wage, ba_wage),
    wage_2026 = wage_nominal * deflator_2030_to_2026
  )
}

wage_df <- bind_rows(wage_data) %>%
  mutate(
    scenario_label = factor(SCENARIO_LABELS[scenario],
                            levels = SCENARIO_LABELS[ALL_SCENARIOS]),
    education = factor(education,
                       levels = c('No Bachelor\'s Degree', 'Bachelor\'s Degree or Higher'))
  )

# Compute percent change vs baseline for bar labels
baseline_wages <- wage_df %>%
  filter(scenario == 'baseline') %>%
  select(education, baseline_wage = wage_2026)

wage_df <- wage_df %>%
  left_join(baseline_wages, by = 'education') %>%
  mutate(
    pct_change = (wage_2026 / baseline_wage - 1) * 100,
    bar_label = ifelse(
      scenario == 'baseline',
      sprintf('$%.2f', wage_2026),
      sprintf('$%.2f\n(%+.1f%%)', wage_2026, pct_change)
    )
  )

fig_wages <- ggplot(wage_df, aes(x = education, y = wage_2026, fill = scenario_label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(
    aes(label = bar_label),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 2.8,
    lineheight = 0.85
  ) +
  scale_fill_manual(values = SCENARIO_COLORS, name = 'Scenario') +
  scale_y_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = paste0('Care Worker Hourly Wages (', ENROLLMENT_YEAR, ')'),
    subtitle = 'Equilibrium wages by worker education level, 2026 dollars',
    x = NULL,
    y = 'Hourly Wage (2026$)'
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = 'bold'),
    legend.position = 'bottom'
  ) +
  guides(fill = guide_legend(nrow = 1))

fig_wages_path <- file.path(REPORT_DIR, 'care_worker_wages_2030.png')
ggsave(fig_wages_path, fig_wages, width = 10, height = 6, dpi = 300)
cat('Saved care worker wage figure to:', fig_wages_path, '\n')


# ---- Figure: Care Prices (2030) ---------------------------------------------

cat('\nGenerating care price figure...\n')

HOURS_PER_FT <- 2000

# Parse equilibrium prices from solver_results text files
price_data <- list()
care_type_labels <- c(
  'low_center'  = 'Low-Priced\nCenter-Based Care',
  'high_center' = 'High-Priced\nCenter-Based Care',
  'home_based'  = 'Home-Based Care'
)

for (scenario in ALL_SCENARIOS) {
  solver_path <- file.path(OUTPUT_ROOT, 'simulation', scenario,
                           'models', 'equilibrium',
                           sprintf('solver_results_%d.txt', ENROLLMENT_YEAR))
  if (!file.exists(solver_path)) {
    cat('  WARNING: Missing solver results for', scenario, '\n')
    next
  }

  solver_lines <- readLines(solver_path)

  # Find PRICES section: header line is "PRICES (Market)"
  prices_idx <- which(grepl('^PRICES', solver_lines))
  # Paid care types are lines +5, +6, +7 (skip Unpaid at +4)
  low_center_line  <- solver_lines[prices_idx + 5]
  high_center_line <- solver_lines[prices_idx + 6]
  home_based_line  <- solver_lines[prices_idx + 7]

  parse_final <- function(line) {

    #--------------------------------------------------------------------------
    # Extract the final equilibrium price from a solver results line.
    # Parses decimal numbers and returns the second one (the 'Final' column).
    #
    # Params:
    #   - line (chr): single line from solver_results text file
    #
    # Returns: (dbl) equilibrium price value
    #--------------------------------------------------------------------------

    nums <- as.numeric(regmatches(line, gregexpr('[0-9]+\\.[0-9]+', line))[[1]])
    nums[2]  # Final is the second number
  }

  price_data[[scenario]] <- tibble(
    scenario = scenario,
    care_type = names(care_type_labels),
    price_hourly = c(parse_final(low_center_line),
                     parse_final(high_center_line),
                     parse_final(home_based_line)),
    price_annual = price_hourly * HOURS_PER_FT * deflator_2030_to_2026
  )
}

price_df <- bind_rows(price_data) %>%
  mutate(
    scenario_label = factor(SCENARIO_LABELS[scenario],
                            levels = SCENARIO_LABELS[ALL_SCENARIOS]),
    care_type_label = factor(care_type_labels[care_type],
                             levels = care_type_labels)
  )

# Compute percent change vs baseline
baseline_prices <- price_df %>%
  filter(scenario == 'baseline') %>%
  select(care_type, baseline_price = price_annual)

price_df <- price_df %>%
  left_join(baseline_prices, by = 'care_type') %>%
  mutate(
    pct_change = (price_annual / baseline_price - 1) * 100,
    bar_label = ifelse(
      scenario == 'baseline',
      sprintf('$%s', formatC(round(price_annual), format = 'd', big.mark = ',')),
      sprintf('$%s\n(%+.1f%%)',
              formatC(round(price_annual), format = 'd', big.mark = ','),
              pct_change)
    )
  )

fig_prices <- ggplot(price_df, aes(x = scenario_label, y = price_annual, fill = scenario_label)) +
  geom_col(width = 0.75) +
  geom_text(
    aes(label = bar_label),
    vjust = -0.3,
    size = 2.5,
    lineheight = 0.85
  ) +
  facet_wrap(~ care_type_label, nrow = 1) +
  scale_fill_manual(values = SCENARIO_COLORS, name = 'Scenario') +
  scale_y_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = paste0('Annual Cost of Full-Time Care (', ENROLLMENT_YEAR, ')'),
    subtitle = 'Equilibrium market prices, 2026 dollars (2,000 hours/year)',
    x = NULL,
    y = 'Annual Cost (2026$)'
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = 'bold'),
    legend.position = 'bottom',
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(face = 'bold', size = 11)
  ) +
  guides(fill = guide_legend(nrow = 1))

fig_prices_path <- file.path(REPORT_DIR, 'care_prices_2030.png')
ggsave(fig_prices_path, fig_prices, width = 12, height = 6, dpi = 300)
cat('Saved care price figure to:', fig_prices_path, '\n')


# ---- Figure: Care Worker FTE Employment (2030) -------------------------------

cat('\nGenerating care worker FTE figure...\n')

# Parse equilibrium labor demand (hours) from solver_results text files
labor_data <- list()

for (scenario in ALL_SCENARIOS) {
  solver_path <- file.path(OUTPUT_ROOT, 'simulation', scenario,
                           'models', 'equilibrium',
                           sprintf('solver_results_%d.txt', ENROLLMENT_YEAR))
  if (!file.exists(solver_path)) {
    cat('  WARNING: Missing solver results for', scenario, '\n')
    next
  }

  solver_lines <- readLines(solver_path)

  # Find LABOR DEMAND section
  labor_idx <- which(grepl('^LABOR DEMAND\\s*$', solver_lines))
  no_ba_line <- solver_lines[labor_idx + 4]
  ba_line    <- solver_lines[labor_idx + 5]

  parse_hours <- function(line) {

    #--------------------------------------------------------------------------
    # Extract equilibrium labor hours from a solver results line.
    # Parses comma-separated numbers and returns the last one.
    #
    # Params:
    #   - line (chr): single line from solver_results text file
    #
    # Returns: (dbl) equilibrium labor hours
    #--------------------------------------------------------------------------

    nums <- regmatches(line, gregexpr('[0-9,]+', line))[[1]]
    as.numeric(gsub(',', '', nums[length(nums)]))
  }

  labor_data[[scenario]] <- tibble(
    scenario = scenario,
    education = c('No Bachelor\'s Degree', 'Bachelor\'s Degree or Higher'),
    hours = c(parse_hours(no_ba_line), parse_hours(ba_line)),
    fte = hours / HOURS_PER_FT
  )
}

labor_df <- bind_rows(labor_data) %>%
  mutate(
    scenario_label = factor(SCENARIO_LABELS[scenario],
                            levels = SCENARIO_LABELS[ALL_SCENARIOS]),
    education = factor(education,
                       levels = c('No Bachelor\'s Degree', 'Bachelor\'s Degree or Higher'))
  )

# Compute percent change vs baseline
baseline_labor <- labor_df %>%
  filter(scenario == 'baseline') %>%
  select(education, baseline_fte = fte)

labor_df <- labor_df %>%
  left_join(baseline_labor, by = 'education') %>%
  mutate(
    pct_change = (fte / baseline_fte - 1) * 100,
    fte_thousands = fte / 1000,
    bar_label = ifelse(
      scenario == 'baseline',
      sprintf('%sK', formatC(round(fte_thousands), format = 'd', big.mark = ',')),
      sprintf('%sK\n(%+.1f%%)',
              formatC(round(fte_thousands), format = 'd', big.mark = ','),
              pct_change)
    )
  )

fig_labor <- ggplot(labor_df, aes(x = education, y = fte_thousands, fill = scenario_label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(
    aes(label = bar_label),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 2.8,
    lineheight = 0.85
  ) +
  scale_fill_manual(values = SCENARIO_COLORS, name = 'Scenario') +
  scale_y_continuous(labels = scales::comma_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = paste0('Care Worker Employment (', ENROLLMENT_YEAR, ')'),
    subtitle = 'Full-time equivalent workers by education level (2,000 hours/year)',
    x = NULL,
    y = 'FTE Workers (thousands)'
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = 'bold'),
    legend.position = 'bottom'
  ) +
  guides(fill = guide_legend(nrow = 1))

fig_labor_path <- file.path(REPORT_DIR, 'care_worker_fte_2030.png')
ggsave(fig_labor_path, fig_labor, width = 10, height = 6, dpi = 300)
cat('Saved care worker FTE figure to:', fig_labor_path, '\n')


# ---- Figures: Distributional Impact by Income Quintile -----------------------

cat('\nGenerating distributional impact figures...\n')

# Load distributional impact for all counterfactual scenarios
dist_data <- list()

for (scenario in COUNTERFACTUALS) {
  dist_path <- file.path(OUTPUT_ROOT, 'simulation', scenario, 'totals', 'deltas', 'distributional_impact.csv')

  if (!file.exists(dist_path)) {
    cat('  WARNING: Missing distributional_impact.csv for', scenario, '\n')
    next
  }

  dist <- read_csv(dist_path, show_col_types = FALSE)

  # Quintiles Q1-Q5
  quintiles <- dist %>%
    filter(dimension == 'parent_income_quintile', group %in% c('Q1', 'Q2', 'Q3', 'Q4', 'Q5')) %>%
    mutate(facet = 'Income Quintile')

  # Top breakouts
  top_groups <- dist %>%
    filter(dimension == 'parent_income_quintile', group %in% c('Top 10%', 'Top 5%', 'Top 1%')) %>%
    mutate(facet = 'Top of Distribution')

  combined <- bind_rows(quintiles, top_groups) %>%
    mutate(scenario = scenario)

  dist_data[[scenario]] <- combined
}

dist_df <- bind_rows(dist_data) %>%
  mutate(
    scenario_label = factor(
      SCENARIO_LABELS[scenario],
      levels = SCENARIO_LABELS[COUNTERFACTUALS]
    ),
    group = factor(group, levels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'Top 10%', 'Top 5%', 'Top 1%')),
    facet = factor(facet, levels = c('Income Quintile', 'Top of Distribution'))
  )


# --- Chart 3: Faceted panel grid (5 scenarios x 2 metrics) ---
# Uses ggh4x::facet_grid2 for independent y-scales per column

library(ggh4x)

dist_panel_df <- dist_df %>%
  mutate(
    pct_of_agi = avg_welfare_income_change / avg_baseline_agi * 100,
    pct_of_agi = if_else(group == 'Q1', NA_real_, pct_of_agi)
  ) %>%
  pivot_longer(
    cols = c(avg_welfare_income_change, pct_of_agi),
    names_to = 'metric',
    values_to = 'value'
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c('avg_welfare_income_change', 'pct_of_agi'),
      labels = c('Economic Value of Policy ($)', 'Economic Value of Policy (% of AGI)')
    )
  )

# Force consistent y-limits within each metric column via geom_blank,
# with zero aligned at the same vertical fraction across both columns.
metric_levels <- levels(dist_panel_df$metric)
y_ranges <- dist_panel_df %>%
  group_by(metric) %>%
  summarise(dmin = min(value, na.rm = TRUE), dmax = max(value, na.rm = TRUE), .groups = 'drop')

# Include zero, then add expansion
y_ranges <- y_ranges %>%
  mutate(
    dmin = pmin(dmin, 0),
    dmax = pmax(dmax, 0),
    raw_range = dmax - dmin,
    dmin = dmin - raw_range * 0.02,
    dmax = dmax + raw_range * 0.12
  )

# Compute where zero sits as a fraction from the bottom for each metric
y_ranges <- y_ranges %>%
  mutate(zero_frac = (0 - dmin) / (dmax - dmin))

# Use the max zero fraction so both columns have zero at the same height
target_frac <- max(y_ranges$zero_frac)

# Adjust limits: expand whichever side is too small to match target_frac
y_limits <- y_ranges %>%
  mutate(
    # new_min such that target_frac = (0 - new_min) / (dmax - new_min)
    # => new_min = -target_frac * dmax / (1 - target_frac)
    needed_min = -target_frac * dmax / (1 - target_frac),
    # new_max such that target_frac = (0 - dmin) / (new_max - dmin)
    # => new_max = -dmin / target_frac + dmin = dmin * (1/target_frac - 1)
    needed_max = -dmin * (1 - target_frac) / target_frac,
    ymin = pmin(dmin, needed_min),
    ymax = pmax(dmax, needed_max)
  ) %>%
  select(metric, ymin, ymax)

blank_df <- dist_panel_df %>%
  distinct(scenario_label, metric) %>%
  left_join(y_limits, by = 'metric') %>%
  pivot_longer(cols = c(ymin, ymax), names_to = 'dummy', values_to = 'value') %>%
  mutate(group = factor('Q1', levels = levels(dist_panel_df$group)))

# Section label annotations
section_labels <- expand.grid(
  scenario_label = levels(dist_panel_df$scenario_label),
  metric = levels(dist_panel_df$metric),
  stringsAsFactors = FALSE
) %>%
  mutate(scenario_label = factor(scenario_label, levels = levels(dist_panel_df$scenario_label)),
         metric = factor(metric, levels = levels(dist_panel_df$metric))) %>%
  cross_join(tibble(
    x = c(3, 7),
    label = c('By Quintile', 'Within Top Quintile')
  ))

fig_dist_panel <- ggplot(dist_panel_df, aes(x = group, y = value, fill = scenario_label)) +
  geom_blank(data = blank_df) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 5.5, linetype = 'dashed', color = 'grey50') +
  geom_text(
    aes(label = if_else(
      is.na(value), '',
      if_else(
        metric == 'Economic Value of Policy ($)',
        format_dollar_label(value),
        sprintf('%.1f%%', value)
      )
    )),
    vjust = -0.3, size = 2.2
  ) +
  # Asterisk on Q1 in percent column
  geom_text(
    data = dist_panel_df %>%
      filter(group == 'Q1', metric == 'Economic Value of Policy (% of AGI)') %>%
      mutate(value = 0),
    aes(label = '*'),
    vjust = 1.2, size = 4, color = 'grey40'
  ) +
  geom_text(
    data = section_labels,
    aes(x = x, y = Inf, label = label),
    inherit.aes = FALSE,
    vjust = 1.3, size = 2.8, fontface = 'italic', color = 'grey40'
  ) +
  scale_fill_manual(values = SCENARIO_COLORS_CF, guide = 'none') +
  scale_y_continuous(expand = expansion(mult = 0)) +
  facet_grid2(
    rows = vars(scenario_label),
    cols = vars(metric),
    scales = 'free',
    independent = 'all',
    switch = 'y'
  ) +
  labs(
    title = 'Distributional Impact by Scenario',
    subtitle = 'Quintiles based on parent AGI distribution',
    x = NULL,
    y = NULL,
    caption = '*Q1 values excluded from percent column due to scale issues caused by small denominators.'
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = 'bold'),
    strip.text.y.left = element_text(face = 'bold', size = 10, angle = 0, hjust = 1),
    strip.text.x = element_text(face = 'bold', size = 11),
    strip.placement = 'outside',
    axis.text.x = element_text(size = 9),
    panel.spacing = unit(0.6, 'lines')
  )

fig_dist_panel_path <- file.path(REPORT_DIR, 'distributional_panel.png')
ggsave(fig_dist_panel_path, fig_dist_panel, width = 14, height = 16, dpi = 300)
cat('Saved distributional panel figure to:', fig_dist_panel_path, '\n')


# ---- Figures: Child Earnings by Parent Quintile ------------------------------

cat('\nGenerating child earnings figures...\n')

# Load child earnings by quintile for all counterfactual scenarios
child_earn_data <- list()

for (scenario in COUNTERFACTUALS) {
  ce_path <- file.path(OUTPUT_ROOT, 'simulation', scenario, 'totals', 'deltas', 'child_earnings_by_quintile.csv')

  if (!file.exists(ce_path)) {
    cat('  WARNING: Missing child_earnings_by_quintile.csv for', scenario, '\n')
    next
  }

  ce <- read_csv(ce_path, show_col_types = FALSE) %>%
    mutate(scenario = scenario)

  child_earn_data[[scenario]] <- ce
}

if (length(child_earn_data) == 0) {
  cat('  Skipping child earnings figures (no data available -- run with -f flag to enable)\n')
} else {
  child_earn_df <- bind_rows(child_earn_data) %>%
    mutate(
      scenario_label = factor(
        SCENARIO_LABELS[scenario],
        levels = SCENARIO_LABELS[COUNTERFACTUALS]
      ),
      quintile = factor(paste0('Q', parent_quintile), levels = paste0('Q', 1:5))
    )

  # --- Chart 1: Dollar change in child earnings by quintile ---

  fig_ce_dollars <- ggplot(child_earn_df, aes(x = quintile, y = avg_earnings_delta_all, fill = scenario_label)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    geom_text(
      aes(label = format_dollar_label(avg_earnings_delta_all)),
      position = position_dodge(width = 0.8),
      vjust = -0.3,
      size = 2.5
    ) +
    scale_fill_manual(values = SCENARIO_COLORS_CF, name = 'Scenario') +
    scale_y_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0.02, 0.1))) +
    labs(
      title = 'Change in Child Earnings at Age 27 by Parent Income Quintile',
      subtitle = 'Average per child, real 2025 dollars (unconditional)',
      x = 'Parent Income Quintile',
      y = 'Change in Earnings at Age 27 ($)'
    ) +
    chart_theme +
    guides(fill = guide_legend(nrow = 1))

  fig_ce_dollars_path <- file.path(REPORT_DIR, 'child_earnings_dollars.png')
  ggsave(fig_ce_dollars_path, fig_ce_dollars, width = 12, height = 6, dpi = 300)
  cat('Saved child earnings (dollars) figure to:', fig_ce_dollars_path, '\n')

  # --- Chart 2: Percent change in child earnings by quintile ---

  fig_ce_pct <- ggplot(child_earn_df, aes(x = quintile, y = avg_pct_change_all, fill = scenario_label)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    geom_text(
      aes(label = sprintf('%.2f%%', avg_pct_change_all)),
      position = position_dodge(width = 0.8),
      vjust = -0.3,
      size = 2.5
    ) +
    scale_fill_manual(values = SCENARIO_COLORS_CF, name = 'Scenario') +
    scale_y_continuous(labels = function(x) paste0(x, '%'), expand = expansion(mult = c(0.02, 0.1))) +
    labs(
      title = 'Change in Child Earnings at Age 27 by Parent Income Quintile (%)',
      subtitle = 'Average per child, unconditional on care type switch',
      x = 'Parent Income Quintile',
      y = 'Change in Earnings at Age 27 (%)'
    ) +
    chart_theme +
    guides(fill = guide_legend(nrow = 1))

  fig_ce_pct_path <- file.path(REPORT_DIR, 'child_earnings_percent.png')
  ggsave(fig_ce_pct_path, fig_ce_pct, width = 12, height = 6, dpi = 300)
  cat('Saved child earnings (percent) figure to:', fig_ce_pct_path, '\n')
}


# ---- Figure: Policy Generosity by AGI ---------------------------------------

cat('\nGenerating policy generosity figure...\n')

# Hypothetical: married parents, 1 child, FT care costing $20K/year
CARE_COST <- 20000
NMI_HH3   <- 76152  # National median income for household size 3 (84% of 4-person baseline)

# AGI grid
agi_grid <- seq(0, 250000, by = 1000)

# ---- Policy subsidy calculators (simplified scalar versions) ----

calc_universal <- function(agi) {

  #----------------------------------------------------------------------------
  # Compute universal (Murray-Kaine) subsidy amount by AGI. No income cap;
  # graduated copay based on ratio of AGI to national median income.
  #
  # Params:
  #   - agi (num vec): adjusted gross income values
  #
  # Returns: (num vec) subsidy amounts in dollars
  #----------------------------------------------------------------------------

  ratio <- agi / NMI_HH3
  copay_rate <- ifelse(ratio <= 0.75, 0,
                ifelse(ratio <= 1.00, 0.02,
                ifelse(ratio <= 1.25, 0.04, 0.07)))
  pmax(0, CARE_COST - pmin(CARE_COST, copay_rate * agi))
}

calc_means_tested <- function(agi) {

  #----------------------------------------------------------------------------
  # Compute means-tested (BBBA) subsidy amount by AGI. Same graduated copay
  # as universal but with a 250% NMI eligibility cap.
  #
  # Params:
  #   - agi (num vec): adjusted gross income values
  #
  # Returns: (num vec) subsidy amounts in dollars
  #----------------------------------------------------------------------------

  ratio <- agi / NMI_HH3
  copay_rate <- ifelse(ratio > 2.50, 1.0,
                ifelse(ratio <= 0.75, 0,
                ifelse(ratio <= 1.00, 0.02,
                ifelse(ratio <= 1.25, 0.04, 0.07))))
  eligible <- copay_rate < 1.0
  ifelse(eligible, pmax(0, CARE_COST - pmin(CARE_COST, copay_rate * agi)), 0)
}

calc_arpa_cdctc <- function(agi) {

  #----------------------------------------------------------------------------
  # Compute incremental ARPA CDCTC benefit over 2026 current-law baseline
  # (OBBBA). Calculates both the ARPA expanded credit and the baseline
  # IRC Sec 21 credit, returning the difference. Uses 2026 MFJ brackets
  # from Tax Foundation / IRS Rev. Proc. 2025-32.
  #
  # Params:
  #   - agi (num vec): adjusted gross income values
  #
  # Returns: (num vec) incremental credit amounts in dollars
  #----------------------------------------------------------------------------

  # 2026 MFJ federal income tax liability (needed for baseline non-refundability)
  MFJ_STD_DEDUCTION <- 32200
  CTC_PER_CHILD     <- 2200
  taxable_income    <- pmax(0, agi - MFJ_STD_DEDUCTION)
  # 2026 MFJ brackets: 10% to $24,800; 12% to $100,800; 22% to $211,400; 24% above
  iit <- pmin(taxable_income, 24800) * 0.10 +
         pmax(pmin(taxable_income, 100800) - 24800, 0) * 0.12 +
         pmax(pmin(taxable_income, 211400) - 100800, 0) * 0.22 +
         pmax(taxable_income - 211400, 0) * 0.24
  # CTC reduces liability (non-refundable portion above $1,700 refundable)
  iit <- pmax(0, iit - CTC_PER_CHILD)

  # Earned income limit: single earner (EI limit = full AGI)
  ei_limit <- agi

  # --- Baseline CDCTC (2026 OBBBA current law, IRC Sec 21) ---
  bl_exp_cap  <- 3000   # $3K per child (unchanged)
  bl_qual_exp <- pmin(CARE_COST, bl_exp_cap)
  # Rate: starts at 50%, phases down to 35% at $60K, then to 20% at $210K (MFJ)
  # Tier 1: -1pp per $2K above $30K (MFJ), floor 35%
  bl_tier1_excess <- ceiling(pmax(agi - 30000, 0) / 2000) * 2000
  bl_rate_tier1 <- pmax(0.50 - bl_tier1_excess * 0.000005, 0.35)
  # Tier 2: -1pp per $4K (MFJ) above $150K, floor 20%
  bl_tier2_excess <- ceiling(pmax(agi - 150000, 0) / 4000) * 4000
  bl_rate <- pmax(bl_rate_tier1 - bl_tier2_excess * 0.0000025, 0.20)
  bl_credit <- bl_qual_exp * bl_rate
  # Non-refundable: capped by income tax liability
  bl_credit <- pmin(bl_credit, iit)

  # --- ARPA CDCTC ---
  arpa_exp_cap  <- 8000   # $8K per child
  arpa_qual_exp <- pmin(CARE_COST, arpa_exp_cap, ei_limit)
  # Additional rate: 30%, phases out above $125K at 0.0005% per dollar ($2K steps)
  arpa_excess_add <- pmax(agi - 125000, 0)
  arpa_excess_add_rounded <- ceiling(arpa_excess_add / 2000) * 2000
  arpa_rate_add <- pmax(0.30 - arpa_excess_add_rounded * 0.000005, 0)
  # Base rate: 20%, phases out above $400K
  arpa_excess_base <- pmax(agi - 400000, 0)
  arpa_excess_base_rounded <- ceiling(arpa_excess_base / 2000) * 2000
  arpa_rate_base <- pmax(0.20 - arpa_excess_base_rounded * 0.000005, 0)
  arpa_credit <- arpa_qual_exp * (arpa_rate_base + arpa_rate_add)
  # Fully refundable: no cap

  # Return incremental benefit over baseline
  pmax(0, arpa_credit - bl_credit)
}

calc_child_ubi <- function(agi) {

  #----------------------------------------------------------------------------
  # Compute child allowance subsidy: flat $1,000 per child with no
  # income phase-out.
  #
  # Params:
  #   - agi (num vec): adjusted gross income values (unused, for interface)
  #
  # Returns: (num vec) subsidy amounts (constant $1,000)
  #----------------------------------------------------------------------------

  rep(1000, length(agi))
}

# Build data frame
GENEROSITY_POLICIES <- c('universal', 'means_tested', 'arpa_cdctc', 'child_ubi_1k')
GENEROSITY_LABELS <- SCENARIO_LABELS[GENEROSITY_POLICIES]

generosity_df <- bind_rows(
  tibble(agi = agi_grid, subsidy = calc_universal(agi_grid),    scenario = 'universal'),
  tibble(agi = agi_grid, subsidy = calc_means_tested(agi_grid), scenario = 'means_tested'),
  tibble(agi = agi_grid, subsidy = calc_arpa_cdctc(agi_grid),  scenario = 'arpa_cdctc'),
  tibble(agi = agi_grid, subsidy = calc_child_ubi(agi_grid),   scenario = 'child_ubi_1k')
) %>%
  mutate(scenario_label = factor(SCENARIO_LABELS[scenario], levels = GENEROSITY_LABELS))

# Use scenario colors (subset to these 4 policies)
gen_colors <- SCENARIO_COLORS[GENEROSITY_LABELS]

fig_generosity <- ggplot(generosity_df, aes(x = agi / 1000, y = subsidy, color = scenario_label)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = gen_colors, name = 'Policy') +
  scale_x_continuous(labels = function(x) paste0('$', x, 'K')) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = 'Policy Subsidy by Household AGI',
    subtitle = sprintf('Hypothetical: married parents, 1 child, full-time center-based care ($%sK/year)',
                        format(CARE_COST / 1000, nsmall = 0)),
    x = 'Adjusted Gross Income',
    y = 'Subsidy ($)'
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = 'bold'),
    legend.position = 'bottom',
    panel.spacing = unit(1, 'lines')
  ) +
  guides(color = guide_legend(nrow = 1))

fig_gen_path <- file.path(REPORT_DIR, 'policy_generosity.png')
ggsave(fig_gen_path, fig_generosity, width = 10, height = 6, dpi = 300)
cat('Saved policy generosity figure to:', fig_gen_path, '\n')


# ---- Figure: Subsidy by Cost of Care ----------------------------------------

cat('\nGenerating subsidy by cost of care figure...\n')

# Cost of care grid: $0 to $30K
cost_grid <- seq(0, 30000, by = 500)
INCOME_LEVELS <- c(50000, 100000, 200000)

calc_universal_cc <- function(agi, care_cost) {

  #----------------------------------------------------------------------------
  # Compute universal subsidy parameterized by both AGI and care cost.
  # Same copay logic as calc_universal but with variable care cost.
  #
  # Params:
  #   - agi (dbl): adjusted gross income (scalar)
  #   - care_cost (num vec): annual cost of care values
  #
  # Returns: (num vec) subsidy amounts in dollars
  #----------------------------------------------------------------------------

  ratio <- agi / NMI_HH3
  copay_rate <- ifelse(ratio <= 0.75, 0,
                ifelse(ratio <= 1.00, 0.02,
                ifelse(ratio <= 1.25, 0.04, 0.07)))
  pmax(0, care_cost - pmin(care_cost, copay_rate * agi))
}

calc_means_tested_cc <- function(agi, care_cost) {

  #----------------------------------------------------------------------------
  # Compute means-tested subsidy parameterized by both AGI and care cost.
  # Same copay logic as calc_means_tested but with variable care cost.
  # Returns zero for households above the 250% NMI eligibility cap.
  #
  # Params:
  #   - agi (dbl): adjusted gross income (scalar)
  #   - care_cost (num vec): annual cost of care values
  #
  # Returns: (num vec) subsidy amounts in dollars
  #----------------------------------------------------------------------------

  ratio <- agi / NMI_HH3
  if (ratio > 2.50) return(rep(0, length(care_cost)))
  copay_rate <- if (ratio <= 0.75) 0
                else if (ratio <= 1.00) 0.02
                else if (ratio <= 1.25) 0.04
                else 0.07
  pmax(0, care_cost - pmin(care_cost, copay_rate * agi))
}

calc_arpa_cdctc_cc <- function(agi, care_cost) {

  #----------------------------------------------------------------------------
  # Compute incremental ARPA CDCTC benefit parameterized by both AGI and
  # care cost. Same credit logic as calc_arpa_cdctc but with variable
  # care cost instead of the fixed CARE_COST constant.
  #
  # Params:
  #   - agi (dbl): adjusted gross income (scalar)
  #   - care_cost (num vec): annual cost of care values
  #
  # Returns: (num vec) incremental credit amounts in dollars
  #----------------------------------------------------------------------------

  # Earned income limit: single earner (EI limit = full AGI)
  ei_limit <- agi

  # IIT liability
  MFJ_STD_DEDUCTION <- 32200
  CTC_PER_CHILD     <- 2200
  taxable_income    <- pmax(0, agi - MFJ_STD_DEDUCTION)
  iit <- pmin(taxable_income, 24800) * 0.10 +
         pmax(pmin(taxable_income, 100800) - 24800, 0) * 0.12 +
         pmax(pmin(taxable_income, 211400) - 100800, 0) * 0.22 +
         pmax(taxable_income - 211400, 0) * 0.24
  iit <- pmax(0, iit - CTC_PER_CHILD)

  # Baseline CDCTC (OBBBA current law, MFJ thresholds)
  bl_qual_exp <- pmin(care_cost, 3000)
  bl_tier1_excess <- ceiling(pmax(agi - 30000, 0) / 2000) * 2000
  bl_rate_tier1 <- pmax(0.50 - bl_tier1_excess * 0.000005, 0.35)
  bl_tier2_excess <- ceiling(pmax(agi - 150000, 0) / 4000) * 4000
  bl_rate <- pmax(bl_rate_tier1 - bl_tier2_excess * 0.0000025, 0.20)
  bl_credit <- pmin(bl_qual_exp * bl_rate, iit)

  # ARPA CDCTC
  arpa_qual_exp <- pmin(care_cost, 8000, ei_limit)
  arpa_excess_add <- pmax(agi - 125000, 0)
  arpa_excess_add_rounded <- ceiling(arpa_excess_add / 2000) * 2000
  arpa_rate_add <- pmax(0.30 - arpa_excess_add_rounded * 0.000005, 0)
  arpa_excess_base <- pmax(agi - 400000, 0)
  arpa_excess_base_rounded <- ceiling(arpa_excess_base / 2000) * 2000
  arpa_rate_base <- pmax(0.20 - arpa_excess_base_rounded * 0.000005, 0)
  arpa_credit <- arpa_qual_exp * (arpa_rate_base + arpa_rate_add)

  pmax(0, arpa_credit - bl_credit)
}

calc_child_ubi_cc <- function(agi, care_cost) {

  #----------------------------------------------------------------------------
  # Compute child allowance subsidy parameterized by AGI and care cost.
  # Flat $1,000 per child regardless of income or care cost.
  #
  # Params:
  #   - agi (dbl): adjusted gross income (unused, for interface consistency)
  #   - care_cost (num vec): annual cost of care values (unused, for interface)
  #
  # Returns: (num vec) subsidy amounts (constant $1,000)
  #----------------------------------------------------------------------------

  rep(1000, length(care_cost))
}

# Build data
cost_care_df <- bind_rows(lapply(INCOME_LEVELS, function(inc) {
  bind_rows(
    tibble(care_cost = cost_grid, subsidy = calc_universal_cc(inc, cost_grid),    scenario = 'universal',    agi = inc),
    tibble(care_cost = cost_grid, subsidy = calc_means_tested_cc(inc, cost_grid), scenario = 'means_tested', agi = inc),
    tibble(care_cost = cost_grid, subsidy = calc_arpa_cdctc_cc(inc, cost_grid),   scenario = 'arpa_cdctc',   agi = inc),
    tibble(care_cost = cost_grid, subsidy = calc_child_ubi_cc(inc, cost_grid),    scenario = 'child_ubi_1k', agi = inc)
  )
})) %>%
  mutate(
    scenario_label = factor(SCENARIO_LABELS[scenario], levels = GENEROSITY_LABELS),
    income_label   = factor(
      paste0('AGI = $', formatC(agi / 1000, format = 'f', digits = 0), 'K'),
      levels = paste0('AGI = $', formatC(INCOME_LEVELS / 1000, format = 'f', digits = 0), 'K')
    )
  )

fig_cost_care <- ggplot(cost_care_df, aes(x = care_cost / 1000, y = subsidy, color = scenario_label)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = gen_colors, name = 'Policy') +
  scale_x_continuous(labels = function(x) paste0('$', x, 'K')) +
  scale_y_continuous(labels = scales::dollar_format()) +
  facet_wrap(~income_label, nrow = 1) +
  labs(
    title = 'Policy Subsidy by Cost of Care',
    subtitle = 'Hypothetical: married parents, 1 child, full-time center-based care',
    x = 'Annual Cost of Care',
    y = 'Subsidy ($)'
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = 'bold'),
    strip.text = element_text(face = 'bold', size = 11),
    legend.position = 'bottom',
    panel.spacing = unit(1, 'lines')
  ) +
  guides(color = guide_legend(nrow = 1))

fig_cc_path <- file.path(REPORT_DIR, 'subsidy_by_care_cost.png')
ggsave(fig_cc_path, fig_cost_care, width = 14, height = 5, dpi = 300)
cat('Saved subsidy by care cost figure to:', fig_cc_path, '\n')
