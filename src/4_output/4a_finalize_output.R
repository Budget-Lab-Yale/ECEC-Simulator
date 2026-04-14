#------------------------------------------------------------------------------
# 4a_finalize_output.R
#
# Output finalization: write summary CSVs, compute deltas, generate Excel.
# Called from: main.R (Phase 4)
#------------------------------------------------------------------------------


finalize_simulation <- function(sim_ctx) {

  #----------------------------------------------------------------------------
  # Writes all output files, computes deltas, and generates Excel reports.
  #
  # Params:
  #   - sim_ctx (list): simulation context with accumulated summaries
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  convert_to_fiscal_year <- function(fiscal_cost_df) {

    #--------------------------------------------------------------------------
    # Converts calendar-year data to fiscal-year by blending 25% prior year
    # and 75% current year for all numeric columns except counts.
    #
    # Params:
    #   - fiscal_cost_df (df): data frame with a 'year' column and numeric cols
    #
    # Returns: (df) same data frame with 'year' renamed to 'fiscal_year' and
    #   numeric columns blended across adjacent years
    #--------------------------------------------------------------------------

    if (is.null(fiscal_cost_df) || nrow(fiscal_cost_df) == 0) {
      if (!is.null(fiscal_cost_df) && 'year' %in% names(fiscal_cost_df)) {
        return(fiscal_cost_df %>% rename(fiscal_year = year))
      }
      return(fiscal_cost_df)
    }

    df <- fiscal_cost_df %>%
      arrange(year)

    count_cols <- c('n_children_weighted', 'n_parent_units_weighted')
    all_cols <- names(df)
    numeric_cols <- all_cols[sapply(df, is.numeric)]
    blend_cols <- setdiff(numeric_cols, c('year', count_cols))

    result <- df %>%
      mutate(across(all_of(blend_cols), ~ 0.25 * lag(.x, default = 0) + 0.75 * .x)) %>%
      rename(fiscal_year = year)

    return(result)
  }


  write_accumulated_summaries <- function(scenario_id, summaries) {

    #--------------------------------------------------------------------------
    # Writes accumulated summary data frames as CSV files to the scenario
    # output directory (totals/levels/).
    #
    # Params:
    #   - scenario_id (chr): scenario identifier for output path lookup
    #   - summaries (list): named list of summary data frames (allocation,
    #     employment, poverty, median_income_thresholds, fiscal_cost)
    #
    # Returns: nothing (side effects only)
    #--------------------------------------------------------------------------

    scenario_info <- get_scenario_info(scenario_id)
    output_dir <- file.path(scenario_info$paths$output, 'totals', 'levels')
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    csv_map <- list(
      list(data = summaries$allocation,               file = 'allocation.csv'),
      list(data = summaries$employment,               file = 'parental_employment.csv'),
      list(data = summaries$poverty,                   file = 'poverty_impact.csv'),
      list(data = summaries$median_income_thresholds,  file = 'median_income_thresholds.csv')
    )
    for (entry in csv_map) {
      if (!is.null(entry$data) && nrow(entry$data) > 0) {
        fwrite(entry$data, file.path(output_dir, entry$file), na = 'NA')
      }
    }
    if (nrow(summaries$fiscal_cost) > 0) {
      fwrite(convert_to_fiscal_year(summaries$fiscal_cost),
             file.path(output_dir, 'budget_effect.csv'), na = 'NA')
    }
  }


  # --------------------------------------------------------------------------
  # Shared delta computation helpers (used for both Excel and CSV output)
  # --------------------------------------------------------------------------

  compute_allocation_deltas <- function(scenario_alloc, baseline_alloc, for_csv = FALSE) {

    #--------------------------------------------------------------------------
    # Computes allocation deltas between scenario and baseline. When for_csv
    # is TRUE, returns total/total_share columns; when FALSE, returns
    # total_baseline/total_change columns (used for Excel output).
    #
    # Params:
    #   - scenario_alloc (df): scenario allocation summary
    #   - baseline_alloc (df): baseline allocation summary
    #   - for_csv (logical): if TRUE, output format for CSV; if FALSE, for Excel
    #
    # Returns: (df) allocation deltas with year, ecec_choice, ecec_hours_choice,
    #   and computed delta columns, or NULL if inputs are empty
    #--------------------------------------------------------------------------

    if (is.null(scenario_alloc) || is.null(baseline_alloc) ||
        nrow(scenario_alloc) == 0 || nrow(baseline_alloc) == 0) return(NULL)

    d <- scenario_alloc %>%
      left_join(baseline_alloc, by = c('year', 'ecec_choice', 'ecec_hours_choice'),
                suffix = c('_scenario', '_baseline'))

    age_cols <- names(scenario_alloc)[grepl('^age_', names(scenario_alloc))]
    for (col in age_cols) {
      scen_col <- paste0(col, '_scenario')
      base_col <- paste0(col, '_baseline')
      if (scen_col %in% names(d) && base_col %in% names(d)) {
        d[[paste0(col, if (!for_csv) '_change' else '')]] <-
          d[[scen_col]] - d[[base_col]]
      }
    }

    if (for_csv) {
      d %>%
        mutate(total = total_scenario - total_baseline,
               total_share = total_share_scenario - total_share_baseline) %>%
        select(year, ecec_choice, ecec_hours_choice, total, total_share, any_of(age_cols))
    } else {
      change_age_cols <- paste0(age_cols, '_change')
      d %>%
        mutate(total_baseline = total_baseline, total_change = total_scenario - total_baseline) %>%
        select(year, ecec_choice, ecec_hours_choice, total_baseline, total_change, any_of(change_age_cols))
    }
  }


  compute_employment_deltas <- function(scenario_emp, baseline_emp) {

    #--------------------------------------------------------------------------
    # Computes employment deltas between scenario and baseline, including
    # counts and shares for not-working, part-time, and full-time categories.
    #
    # Params:
    #   - scenario_emp (df): scenario employment summary
    #   - baseline_emp (df): baseline employment summary
    #
    # Returns: (df) employment deltas by year and sex, or NULL if inputs
    #   are empty
    #--------------------------------------------------------------------------

    if (is.null(scenario_emp) || is.null(baseline_emp) ||
        nrow(scenario_emp) == 0 || nrow(baseline_emp) == 0) return(NULL)

    r <- scenario_emp %>%
      left_join(baseline_emp, by = c('year', 'sex'), suffix = c('_scenario', '_baseline')) %>%
      mutate(
        nw = nw_scenario - nw_baseline,
        nw_share = nw_share_scenario - nw_share_baseline,
        pt = pt_scenario - pt_baseline,
        pt_share = pt_share_scenario - pt_share_baseline,
        ft = ft_scenario - ft_baseline,
        ft_share = ft_share_scenario - ft_share_baseline,
        total = total_scenario - total_baseline
      )

    output_cols <- c('year', 'sex', 'nw', 'nw_share', 'pt', 'pt_share', 'ft', 'ft_share', 'total')
    if ('baseline_emp_rate_baseline' %in% names(r)) {
      r <- r %>%
        mutate(baseline_emp_rate = baseline_emp_rate_baseline,
               emp_rate = emp_rate_scenario,
               emp_rate_change = emp_rate_scenario - emp_rate_baseline)
      output_cols <- c(output_cols, 'baseline_emp_rate', 'emp_rate', 'emp_rate_change')
    } else if ('emp_rate_scenario' %in% names(r)) {
      r <- r %>%
        mutate(emp_rate = emp_rate_scenario,
               emp_rate_change = emp_rate_scenario - emp_rate_baseline)
      output_cols <- c(output_cols, 'emp_rate', 'emp_rate_change')
    }

    r %>% select(all_of(output_cols))
  }


  compute_budget_deltas <- function(scenario_fiscal, baseline_fiscal, mechanical_fiscal,
                                    scenario_info_for_phase_in, detailed = FALSE) {

    #--------------------------------------------------------------------------
    # Computes budget deltas with mechanical/behavioral decomposition.
    # Decomposes net budget effect into mechanical (no behavioral response)
    # and behavioral (price/behavior changes) components, with phase-in
    # scaling applied to the behavioral portion.
    #
    # Params:
    #   - scenario_fiscal (df): scenario fiscal cost summary
    #   - baseline_fiscal (df): baseline fiscal cost summary
    #   - mechanical_fiscal (df): mechanical (no-response) fiscal summary
    #   - scenario_info_for_phase_in (list): scenario info with phase_in
    #     factors and years for behavioral scaling
    #   - detailed (logical): if TRUE, return per-component mechanical/
    #     behavioral breakdown columns and n_children_weighted (for CSV);
    #     if FALSE, return summary columns only (for Excel)
    #
    # Returns: (df) budget deltas by year with demand/supply/employer subsidy,
    #   CDCTC, tax change, and mechanical/behavioral decomposition columns,
    #   or NULL if inputs are empty
    #--------------------------------------------------------------------------

    if (is.null(scenario_fiscal) || is.null(baseline_fiscal) ||
        nrow(scenario_fiscal) == 0 || nrow(baseline_fiscal) == 0) return(NULL)

    sf <- scenario_fiscal
    bf <- baseline_fiscal
    mf <- mechanical_fiscal

    if (!'total_employer_subsidy' %in% names(sf)) sf$total_employer_subsidy <- 0
    if (!'total_employer_subsidy' %in% names(bf)) bf$total_employer_subsidy <- 0

    d <- sf %>%
      left_join(
        bf %>% select(year,
          total_demand_subsidy_baseline = total_demand_subsidy,
          total_supply_subsidy_baseline = total_supply_subsidy,
          total_employer_subsidy_baseline = total_employer_subsidy,
          total_cdctc_cost_baseline = total_cdctc_cost,
          total_tax_revenue_baseline = total_tax_revenue),
        by = 'year'
      )

    if (!is.null(mf) && nrow(mf) > 0) {
      d <- d %>%
        left_join(
          mf %>% select(year, mechanical_demand_subsidy, mechanical_supply_subsidy,
            any_of('mechanical_employer_subsidy'), mechanical_cdctc_cost, mechanical_tax_change),
          by = 'year'
        )
    } else {
      d <- d %>%
        mutate(mechanical_demand_subsidy = NA_real_, mechanical_supply_subsidy = NA_real_,
               mechanical_employer_subsidy = NA_real_, mechanical_cdctc_cost = NA_real_,
               mechanical_tax_change = NA_real_)
    }
    if (!'mechanical_employer_subsidy' %in% names(d)) d$mechanical_employer_subsidy <- NA_real_

    # Validate required columns
    req_cols <- c('total_demand_subsidy_baseline', 'total_supply_subsidy_baseline',
      'total_employer_subsidy_baseline', 'total_cdctc_cost_baseline', 'total_tax_revenue_baseline')
    cols_missing <- setdiff(req_cols, names(d))
    if (length(cols_missing) > 0) {
      stop(paste0('Budget deltas missing required baseline columns: ', paste(cols_missing, collapse = ', ')))
    }
    cols_with_na <- req_cols[vapply(req_cols, function(col) any(is.na(d[[col]])), logical(1))]
    if (length(cols_with_na) > 0) {
      stop(paste0('Budget deltas encountered NA in required baseline columns: ', paste(cols_with_na, collapse = ', ')))
    }

    # Phase-in lookup
    if (!is.null(scenario_info_for_phase_in$phase_in)) {
      pil <- tibble(year = scenario_info_for_phase_in$years,
                    phase_in_factor = scenario_info_for_phase_in$phase_in)
    } else {
      pil <- tibble(year = unique(d$year), phase_in_factor = 1)
    }

    d %>%
      left_join(pil, by = 'year') %>%
      mutate(
        phase_in_factor = replace_na(phase_in_factor, 1),
        full_demand_subsidy = total_demand_subsidy - total_demand_subsidy_baseline,
        full_supply_subsidy = total_supply_subsidy - total_supply_subsidy_baseline,
        full_employer_subsidy = total_employer_subsidy - total_employer_subsidy_baseline,
        full_cdctc_cost = total_cdctc_cost - total_cdctc_cost_baseline,
        full_tax_change = total_tax_revenue - total_tax_revenue_baseline,
        full_budget_effect = full_tax_change - full_demand_subsidy - full_supply_subsidy
          - full_employer_subsidy - full_cdctc_cost,
        mechanical_cdctc_cost = mechanical_cdctc_cost - total_cdctc_cost_baseline,
        mechanical_budget_effect = if_else(!is.na(mechanical_demand_subsidy),
          mechanical_tax_change - mechanical_demand_subsidy - mechanical_supply_subsidy -
            mechanical_employer_subsidy - mechanical_cdctc_cost, NA_real_),
        full_behavioral_demand_subsidy = full_demand_subsidy - mechanical_demand_subsidy,
        full_behavioral_supply_subsidy = full_supply_subsidy - mechanical_supply_subsidy,
        full_behavioral_employer_subsidy = full_employer_subsidy - mechanical_employer_subsidy,
        full_behavioral_cdctc_cost = full_cdctc_cost - mechanical_cdctc_cost,
        full_behavioral_tax_change = full_tax_change - mechanical_tax_change,
        full_behavioral_budget_effect = if_else(!is.na(mechanical_budget_effect),
          full_budget_effect - mechanical_budget_effect, NA_real_),
        # Phase-in scaled behavioral (per-component and total)
        behavioral_demand_subsidy = if_else(!is.na(full_behavioral_demand_subsidy),
          phase_in_factor * full_behavioral_demand_subsidy, NA_real_),
        behavioral_supply_subsidy = if_else(!is.na(full_behavioral_supply_subsidy),
          phase_in_factor * full_behavioral_supply_subsidy, NA_real_),
        behavioral_employer_subsidy = if_else(!is.na(full_behavioral_employer_subsidy),
          phase_in_factor * full_behavioral_employer_subsidy, NA_real_),
        behavioral_cdctc_cost = if_else(!is.na(full_behavioral_cdctc_cost),
          phase_in_factor * full_behavioral_cdctc_cost, NA_real_),
        behavioral_tax_change = if_else(!is.na(full_behavioral_tax_change),
          phase_in_factor * full_behavioral_tax_change, NA_real_),
        behavioral_budget_effect = if_else(!is.na(full_behavioral_budget_effect),
          phase_in_factor * full_behavioral_budget_effect, NA_real_),
        # Total = mechanical + behavioral (with fallback when decomposition unavailable)
        total_budget_effect = if_else(!is.na(mechanical_budget_effect),
          mechanical_budget_effect + behavioral_budget_effect, full_budget_effect),
        total_demand_subsidy = if_else(!is.na(mechanical_demand_subsidy),
          mechanical_demand_subsidy + behavioral_demand_subsidy, full_demand_subsidy),
        total_supply_subsidy = if_else(!is.na(mechanical_supply_subsidy),
          mechanical_supply_subsidy + behavioral_supply_subsidy, full_supply_subsidy),
        total_employer_subsidy = if_else(!is.na(mechanical_employer_subsidy),
          mechanical_employer_subsidy + behavioral_employer_subsidy, full_employer_subsidy),
        total_cdctc_cost = if_else(!is.na(mechanical_cdctc_cost),
          mechanical_cdctc_cost + behavioral_cdctc_cost, full_cdctc_cost),
        total_tax_change = if_else(!is.na(mechanical_tax_change),
          mechanical_tax_change + behavioral_tax_change, full_tax_change)
      ) %>%
      {
        if (detailed) {
          select(., year, total_demand_subsidy, total_supply_subsidy, total_employer_subsidy,
            total_cdctc_cost, total_tax_change, total_budget_effect,
            mechanical_demand_subsidy, mechanical_supply_subsidy, mechanical_employer_subsidy,
            mechanical_cdctc_cost, mechanical_tax_change, mechanical_budget_effect,
            behavioral_demand_subsidy, behavioral_supply_subsidy, behavioral_employer_subsidy,
            behavioral_cdctc_cost, behavioral_tax_change, behavioral_budget_effect,
            n_children_weighted)
        } else {
          select(., year, total_demand_subsidy, total_supply_subsidy, total_employer_subsidy,
            total_cdctc_cost, total_tax_change, total_budget_effect,
            mechanical_budget_effect, behavioral_budget_effect)
        }
      }
  }


  agg_care_type <- function(alloc) {

    #--------------------------------------------------------------------------
    # Aggregates allocation data by broad care type (Parent-Only,
    # Center-Based, Home-Based) for the console summary display.
    #
    # Params:
    #   - alloc (df): allocation data with ecec_choice and total columns
    #
    # Returns: (df) wide-format data with year and one column per care type
    #--------------------------------------------------------------------------

    alloc %>%
      mutate(
        care_type = case_when(
          ecec_choice == 'Parent Only' ~ 'Parent-Only',
          ecec_choice %in% c('High-Priced Center-Based', 'Low-Priced Center-Based',
                             'Unpaid Center-Based') ~ 'Center-Based',
          ecec_choice %in% c('Paid Home-Based', 'Unpaid Home-Based') ~ 'Home-Based',
          TRUE ~ 'Other'
        )
      ) %>%
      group_by(year, care_type) %>%
      summarise(total = sum(total), .groups = 'drop') %>%
      pivot_wider(names_from = care_type, values_from = total, values_fill = 0)
  }


  write_excel_summary <- function(scenario_id, scenario_summaries, baseline_summaries = NULL) {

    #--------------------------------------------------------------------------
    # Creates a multi-sheet Excel workbook summarizing scenario results.
    # For baseline scenarios, writes levels sheets; for counterfactuals,
    # writes delta sheets plus distributional, poverty, child earnings,
    # and fiscal NPV sheets.
    #
    # Params:
    #   - scenario_id (chr): scenario identifier for output path lookup
    #   - scenario_summaries (list): named list of scenario summary data frames
    #   - baseline_summaries (list): named list of baseline summary data frames,
    #     or NULL if this is the baseline scenario
    #
    # Returns: (chr) file path to the saved Excel workbook
    #--------------------------------------------------------------------------

    scenario_info <- get_scenario_info(scenario_id)
    output_dir <- file.path(scenario_info$paths$output, 'totals')
    is_baseline <- is.null(baseline_summaries)

    wb <- createWorkbook()

    make_style <- function(halign = 'right', fgFill = '#FFFFFF', numFmt = 'GENERAL',
                           fontSize = 11, fontColour = '#000000', textDecoration = NULL,
                           borderColour = '#D9D9D9', wrapText = FALSE) {

      #------------------------------------------------------------------------
      # Style factory: creates an openxlsx cell style with common defaults
      # and caller-specified overrides.
      #
      # Params:
      #   - halign (chr): horizontal alignment
      #   - fgFill (chr): background fill color hex
      #   - numFmt (chr): number format string
      #   - fontSize (int): font size in points
      #   - fontColour (chr): font color hex
      #   - textDecoration (chr): text decoration (e.g. 'bold'), or NULL
      #   - borderColour (chr): border color hex
      #   - wrapText (logical): whether to wrap text in cell
      #
      # Returns: (Style) openxlsx Style object
      #------------------------------------------------------------------------

      createStyle(fontName = 'Calibri', fontSize = fontSize, fontColour = fontColour,
        fgFill = fgFill, halign = halign, valign = 'center',
        textDecoration = textDecoration, border = 'TopBottomLeftRight',
        borderColour = borderColour, borderStyle = 'thin', numFmt = numFmt,
        wrapText = wrapText)
    }

    styles <- list(
      header   = make_style(halign = 'center', fgFill = '#F2F2F2', textDecoration = 'bold',
                             borderColour = '#BFBFBF'),
      data     = make_style(),
      text     = make_style(halign = 'left'),
      number   = make_style(numFmt = '#,##0'),
      currency = make_style(numFmt = '$#,##0'),
      percent  = make_style(numFmt = '0.0%'),
      pct_pp   = make_style(numFmt = '0.0"%"'),
      pct_dec  = make_style(numFmt = '0.00%'),
      title    = createStyle(fontName = 'Calibri', fontSize = 14, fontColour = '#000000',
                   textDecoration = 'bold', halign = 'left'),
      note     = createStyle(fontName = 'Calibri', fontSize = 10, fontColour = '#666666',
                   halign = 'left', wrapText = TRUE)
    )

    year_header_style <- make_style(halign = 'center', fgFill = '#E6E6E6',
                                     textDecoration = 'bold', borderColour = '#BFBFBF')

    write_cell <- function(sheet, value, row, col, style) {

      #------------------------------------------------------------------------
      # Writes a single cell value and applies a style to it.
      #
      # Params:
      #   - sheet (chr): worksheet name
      #   - value: cell value to write
      #   - row (int): row index
      #   - col (int): column index
      #   - style (Style): openxlsx Style object to apply
      #
      # Returns: nothing (side effects only)
      #------------------------------------------------------------------------

      writeData(wb, sheet, value, startRow = row, startCol = col)
      addStyle(wb, sheet, style, rows = row, cols = col)
    }

    write_year_header <- function(sheet, yr, n_cols, current_row, header_row) {

      #------------------------------------------------------------------------
      # Writes a year-header bar (merged row with year label) followed by
      # column headers, and returns the next available row index.
      #
      # Params:
      #   - sheet (chr): worksheet name
      #   - yr (int): year value for the header bar
      #   - n_cols (int): number of columns to span
      #   - current_row (int): row index to start writing at
      #   - header_row (chr vec): column header labels
      #
      # Returns: (int) next available row index after the headers
      #------------------------------------------------------------------------

      writeData(wb, sheet, yr, startRow = current_row, startCol = 1)
      mergeCells(wb, sheet, cols = 1:n_cols, rows = current_row)
      addStyle(wb, sheet, year_header_style, rows = current_row, cols = 1:n_cols, gridExpand = TRUE)
      current_row <- current_row + 1
      writeData(wb, sheet, t(header_row), startRow = current_row, startCol = 1, colNames = FALSE)
      addStyle(wb, sheet, styles$header, rows = current_row, cols = 1:n_cols, gridExpand = TRUE)
      current_row + 1
    }

    .write_allocation_sheet <- function(data, sheet_name) {

      #------------------------------------------------------------------------
      # Writes an allocation worksheet showing care type distribution by
      # year, either as levels or as deltas from baseline.
      #
      # Params:
      #   - data (df): allocation data (levels or deltas format)
      #   - sheet_name (chr): name for the worksheet tab
      #
      # Returns: nothing (side effects only)
      #------------------------------------------------------------------------

      if (is.null(data) || nrow(data) == 0) return()
      addWorksheet(wb, sheet_name)
      is_delta <- 'total_change' %in% names(data)
      write_cell(sheet_name, if (is_delta) 'Child Care Allocation Change' else 'Child Care Allocation',
                 1, 1, styles$title)
      age_cols <- if (is_delta) names(data)[grepl('^age_.*_change$', names(data))]
                  else names(data)[grepl('^age_', names(data))]
      data <- data %>% arrange(year, ecec_choice, ecec_hours_choice)
      if (is_delta) {
        age_headers <- paste0(gsub('age_', 'Age ', gsub('_change$', '', age_cols)), ' (Change)')
        header_row <- c('Care Type', 'Hours', 'Baseline Level', 'Scenario Level', 'Total Change', age_headers)
      } else {
        header_row <- c('Care Type', 'Hours', 'Total', 'Share', gsub('age_', 'Age ', age_cols))
      }
      n_cols <- length(header_row)
      current_row <- 3
      for (yr in unique(data$year)) {
        year_data <- data %>% filter(year == yr)
        current_row <- write_year_header(sheet_name, yr, n_cols, current_row, header_row)
        for (i in 1:nrow(year_data)) {
          rd <- year_data[i, ]
          write_cell(sheet_name, rd$ecec_choice, current_row, 1, styles$text)
          write_cell(sheet_name, rd$ecec_hours_choice, current_row, 2, styles$text)
          if (is_delta) {
            write_cell(sheet_name, rd$total_baseline, current_row, 3, styles$number)
            write_cell(sheet_name, rd$total_baseline + rd$total_change, current_row, 4, styles$number)
            write_cell(sheet_name, rd$total_change, current_row, 5, styles$number)
            col_num <- 6
          } else {
            write_cell(sheet_name, rd$total, current_row, 3, styles$number)
            write_cell(sheet_name, rd$total_share, current_row, 4, styles$percent)
            col_num <- 5
          }
          for (ac in age_cols) {
            write_cell(sheet_name, rd[[ac]], current_row, col_num, styles$number)
            col_num <- col_num + 1
          }
          current_row <- current_row + 1
        }
        current_row <- current_row + 1
      }
      setColWidths(wb, sheet_name, cols = 1, widths = 14)
      setColWidths(wb, sheet_name, cols = 2, widths = 10)
      setColWidths(wb, sheet_name, cols = 3:n_cols, widths = 12)
    }

    .write_employment_sheet <- function(data, sheet_name) {

      #------------------------------------------------------------------------
      # Writes an employment worksheet showing parental employment by sex
      # and year, either as levels or as deltas from baseline.
      #
      # Params:
      #   - data (df): employment data (levels or deltas format)
      #   - sheet_name (chr): name for the worksheet tab
      #
      # Returns: nothing (side effects only)
      #------------------------------------------------------------------------

      if (is.null(data) || nrow(data) == 0) return()
      addWorksheet(wb, sheet_name)
      is_delta <- 'emp_rate_change' %in% names(data)
      has_baseline <- 'baseline_emp_rate' %in% names(data)
      has_emp_rate <- 'emp_rate' %in% names(data)
      data <- data %>%
        mutate(parent_type = case_when(
          sex == 'f' ~ 'Mothers', sex == 'm' ~ 'Fathers', sex == 'all' ~ 'All Parents'
        ))
      write_cell(sheet_name, if (is_delta) 'Parental Employment Change' else 'Parental Employment',
                 1, 1, styles$title)
      if (is_delta) {
        header_row <- c('Parent Type', 'NW Change', 'NW Share Chg', 'PT Change', 'PT Share Chg',
                        'FT Change', 'FT Share Chg', 'Total Chg')
        if (has_baseline) header_row <- c(header_row, 'Baseline Emp Rate', 'Policy Emp Rate', 'Emp Rate Chg')
        else if (has_emp_rate) header_row <- c(header_row, 'Policy Emp Rate', 'Emp Rate Chg')
      } else {
        header_row <- c('Parent Type', 'Not Working', 'NW Share', 'Part-Time', 'PT Share',
                        'Full-Time', 'FT Share', 'Total')
        if (has_baseline) header_row <- c(header_row, 'Baseline Emp Rate', 'Emp Rate')
        else if (has_emp_rate) header_row <- c(header_row, 'Emp Rate')
      }
      n_cols <- length(header_row)
      data <- data %>%
        mutate(parent_type = factor(parent_type, levels = c('Mothers', 'Fathers', 'All Parents'))) %>%
        arrange(year, parent_type)
      current_row <- 3
      for (yr in unique(data$year)) {
        year_data <- data %>% filter(year == yr)
        current_row <- write_year_header(sheet_name, yr, n_cols, current_row, header_row)
        for (i in 1:nrow(year_data)) {
          rd <- year_data[i, ]
          # Column spec: (column_name, col_index, style)
          write_cell(sheet_name, as.character(rd$parent_type), current_row, 1, styles$text)
          col_specs <- list(
            list(rd$nw, 2, styles$number), list(rd$nw_share, 3, styles$percent),
            list(rd$pt, 4, styles$number), list(rd$pt_share, 5, styles$percent),
            list(rd$ft, 6, styles$number), list(rd$ft_share, 7, styles$percent),
            list(rd$total, 8, styles$number)
          )
          for (cs in col_specs) write_cell(sheet_name, cs[[1]], current_row, cs[[2]], cs[[3]])
          col_num <- 9
          if (has_baseline) {
            write_cell(sheet_name, rd$baseline_emp_rate, current_row, col_num, styles$percent)
            col_num <- col_num + 1
          }
          if (has_emp_rate) {
            write_cell(sheet_name, rd$emp_rate, current_row, col_num, styles$percent)
            col_num <- col_num + 1
          }
          if (is_delta && 'emp_rate_change' %in% names(rd)) {
            write_cell(sheet_name, rd$emp_rate_change * 100, current_row, col_num, styles$pct_pp)
          }
          current_row <- current_row + 1
        }
        current_row <- current_row + 1
      }
      setColWidths(wb, sheet_name, cols = 1, widths = 14)
      setColWidths(wb, sheet_name, cols = 2:n_cols, widths = 14)
    }

    .write_budget_sheet_levels <- function(data, sheet_name) {

      #------------------------------------------------------------------------
      # Writes a budget summary worksheet showing fiscal levels (demand
      # subsidy, supply subsidy, employer subsidy, tax revenue) by year.
      #
      # Params:
      #   - data (df): fiscal cost data with per-child metrics
      #   - sheet_name (chr): name for the worksheet tab
      #
      # Returns: nothing (side effects only)
      #------------------------------------------------------------------------

      if (is.null(data) || nrow(data) == 0) return()
      addWorksheet(wb, sheet_name)
      write_cell(sheet_name, 'Budget Summary (Levels)', 1, 1, styles$title)
      data <- convert_to_fiscal_year(data)
      header_row <- c('Fiscal Year', 'Demand Subsidy ($B)', 'Supply Subsidy ($B)', 'Employer Subsidy ($B)',
                      'Tax Revenue ($B)', 'Subsidy/Child', 'Supply Sub/Child', 'Employer Sub/Child', 'Tax/Child')
      writeData(wb, sheet_name, t(header_row), startRow = 3, startCol = 1, colNames = FALSE)
      addStyle(wb, sheet_name, styles$header, rows = 3, cols = 1:length(header_row), gridExpand = TRUE)
      current_row <- 4
      prev_year <- NULL
      for (i in 1:nrow(data)) {
        rd <- data[i, ]
        if (!is.null(prev_year) && rd$fiscal_year != prev_year) current_row <- current_row + 1
        prev_year <- rd$fiscal_year
        emp_sub <- if ('total_employer_subsidy' %in% names(rd)) rd$total_employer_subsidy else 0
        emp_sub_pc <- if ('employer_subsidy_per_child' %in% names(rd)) rd$employer_subsidy_per_child else 0
        col_specs <- list(
          list(rd$fiscal_year, 1, styles$data),
          list(rd$total_demand_subsidy, 2, styles$currency),
          list(rd$total_supply_subsidy, 3, styles$currency),
          list(emp_sub, 4, styles$currency),
          list(rd$total_tax_revenue, 5, styles$currency),
          list(round(rd$demand_subsidy_per_child, -1), 6, styles$currency),
          list(round(rd$supply_subsidy_per_child, -1), 7, styles$currency),
          list(round(emp_sub_pc, -1), 8, styles$currency),
          list(round(rd$tax_revenue_per_child, -1), 9, styles$currency)
        )
        for (cs in col_specs) write_cell(sheet_name, cs[[1]], current_row, cs[[2]], cs[[3]])
        current_row <- current_row + 1
      }
      setColWidths(wb, sheet_name, cols = 1, widths = 12)
      setColWidths(wb, sheet_name, cols = 2:9, widths = 18)
    }

    .write_budget_sheet_deltas <- function(data, sheet_name) {

      #------------------------------------------------------------------------
      # Writes a budget impact worksheet showing changes from baseline,
      # including mechanical vs behavioral decomposition when available.
      #
      # Params:
      #   - data (df): budget delta data with decomposition columns
      #   - sheet_name (chr): name for the worksheet tab
      #
      # Returns: nothing (side effects only)
      #------------------------------------------------------------------------

      if (is.null(data) || nrow(data) == 0) return()
      addWorksheet(wb, sheet_name)
      write_cell(sheet_name, 'Budget Impact (Change from Baseline)', 1, 1, styles$title)
      data <- convert_to_fiscal_year(data)
      header_row <- c('Fiscal Year', 'Demand Subsidy', 'Supply Subsidy', 'Employer Subsidy', 'CDCTC Cost', 'Tax Change', 'Net Budget Effect')
      writeData(wb, sheet_name, t(header_row), startRow = 3, startCol = 1, colNames = FALSE)
      addStyle(wb, sheet_name, styles$header, rows = 3, cols = 1:length(header_row), gridExpand = TRUE)
      current_row <- 4
      for (i in 1:nrow(data)) {
        rd <- data[i, ]
        emp_sub <- if ('total_employer_subsidy' %in% names(rd)) rd$total_employer_subsidy else 0
        cdctc <- if ('total_cdctc_cost' %in% names(rd)) rd$total_cdctc_cost else 0
        col_specs <- list(
          list(rd$fiscal_year, 1, styles$data),
          list(rd$total_demand_subsidy, 2, styles$currency),
          list(rd$total_supply_subsidy, 3, styles$currency),
          list(emp_sub, 4, styles$currency),
          list(cdctc, 5, styles$currency),
          list(rd$total_tax_change, 6, styles$currency),
          list(rd$total_budget_effect, 7, styles$currency)
        )
        for (cs in col_specs) write_cell(sheet_name, cs[[1]], current_row, cs[[2]], cs[[3]])
        current_row <- current_row + 1
      }
      setColWidths(wb, sheet_name, cols = 1, widths = 12)
      setColWidths(wb, sheet_name, cols = 2:7, widths = 16)
      if ('mechanical_budget_effect' %in% names(data)) {
        current_row <- current_row + 2
        write_cell(sheet_name, 'Decomposition: Mechanical vs Behavioral', current_row, 1, styles$title)
        current_row <- current_row + 2
        header_row2 <- c('Fiscal Year', 'Mechanical Effect', 'Behavioral Effect', 'Total Effect')
        writeData(wb, sheet_name, t(header_row2), startRow = current_row, startCol = 1, colNames = FALSE)
        addStyle(wb, sheet_name, styles$header, rows = current_row, cols = 1:4, gridExpand = TRUE)
        current_row <- current_row + 1
        for (i in 1:nrow(data)) {
          rd <- data[i, ]
          col_specs <- list(
            list(rd$fiscal_year, 1, styles$data),
            list(rd$mechanical_budget_effect, 2, styles$currency),
            list(rd$behavioral_budget_effect, 3, styles$currency),
            list(rd$total_budget_effect, 4, styles$currency)
          )
          for (cs in col_specs) write_cell(sheet_name, cs[[1]], current_row, cs[[2]], cs[[3]])
          current_row <- current_row + 1
        }
      }
    }

    # Determine what to show
    if (is_baseline) {
      cat('  Creating Excel summary for baseline (levels)...\n')
      .write_allocation_sheet(scenario_summaries$allocation, 'Allocation')
      .write_employment_sheet(scenario_summaries$employment, 'Employment')
      .write_budget_sheet_levels(scenario_summaries$fiscal_cost, 'Budget')

    } else {
      cat('  Creating Excel summary for', scenario_id, '(deltas)...\n')

      allocation_deltas <- compute_allocation_deltas(
        scenario_summaries$allocation, baseline_summaries$allocation, for_csv = FALSE)
      employment_deltas <- compute_employment_deltas(
        scenario_summaries$employment, baseline_summaries$employment)
      budget_deltas <- compute_budget_deltas(
        scenario_summaries$fiscal_cost, baseline_summaries$fiscal_cost,
        scenario_summaries$mechanical_fiscal, scenario_info)

      .write_allocation_sheet(allocation_deltas, 'Allocation Change')
      .write_employment_sheet(employment_deltas, 'Employment Change')
      .write_budget_sheet_deltas(budget_deltas, 'Budget Impact')

      # Distributional sheet
      {
        dist_data <- scenario_summaries$distributional
        if (!is.null(dist_data) && nrow(dist_data) > 0) {
          dist_sheet <- 'Distributional'
          addWorksheet(wb, dist_sheet)
          dist_current_row <- 1

          write_welfare_decomp_table <- function(section_title, df, group_header,
                                                  group_levels = NULL, group_label_fn = identity) {

            #------------------------------------------------------------------
            # Writes a welfare decomposition table section within the
            # distributional sheet, showing mechanical/welfare/total
            # breakdowns for income changes, gains/losses, ECEC burden,
            # and employment rates by group.
            #
            # Params:
            #   - section_title (chr): title for this table section
            #   - df (df): distributional data filtered to one dimension
            #   - group_header (chr): column header for the group label
            #   - group_levels (chr vec): ordered factor levels, or NULL
            #   - group_label_fn (fn): function to transform group labels
            #
            # Returns: nothing (side effects only, updates dist_current_row)
            #------------------------------------------------------------------

            if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
            header_top <- c(group_header, 'Families', 'Base Income',
              'Avg $ Change', '', '', '% of AGI', '', '', '% Gain', '', '',
              '% Lose', '', '', 'ECEC Burden Level', '', '', '', 'Employment Rate', '', '')
            header_bottom <- c('', '', '',
              'Mechanical', 'Welfare', 'Total', 'Mechanical', 'Welfare', 'Total',
              'Mechanical', 'Welfare', 'Total', 'Mechanical', 'Welfare', 'Total',
              'Baseline', 'Mechanical', 'Welfare', 'Total', 'Baseline', 'Policy', 'pp Change')
            n_cols <- length(header_top)
            write_cell(dist_sheet, section_title, dist_current_row, 1, styles$title)
            dist_current_row <<- dist_current_row + 2
            df <- df %>% mutate(group = group_label_fn(as.character(group)))
            if (!is.null(group_levels)) df <- df %>% mutate(group = factor(group, levels = group_levels))
            df <- df %>% arrange(year, group)
            for (yr in unique(df$year)) {
              year_data <- df %>% filter(year == yr)
              writeData(wb, dist_sheet, yr, startRow = dist_current_row, startCol = 1)
              mergeCells(wb, dist_sheet, cols = 1:n_cols, rows = dist_current_row)
              addStyle(wb, dist_sheet, year_header_style, rows = dist_current_row, cols = 1:n_cols, gridExpand = TRUE)
              dist_current_row <<- dist_current_row + 1
              writeData(wb, dist_sheet, t(header_top), startRow = dist_current_row, startCol = 1, colNames = FALSE)
              writeData(wb, dist_sheet, t(header_bottom), startRow = dist_current_row + 1, startCol = 1, colNames = FALSE)
              addStyle(wb, dist_sheet, styles$header, rows = dist_current_row:(dist_current_row + 1), cols = 1:n_cols, gridExpand = TRUE)
              for (mc in list(1, 2, 3)) mergeCells(wb, dist_sheet, cols = mc, rows = dist_current_row:(dist_current_row + 1))
              for (mc in list(4:6, 7:9, 10:12, 13:15, 16:19, 20:22)) mergeCells(wb, dist_sheet, cols = mc, rows = dist_current_row)
              dist_current_row <<- dist_current_row + 2
              for (i in 1:nrow(year_data)) {
                rd <- year_data[i, ]
                col_specs <- list(
                  list(as.character(rd$group), 1, styles$text),
                  list(rd$n_families, 2, styles$number),
                  list(round(rd$avg_baseline_net_income, -1), 3, styles$currency),
                  list(round(rd$avg_mechanical_income_change, -1), 4, styles$currency),
                  list(round(rd$avg_welfare_income_change, -1), 5, styles$currency),
                  list(round(rd$avg_total_income_change, -1), 6, styles$currency),
                  list(rd$pct_mechanical_income_change, 7, styles$pct_pp),
                  list(rd$pct_welfare_income_change, 8, styles$pct_pp),
                  list(rd$pct_total_income_change, 9, styles$pct_pp),
                  list(rd$frac_mechanical_gain, 10, styles$percent),
                  list(rd$frac_welfare_gain, 11, styles$percent),
                  list(rd$frac_total_gain, 12, styles$percent),
                  list(rd$frac_mechanical_loss, 13, styles$percent),
                  list(rd$frac_welfare_loss, 14, styles$percent),
                  list(rd$frac_total_loss, 15, styles$percent),
                  list(rd$avg_baseline_ecec_burden_level, 16, styles$percent),
                  list(rd$avg_mechanical_ecec_burden_level, 17, styles$percent),
                  list(rd$avg_welfare_ecec_burden_level, 18, styles$percent),
                  list(rd$avg_total_ecec_burden_level, 19, styles$percent)
                )
                for (cs in col_specs) write_cell(dist_sheet, cs[[1]], dist_current_row, cs[[2]], cs[[3]])
                if ('baseline_employment_rate' %in% names(rd)) {
                  write_cell(dist_sheet, rd$baseline_employment_rate, dist_current_row, 20, styles$percent)
                  write_cell(dist_sheet, rd$policy_employment_rate, dist_current_row, 21, styles$percent)
                  write_cell(dist_sheet, rd$pp_employment_change, dist_current_row, 22, styles$pct_pp)
                }
                dist_current_row <<- dist_current_row + 1
              }
              dist_current_row <<- dist_current_row + 1
            }
            dist_current_row <<- dist_current_row + 1
          }

          dist_cols <- c('year', 'group', 'n_families', 'avg_baseline_net_income',
            'avg_mechanical_income_change', 'avg_welfare_income_change', 'avg_total_income_change',
            'pct_mechanical_income_change', 'pct_welfare_income_change', 'pct_total_income_change',
            'frac_mechanical_gain', 'frac_welfare_gain', 'frac_total_gain',
            'frac_mechanical_loss', 'frac_welfare_loss', 'frac_total_loss',
            'avg_baseline_ecec_burden_level', 'avg_mechanical_ecec_burden_level',
            'avg_welfare_ecec_burden_level', 'avg_total_ecec_burden_level',
            'baseline_employment_rate', 'policy_employment_rate', 'pp_employment_change')

          strip_prefix <- function(x) {
            #------------------------------------------------------------------
            # Strips leading numeric prefixes (e.g. '01_') from group labels.
            #
            # Params:
            #   - x (chr vec): character vector of labels
            #
            # Returns: (chr vec) labels with numeric prefixes removed
            #------------------------------------------------------------------
            gsub('^[0-9]+_', '', x)
          }
          income_levels <- c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'Top 10%', 'Top 5%', 'Top 1%')
          agi_levels <- c('$0-10k', '$10-25k', '$25-50k', '$50-75k', '$75-100k',
            '$100-150k', '$150-200k', '$200-250k', '$250-500k', '$500k+')
          parent_age_levels <- c('Under 25', '25-29', '30-34', '35-39', '40+')

          tables <- list(
            list('Overall', 'overall', 'Group'),
            list('Distributional Impact by Income Quintile', 'income_quintile', 'Income Group', income_levels),
            list('Distributional Impact by Parent Income Quintile', 'parent_income_quintile', 'Income Group', income_levels),
            list('Distributional Impact by AGI Group', 'agi_group', 'AGI Group', agi_levels, strip_prefix),
            list('By Marital Status', 'marital', 'Status'),
            list('By Number of Children', 'n_children', 'Children'),
            list('By Parent Age', 'parent_age', 'Parent Age', parent_age_levels, strip_prefix)
          )
          for (tbl in tables) {
            tbl_data <- dist_data %>% filter(dimension == tbl[[2]]) %>% select(any_of(dist_cols))
            write_welfare_decomp_table(tbl[[1]], tbl_data, tbl[[3]],
              group_levels = if (length(tbl) >= 4) tbl[[4]] else NULL,
              group_label_fn = if (length(tbl) >= 5) tbl[[5]] else identity)
          }

          setColWidths(wb, dist_sheet, cols = 1, widths = 18)
          setColWidths(wb, dist_sheet, cols = 2:3, widths = 13)
          setColWidths(wb, dist_sheet, cols = 4:22, widths = 12)
        }
      }

      # Poverty sheet
      {
        pov_data <- scenario_summaries$poverty
        if (!is.null(pov_data) && nrow(pov_data) > 0) {
          pov_sheet <- 'Poverty Impact'
          addWorksheet(wb, pov_sheet)
          pov_current_row <- 1
          write_cell(pov_sheet, 'SPM Poverty Impact', pov_current_row, 1, styles$title)
          pov_current_row <- pov_current_row + 2

          pov_data <- pov_data %>%
            mutate(universe_label = case_when(
              universe == 'total' ~ 'Total Population',
              universe == 'children' ~ 'Children Only',
              TRUE ~ universe
            )) %>%
            arrange(year, desc(universe))

          # Section 1: Poverty Rates
          write_cell(pov_sheet, 'Poverty Rates', pov_current_row, 1, styles$title)
          pov_current_row <- pov_current_row + 1
          pov_header <- c('Universe', 'Population', 'Baseline Rate', 'Policy Rate',
            'Rate Change (Total)', 'Rate Change (Mech)', 'Rate Change (Behav)')
          pov_n_cols <- length(pov_header)
          pov_years <- unique(pov_data$year)

          for (yr in pov_years) {
            year_data <- pov_data %>% filter(year == yr)
            pov_current_row <- write_year_header(pov_sheet, yr, pov_n_cols, pov_current_row, pov_header)
            for (i in 1:nrow(year_data)) {
              rd <- year_data[i, ]
              col_specs <- list(
                list(rd$universe_label, 1, styles$text),
                list(rd$n_people, 2, styles$number),
                list(rd$poverty_rate_baseline, 3, styles$pct_dec),
                list(rd$poverty_rate_policy, 4, styles$pct_dec),
                list(rd$poverty_rate_change_total, 5, styles$pct_dec),
                list(rd$poverty_rate_change_mechanical, 6, styles$pct_dec),
                list(rd$poverty_rate_change_behavioral, 7, styles$pct_dec)
              )
              for (cs in col_specs) write_cell(pov_sheet, cs[[1]], pov_current_row, cs[[2]], cs[[3]])
              pov_current_row <- pov_current_row + 1
            }
            pov_current_row <- pov_current_row + 1
          }

          pov_current_row <- pov_current_row + 1

          # Section 2: Net Change in Poverty
          write_cell(pov_sheet, 'Net Change in Number in Poverty (negative = reduction)',
                     pov_current_row, 1, styles$title)
          pov_current_row <- pov_current_row + 1
          pov_header2 <- c('Universe', 'Baseline Poor', 'Policy Poor', 'Net Change (Total)', 'Net Change (Mech)', 'Net Change (Behav)')
          pov_n_cols2 <- length(pov_header2)

          for (yr in pov_years) {
            year_data <- pov_data %>% filter(year == yr)
            pov_current_row <- write_year_header(pov_sheet, yr, pov_n_cols2, pov_current_row, pov_header2)
            for (i in 1:nrow(year_data)) {
              rd <- year_data[i, ]
              col_specs <- list(
                list(rd$universe_label, 1, styles$text),
                list(rd$n_poor_baseline, 2, styles$number),
                list(rd$n_poor_policy, 3, styles$number),
                list(rd$net_change_total, 4, styles$number),
                list(rd$net_change_mechanical, 5, styles$number),
                list(rd$net_change_behavioral, 6, styles$number)
              )
              for (cs in col_specs) write_cell(pov_sheet, cs[[1]], pov_current_row, cs[[2]], cs[[3]])
              pov_current_row <- pov_current_row + 1
            }
            pov_current_row <- pov_current_row + 1
          }

          setColWidths(wb, pov_sheet, cols = 1, widths = 18)
          setColWidths(wb, pov_sheet, cols = 2:7, widths = 16)
        }
      }

      # Child Earnings sheet
      {
        ce_overall <- scenario_summaries$child_earnings_overall
        ce_by_quintile <- scenario_summaries$child_earnings_by_quintile
        ce_by_transition <- scenario_summaries$child_earnings_by_transition
        ce_by_age <- scenario_summaries$child_earnings_by_age
        ce_sheet <- 'Child Earnings'

        ce_has_data <- (!is.null(ce_overall) && nrow(ce_overall) > 0) ||
                       (!is.null(ce_by_quintile) && nrow(ce_by_quintile) > 0)

        if (ce_has_data) {
          addWorksheet(wb, ce_sheet)
          ce_pct_style <- make_style(numFmt = '0.00"%"')
          ce_billions_style <- make_style(numFmt = '$#,##0.0"B"')
          ce_millions_style <- make_style(numFmt = '$#,##0"M"')
          ce_row <- 1

          # Section 1: Overall Summary
          if (!is.null(ce_overall) && nrow(ce_overall) > 0) {
            write_cell(ce_sheet, 'Child Earnings Impact (Long-Run)', ce_row, 1, styles$title)
            ce_row <- ce_row + 1
            write_cell(ce_sheet, 'Based on Chetty et al. (2011): 1 SD test score = 13.1% higher earnings at age 27',
              ce_row, 1, styles$note)
            ce_row <- ce_row + 2
            ce_header <- c('Year', 'Children', 'Switched', '% Switched', 'Total Impact',
              'Avg (All)', 'Avg (Switchers)', '% Change', 'Baseline Earn')
            ce_n_cols <- length(ce_header)
            writeData(wb, ce_sheet, t(ce_header), startRow = ce_row, startCol = 1, colNames = FALSE)
            addStyle(wb, ce_sheet, styles$header, rows = ce_row, cols = 1:ce_n_cols, gridExpand = TRUE)
            ce_row <- ce_row + 1
            for (i in 1:nrow(ce_overall)) {
              rd <- ce_overall[i, ]
              col_specs <- list(
                list(rd$year, 1, styles$data),
                list(rd$n_children, 2, styles$number),
                list(rd$n_children_switched, 3, styles$number),
                list(rd$pct_switched / 100, 4, styles$percent),
                list(rd$total_earnings_delta / 1e9, 5, ce_billions_style),
                list(round(rd$avg_earnings_delta_all, 0), 6, styles$currency),
                list(round(rd$avg_earnings_delta_switchers, 0), 7, styles$currency),
                list(rd$avg_pct_change_switchers, 8, ce_pct_style),
                list(round(rd$avg_baseline_earnings, 0), 9, styles$currency)
              )
              for (cs in col_specs) write_cell(ce_sheet, cs[[1]], ce_row, cs[[2]], cs[[3]])
              ce_row <- ce_row + 1
            }
            ce_row <- ce_row + 2
          }

          # Section 2: By Parent Income Quintile
          if (!is.null(ce_by_quintile) && nrow(ce_by_quintile) > 0) {
            write_cell(ce_sheet, 'Impact by Parent Income Quintile', ce_row, 1, styles$title)
            ce_row <- ce_row + 2
            ce_q_header <- c('Quintile', 'Children', 'Switched', '% Switched',
              'Total Impact', 'Avg (All)', 'Avg % Change', 'Share of Benefits')
            ce_q_n_cols <- length(ce_q_header)
            for (yr in unique(ce_by_quintile$year)) {
              year_data <- ce_by_quintile %>% filter(year == yr)
              ce_row <- write_year_header(ce_sheet, yr, ce_q_n_cols, ce_row, ce_q_header)
              for (i in 1:nrow(year_data)) {
                rd <- year_data[i, ]
                q_label <- paste0('Q', rd$parent_quintile,
                  if (rd$parent_quintile == 1) ' (Lowest)'
                  else if (rd$parent_quintile == 5) ' (Highest)' else '')
                col_specs <- list(
                  list(q_label, 1, styles$text),
                  list(rd$n_children, 2, styles$number),
                  list(rd$n_children_switched, 3, styles$number),
                  list(rd$pct_switched / 100, 4, styles$percent),
                  list(rd$total_earnings_delta / 1e9, 5, ce_billions_style),
                  list(round(rd$avg_earnings_delta_all, 0), 6, styles$currency),
                  list(rd$avg_pct_change_all, 7, ce_pct_style),
                  list(rd$share_of_total_benefits / 100, 8, styles$percent)
                )
                for (cs in col_specs) write_cell(ce_sheet, cs[[1]], ce_row, cs[[2]], cs[[3]])
                ce_row <- ce_row + 1
              }
              ce_row <- ce_row + 1
            }
            ce_row <- ce_row + 1
          }

          # Section 3: By Age Group
          if (!is.null(ce_by_age) && nrow(ce_by_age) > 0) {
            write_cell(ce_sheet, 'Impact by Child Age Group', ce_row, 1, styles$title)
            ce_row <- ce_row + 2
            ce_a_header <- c('Age Group', 'Children', 'Switched', '% Switched',
              'Total Impact', 'Avg (All)', 'Avg (Switchers)')
            ce_a_n_cols <- length(ce_a_header)
            for (yr in unique(ce_by_age$year)) {
              year_data <- ce_by_age %>% filter(year == yr)
              ce_row <- write_year_header(ce_sheet, yr, ce_a_n_cols, ce_row, ce_a_header)
              for (i in 1:nrow(year_data)) {
                rd <- year_data[i, ]
                age_label <- if (rd$age_group == '<3yrs') 'Under 3 years' else '3-4 years'
                col_specs <- list(
                  list(age_label, 1, styles$text),
                  list(rd$n_children, 2, styles$number),
                  list(rd$n_children_switched, 3, styles$number),
                  list(rd$pct_switched / 100, 4, styles$percent),
                  list(rd$total_earnings_delta / 1e9, 5, ce_billions_style),
                  list(round(rd$avg_earnings_delta_all, 0), 6, styles$currency),
                  list(round(rd$avg_earnings_delta_switchers, 0), 7, styles$currency)
                )
                for (cs in col_specs) write_cell(ce_sheet, cs[[1]], ce_row, cs[[2]], cs[[3]])
                ce_row <- ce_row + 1
              }
              ce_row <- ce_row + 1
            }
            ce_row <- ce_row + 1
          }

          # Section 4: Top Care Transitions
          if (!is.null(ce_by_transition) && nrow(ce_by_transition) > 0) {
            write_cell(ce_sheet, 'Top Care Type Transitions (by total impact)', ce_row, 1, styles$title)
            ce_row <- ce_row + 2
            ce_t_header <- c('From', 'To', 'Children', 'Test Score Delta', 'Avg Earnings Delta', 'Total Impact')
            ce_t_n_cols <- length(ce_t_header)
            for (yr in unique(ce_by_transition$year)) {
              year_data <- ce_by_transition %>%
                filter(year == yr) %>% arrange(desc(abs(total_earnings_delta))) %>% head(10)
              ce_row <- write_year_header(ce_sheet, yr, ce_t_n_cols, ce_row, ce_t_header)
              for (i in 1:nrow(year_data)) {
                rd <- year_data[i, ]
                col_specs <- list(
                  list(rd$from.ecec_type, 1, styles$text),
                  list(rd$to.ecec_type, 2, styles$text),
                  list(rd$n_children, 3, styles$number),
                  list(round(rd$avg_test_score_delta, 2), 4, styles$data),
                  list(round(rd$avg_earnings_delta, 0), 5, styles$currency),
                  list(rd$total_earnings_delta / 1e6, 6, ce_millions_style)
                )
                for (cs in col_specs) write_cell(ce_sheet, cs[[1]], ce_row, cs[[2]], cs[[3]])
                ce_row <- ce_row + 1
              }
              ce_row <- ce_row + 1
            }
          }

          setColWidths(wb, ce_sheet, cols = 1:2, widths = 22)
          setColWidths(wb, ce_sheet, cols = 3:9, widths = 14)
        }
      }

      # Fiscal NPV sheet
      {
        fnpv <- scenario_summaries$fiscal_npv
        fnpv_q <- scenario_summaries$fiscal_npv_by_quintile
        if (!is.null(fnpv) && nrow(fnpv) > 0) {
          fnpv_sheet <- 'Fiscal NPV'
          addWorksheet(wb, fnpv_sheet)
          fnpv_billions_style <- make_style(numFmt = '$#,##0.00"B"')
          fnpv_ratio_style <- make_style(numFmt = '0.00')
          fnpv_pct_style <- make_style(numFmt = '0.0"%"')
          fnpv_row <- 1

          write_kv <- function(label, value, value_style) {

            #------------------------------------------------------------------
            # Writes a label-value row pair on the fiscal NPV sheet and
            # advances the row counter.
            #
            # Params:
            #   - label (chr): row label text
            #   - value: cell value to write
            #   - value_style (Style): openxlsx Style for the value cell
            #
            # Returns: nothing (side effects only, updates fnpv_row)
            #------------------------------------------------------------------

            write_cell(fnpv_sheet, label, fnpv_row, 1, styles$text)
            write_cell(fnpv_sheet, value, fnpv_row, 2, value_style)
            fnpv_row <<- fnpv_row + 1
          }

          # Section 1: Summary NPV
          write_cell(fnpv_sheet, 'Fiscal NPV Analysis', fnpv_row, 1, styles$title)
          fnpv_row <- fnpv_row + 1
          write_cell(fnpv_sheet, 'NPV of child tax revenue from increased earnings minus policy costs',
            fnpv_row, 1, styles$note)
          fnpv_row <- fnpv_row + 2
          writeData(wb, fnpv_sheet, t(c('Metric', 'Value')), startRow = fnpv_row, startCol = 1, colNames = FALSE)
          addStyle(wb, fnpv_sheet, styles$header, rows = fnpv_row, cols = 1:2, gridExpand = TRUE)
          fnpv_row <- fnpv_row + 1

          if (!'discount_rate' %in% names(fnpv)) stop('Fiscal NPV summary requires a discount_rate column with a 3% row.')
          npv_row <- fnpv %>% filter(abs(discount_rate - 0.03) < 1e-9)
          if (nrow(npv_row) == 0) stop('Fiscal NPV summary requires a 3% discount-rate row.')
          npv_row <- npv_row %>% slice(1)

          kv_rows <- list(
            list('Return NPV (child tax revenue)', npv_row$return_npv, fnpv_billions_style),
            list('Cost NPV (policy subsidies)', npv_row$cost_npv, fnpv_billions_style),
            list('Net NPV', npv_row$net_npv, fnpv_billions_style),
            list('Share Recovered (return / cost)', npv_row$share_recovered, fnpv_ratio_style)
          )
          for (kv in kv_rows) write_kv(kv[[1]], kv[[2]], kv[[3]])
          fnpv_row <- fnpv_row + 1

          # Section 2: Assumptions
          write_cell(fnpv_sheet, 'Assumptions', fnpv_row, 1, styles$title)
          fnpv_row <- fnpv_row + 1
          write_cell(fnpv_sheet, 'Based on Hendren & Sprung-Keyser (2020) methodology',
            fnpv_row, 1, styles$note)
          fnpv_row <- fnpv_row + 2
          writeData(wb, fnpv_sheet, t(c('Parameter', 'Value')), startRow = fnpv_row, startCol = 1, colNames = FALSE)
          addStyle(wb, fnpv_sheet, styles$header, rows = fnpv_row, cols = 1:2, gridExpand = TRUE)
          fnpv_row <- fnpv_row + 1

          assumption_rows <- list(
            list('Discount rate', npv_row$discount_rate * 100, fnpv_pct_style),
            list('Marginal tax rate', npv_row$mtr * 100, fnpv_pct_style),
            list('Analysis age', npv_row$analysis_age, styles$data),
            list('Working years (start-end)', paste0(npv_row$start_work_age, '-', npv_row$end_work_age), styles$data),
            list('Earnings years tracked', npv_row$n_earnings_years, styles$data)
          )
          for (kv in assumption_rows) write_kv(kv[[1]], kv[[2]], kv[[3]])
          fnpv_row <- fnpv_row + 1

          # Section 3: Cost Components
          write_cell(fnpv_sheet, 'Annual Cost Components (Deltas from Baseline)', fnpv_row, 1, styles$title)
          fnpv_row <- fnpv_row + 2
          writeData(wb, fnpv_sheet, t(c('Component', 'Amount')), startRow = fnpv_row, startCol = 1, colNames = FALSE)
          addStyle(wb, fnpv_sheet, styles$header, rows = fnpv_row, cols = 1:2, gridExpand = TRUE)
          fnpv_row <- fnpv_row + 1

          cost_rows <- list(
            list('Demand subsidy delta', npv_row$annual_demand_subsidy_delta),
            list('Supply subsidy delta', npv_row$annual_supply_subsidy_delta),
            list('CDCTC delta', npv_row$annual_cdctc_delta),
            list('Parent tax delta (offset)', npv_row$annual_parent_tax_delta),
            list('Annual net cost', npv_row$annual_net_cost)
          )
          for (kv in cost_rows) write_kv(kv[[1]], kv[[2]], fnpv_billions_style)
          fnpv_row <- fnpv_row + 1

          # Section 4: Returns by Quintile
          if (!is.null(fnpv_q) && nrow(fnpv_q) > 0) {
            write_cell(fnpv_sheet, 'Returns by Parent Income Quintile', fnpv_row, 1, styles$title)
            fnpv_row <- fnpv_row + 2
            fnpv_q_header <- c('Quintile', 'Children', 'Switched', '% Switched', 'Return NPV', 'Avg Earnings Delta', 'Share')
            fnpv_q_n_cols <- length(fnpv_q_header)
            writeData(wb, fnpv_sheet, t(fnpv_q_header), startRow = fnpv_row, startCol = 1, colNames = FALSE)
            addStyle(wb, fnpv_sheet, styles$header, rows = fnpv_row, cols = 1:fnpv_q_n_cols, gridExpand = TRUE)
            fnpv_row <- fnpv_row + 1
            for (i in 1:nrow(fnpv_q)) {
              rd <- fnpv_q[i, ]
              col_specs <- list(
                list(paste0('Q', rd$parent_quintile), 1, styles$text),
                list(rd$n_children, 2, styles$number),
                list(rd$n_switchers, 3, styles$number),
                list(rd$pct_switched / 100, 4, styles$percent),
                list(rd$total_return_npv, 5, fnpv_billions_style),
                list(round(rd$avg_earnings_delta, 0), 6, styles$currency),
                list(rd$share_of_returns / 100, 7, styles$percent)
              )
              for (cs in col_specs) write_cell(fnpv_sheet, cs[[1]], fnpv_row, cs[[2]], cs[[3]])
              fnpv_row <- fnpv_row + 1
            }
          }

          setColWidths(wb, fnpv_sheet, cols = 1, widths = 30)
          setColWidths(wb, fnpv_sheet, cols = 2:7, widths = 16)
        }
      }
    }

    excel_path <- file.path(output_dir, paste0('summary_', scenario_id, '.xlsx'))
    saveWorkbook(wb, excel_path, overwrite = TRUE)
    cat('  Excel summary saved:', excel_path, '\n')
    return(excel_path)
  }


  # Main body
  baseline_id <- sim_ctx$baseline_id
  counterfactual_ids <- sim_ctx$counterfactual_ids
  summary_accumulators <- sim_ctx$summary_accumulators

  cat('\n========================================\n')
  cat('Writing summary files...\n')
  cat('========================================\n')

  write_accumulated_summaries(baseline_id, summary_accumulators[[baseline_id]])

  for (scenario_id in counterfactual_ids) {
    write_accumulated_summaries(scenario_id, summary_accumulators[[scenario_id]])

    # Write delta CSVs using shared helpers
    {
      scenario_summaries <- summary_accumulators[[scenario_id]]
      baseline_summaries <- summary_accumulators[[baseline_id]]

      delta_scenario_info <- get_scenario_info(scenario_id)
      delta_output_dir <- file.path(delta_scenario_info$paths$output, 'totals', 'deltas')
      dir.create(delta_output_dir, showWarnings = FALSE, recursive = TRUE)

      # Allocation deltas (CSV format: total/total_share columns)
      alloc_d <- compute_allocation_deltas(
        scenario_summaries$allocation, baseline_summaries$allocation, for_csv = TRUE)
      if (!is.null(alloc_d)) fwrite(alloc_d, file.path(delta_output_dir, 'allocation.csv'), na = 'NA')

      # Employment deltas
      emp_d <- compute_employment_deltas(scenario_summaries$employment, baseline_summaries$employment)
      if (!is.null(emp_d)) fwrite(emp_d, file.path(delta_output_dir, 'parental_employment.csv'), na = 'NA')

      # Budget deltas (CSV with full per-component decomposition)
      if (nrow(scenario_summaries$fiscal_cost) > 0 && nrow(baseline_summaries$fiscal_cost) > 0) {
        if (!'total_employer_subsidy' %in% names(scenario_summaries$fiscal_cost))
          stop('Scenario fiscal_cost is missing total_employer_subsidy (unexpected).')
        if (!'total_employer_subsidy' %in% names(baseline_summaries$fiscal_cost))
          stop('Baseline fiscal_cost is missing total_employer_subsidy (unexpected).')

        bd <- compute_budget_deltas(
          scenario_summaries$fiscal_cost,
          baseline_summaries$fiscal_cost,
          scenario_summaries$mechanical_fiscal,
          delta_scenario_info,
          detailed = TRUE
        )
        if (!is.null(bd)) {
          fwrite(convert_to_fiscal_year(bd), file.path(delta_output_dir, 'budget_effect.csv'), na = 'NA')
        }
      }

      # Write optional summary CSVs
      optional_outputs <- c(
        child_earnings_overall = 'child_earnings_overall.csv', child_earnings_by_quintile = 'child_earnings_by_quintile.csv',
        child_earnings_by_transition = 'child_earnings_by_transition.csv', child_earnings_by_age = 'child_earnings_by_age.csv',
        fiscal_npv = 'fiscal_npv.csv', fiscal_npv_by_quintile = 'fiscal_npv_by_quintile.csv',
        distributional = 'distributional_impact.csv', poverty = 'poverty_impact.csv'
      )
      for (key in names(optional_outputs)) {
        d <- scenario_summaries[[key]]
        if (!is.null(d) && nrow(d) > 0) fwrite(d, file.path(delta_output_dir, optional_outputs[[key]]), na = 'NA')
      }
    }
  }

  cat('\n----------------------------------------\n')
  cat('Generating Excel summaries...\n')
  cat('----------------------------------------\n')

  write_excel_summary(baseline_id, summary_accumulators[[baseline_id]])

  for (scenario_id in counterfactual_ids) {
    write_excel_summary(
      scenario_id,
      summary_accumulators[[scenario_id]],
      summary_accumulators[[baseline_id]]
    )
  }

  # --------------------------------------------------------------------------
  # Regression comparison (if --reference supplied)
  # --------------------------------------------------------------------------

  if (exists('reference_interface') && !is.null(reference_interface)) {

    ref_root <- file.path(default_paths$roots$output, reference_interface, 'simulation')

    if (!dir.exists(ref_root)) {
      cat('Warning: Reference interface directory not found:', ref_root, '\n')
      cat('         Skipping regression comparison.\n')
    } else {
      cat('\n----------------------------------------\n')
      cat('Generating regression comparisons vs', reference_interface, '...\n')
      cat('----------------------------------------\n')

      all_scenario_ids <- c(baseline_id, counterfactual_ids)

      for (scenario_id in all_scenario_ids) {

        ref_levels_dir <- file.path(ref_root, scenario_id, 'totals', 'levels')
        if (!dir.exists(ref_levels_dir)) {
          cat('  Skipping', scenario_id, '- not found in reference run\n')
          next
        }

        cur_info <- get_scenario_info(scenario_id)
        cur_levels_dir <- file.path(cur_info$paths$output, 'totals', 'levels')

        regression_rows <- list()

        # --- Helper: flatten a levels CSV into regression rows ---
        flatten_levels <- function(table_name, ref_file, cur_file, key_cols, metric_cols) {

          #----------------------------------------------------------------------
          # Reads reference and current levels CSVs, joins on key columns, and
          # produces one regression row per key-metric combination with absolute
          # and percentage change plus flag classification.
          #
          # Params:
          #   - table_name (chr): label for the 'table' column (e.g. 'allocation')
          #   - ref_file (chr): path to reference CSV
          #   - cur_file (chr): path to current CSV
          #   - key_cols (chr vec): columns to join on (e.g. year, sex)
          #   - metric_cols (chr vec): numeric columns to compare, or NULL to
          #     auto-detect all numeric columns not in key_cols
          #
          # Returns: (list) list of 1-row data frames, each with columns:
          #   table, year, group, metric, reference, current, change, pct_change, flag
          #----------------------------------------------------------------------

          if (!file.exists(ref_file) || !file.exists(cur_file)) return(list())

          ref_df <- fread(ref_file)
          cur_df <- fread(cur_file)

          # Auto-detect metric columns if not specified
          if (is.null(metric_cols)) {
            numeric_cols <- names(cur_df)[sapply(cur_df, is.numeric)]
            metric_cols <- setdiff(numeric_cols, key_cols)
          }

          # Only compare columns present in both
          metric_cols <- intersect(metric_cols, intersect(names(ref_df), names(cur_df)))
          join_keys <- intersect(key_cols, intersect(names(ref_df), names(cur_df)))

          if (length(metric_cols) == 0 || length(join_keys) == 0) return(list())

          merged <- merge(ref_df, cur_df, by = join_keys, suffixes = c('_ref', '_cur'))

          rows <- list()
          for (i in seq_len(nrow(merged))) {
            row <- merged[i, ]

            # Build group label from non-year key columns
            group_cols <- setdiff(join_keys, c('year', 'fiscal_year'))
            group_label <- if (length(group_cols) > 0) {
              paste(sapply(group_cols, function(gc) as.character(row[[gc]])), collapse = '/')
            } else {
              ''
            }

            yr <- if ('year' %in% join_keys) {
              row[['year']]
            } else if ('fiscal_year' %in% join_keys) {
              row[['fiscal_year']]
            } else {
              NA_integer_
            }

            for (metric in metric_cols) {
              ref_val <- as.numeric(row[[paste0(metric, '_ref')]])
              cur_val <- as.numeric(row[[paste0(metric, '_cur')]])

              if (is.na(ref_val) && is.na(cur_val)) next

              abs_change <- cur_val - ref_val
              pct_change <- if (!is.na(ref_val) && ref_val != 0) {
                abs_change / abs(ref_val)
              } else {
                NA_real_
              }

              flag <- ''
              if (!is.na(pct_change)) {
                if (abs(pct_change) >= 0.20) {
                  flag <- 'FLAG'
                } else if (abs(pct_change) >= 0.05) {
                  flag <- 'NOTABLE'
                }
              }

              rows[[length(rows) + 1]] <- data.frame(
                table      = table_name,
                year       = yr,
                group      = group_label,
                metric     = metric,
                reference  = ref_val,
                current    = cur_val,
                change     = abs_change,
                pct_change = pct_change,
                flag       = flag,
                stringsAsFactors = FALSE
              )
            }
          }
          rows
        }

        # --- Allocation ---
        regression_rows <- c(regression_rows, flatten_levels(
          'allocation',
          file.path(ref_levels_dir, 'allocation.csv'),
          file.path(cur_levels_dir, 'allocation.csv'),
          key_cols = c('year', 'ecec_choice', 'ecec_hours_choice'),
          metric_cols = NULL
        ))

        # --- Employment ---
        regression_rows <- c(regression_rows, flatten_levels(
          'employment',
          file.path(ref_levels_dir, 'parental_employment.csv'),
          file.path(cur_levels_dir, 'parental_employment.csv'),
          key_cols = c('year', 'sex'),
          metric_cols = NULL
        ))

        # --- Budget (uses fiscal_year after calendar-to-fiscal conversion) ---
        regression_rows <- c(regression_rows, flatten_levels(
          'budget',
          file.path(ref_levels_dir, 'budget_effect.csv'),
          file.path(cur_levels_dir, 'budget_effect.csv'),
          key_cols = c('fiscal_year'),
          metric_cols = NULL
        ))

        # --- Poverty ---
        regression_rows <- c(regression_rows, flatten_levels(
          'poverty',
          file.path(ref_levels_dir, 'poverty_impact.csv'),
          file.path(cur_levels_dir, 'poverty_impact.csv'),
          key_cols = c('year', 'universe'),
          metric_cols = NULL
        ))

        # --- Median income thresholds ---
        regression_rows <- c(regression_rows, flatten_levels(
          'median_income',
          file.path(ref_levels_dir, 'median_income_thresholds.csv'),
          file.path(cur_levels_dir, 'median_income_thresholds.csv'),
          key_cols = c('year'),
          metric_cols = NULL
        ))

        # --- Delta CSVs (counterfactuals only) ---
        ref_deltas_dir <- file.path(ref_root, scenario_id, 'totals', 'deltas')
        cur_deltas_dir <- file.path(cur_info$paths$output, 'totals', 'deltas')

        if (dir.exists(ref_deltas_dir) && dir.exists(cur_deltas_dir)) {

          regression_rows <- c(regression_rows, flatten_levels(
            'delta_allocation',
            file.path(ref_deltas_dir, 'allocation.csv'),
            file.path(cur_deltas_dir, 'allocation.csv'),
            key_cols = c('year', 'ecec_choice', 'ecec_hours_choice'),
            metric_cols = NULL
          ))

          regression_rows <- c(regression_rows, flatten_levels(
            'delta_employment',
            file.path(ref_deltas_dir, 'parental_employment.csv'),
            file.path(cur_deltas_dir, 'parental_employment.csv'),
            key_cols = c('year', 'sex'),
            metric_cols = NULL
          ))

          regression_rows <- c(regression_rows, flatten_levels(
            'delta_budget',
            file.path(ref_deltas_dir, 'budget_effect.csv'),
            file.path(cur_deltas_dir, 'budget_effect.csv'),
            key_cols = c('fiscal_year'),
            metric_cols = NULL
          ))

          regression_rows <- c(regression_rows, flatten_levels(
            'delta_poverty',
            file.path(ref_deltas_dir, 'poverty_impact.csv'),
            file.path(cur_deltas_dir, 'poverty_impact.csv'),
            key_cols = c('year', 'universe'),
            metric_cols = NULL
          ))

          regression_rows <- c(regression_rows, flatten_levels(
            'delta_distributional',
            file.path(ref_deltas_dir, 'distributional_impact.csv'),
            file.path(cur_deltas_dir, 'distributional_impact.csv'),
            key_cols = c('year', 'dimension', 'group'),
            metric_cols = NULL
          ))

          regression_rows <- c(regression_rows, flatten_levels(
            'delta_child_earnings',
            file.path(ref_deltas_dir, 'child_earnings_overall.csv'),
            file.path(cur_deltas_dir, 'child_earnings_overall.csv'),
            key_cols = c('year'),
            metric_cols = NULL
          ))

          regression_rows <- c(regression_rows, flatten_levels(
            'delta_fiscal_npv',
            file.path(ref_deltas_dir, 'fiscal_npv.csv'),
            file.path(cur_deltas_dir, 'fiscal_npv.csv'),
            key_cols = c('year'),
            metric_cols = NULL
          ))
        }

        # Write regression CSV
        if (length(regression_rows) > 0) {
          regression_df <- rbindlist(regression_rows)
          out_path <- file.path(cur_info$paths$output, 'totals', 'regression.csv')
          fwrite(regression_df, out_path, na = 'NA')

          n_flags <- sum(regression_df$flag == 'FLAG', na.rm = TRUE)
          n_notable <- sum(regression_df$flag == 'NOTABLE', na.rm = TRUE)
          cat(sprintf('  %s: %d metrics, %d FLAG, %d NOTABLE -> %s\n',
                      scenario_id, nrow(regression_df), n_flags, n_notable, out_path))
        } else {
          cat('  ', scenario_id, ': no comparable metrics found\n')
        }
      }
    }
  }

  # Console summary
  {
    years <- sim_ctx$years_to_run

    fmt_with_pct <- function(val, baseline_val, divisor = 1000) {

      #------------------------------------------------------------------------
      # Formats a numeric value divided by a divisor, appending the percent
      # change from a baseline value when available.
      #
      # Params:
      #   - val (dbl): value to format
      #   - baseline_val (dbl): baseline value for percent change computation
      #   - divisor (dbl): divisor to apply before formatting (default 1000)
      #
      # Returns: (chr) formatted string like '1,234 (+5.2%)', or 'N/A'
      #------------------------------------------------------------------------

      if (is.null(val) || length(val) == 0 || is.na(val)) {
        return('N/A')
      }
      val_k <- round(val / divisor, 0)
      if (!is.null(baseline_val) && length(baseline_val) > 0 && !is.na(baseline_val) && baseline_val != 0) {
        pct <- (val - baseline_val) / baseline_val * 100
        sprintf('%s (%+.1f%%)', format(val_k, big.mark = ','), pct)
      } else {
        format(val_k, big.mark = ',')
      }
    }

    safe_col <- function(df, col_name) {

      #------------------------------------------------------------------------
      # Safely extracts a column value from a data frame, returning NA_real_
      # if the column does not exist.
      #
      # Params:
      #   - df (df): data frame to extract from
      #   - col_name (chr): column name to look up
      #
      # Returns: (dbl) column value, or NA_real_ if column is missing
      #------------------------------------------------------------------------

      if (col_name %in% names(df)) {
        df[[col_name]]
      } else {
        NA_real_
      }
    }

    cat('\n')
    cat('================================================================================\n')
    cat('                              RUN SUMMARY                                       \n')
    cat('================================================================================\n')

    all_scenarios <- c(baseline_id, counterfactual_ids)
    baseline_info <- get_scenario_info(baseline_id)

    cat('\n--- EQUILIBRIUM PRICES BY SCENARIO ---\n')
    cat('Sectors: [Unpaid CB, Low-Priced CB, High-Priced CB, Paid HB]\n\n')

    baseline_prices <- list()
    if (length(counterfactual_ids) > 0) {
      cf_info <- get_scenario_info(counterfactual_ids[1])
      for (year in years) {
        price_file <- file.path(cf_info$paths$output, 'models', 'equilibrium',
                                paste0('price_deltas_', year, '.csv'))
        if (file.exists(price_file)) {
          price_df <- read_csv(price_file, show_col_types = FALSE)
          baseline_prices[[as.character(year)]] <- price_df$baseline_price
        }
      }
    }

    for (sid in all_scenarios) {
      scenario_info <- get_scenario_info(sid)
      cat(sprintf('  %s:\n', sid))

      for (year in years) {
        prices <- NULL

        if (sid == baseline_id) {
          prices <- baseline_prices[[as.character(year)]]
          if (is.null(prices)) {
            supply_file <- file.path(scenario_info$paths$output, 'supply',
                                     paste0('equilibrium_prices_', year, '.csv'))
            if (file.exists(supply_file)) {
              prices <- read_csv(supply_file, show_col_types = FALSE)$price
            }
          }
        } else {
          price_file <- file.path(scenario_info$paths$output, 'models', 'equilibrium',
                                  paste0('price_deltas_', year, '.csv'))
          if (file.exists(price_file)) {
            price_df <- read_csv(price_file, show_col_types = FALSE)
            prices <- price_df$counterfactual_price
          }
        }

        if (!is.null(prices)) {
          cat(sprintf('    %d: [%s]\n', year, paste(round(prices, 2), collapse = ', ')))
        }
      }
    }

    cat('\n--- ALLOCATION BY CARE TYPE (thousands, % chg vs baseline) ---\n\n')

    baseline_allocation_file <- file.path(baseline_info$paths$output, 'totals', 'levels', 'allocation.csv')
    baseline_allocation_agg <- NULL

    if (file.exists(baseline_allocation_file)) {
      baseline_allocation_agg <- agg_care_type(read_csv(baseline_allocation_file, show_col_types = FALSE))
    }

    for (sid in all_scenarios) {
      scenario_info <- get_scenario_info(sid)
      allocation_file <- file.path(scenario_info$paths$output, 'totals', 'levels', 'allocation.csv')

      if (!file.exists(allocation_file)) next

      allocation_agg <- agg_care_type(read_csv(allocation_file, show_col_types = FALSE))

      cat(sprintf('  %s:\n', sid))
      cat(sprintf('    %-6s %18s %18s %18s\n', 'Year', 'Parent-Only', 'Center-Based', 'Home-Based'))

      if (nrow(allocation_agg) == 0) {
        cat('    (no data)\n')
      } else {
        for (i in seq_len(nrow(allocation_agg))) {
          row <- allocation_agg[i, ]
          yr <- row$year

          bl_row <- if (!is.null(baseline_allocation_agg) && nrow(baseline_allocation_agg) > 0 && sid != baseline_id) {
            baseline_allocation_agg %>% filter(year == yr)
          } else {
            NULL
          }

          val_parent <- safe_col(row, 'Parent-Only')
          val_center <- safe_col(row, 'Center-Based')
          val_home   <- safe_col(row, 'Home-Based')

          if (sid == baseline_id || is.null(bl_row) || nrow(bl_row) == 0) {
            fmt_val <- function(v) {
              #------------------------------------------------------------------
              # Formats a value in thousands with comma separators.
              #
              # Params:
              #   - v (dbl): Value to format
              #
              # Returns: (chr) Formatted string
              #------------------------------------------------------------------
              if (is.na(v)) 'N/A' else format(round(v / 1000, 0), big.mark = ',')
            }
            cat(sprintf('    %-6d %18s %18s %18s\n',
                        yr, fmt_val(val_parent), fmt_val(val_center), fmt_val(val_home)
            ))
          } else {
            cat(sprintf('    %-6d %18s %18s %18s\n',
                        yr,
                        fmt_with_pct(val_parent, safe_col(bl_row, 'Parent-Only')),
                        fmt_with_pct(val_center, safe_col(bl_row, 'Center-Based')),
                        fmt_with_pct(val_home, safe_col(bl_row, 'Home-Based'))
            ))
          }
        }
      }
      cat('\n')
    }

    cat('--- LABOR SUPPLY BY SEX (million hours, % chg vs baseline) ---\n\n')

    baseline_emp_file <- file.path(baseline_info$paths$output, 'totals', 'levels', 'parental_employment.csv')
    baseline_emp <- NULL

    if (file.exists(baseline_emp_file)) {
      baseline_emp <- read_csv(baseline_emp_file, show_col_types = FALSE)
    }

    for (sid in all_scenarios) {
      scenario_info <- get_scenario_info(sid)
      scenario_emp_file <- file.path(scenario_info$paths$output, 'totals', 'levels', 'parental_employment.csv')

      if (!file.exists(scenario_emp_file)) next

      scenario_emp <- read_csv(scenario_emp_file, show_col_types = FALSE)

      labor_by_sex <- scenario_emp %>%
        mutate(hours = (pt * 20 + ft * 40) * 52) %>%
        select(year, sex, hours) %>%
        pivot_wider(names_from = sex, values_from = hours)

      cat(sprintf('  %s:\n', sid))
      cat(sprintf('    %-6s %18s %18s %18s\n', 'Year', 'Female', 'Male', 'Total'))

      if (nrow(labor_by_sex) == 0) {
        cat('    (no data)\n')
      } else {
        for (i in seq_len(nrow(labor_by_sex))) {
          row <- labor_by_sex[i, ]
          yr <- row$year

          bl_labor <- if (!is.null(baseline_emp) && nrow(baseline_emp) > 0 && sid != baseline_id) {
            baseline_emp %>%
              filter(year == yr) %>%
              mutate(hours = (pt * 20 + ft * 40) * 52) %>%
              select(sex, hours) %>%
              pivot_wider(names_from = sex, values_from = hours)
          } else {
            NULL
          }

          divisor <- 1e6

          val_f   <- safe_col(row, 'f')
          val_m   <- safe_col(row, 'm')
          val_all <- safe_col(row, 'all')

          if (sid == baseline_id || is.null(bl_labor) || nrow(bl_labor) == 0) {
            fmt_val <- function(v) {
              #------------------------------------------------------------------
              # Formats a value divided by divisor with comma separators.
              #
              # Params:
              #   - v (dbl): Value to format
              #
              # Returns: (chr) Formatted string
              #------------------------------------------------------------------
              if (is.na(v)) 'N/A' else format(round(v / divisor, 0), big.mark = ',')
            }
            cat(sprintf('    %-6d %18s %18s %18s\n',
                        yr, fmt_val(val_f), fmt_val(val_m), fmt_val(val_all)
            ))
          } else {
            cat(sprintf('    %-6d %18s %18s %18s\n',
                        yr,
                        fmt_with_pct(val_f, safe_col(bl_labor, 'f'), divisor),
                        fmt_with_pct(val_m, safe_col(bl_labor, 'm'), divisor),
                        fmt_with_pct(val_all, safe_col(bl_labor, 'all'), divisor)
            ))
          }
        }
      }
      cat('\n')
    }

    if (length(counterfactual_ids) > 0) {

      cat('--- BUDGET EFFECT BY YEAR (counterfactuals only) ---\n\n')
      cat('    (positive = lower deficit, negative = higher deficit)\n')
      cat('    Decomposition: Total = Mechanical + Behavioral\n')
      cat('    Mechanical = policy cost with no behavioral response\n')
      cat('    Behavioral = additional from behavior/price changes\n\n')

      for (sid in counterfactual_ids) {
        scenario_info <- get_scenario_info(sid)
        budget_file <- file.path(scenario_info$paths$output, 'totals', 'deltas', 'budget_effect.csv')

        if (!file.exists(budget_file)) next

        budget_data <- read_csv(budget_file, show_col_types = FALSE)

        cat(sprintf('  %s:\n', sid))

        for (i in seq_len(nrow(budget_data))) {
          row <- budget_data[i, ]

          mech_cdctc <- if ('mechanical_cdctc_cost' %in% names(row)) row$mechanical_cdctc_cost else 0
          behav_cdctc <- if ('behavioral_cdctc_cost' %in% names(row)) row$behavioral_cdctc_cost else 0
          total_cdctc <- if ('total_cdctc_cost' %in% names(row)) row$total_cdctc_cost else 0
          mech_employer <- if ('mechanical_employer_subsidy' %in% names(row)) row$mechanical_employer_subsidy else 0
          behav_employer <- if ('behavioral_employer_subsidy' %in% names(row)) row$behavioral_employer_subsidy else 0
          total_employer <- if ('total_employer_subsidy' %in% names(row)) row$total_employer_subsidy else 0

          cat(sprintf('\n                         Demand Sub     Supply Sub   Employer Sub      CDCTC Cost     Tax Change  Net Budget Effect\n'))
          cat(sprintf('      %-14s %14s %14s %14s %14s %14s %18s\n',
                      'Mechanical:',
                      sprintf('$%.2fB', row$mechanical_demand_subsidy),
                      sprintf('$%.2fB', row$mechanical_supply_subsidy),
                      sprintf('$%.2fB', mech_employer),
                      sprintf('$%.2fB', mech_cdctc),
                      '$0.00B',
                      sprintf('$%.2fB', row$mechanical_budget_effect)))
          cat(sprintf('      %-14s %14s %14s %14s %14s %14s %18s\n',
                      'Behavioral:',
                      sprintf('$%.2fB', row$behavioral_demand_subsidy),
                      sprintf('$%.2fB', row$behavioral_supply_subsidy),
                      sprintf('$%.2fB', behav_employer),
                      sprintf('$%.2fB', behav_cdctc),
                      sprintf('$%.2fB', row$behavioral_tax_change),
                      sprintf('$%.2fB', row$behavioral_budget_effect)))
          cat(sprintf('      %-14s %14s %14s %14s %14s %14s %18s\n',
                      'TOTAL:',
                      sprintf('$%.2fB', row$total_demand_subsidy),
                      sprintf('$%.2fB', row$total_supply_subsidy),
                      sprintf('$%.2fB', total_employer),
                      sprintf('$%.2fB', total_cdctc),
                      sprintf('$%.2fB', row$total_tax_change),
                      sprintf('$%.2fB', row$total_budget_effect)))
        }
        cat('\n')
      }
    }

    cat('================================================================================\n')
  }

}
