#------------------------------------------------------------------------------
# constants.R
#
# Centralized constants for the ECEC Simulator.
#
# This file contains all global constants used throughout the simulation.
# Constants are organized by domain:
#   - Care hours and schedule
#   - Tax parameters
#   - Optimizer settings
#   - Demand model parameters
#   - OASI tax maximums (known historical values)
#   - Employment targeting groups
#   - Parent unit types
#   - Simulation variable columns
#   - AGI binning and EMTR columns
#
# Called from: all phases (1a, 1b, 2a, 3a, 3b, 3c, 3d, 4a)
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# Care Hours and Schedule
#------------------------------------------------------------------------------

# Weekly care hours by type
FT_CARE_HOURS  <- 40   # Full-time care hours per week
PT_CARE_HOURS  <- 10   # Part-time care hours per week

# Annual schedule
WEEKS_PER_YEAR <- 50   # Weeks of care per year (assumes 2 weeks off)

# Annual hours by care intensity (hours_choice -> annual hours)
HOURS_ANNUAL <- c(
  'none' = 0,
  'pt'   = PT_CARE_HOURS * WEEKS_PER_YEAR,
  'ft'   = FT_CARE_HOURS * WEEKS_PER_YEAR
)

# Sector mapping (ecec_type -> sector_id)
SECTOR_MAPPING <- tibble(
  ecec_type = c('Unpaid Center-Based', 'Low-Priced Center-Based',
                'High-Priced Center-Based', 'Paid Home-Based'),
  sector_id = c(1, 2, 3, 4)
)



#------------------------------------------------------------------------------
# Payroll Tax Parameters (Employee Share)
#------------------------------------------------------------------------------

MEDICARE_TAX_RATE <- 0.0145  # Medicare tax rate (1.45%)
OASI_TAX_RATE     <- 0.062   # Old-Age and Survivors Insurance rate (6.2%)



#------------------------------------------------------------------------------
# OASI Tax Maximum (Social Security Wage Base)
#
# Known actual values for anchoring projections.
# Future years are projected using AWI growth from macro projections.
#------------------------------------------------------------------------------

OASI_TAX_MAX_KNOWN <- list(
  '2019' = 132900,  # Base year for alpha matrix calibration
  '2023' = 160200,
  '2024' = 168600,
  '2025' = 176100
)



#------------------------------------------------------------------------------
# Equilibrium Solver Settings
#------------------------------------------------------------------------------

# Initial prices for optimizer (sector 1 fixed at 0)
INITIAL_PRICES <- c(0, 1, 1, 1)

# Price bounds for L-BFGS-B optimizer ($/hour)
PRICE_LOWER_BOUND <- 0.01   # Minimum price (prevents degenerate solutions)
PRICE_UPPER_BOUND <- 100    # Maximum reasonable childcare price

# Optimizer tolerance (factr parameter for L-BFGS-B)
# factr = 1e10 gives approximately 3 decimal place accuracy
# Lower values (e.g., 1e7) give more precision but slower convergence
OPTIMIZER_TOLERANCE <- 1e10

# Maximum acceptable residual gap for equilibrium convergence
# Solver is only considered converged if both optimizer converges AND
# the sum of squared residual gaps is below this threshold
EQUILIBRIUM_RESIDUAL_THRESHOLD <- 1e6

# Maximum acceptable delta gap (in $/hour) for equilibrium convergence
# Convergence requires max|delta_gap| <= this threshold, where:
#   delta_gap = (P_supply - unit_cost) - target_delta
EQUILIBRIUM_MAX_ABS_DELTA_GAP <- 0.01



#------------------------------------------------------------------------------
# Demand Model Parameters
#------------------------------------------------------------------------------

# Large negative value for infeasible/NA net incomes in logit
# Used to effectively zero out probability of infeasible choices
INFEASIBLE_NET_INCOME <- -1e9

# Minimum probability floor to avoid log(0) in V = log(p) calculations
# Used when computing log-probabilities for discrete simulation with Gumbel shocks
LOG_PROB_FLOOR <- 1e-300

# Minimum share floor to avoid log(0) in elasticity calculations
# Used when computing employment and care shares for percentage changes
SHARE_FLOOR <- 1e-12

# Default care demand elasticity target (Herbst 2023 meta-analysis)
# This is the standard value used in processing.R
DEFAULT_CARE_ELASTICITY_TARGET <- -0.468

# Care cost employment elasticity target for demand calibration
# Measures employment response to childcare cost changes
# Alpha-based CRRA utility model: V = beta * u(NI) where NI = Y - C
DEFAULT_CARE_COST_ELASTICITY_TARGET <- -0.15

# Wage elasticity target for demand calibration
# Measures employment response to wage changes
DEFAULT_WAGE_ELASTICITY_TARGET <- 0.35

# Paid care elasticity target for demand calibration
# Measures paid care hours response to cost changes
DEFAULT_PAID_CARE_ELASTICITY_TARGET <- -0.5



#------------------------------------------------------------------------------
# Employment Targeting Groups
#
# Groups (sex x age bin):
#   - F_u25: Female, age < 25
#   - M_u25: Male, age < 25
#   - F_25_35: Female, 25 <= age < 35
#   - M_25_35: Male, 25 <= age < 35
#   - F_35_45: Female, 35 <= age < 45
#   - M_35_45: Male, 35 <= age < 45
#   - F_45plus: Female, age >= 45
#   - M_45plus: Male, age >= 45
#
# Called from: 3b (run_simulation_year), 3c (run_scenario / equilibrium)
#------------------------------------------------------------------------------

# Group order used throughout employment targeting
EMPLOYMENT_TARGETING_GROUPS <- c('F_u25', 'M_u25', 'F_25_35', 'M_25_35',
                                  'F_35_45', 'M_35_45', 'F_45plus', 'M_45plus')



#------------------------------------------------------------------------------
# Parent Unit Types
#
# Parent units are split by number of children only:
#   - c1: 1 child (45 choices)
#   - c2plus: 2+ children (675 choices, uses 2-child model)
#
# The p1/p2 (single vs married parent) distinction was removed as all
# parameters were identical across n_parents. The n_children distinction
# remains because choice set sizes differ.
#
# Called from: 3b, 3c, 3d
#------------------------------------------------------------------------------

# Names for the 2 parent unit types (used as list names)
PARENT_UNIT_NAMES <- c('c1', 'c2plus')

# Effective n_children for choice set size (1 -> 45 choices, 2 -> 675 choices)
# 2+ children are treated as 2 for the choice set
PARENT_UNIT_N_CHILDREN <- c(1, 2)

# Child category labels (for reporting/display)
PARENT_UNIT_CHILD_CATEGORY <- c('1', '2+')



#------------------------------------------------------------------------------
# Simulation Variable Columns
#
# Column names added by add_simulation_variables().
# These will be removed before writing output to disk.
#------------------------------------------------------------------------------

SIMULATION_VARIABLE_COLS <- c(
  'median_income',
  'cpi_factor_2019',
  'cpi_chain_factor_2019',
  'cpi_chain_factor_2026'
)



#------------------------------------------------------------------------------
# AGI Binning (Percentile-Based)
#
# Tax utility constants for statistical tax matching.
#------------------------------------------------------------------------------

# Percentile breaks for AGI bins
# Using percentile ranks avoids the need for inflation/growth adjustments
# since relative ranks are stable over time
AGI_PERCENTILE_BREAKS <- c(
  seq(0, 90, by = 2),   # Tier 1: n_dep_u13 matching (46 bins)
  seq(92, 98, by = 2),  # Tier 2: has_dep matching (4 bins)
  99, 99.5, 100         # Tier 3: joint-only matching (3 bins)
)

# Tier boundaries (percentiles)
TIER1_MAX_PERCENTILE <- 90   # Below this: match on (pct_bin, joint, n_dep_u13)
TIER2_MAX_PERCENTILE <- 98   # Below this: match on (pct_bin, joint, has_dep)
                              # Above this: match on (pct_bin, joint)

# EMTR column names (marginal tax rates at different wage percentiles)
# NOTE: Keep synchronized with shared_functions/prepare_donor_pool.R and 3b tax matching
MTR_COLS <- c(
  'mtr_wages_p0', 'mtr_wages_p5', 'mtr_wages_p10', 'mtr_wages_p15',
  'mtr_wages_p20', 'mtr_wages_p25', 'mtr_wages_p30', 'mtr_wages_p35',
  'mtr_wages_p40', 'mtr_wages_p45', 'mtr_wages_p50', 'mtr_wages_p55',
  'mtr_wages_p60', 'mtr_wages_p65', 'mtr_wages_p70', 'mtr_wages_p75',
  'mtr_wages_p80', 'mtr_wages_p85', 'mtr_wages_p90', 'mtr_wages_p91',
  'mtr_wages_p92', 'mtr_wages_p93', 'mtr_wages_p94', 'mtr_wages_p95',
  'mtr_wages_p96', 'mtr_wages_p97', 'mtr_wages_p98', 'mtr_wages_p99',
  'mtr_wages_p99.9', 'mtr_wages_p99.99', 'mtr_wages_p100'
)
