#------------------------------------------------------------------------------
# get_choice_catalog.R
#
# Defines the complete choice space for the ECEC simulator. This catalog maps
# choice indices to their semantic meaning, enabling wide-format representation
# of choices across households.
#
# There are 15 specific child care choices (type + hours combinations):
#   - 7 types with pt/ft options = 14 choices
#   - Parent Only with 'none' = 1 choice
#
# Combined with 3 employment options:
#   - 1 child:  15 x 3 =  45 choices
#   - 2 children: 15^2 x 3 = 675 choices
#
# Only 4 ECEC types enter market equilibrium (compete for labor):
#   [1] Unpaid Center-Based (price fixed at 0)
#   [2] Low-Priced Center-Based
#   [3] High-Priced Center-Based
#   [4] Paid Home-Based
#
# Called from: 2a (run_calibration), 3c (run_scenario), 3d (accumulate_results)
#------------------------------------------------------------------------------



#-----------
# Constants
#-----------

# The 15 specific child care choices (type + hours combinations)
# These are NOT all combinations - Parent Only only has 'none', others have pt/ft
CHILD_CARE_CHOICES <- tibble::tribble(
  ~ecec_type,                  ~hours_choice,
  'High-Priced Center-Based',  'ft',
  'High-Priced Center-Based',  'pt',
  'Low-Priced Center-Based',   'ft',
  'Low-Priced Center-Based',   'pt',
  'Other Paid',                'ft',
  'Other Paid',                'pt',
  'Other Unpaid',              'ft',
  'Other Unpaid',              'pt',
  'Paid Home-Based',           'ft',
  'Paid Home-Based',           'pt',
  'Parent Only',               'none',
  'Unpaid Center-Based',       'ft',
  'Unpaid Center-Based',       'pt',
  'Unpaid Home-Based',         'ft',
  'Unpaid Home-Based',         'pt'
)

# Employment choices for the primary caregiver (order matters for indexing)
# 'none' = primary caregiver not working
# 'pt'   = primary caregiver part-time (<35 hrs/week)
# 'ft'   = primary caregiver full-time (35+ hrs/week)
# Note: Secondary parent's employment status is unchanged across choices
EMPLOYMENT_CHOICES <- c('none', 'pt', 'ft')

# Number of child care choices
N_CHILD_CHOICES <- nrow(CHILD_CARE_CHOICES)  # 15

# Total choices by number of children
N_CHOICES_1_CHILD <- N_CHILD_CHOICES^1 * length(EMPLOYMENT_CHOICES)
N_CHOICES_2_CHILD <- N_CHILD_CHOICES^2 * length(EMPLOYMENT_CHOICES)

# Market sectors for equilibrium - these 4 types compete for labor
# Price vector: P[1]=0 (unpaid), P[2:4] determined by equilibrium
MARKET_SECTORS <- c(
  'Unpaid Center-Based',
  'Low-Priced Center-Based',
  'High-Priced Center-Based',
  'Paid Home-Based'
)



get_choice_catalog <- function(n_children, cache = TRUE) {

  #----------------------------------------------------------------------------
  # Returns the choice catalog for families with n_children.
  # Caches result in package environment to avoid rebuilding.
  #
  # Builds the child choice grid (15 care choices with sector IDs) and
  # expands to full choice catalog (care x employment combinations).
  #
  # Params:
  #   - n_children (int): number of children (1 or 2)
  #   - cache (logical): whether to use/store cached catalog
  #
  # Returns:
  #   tibble with columns:
  #     - choice_id (int): 1-based index (1-45 or 1-675)
  #     - employment_choice (chr): 'none', 'pt', or 'ft'
  #     - child1_choice_id, child1_ecec_type, child1_hours_choice, child1_market_sector_id
  #     - child2_* (same columns, only if n_children == 2)
  #----------------------------------------------------------------------------

  cache_name <- paste0('choice_catalog_', n_children)

  if (cache && exists(cache_name, envir = .choice_cache)) {
    return(get(cache_name, envir = .choice_cache))
  }

  if (!n_children %in% c(1, 2)) {
    stop('n_children must be 1 or 2')
  }

  # Build child choice grid: 15 specific care choices with sector IDs
  child_grid <- CHILD_CARE_CHOICES %>%
    mutate(
      child_choice_id  = row_number(),
      market_sector_id = match(ecec_type, MARKET_SECTORS)
    ) %>%
    select(child_choice_id, ecec_type, hours_choice, market_sector_id)

  child1_grid <- child_grid %>%
    rename(child1_choice_id = child_choice_id) %>%
    rename_with(~ paste0('child1_', .x), .cols = -child1_choice_id)

  if (n_children == 1) {

    # 1-child families: 15 care choices x 3 employment = 45 total
    catalog <- expand_grid(
      employment_choice = EMPLOYMENT_CHOICES,
      child1_choice_id  = child_grid$child_choice_id
    ) %>%
      left_join(child1_grid, by = 'child1_choice_id') %>%
      mutate(choice_id = row_number(), .before = 1)

  } else {

    # 2-child families: 15 x 15 care choices x 3 employment = 675 total
    child2_grid <- child_grid %>%
      rename(child2_choice_id = child_choice_id) %>%
      rename_with(~ paste0('child2_', .x), .cols = -child2_choice_id)

    catalog <- expand_grid(
      employment_choice = EMPLOYMENT_CHOICES,
      child1_choice_id  = child_grid$child_choice_id,
      child2_choice_id  = child_grid$child_choice_id
    ) %>%
      left_join(child1_grid, by = 'child1_choice_id') %>%
      left_join(child2_grid, by = 'child2_choice_id') %>%
      mutate(choice_id = row_number(), .before = 1)
  }

  if (cache) {
    assign(cache_name, catalog, envir = .choice_cache)
  }

  return(catalog)
}


# Cache environment for choice catalogs
.choice_cache <- new.env(parent = emptyenv())
