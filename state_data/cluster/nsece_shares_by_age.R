#------------------------------------------------------------------------------
# nsece_shares_by_age.R -- NSECE 2019 care-type shares of children 0-4, by
# single year of age and age group (0-2, 3-4).
#
# DIAGNOSTIC ONLY -- writes nothing build or simulator reads. Places NSECE (the
# survey the care-choice model is trained on) beside the model's p0 and
# NHES-ECPP by age: says whether the paid-center gap at ages 3-4 is model vs
# NSECE or NSECE vs NHES.
#
# Same classification as 02_derive_nsece_constants.R and 1a (universe A: any
# regular arrangement, NSECE five-hour screener; K-8 excluded; TOC 5 -- lessons,
# drop-in, church care, recreation -- Other); Parent Only = children
# 0-4 w/out a primary arrangement. Paid price bands pooled into Center-Based.
# Part-time = primary-type hours under 20.
#
# Usage (repo root, cluster; licensed data):
#   Rscript state_data/cluster/nsece_shares_by_age.R --nsece <dir> [--out <file>]
#------------------------------------------------------------------------------

# read_household(), classify_children() (unknown payment drawn),
# primary_arrangement(), PAID_CENTRE, PT_FT_CUT; main() guarded
source('state_data/code/02_derive_nsece_constants.R')



shares_by <- function(kids, prim, cell) {

  #----------------------------------------------------------------------------
  # Weighted share of children 0-4 by care type, part-time share by type, per
  # cell.
  #
  # Params:
  #   - kids (tibble): every child 0-4 x label draw (classify_children(),
  #       child_weight / M, rec = 1 / M)
  #   - prim (tibble): primary-arrangement rows joined to kids
  #   - cell (function): age -> cell label
  #
  # Returns: (tibble) cell, type, share, part_time, n (mean records per
  #   draw) -- shares pooled over label draws
  #----------------------------------------------------------------------------

  all <- kids %>%
    left_join(select(prim, hh_id, child_id, draw, ecec_choice, hours),
              by = c('hh_id', 'child_id', 'draw')) %>%
    mutate(type = case_when(is.na(ecec_choice) ~ 'Parent Only',
                            ecec_choice %in% PAID_CENTRE ~ 'Center-Based',
                            TRUE ~ ecec_choice),
           cell = cell(age))
  bind_rows(all, mutate(all, cell = 'all 0-4')) %>%
    group_by(cell) %>%
    mutate(total = sum(child_weight)) %>%
    group_by(cell, type) %>%
    summarise(share = sum(child_weight) / first(total),
              part_time = sum(child_weight[!is.na(hours) & hours < PT_FT_CUT]) /
                          sum(child_weight),
              n = n_rec(rec), .groups = 'drop')
}



main_by_age <- function() {

  # Reads NSECE urban density (02_ read_household()): state setup setting
  require_urban_density_on('state_data/cluster/nsece_shares_by_age.R')

  root <- cli_value('--nsece', Sys.getenv('NSECE_ROOT'))
  if (!nzchar(root)) stop('pass --nsece <dir> or set NSECE_ROOT')
  out <- cli_value('--out', 'nsece_shares_by_age.csv')

  # Unknown payment: drawn paid status, NSECE_MI_DRAWS draws pooled; no
  # draw -> child out of numerator, denominator (02_ classify_children())
  cc   <- classify_children(read_household(root))
  kids <- cc$kids
  prim <- cc$prim
  res <- bind_rows(
    shares_by(kids, prim, function(a) ifelse(a <= 2, '0-2', '3-4')),
    shares_by(kids, prim, function(a) paste('age', a)) %>% filter(cell != 'all 0-4')) %>%
    arrange(cell, desc(share))
  write_csv(res, out)
  show_df(res %>% filter(cell %in% c('0-2', '3-4', 'all 0-4')) %>%
            mutate(share = round(100 * share, 1), part_time = round(100 * part_time, 1)))
  cat('wrote', out, '\n')
  invisible(res)
}


if (sys.nframe() == 0) main_by_age()
