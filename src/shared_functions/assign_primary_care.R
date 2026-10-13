assign_primary_care <- function(prov) {

  #----------------------------------------------------------------------------
  # One NSECE child's primary care arrangement (type, hours) from its
  # provider rows -- Borowsky method: rank first (paid before unpaid, centre
  # first), hours summed within type. High-price centre outranks low-price,
  # as simulator standard (main 1a): child using both tiers -> High
  # (2026-10-13; more summed hours won 10-12 to 10-13).
  #
  # Unknown payment (unknown_kind set): counting unknown rows (regular,
  # hours > 0) take drawn label (imputed_choice, impute_unknown_payment() --
  # draw for every counting unknown row). Child whose label (type x hours
  # band, forest target) same under every paid / unpaid classification gets
  # same label from any draw -- no enumeration needed (2026-10-12;
  # enumerated, draws only where label differed, same day). Missing draw ->
  # NA (child out). Counting row: regular == 1, hours > 0 -- both resolved
  # upstream by resolve_regular_care() (REGCARE -1 / -2 by TOC, zero-hour
  # regular rows imputed hours).
  #
  # Params:
  #   - prov (df): child's provider rows -- regular (num), hours (num),
  #       ecec_choice (chr; NA for unknown payment), unknown_kind (chr:
  #       'center' | 'home' | 'other' | NA); optional imputed_choice (chr:
  #       drawn label of unknown-payment row). Zero rows -> Parent Only
  #
  # Returns: (tibble) one row -- ecec_choice (chr), hours (num), imputed
  #   (lgl: child has counting unknown-payment row, label uses its draw)
  #----------------------------------------------------------------------------

  category_priority <- ECEC_PRIMARY_PRIORITY   # constants.R

  # Unpaid label by unknown_kind -- stands in on non-counting unknown rows
  unpaid_label <- c(center = 'Unpaid Center-Based', home = 'Unpaid Home-Based',
                    other = 'Other Unpaid')

  counts <- !is.na(prov$regular) & prov$regular == 1 &
            !is.na(prov$hours) & prov$hours > 0

  classify <- function(labels) {

    use <- counts & !is.na(labels)
    if (any(use)) {
      h <- tapply(prov$hours[use], labels[use], sum)
      cats <- names(h)
      pos <- match(cats, category_priority)
      top <- order(pos)[1]   # one row per type after summing -- rank alone
      choice <- cats[top]
      hours <- unname(h[top])
    } else {
      # No provider rows, or none counting -> Parent Only. Regularity always
      # resolved (resolve_regular_care()), so no NA bucket -- same as 02
      # (2026-10-12; rows all NA in regular / label -> NA before)
      choice <- 'Parent Only'
      hours <- 0
    }
    band <- if (is.na(choice)) NA_character_ else if (hours == 0) 'none' else if (hours < PT_FT_CUT_HOURS) 'pt' else 'ft'
    list(key = paste(choice, band), value = tibble(ecec_choice = choice, hours = hours))
  }

  labels <- as.character(prov$ecec_choice)
  unk <- which(!is.na(prov$unknown_kind))
  labels[unk] <- unpaid_label[prov$unknown_kind[unk]]
  rows <- unk[counts[unk]]
  if (length(rows) == 0) return(mutate(classify(labels)$value, imputed = FALSE))

  # Counting unknown rows take drawn label; missing draw -> NA
  imp <- if ('imputed_choice' %in% names(prov)) prov$imputed_choice else rep(NA_character_, nrow(prov))
  if (any(is.na(imp[rows]))) {
    return(tibble(ecec_choice = NA_character_, hours = NA_real_, imputed = FALSE))
  }
  labels[rows] <- imp[rows]
  mutate(classify(labels)$value, imputed = TRUE)
}
