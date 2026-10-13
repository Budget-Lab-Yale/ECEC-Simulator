assign_primary_care <- function(prov) {

  #----------------------------------------------------------------------------
  # One NSECE child's primary care arrangement (type, hours) from its
  # provider rows -- Borowsky method: rank first (paid before unpaid, centre
  # first), hours summed within type. Paid-centre tiers share one rank -- tier
  # w/ more summed hours wins, High on exact tie (2026-10-12; High beat Low
  # regardless of hours before).
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

  category_priority <- c(
    'K-8',
    'High-Priced Center-Based',
    'Low-Priced Center-Based',
    'Unpaid Center-Based',
    'Paid Home-Based',
    'Other Paid',
    'Unpaid Home-Based',
    'Other Unpaid'
  )
  category_rank <- c(1, 2, 2, 3, 4, 5, 6, 7)

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
      top <- order(category_rank[pos], -h, pos)[1]
      choice <- cats[top]
      hours <- unname(h[top])
    } else {
      # No provider rows, or none counting -> Parent Only. Regularity always
      # resolved (resolve_regular_care()), so no NA bucket -- same as 02
      # (2026-10-12; rows all NA in regular / label -> NA before)
      choice <- 'Parent Only'
      hours <- 0
    }
    band <- if (is.na(choice)) NA_character_ else if (hours == 0) 'none' else if (hours < 20) 'pt' else 'ft'
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
