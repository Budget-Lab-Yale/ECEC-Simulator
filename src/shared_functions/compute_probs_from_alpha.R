compute_probs_from_alpha <- function(alpha, V) {

  #----------------------------------------------------------------------------
  # Computes choice probabilities from alpha and V matrices.
  #
  # Model:
  #   p_ij = exp(alpha_ij + V_ij) / sum_k exp(alpha_ik + V_ik)
  #
  # Alpha matrix is stored with K-1 columns (alpha_1 = 0 for normalization).
  # This function prepends a zero column to reconstruct full alpha matrix.
  #
  # Uses numerically stable softmax (subtract row max before exp).
  #
  # Params:
  #   - alpha (dbl matrix): n x (K-1) normalized alpha values (alpha_1 = 0)
  #   - V (dbl matrix): n x K utility values
  #
  # Returns:
  #   dbl matrix: n x K choice probabilities (rows sum to 1)
  #----------------------------------------------------------------------------

  n <- nrow(alpha)
  K <- ncol(V)

  # Prepend zero column for alpha_1 (normalization)
  alpha_full <- cbind(0, alpha)

  # Compute log-unnormalized probabilities
  log_unnorm <- alpha_full + V

  # Numerically stable softmax: subtract row max before exp
  row_max <- apply(log_unnorm, 1, max)
  exp_shifted <- exp(log_unnorm - row_max)
  Z <- rowSums(exp_shifted)

  # Normalize
  result <- exp_shifted / Z

  # Validate result
  bad_rows <- rowSums(!is.finite(result)) > 0
  if (any(bad_rows)) {
    n_bad <- sum(bad_rows)
    stop(sprintf('compute_probs_from_alpha: %d rows produced NaN/Inf. Check alpha and V values.', n_bad))
  }

  result
}
