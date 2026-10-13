
#' Acceptance Decision Fragility
#'
#' Computes whether an accept-favouring decision changes under plausible
#' uncertainty around an estimated defect probability.
#'
#' @param n Sample size.
#' @param c Acceptance number.
#' @param p_hat Estimated defect probability.
#' @param uncertainty_width Half-width of uncertainty interval.
#' @param threshold Acceptance probability threshold.
#' @param grid_points Number of grid points in uncertainty interval.
#'
#' @return Integer value: 1 if fragile, 0 if stable.
#' @export
#'
#' @examples
#' decision_fragility(n = 132, c = 3, p_hat = 0.03)
decision_fragility <- function(n,
                               c,
                               p_hat,
                               uncertainty_width = 0.01,
                               threshold = 0.50,
                               grid_points = 101) {

  p_low <- max(0, p_hat - uncertainty_width)
  p_high <- min(1, p_hat + uncertainty_width)

  p_grid <- seq(p_low, p_high, length.out = grid_points)

  accept_probs <- prob_accept(n = n, c = c, p = p_grid)

  decisions <- accept_probs >= threshold

  as.integer(length(unique(decisions)) > 1)
}

