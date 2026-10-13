#' Select Low-Cost, Balanced, and High-Safety DRAS Policies
#'
#' Selects three representative policies from a DRAS tuning table:
#' a low-cost policy, a balanced policy, and a high-safety policy.
#'
#' @param tuning_results Data frame returned by `tune_dras_policy()`.
#' @param cost_col Column name for inspection burden.
#' @param safety_col Column name for false-acceptance burden.
#'
#' @return A data frame containing three selected DRAS policies.
#' @export
#'
#' @examples
#' tuned <- tune_dras_policy(
#'   n_reps = 2,
#'   n_lots = 30,
#'   scenario = "sudden_deterioration",
#'   lambda_values = c(0.20, 0.40),
#'   warning_values = c(0.03),
#'   tighten_values = c(1.50, 2.00)
#' )
#' select_dras_policy(tuned)
select_dras_policy <- function(tuning_results,
                               cost_col = "mean_avg_sample_number",
                               safety_col = "mean_false_accepts") {

  if (!is.data.frame(tuning_results)) {
    stop("tuning_results must be a data frame.")
  }

  required_cols <- c(cost_col, safety_col)

  if (!all(required_cols %in% names(tuning_results))) {
    stop("cost_col and safety_col must be columns in tuning_results.")
  }

  x <- tuning_results

  # Low-cost policy: minimum inspection burden
  low_cost <- x[which.min(x[[cost_col]]), , drop = FALSE]
  low_cost$policy_type <- "Low-cost DRAS"

  # High-safety policy: minimum false acceptance burden
  high_safety <- x[which.min(x[[safety_col]]), , drop = FALSE]
  high_safety$policy_type <- "High-safety DRAS"

  # Balanced policy: closest to ideal after normalising cost and safety
  cost_range <- range(x[[cost_col]], na.rm = TRUE)
  safety_range <- range(x[[safety_col]], na.rm = TRUE)

  if (diff(cost_range) == 0) {
    cost_norm <- rep(0, nrow(x))
  } else {
    cost_norm <- (x[[cost_col]] - cost_range[1]) / diff(cost_range)
  }

  if (diff(safety_range) == 0) {
    safety_norm <- rep(0, nrow(x))
  } else {
    safety_norm <- (x[[safety_col]] - safety_range[1]) / diff(safety_range)
  }

  balanced_score <- cost_norm + safety_norm

  balanced <- x[which.min(balanced_score), , drop = FALSE]
  balanced$policy_type <- "Balanced DRAS"

  selected <- rbind(low_cost, balanced, high_safety)

  # Put policy_type first
  selected <- selected[, c("policy_type", setdiff(names(selected), "policy_type")), drop = FALSE]

  rownames(selected) <- NULL

  selected
}
