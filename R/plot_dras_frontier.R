#' Plot the DRAS Safety-Cost Frontier
#'
#' Plots the safety-cost trade-off from a DRAS tuning table.
#' The x-axis represents inspection burden and the y-axis represents
#' false-acceptance burden.
#'
#' @param tuning_results Data frame returned by `tune_dras_policy()`.
#' @param selected_policies Optional data frame returned by `select_dras_policy()`.
#' @param cost_col Column name for inspection burden.
#' @param safety_col Column name for false-acceptance burden.
#' @param main Plot title.
#'
#' @return Invisibly returns `tuning_results`.
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
#' selected <- select_dras_policy(tuned)
#' plot_dras_frontier(tuned, selected)
plot_dras_frontier <- function(tuning_results,
                               selected_policies = NULL,
                               cost_col = "mean_avg_sample_number",
                               safety_col = "mean_false_accepts",
                               main = "DRAS Safety-Cost Frontier") {

  if (!is.data.frame(tuning_results)) {
    stop("tuning_results must be a data frame.")
  }

  required_cols <- c(cost_col, safety_col)

  if (!all(required_cols %in% names(tuning_results))) {
    stop("cost_col and safety_col must be columns in tuning_results.")
  }

  graphics::plot(
    tuning_results[[cost_col]],
    tuning_results[[safety_col]],
    xlab = "Mean sample number",
    ylab = "Mean false accepts",
    main = main,
    pch = 19
  )

  if (!is.null(selected_policies)) {
    if (!all(required_cols %in% names(selected_policies))) {
      stop("selected_policies must contain cost_col and safety_col.")
    }

    graphics::points(
      selected_policies[[cost_col]],
      selected_policies[[safety_col]],
      pch = 17,
      cex = 1.3
    )

    if ("policy_type" %in% names(selected_policies)) {
      graphics::text(
        selected_policies[[cost_col]],
        selected_policies[[safety_col]],
        labels = selected_policies$policy_type,
        pos = 4,
        cex = 0.8
      )
    }
  }

  invisible(tuning_results)
}
