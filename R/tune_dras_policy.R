#' Tune DRAS Policies
#'
#' Searches over EWMA smoothing parameters, warning thresholds, and
#' tightening factors to identify safety-cost trade-offs for DRAS.
#'
#' @param n_reps Number of Monte Carlo replications per policy.
#' @param n_lots Number of lots per replication.
#' @param scenario Defect-process scenario.
#' @param lambda_values Candidate EWMA smoothing values.
#' @param warning_values Candidate warning thresholds.
#' @param tighten_values Candidate tightening factors.
#' @param aql Acceptable quality level.
#' @param ltpd Limiting quality level.
#' @param alpha Maximum producer risk.
#' @param beta Maximum consumer risk.
#' @param seed Optional random seed.
#'
#' @return A data frame of policy performance results.
#' @export
#'
#' @examples
#' tune_dras_policy(
#'   n_reps = 2,
#'   n_lots = 30,
#'   scenario = "sudden_deterioration",
#'   lambda_values = c(0.20, 0.40),
#'   warning_values = c(0.03),
#'   tighten_values = c(1.50)
#' )
tune_dras_policy <- function(n_reps = 100,
                             n_lots = 200,
                             scenario = c("stable",
                                          "sudden_deterioration",
                                          "gradual_drift"),
                             lambda_values = c(0.05, 0.10, 0.20, 0.30, 0.40),
                             warning_values = c(0.02, 0.03, 0.04),
                             tighten_values = c(1.25, 1.50, 2.00),
                             aql = 0.01,
                             ltpd = 0.05,
                             alpha = 0.05,
                             beta = 0.10,
                             seed = NULL) {

  scenario <- match.arg(scenario)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  grid <- expand.grid(
    lambda = lambda_values,
    warning_threshold = warning_values,
    tighten_factor = tighten_values
  )

  out <- vector("list", nrow(grid))

  for (i in seq_len(nrow(grid))) {

    res <- run_dras_experiment(
      n_reps = n_reps,
      n_lots = n_lots,
      scenario = scenario,
      aql = aql,
      ltpd = ltpd,
      alpha = alpha,
      beta = beta,
      lambda = grid$lambda[i],
      warning_threshold = grid$warning_threshold[i],
      tighten_factor = grid$tighten_factor[i]
    )

    dras_row <- res[res$method == "DRAS", , drop = FALSE]

    dras_row$lambda <- grid$lambda[i]
    dras_row$warning_threshold <- grid$warning_threshold[i]
    dras_row$tighten_factor <- grid$tighten_factor[i]

    dras_row$safety_cost_score <-
      10 * dras_row$mean_false_accepts +
      2 * dras_row$mean_false_rejects +
      0.01 * dras_row$mean_avg_sample_number +
      0.01 * dras_row$mean_tightened_lots

    out[[i]] <- dras_row
  }

  results <- do.call(rbind, out)

  rownames(results) <- NULL

  results[order(results$safety_cost_score), ]
}
