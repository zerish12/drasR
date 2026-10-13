#' Run a DRAS Monte Carlo Experiment
#'
#' Runs repeated simulations comparing fixed acceptance sampling with
#' EWMA-based drift-aware risk-guaranteed acceptance sampling.
#'
#' @param n_reps Number of Monte Carlo replications.
#' @param n_lots Number of lots per replication.
#' @param scenario Defect-process scenario.
#' @param aql Acceptable quality level.
#' @param ltpd Limiting quality level.
#' @param alpha Maximum producer risk.
#' @param beta Maximum consumer risk.
#' @param lambda EWMA smoothing parameter.
#' @param warning_threshold EWMA warning threshold.
#' @param tighten_factor Multiplicative tightening factor.
#' @param seed Optional random seed.
#'
#' @return A data frame of summary results by method.
#' @export
#'
#' @examples
#' run_dras_experiment(n_reps = 10, scenario = "sudden_deterioration")
run_dras_experiment <- function(n_reps = 100,
                                n_lots = 200,
                                scenario = c("stable",
                                             "sudden_deterioration",
                                             "gradual_drift"),
                                aql = 0.01,
                                ltpd = 0.05,
                                alpha = 0.05,
                                beta = 0.10,
                                lambda = 0.40,
                                warning_threshold = 0.02,
                                tighten_factor = 2.00,
                                seed = NULL) {

  scenario <- match.arg(scenario)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  plan <- find_plan(
    aql = aql,
    ltpd = ltpd,
    alpha = alpha,
    beta = beta
  )

  all_results <- vector("list", n_reps * 2)
  counter <- 1

  for (rep in seq_len(n_reps)) {

    p <- simulate_defect_process(
      n_lots = n_lots,
      scenario = scenario,
      p0 = aql,
      p1 = ltpd + 0.01
    )

    lots <- simulate_lots(p)

    fixed <- apply_fixed_plan(
      lots,
      n = plan$n,
      c = plan$c,
      aql = aql
    )

    dras <- dras_ewma(
      lots,
      n0 = plan$n,
      c0 = plan$c,
      lambda = lambda,
      warning_threshold = warning_threshold,
      tighten_factor = tighten_factor,
      aql = aql
    )

    fixed_eval <- evaluate_sampling(fixed, "Fixed ASP")
    dras_eval <- evaluate_sampling(dras, "DRAS")

    fixed_eval$replication <- rep
    dras_eval$replication <- rep
    fixed_eval$scenario <- scenario
    dras_eval$scenario <- scenario

    all_results[[counter]] <- fixed_eval
    counter <- counter + 1

    all_results[[counter]] <- dras_eval
    counter <- counter + 1
  }

  results <- do.call(rbind, all_results)

  summary <- stats::aggregate(
    cbind(avg_sample_number, false_accepts, false_rejects,
          false_accept_rate, false_reject_rate, tightened_lots) ~ method + scenario,
    data = results,
    FUN = mean
  )

  names(summary) <- c(
    "method",
    "scenario",
    "mean_avg_sample_number",
    "mean_false_accepts",
    "mean_false_rejects",
    "mean_false_accept_rate",
    "mean_false_reject_rate",
    "mean_tightened_lots"
  )

  fixed_fa <- summary$mean_false_accepts[summary$method == "Fixed ASP"]

  summary$false_accept_reduction_percent <-
    ifelse(
      fixed_fa > 0,
      100 * (fixed_fa - summary$mean_false_accepts) / fixed_fa,
      NA_real_
    )

  summary
}
