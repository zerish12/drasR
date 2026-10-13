
#' Evaluate Lot-Disposition Results
#'
#' Evaluates false acceptance, false rejection, inspection burden,
#' and tightened-inspection frequency.
#'
#' @param results Data frame returned by `apply_fixed_plan()` or `dras_ewma()`.
#' @param method_name Name of method.
#'
#' @return One-row data frame of performance metrics.
#' @export
#'
#' @examples
#' plan <- find_plan(0.01, 0.05)
#' p <- simulate_defect_process(20)
#' lots <- simulate_lots(p)
#' res <- apply_fixed_plan(lots, plan$n, plan$c)
#' evaluate_sampling(res, "Fixed ASP")
evaluate_sampling <- function(results, method_name = "method") {

  unacceptable <- results$true_state == "unacceptable"
  acceptable <- results$true_state == "acceptable"
  accepted <- results$decision == "accept"
  rejected <- results$decision == "reject"

  false_accepts <- sum(unacceptable & accepted)
  false_rejects <- sum(acceptable & rejected)

  false_accept_rate <- if (sum(unacceptable) > 0) {
    false_accepts / sum(unacceptable)
  } else {
    NA_real_
  }

  false_reject_rate <- if (sum(acceptable) > 0) {
    false_rejects / sum(acceptable)
  } else {
    NA_real_
  }

  tightened_lots <- if ("mode" %in% names(results)) {
    sum(results$mode == "tightened")
  } else {
    0
  }

  data.frame(
    method = method_name,
    n_lots = nrow(results),
    avg_sample_number = mean(results$n),
    accepted_lots = sum(accepted),
    rejected_lots = sum(rejected),
    false_accepts = false_accepts,
    false_rejects = false_rejects,
    false_accept_rate = false_accept_rate,
    false_reject_rate = false_reject_rate,
    tightened_lots = tightened_lots
  )
}

