
#' Simulate a Defect Process
#'
#' Simulates lot-level defect probabilities under stable, sudden
#' deterioration, or gradual drift scenarios.
#'
#' @param n_lots Number of lots.
#' @param scenario One of `"stable"`, `"sudden_deterioration"`,
#'   or `"gradual_drift"`.
#' @param p0 Initial defect probability.
#' @param p1 Deteriorated defect probability.
#'
#' @return Numeric vector of true defect probabilities.
#' @export
#'
#' @examples
#' simulate_defect_process(10, scenario = "gradual_drift")
simulate_defect_process <- function(n_lots = 200,
                                    scenario = c("stable",
                                                 "sudden_deterioration",
                                                 "gradual_drift"),
                                    p0 = 0.01,
                                    p1 = 0.06) {

  scenario <- match.arg(scenario)

  if (scenario == "stable") {
    return(rep(p0, n_lots))
  }

  if (scenario == "sudden_deterioration") {
    p <- rep(p0, n_lots)
    p[(floor(n_lots / 2) + 1):n_lots] <- p1
    return(p)
  }

  if (scenario == "gradual_drift") {
    return(seq(p0, p1, length.out = n_lots))
  }
}


#' Simulate Lot Inspection Data
#'
#' @param p_true Numeric vector of true defect probabilities.
#' @param lot_size Lot size.
#'
#' @return Data frame of simulated lots.
#' @export
#'
#' @examples
#' p <- simulate_defect_process(20)
#' simulate_lots(p)
simulate_lots <- function(p_true, lot_size = 1000) {

  data.frame(
    lot = seq_along(p_true),
    p_true = p_true,
    lot_size = lot_size,
    true_defects = stats::rbinom(
      n = length(p_true),
      size = lot_size,
      prob = p_true
    )
  )
}


#' Apply a Fixed Acceptance Sampling Plan
#'
#' @param lots Data frame with column `p_true`.
#' @param n Sample size.
#' @param c Acceptance number.
#' @param aql Acceptable quality level.
#'
#' @return Data frame with lot decisions.
#' @export
#'
#' @examples
#' plan <- find_plan(0.01, 0.05)
#' p <- simulate_defect_process(20)
#' lots <- simulate_lots(p)
#' apply_fixed_plan(lots, n = plan$n, c = plan$c)
apply_fixed_plan <- function(lots, n, c, aql = 0.01) {

  if (!"p_true" %in% names(lots)) {
    stop("lots must contain a column named p_true.")
  }

  sample_defects <- stats::rbinom(
    n = nrow(lots),
    size = n,
    prob = lots$p_true
  )

  data.frame(
    lot = lots$lot,
    p_true = lots$p_true,
    n = n,
    c = c,
    sample_defects = sample_defects,
    observed_rate = sample_defects / n,
    decision = ifelse(sample_defects <= c, "accept", "reject"),
    true_state = ifelse(lots$p_true <= aql, "acceptable", "unacceptable")
  )
}


#' Apply EWMA Drift-Aware Acceptance Sampling
#'
#' Applies a drift-aware acceptance sampling policy using an EWMA statistic
#' to trigger tightened inspection.
#'
#' @param lots Data frame with column `p_true`.
#' @param n0 Baseline sample size.
#' @param c0 Baseline acceptance number.
#' @param lambda EWMA smoothing parameter.
#' @param warning_threshold Drift warning threshold.
#' @param tighten_factor Multiplicative factor applied to `n0` under
#'   tightened inspection.
#' @param aql Acceptable quality level.
#'
#' @return Data frame with adaptive lot decisions.
#' @export
#'
#' @examples
#' plan <- find_plan(0.01, 0.05)
#' p <- simulate_defect_process(50, "sudden_deterioration")
#' lots <- simulate_lots(p)
#' dras_ewma(lots, n0 = plan$n, c0 = plan$c)
dras_ewma <- function(lots,
                      n0,
                      c0,
                      lambda = 0.20,
                      warning_threshold = 0.03,
                      tighten_factor = 1.50,
                      aql = 0.01) {

  if (!"p_true" %in% names(lots)) {
    stop("lots must contain a column named p_true.")
  }

  if (lambda <= 0 || lambda > 1) {
    stop("lambda must be in (0, 1].")
  }

  z <- aql
  out <- vector("list", nrow(lots))

  for (i in seq_len(nrow(lots))) {

    if (z > warning_threshold) {
      mode <- "tightened"
      n_t <- ceiling(n0 * tighten_factor)
    } else {
      mode <- "normal"
      n_t <- n0
    }

    defects <- stats::rbinom(1, size = n_t, prob = lots$p_true[i])
    obs_rate <- defects / n_t

    z <- lambda * obs_rate + (1 - lambda) * z

    out[[i]] <- data.frame(
      lot = lots$lot[i],
      p_true = lots$p_true[i],
      mode = mode,
      n = n_t,
      c = c0,
      sample_defects = defects,
      observed_rate = obs_rate,
      ewma_rate = z,
      decision = ifelse(defects <= c0, "accept", "reject"),
      true_state = ifelse(lots$p_true[i] <= aql, "acceptable", "unacceptable")
    )
  }

  do.call(rbind, out)
}

