
#' Probability of Accepting a Lot
#'
#' Computes the probability of accepting a lot under a binomial
#' acceptance sampling plan.
#'
#' @param n Sample size.
#' @param c Acceptance number. The lot is accepted if observed defects
#'   are less than or equal to `c`.
#' @param p Defect probability.
#'
#' @return Probability of lot acceptance.
#' @export
#'
#' @examples
#' prob_accept(n = 132, c = 3, p = 0.01)
prob_accept <- function(n, c, p) {
  stats::pbinom(q = c, size = n, prob = p)
}


#' Producer Risk
#'
#' Computes producer risk, defined as the probability of rejecting
#' a lot at the acceptable quality level.
#'
#' @param n Sample size.
#' @param c Acceptance number.
#' @param aql Acceptable quality level.
#'
#' @return Producer risk.
#' @export
#'
#' @examples
#' producer_risk(n = 132, c = 3, aql = 0.01)
producer_risk <- function(n, c, aql) {
  1 - prob_accept(n = n, c = c, p = aql)
}


#' Consumer Risk
#'
#' Computes consumer risk, defined as the probability of accepting
#' a lot at the limiting quality level.
#'
#' @param n Sample size.
#' @param c Acceptance number.
#' @param ltpd Limiting quality level.
#'
#' @return Consumer risk.
#' @export
#'
#' @examples
#' consumer_risk(n = 132, c = 3, ltpd = 0.05)
consumer_risk <- function(n, c, ltpd) {
  prob_accept(n = n, c = c, p = ltpd)
}


#' Find a Single Acceptance Sampling Plan
#'
#' Finds the smallest sample size and acceptance number satisfying
#' producer-risk and consumer-risk constraints.
#'
#' @param aql Acceptable quality level.
#' @param ltpd Limiting quality level.
#' @param alpha Maximum producer risk.
#' @param beta Maximum consumer risk.
#' @param max_n Maximum sample size to search.
#'
#' @return A data frame with sample size, acceptance number, producer risk,
#'   and consumer risk.
#' @export
#'
#' @examples
#' find_plan(aql = 0.01, ltpd = 0.05, alpha = 0.05, beta = 0.10)
find_plan <- function(aql, ltpd, alpha = 0.05, beta = 0.10, max_n = 500) {

  if (aql <= 0 || ltpd <= 0 || aql >= ltpd) {
    stop("Require 0 < aql < ltpd.")
  }

  if (alpha <= 0 || alpha >= 1 || beta <= 0 || beta >= 1) {
    stop("alpha and beta must be between 0 and 1.")
  }

  for (n in seq_len(max_n)) {
    for (c in 0:n) {

      pr <- producer_risk(n = n, c = c, aql = aql)
      cr <- consumer_risk(n = n, c = c, ltpd = ltpd)

      if (pr <= alpha && cr <= beta) {
        return(data.frame(
          n = n,
          c = c,
          producer_risk = pr,
          consumer_risk = cr
        ))
      }
    }
  }

  stop("No plan found. Increase max_n.")
}

