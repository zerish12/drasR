#' Monitor Real Lot Inspection Data Using DRAS
#'
#' Applies an EWMA-based DRAS monitoring rule to real lot-level inspection
#' data prepared by `prepare_lot_data()`. Unlike simulation functions, this
#' function does not require the true defect probability.
#'
#' @param data A data frame returned by `prepare_lot_data()`.
#' @param baseline_sample_size Baseline sample size under normal inspection.
#' @param acceptance_number Acceptance number used for lot disposition.
#' @param lambda EWMA smoothing parameter.
#' @param warning_threshold EWMA warning threshold.
#' @param tighten_factor Multiplicative tightening factor used when the
#'   EWMA statistic exceeds the warning threshold.
#' @param initial_rate Initial EWMA value. Defaults to 0.01.
#'
#' @return A data frame with EWMA statistic, inspection mode, and
#'   recommended next sample size.
#' @export
#'
#' @examples
#' raw_data <- data.frame(
#'   lot_id = paste0("L", 1:6),
#'   sample_size = rep(100, 6),
#'   defects_found = c(1, 0, 1, 4, 5, 6)
#' )
#'
#' prepared <- prepare_lot_data(
#'   raw_data,
#'   lot_id = "lot_id",
#'   sample_size = "sample_size",
#'   defects = "defects_found"
#' )
#'
#' dras_monitor(prepared, baseline_sample_size = 132, acceptance_number = 3)
dras_monitor <- function(data,
                         baseline_sample_size,
                         acceptance_number,
                         lambda = 0.20,
                         warning_threshold = 0.03,
                         tighten_factor = 1.50,
                         initial_rate = 0.01) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  required <- c("lot", "sample_size", "sample_defects", "observed_rate")

  missing_required <- setdiff(required, names(data))

  if (length(missing_required) > 0) {
    stop(
      "data must contain columns: ",
      paste(required, collapse = ", "),
      ". Missing: ",
      paste(missing_required, collapse = ", ")
    )
  }

  if (lambda <= 0 || lambda > 1) {
    stop("lambda must be in (0, 1].")
  }

  if (baseline_sample_size <= 0) {
    stop("baseline_sample_size must be positive.")
  }

  if (acceptance_number < 0) {
    stop("acceptance_number cannot be negative.")
  }

  z <- initial_rate
  out <- data
  out$ewma_rate <- NA_real_
  out$inspection_mode <- NA_character_
  out$recommended_next_sample_size <- NA_real_
  out$lot_decision <- NA_character_
  out$dras_signal <- NA_character_

  for (i in seq_len(nrow(out))) {

    z <- lambda * out$observed_rate[i] + (1 - lambda) * z

    if (z > warning_threshold) {
      mode <- "tightened"
      next_n <- ceiling(baseline_sample_size * tighten_factor)
      signal <- "warning"
    } else {
      mode <- "normal"
      next_n <- baseline_sample_size
      signal <- "normal"
    }

    decision <- ifelse(
      out$sample_defects[i] <= acceptance_number,
      "accept",
      "reject"
    )

    out$ewma_rate[i] <- z
    out$inspection_mode[i] <- mode
    out$recommended_next_sample_size[i] <- next_n
    out$lot_decision[i] <- decision
    out$dras_signal[i] <- signal
  }

  out
}
