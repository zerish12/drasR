#' Summarise DRAS Monitoring Results
#'
#' Summarises the output from `dras_monitor()` to provide an audit-friendly
#' overview of lot decisions, warnings, inspection modes, and defect rates.
#'
#' @param monitored_data A data frame returned by `dras_monitor()`.
#' @param by_supplier Logical. If TRUE and supplier information is available,
#'   returns summaries by supplier.
#'
#' @return A data frame containing DRAS monitoring summary statistics.
#' @export
#'
#' @examples
#' raw_data <- data.frame(
#'   lot_id = paste0("L", 1:6),
#'   supplier = c("A", "A", "A", "B", "B", "B"),
#'   sample_size = rep(100, 6),
#'   defects_found = c(1, 0, 1, 4, 5, 6)
#' )
#'
#' prepared <- prepare_lot_data(
#'   raw_data,
#'   lot_id = "lot_id",
#'   sample_size = "sample_size",
#'   defects = "defects_found",
#'   supplier = "supplier"
#' )
#'
#' monitored <- dras_monitor(
#'   prepared,
#'   baseline_sample_size = 132,
#'   acceptance_number = 3
#' )
#'
#' summarise_dras_monitor(monitored)
summarise_dras_monitor <- function(monitored_data, by_supplier = FALSE) {

  if (!is.data.frame(monitored_data)) {
    stop("monitored_data must be a data frame.")
  }

  required <- c(
    "sample_size",
    "sample_defects",
    "observed_rate",
    "inspection_mode",
    "lot_decision",
    "dras_signal"
  )

  missing_required <- setdiff(required, names(monitored_data))

  if (length(missing_required) > 0) {
    stop(
      "monitored_data appears not to be output from dras_monitor(). Missing: ",
      paste(missing_required, collapse = ", ")
    )
  }

  summarise_one <- function(x, label = "All") {
    data.frame(
      group = label,
      n_lots = nrow(x),
      accepted_lots = sum(x$lot_decision == "accept", na.rm = TRUE),
      rejected_lots = sum(x$lot_decision == "reject", na.rm = TRUE),
      normal_lots = sum(x$inspection_mode == "normal", na.rm = TRUE),
      tightened_lots = sum(x$inspection_mode == "tightened", na.rm = TRUE),
      warning_lots = sum(x$dras_signal == "warning", na.rm = TRUE),
      mean_observed_rate = mean(x$observed_rate, na.rm = TRUE),
      max_observed_rate = max(x$observed_rate, na.rm = TRUE),
      mean_sample_size = mean(x$sample_size, na.rm = TRUE),
      total_sampled = sum(x$sample_size, na.rm = TRUE),
      total_defects_found = sum(x$sample_defects, na.rm = TRUE)
    )
  }

  if (by_supplier) {
    if (!"supplier_id" %in% names(monitored_data)) {
      stop("supplier_id column not found. Set by_supplier = FALSE or provide supplier in prepare_lot_data().")
    }

    suppliers <- unique(monitored_data$supplier_id)

    out <- lapply(suppliers, function(s) {
      summarise_one(monitored_data[monitored_data$supplier_id == s, , drop = FALSE], label = s)
    })

    return(do.call(rbind, out))
  }

  summarise_one(monitored_data)
}
