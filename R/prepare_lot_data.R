#' Prepare Real Lot-Level Inspection Data
#'
#' Converts real industrial or pharmaceutical inspection data into a
#' standard format suitable for DRAS analysis.
#'
#' @param data A data frame containing lot-level inspection records.
#' @param lot_id Name of the lot identifier column.
#' @param sample_size Name of the inspected sample-size column.
#' @param defects Name of the observed defect-count column.
#' @param lot_size Optional name of the lot-size column.
#' @param date Optional name of the date column.
#' @param supplier Optional name of the supplier identifier column.
#'
#' @return A data frame in DRAS-ready format.
#' @export
#'
#' @examples
#' raw_data <- data.frame(
#'   lot_id = paste0("L", 1:5),
#'   date = as.Date("2026-01-01") + 0:4,
#'   supplier = c("A", "A", "A", "B", "B"),
#'   lot_size = rep(1000, 5),
#'   sample_size = rep(100, 5),
#'   defects_found = c(1, 0, 2, 5, 4)
#' )
#'
#' prepare_lot_data(
#'   raw_data,
#'   lot_id = "lot_id",
#'   sample_size = "sample_size",
#'   defects = "defects_found",
#'   lot_size = "lot_size",
#'   date = "date",
#'   supplier = "supplier"
#' )
prepare_lot_data <- function(data,
                             lot_id,
                             sample_size,
                             defects,
                             lot_size = NULL,
                             date = NULL,
                             supplier = NULL) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  required <- c(lot_id, sample_size, defects)

  optional <- c(lot_size, date, supplier)
  optional <- optional[!is.null(optional)]

  missing_required <- setdiff(required, names(data))

  if (length(missing_required) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_required, collapse = ", ")
    )
  }

  missing_optional <- setdiff(optional, names(data))

  if (length(missing_optional) > 0) {
    stop(
      "Missing optional columns supplied by user: ",
      paste(missing_optional, collapse = ", ")
    )
  }

  out <- data.frame(
    lot = seq_len(nrow(data)),
    lot_id = data[[lot_id]],
    sample_size = data[[sample_size]],
    sample_defects = data[[defects]]
  )

  if (!is.null(lot_size)) {
    out$lot_size <- data[[lot_size]]
  } else {
    out$lot_size <- NA_real_
  }

  if (!is.null(date)) {
    out$date <- data[[date]]
  }

  if (!is.null(supplier)) {
    out$supplier_id <- data[[supplier]]
  }

  if (any(is.na(out$sample_size))) {
    stop("sample_size contains missing values.")
  }

  if (any(is.na(out$sample_defects))) {
    stop("sample_defects contains missing values.")
  }

  if (any(out$sample_size <= 0)) {
    stop("sample_size must be positive.")
  }

  if (any(out$sample_defects < 0)) {
    stop("sample_defects cannot be negative.")
  }

  if (any(out$sample_defects > out$sample_size)) {
    stop("sample_defects cannot exceed sample_size.")
  }

  out$observed_rate <- out$sample_defects / out$sample_size

  out
}
