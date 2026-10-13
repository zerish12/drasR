#' Write a DRAS Audit Report
#'
#' Writes DRAS monitoring outputs and audit summaries to CSV files.
#' This function is intended for industrial and quality-assurance workflows
#' where lot-level monitoring decisions must be saved for review.
#'
#' @param monitored_data A data frame returned by `dras_monitor()`.
#' @param output_dir Directory where report files should be written.
#' @param include_supplier_summary Logical. If TRUE and supplier information
#'   is available, writes a supplier-level summary.
#'
#' @return Invisibly returns a character vector of written file paths.
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
#' temp_dir <- tempfile("dras_report_")
#' write_dras_report(monitored, output_dir = temp_dir)
write_dras_report <- function(monitored_data,
                              output_dir = "dras_report",
                              include_supplier_summary = TRUE) {

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

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  lot_file <- file.path(output_dir, "dras_lot_monitoring.csv")
  overall_file <- file.path(output_dir, "dras_overall_summary.csv")

  utils::write.csv(monitored_data, lot_file, row.names = FALSE)

  overall_summary <- summarise_dras_monitor(monitored_data)
  utils::write.csv(overall_summary, overall_file, row.names = FALSE)

  written_files <- c(lot_file, overall_file)

  if (include_supplier_summary && "supplier_id" %in% names(monitored_data)) {
    supplier_file <- file.path(output_dir, "dras_supplier_summary.csv")
    supplier_summary <- summarise_dras_monitor(monitored_data, by_supplier = TRUE)
    utils::write.csv(supplier_summary, supplier_file, row.names = FALSE)
    written_files <- c(written_files, supplier_file)
  }

  invisible(written_files)
}
