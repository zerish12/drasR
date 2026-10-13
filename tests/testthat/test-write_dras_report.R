test_that("write_dras_report writes CSV report files", {
  raw_data <- data.frame(
    lot_id = paste0("L", 1:6),
    supplier = c("A", "A", "A", "B", "B", "B"),
    sample_size = rep(100, 6),
    defects_found = c(1, 0, 1, 4, 5, 6)
  )

  prepared <- prepare_lot_data(
    raw_data,
    lot_id = "lot_id",
    sample_size = "sample_size",
    defects = "defects_found",
    supplier = "supplier"
  )

  monitored <- dras_monitor(
    prepared,
    baseline_sample_size = 132,
    acceptance_number = 3
  )

  output_dir <- tempfile("dras_report_")

  files <- write_dras_report(
    monitored,
    output_dir = output_dir
  )

  expect_true(all(file.exists(files)))
  expect_true(any(grepl("dras_lot_monitoring.csv", files)))
  expect_true(any(grepl("dras_overall_summary.csv", files)))
})
