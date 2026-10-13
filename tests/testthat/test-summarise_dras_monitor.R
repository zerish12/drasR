test_that("summarise_dras_monitor summarises monitoring output", {
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

  summary <- summarise_dras_monitor(monitored)
  supplier_summary <- summarise_dras_monitor(monitored, by_supplier = TRUE)

  expect_true(is.data.frame(summary))
  expect_true(is.data.frame(supplier_summary))
  expect_true("warning_lots" %in% names(summary))
  expect_equal(summary$n_lots, 6)
  expect_equal(nrow(supplier_summary), 2)
})
