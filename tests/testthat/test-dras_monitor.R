test_that("dras_monitor works on prepared real inspection data", {
  raw_data <- data.frame(
    lot_id = paste0("L", 1:6),
    sample_size = rep(100, 6),
    defects_found = c(1, 0, 1, 4, 5, 6)
  )

  prepared <- prepare_lot_data(
    raw_data,
    lot_id = "lot_id",
    sample_size = "sample_size",
    defects = "defects_found"
  )

  monitored <- dras_monitor(
    prepared,
    baseline_sample_size = 132,
    acceptance_number = 3
  )

  expect_true(is.data.frame(monitored))
  expect_true("ewma_rate" %in% names(monitored))
  expect_true("inspection_mode" %in% names(monitored))
  expect_true("recommended_next_sample_size" %in% names(monitored))
  expect_equal(nrow(monitored), 6)
})
