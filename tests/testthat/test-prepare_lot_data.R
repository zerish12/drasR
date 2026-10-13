test_that("prepare_lot_data prepares valid lot data", {
  raw_data <- data.frame(
    lot_id = paste0("L", 1:5),
    date = as.Date("2026-01-01") + 0:4,
    supplier = c("A", "A", "A", "B", "B"),
    lot_size = rep(1000, 5),
    sample_size = rep(100, 5),
    defects_found = c(1, 0, 2, 5, 4)
  )

  prepared <- prepare_lot_data(
    raw_data,
    lot_id = "lot_id",
    sample_size = "sample_size",
    defects = "defects_found",
    lot_size = "lot_size",
    date = "date",
    supplier = "supplier"
  )

  expect_true(is.data.frame(prepared))
  expect_true("observed_rate" %in% names(prepared))
  expect_equal(nrow(prepared), 5)
  expect_equal(prepared$sample_defects[1], 1)
})
