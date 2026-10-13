test_that("run_dras_experiment returns valid summary", {
  res <- run_dras_experiment(n_reps = 5, scenario = "sudden_deterioration", seed = 123)
  expect_true(is.data.frame(res))
  expect_true(all(c("method", "scenario", "mean_false_accepts") %in% names(res)))
  expect_true(nrow(res) == 2)
})
