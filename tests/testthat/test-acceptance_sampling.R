test_that("prob_accept returns valid probability", {
  x <- prob_accept(n = 10, c = 1, p = 0.05)
  expect_true(is.numeric(x))
  expect_true(x >= 0 && x <= 1)
})

test_that("find_plan returns risk-valid plan", {
  plan <- find_plan(aql = 0.01, ltpd = 0.05, alpha = 0.05, beta = 0.10)
  expect_true(plan$producer_risk <= 0.05)
  expect_true(plan$consumer_risk <= 0.10)
})

test_that("DRAS produces one row per lot", {
  plan <- find_plan(aql = 0.01, ltpd = 0.05)
  p <- simulate_defect_process(n_lots = 20, scenario = "sudden_deterioration")
  lots <- simulate_lots(p)
  res <- dras_ewma(lots, n0 = plan$n, c0 = plan$c)
  expect_equal(nrow(res), 20)
})

test_that("pareto frontier returns data frame", {
  df <- data.frame(
    mean_avg_sample_number = c(100, 120, 150),
    mean_false_accepts = c(5, 3, 2)
  )

  pf <- pareto_frontier(df)
  expect_true(is.data.frame(pf))
  expect_true(nrow(pf) >= 1)
})
