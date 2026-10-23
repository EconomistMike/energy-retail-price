source(testthat::test_path("..", "..", "checks", "tariff-quality.R"))

invented_tariffs <- function() {
  common <- data.frame(planId = "invented-electricity", contract_type = "ELECTRICITY",
    start_date = "11-01", end_date = "03-31", stringsAsFactors = FALSE)
  supply <- cbind(common, daily_type = "SINGLE", band_idx = NA_integer_, measure_unit = "DAYS", unit_price = "1.25")
  single <- cbind(common[c(1, 1), ], measure_unit = "KWH", unit_price = c("0.20", "0.30"), volume_kwh = c(100, NA_real_))
  tou <- cbind(common, measure_unit = "KWH", unit_price = "-0.05", volume_kwh = NA_real_,
    usage_type = "OFF_PEAK", start_time = "22:00", end_time = "07:00")
  tou$days <- list(c("MON", "TUE"))
  demand <- cbind(common, measure_unit = "KW", amount = "0.75", measurement_per = "MONTH",
    charge_period = "DAY", start_time = "16:00", end_time = "20:00")
  demand$days <- list(c("MON", "FRI"))
  list(supply = supply, single = single, tou = tou, demand = demand)
}

expect_issue <- function(tables, family, field) {
  out <- prd_tariff_issues(tables)
  expect_true(any(out$table == family & out$field == field),
    info = paste("Expected diagnostic for", family, field))
  expect_error(assert_prd_tariffs(tables), class = "prd_tariff_check_error")
}
