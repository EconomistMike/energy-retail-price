test_that("supported production flatteners preserve raw tariff amounts and schema", {
  prd <- new.env(parent = globalenv())
  sys.source(testthat::test_path("..", "..", "R", "functions_prd.R"), envir = prd)
  period <- list(displayName = "Invented summer", startDate = "11-01", endDate = "03-31",
    dailySupplyChargeType = "SINGLE", dailySupplyCharge = "1.25",
    singleRate = list(period = "P1M", generalUnitPrice = "0.44", rates = list(
      list(measureUnit = "KWH", unitPrice = "0.20", volume = 100),
      list(measureUnit = "KWH", unitPrice = "0.30"))),
    timeOfUseRates = list(list(type = "OFF_PEAK", rates = list(
      list(measureUnit = "KWH", unitPrice = "-0.05")), timeOfUse = list(
        list(days = c("MON", "TUE"), startTime = "22:00", endTime = "07:00")))),
    demandCharges = list(list(amount = "0.75", measureUnit = "KW", measurementPeriod = "MONTH",
      chargePeriod = "DAY", startTime = "16:00", endTime = "20:00", days = c("MON", "FRI"))))
  # Match the production JSON parser: arrays remain lists of scalar tokens.
  payload <- jsonlite::fromJSON(jsonlite::toJSON(list(tariffPeriod = list(period)),
    auto_unbox = TRUE), simplifyVector = FALSE)
  tables <- prd$tidy_tariffs(payload, "invented-electricity", "ELECTRICITY")
  expect_identical(tables$tou$days[[1]], list("MON", "TUE"))
  expect_identical(tables$demand$days[[1]], list("MON", "FRI"))
  expect_true(assert_prd_tariffs(tables))
  expect_equal(tables$supply$unit_price, "1.25")
  expect_equal(tables$single$unit_price, c("0.20", "0.30"))
  expect_equal(as.numeric(tables$single$unit_price) * 100, c(20, 30))
  expect_equal(tables$single$general_unit_price, rep("0.44", 2))
  expect_equal(tables$single$volume_kwh, c(100, NA_real_))
  expect_equal(tables$tou$unit_price, "-0.05")
  expect_equal(tables$demand$amount, "0.75")
  expect_equal(tables$demand$measurement_per, "MONTH")
  expect_equal(tables$demand$charge_period, "DAY")
  period$singleRate$rates <- list(list(measureUnit = "MJ", unitPrice = "0.025", volume = 50))
  gas_period <- period[c("displayName", "startDate", "endDate", "singleRate")]
  gas <- prd$tidy_tariffs(list(tariffPeriod = list(gas_period)), "invented-gas", "GAS")
  expect_true(assert_prd_tariffs(gas))
  expect_equal(gas$single$unit_price, "0.025")
  expect_equal(gas$single$measure_unit, "MJ")
  expect_equal(gas$single$volume_kwh, 50)
})
