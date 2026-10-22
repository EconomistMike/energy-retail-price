# Exercise the public flattener only; sourcing defines functions but fetches no data.
fit_source <- new.env(parent = globalenv())
sys.source(testthat::test_path("..", "..", "R", "functions_prd.R"), envir = fit_source)

test_that("single feed-in offers retain separate raw prices, units and band volumes", {
  contract <- list(solarFeedInTariff = list(
    list(displayName = "Invented retailer offer", scheme = "CURRENT", payerType = "RETAILER",
      singleTariff = list(rates = list(
        list(measureUnit = "KWH", unitPrice = "0.075", volume = 120),
        list(measureUnit = "KWH", unitPrice = "0.030")))),
    list(displayName = "Invented government offer", scheme = "PREMIUM", payerType = "GOVERNMENT",
      singleTariff = list(rates = list(
        list(measureUnit = "KWH", unitPrice = "-0.025", volume = 0))))))

  out <- fit_source$flatten_solar_fit(contract, "invented-fit", "ELECTRICITY")
  expected <- tibble::tibble(
    planId = rep("invented-fit", 3), contract_type = rep("ELECTRICITY", 3),
    tariffUType = rep("singleTariff", 3),
    displayName = c("Invented retailer offer", "Invented retailer offer", "Invented government offer"),
    scheme = c("CURRENT", "CURRENT", "PREMIUM"),
    payerType = c("RETAILER", "RETAILER", "GOVERNMENT"),
    measure_unit = rep("KWH", 3), unit_price = c("0.075", "0.030", "-0.025"),
    volume = c(120, NA_real_, 0))
  expect_equal(out$rates, expected)
  expect_equal(nrow(out$windows), 0L)
})

test_that("time-varying feed-in bands and windows stay separate and retain their labels", {
  contract <- list(solarFeedInTariff = list(list(scheme = "VARIABLE", payerType = "RETAILER",
    timeVaryingTariffs = list(
      list(displayName = "Invented evening", type = "PEAK",
        rates = list(list(measureUnit = "KWH", unitPrice = "0.19", volume = 50),
          list(measureUnit = "KWH", unitPrice = "0.08")),
        timeVariations = list(
          list(days = c("MON", "WED"), startTime = "16:00:00+10:00", endTime = "20:00:00+10:00"),
          list(days = "FRI", startTime = "22:00", endTime = "07:00"))),
      list(displayName = "Invented weekend", type = "OFF_PEAK",
        rates = list(list(measureUnit = "KWH", unitPrice = "0")),
        timeVariations = list(list(days = c("SAT", "SUN"), startTime = "00:00", endTime = "24:00")))))))

  out <- fit_source$flatten_solar_fit(contract, "invented-timed-fit", "ELECTRICITY")
  expect_equal(out$rates, tibble::tibble(
    planId = rep("invented-timed-fit", 3), contract_type = rep("ELECTRICITY", 3),
    tariffUType = rep("timeVaryingTariffs", 3),
    displayName = c("Invented evening", "Invented evening", "Invented weekend"),
    scheme = rep("VARIABLE", 3), payerType = rep("RETAILER", 3),
    usage_type = c("PEAK", "PEAK", "OFF_PEAK"), measure_unit = rep("KWH", 3),
    unit_price = c("0.19", "0.08", "0"), volume = c(50, NA_real_, NA_real_)))
  expect_equal(out$windows, tibble::tibble(
    planId = rep("invented-timed-fit", 3), contract_type = rep("ELECTRICITY", 3),
    displayName = c("Invented evening", "Invented evening", "Invented weekend"),
    usage_type = c("PEAK", "PEAK", "OFF_PEAK"),
    days = list(c("MON", "WED"), "FRI", c("SAT", "SUN")),
    start_time = c("16:00:00+10:00", "22:00", "00:00"),
    end_time = c("20:00:00+10:00", "07:00", "24:00")))
})

test_that("a mixed contract keeps single and timed offers without conflating shared labels", {
  contract <- list(solarFeedInTariff = list(
    list(displayName = "Invented shared label", scheme = "PREMIUM", payerType = "GOVERNMENT",
      singleTariff = list(rates = list(list(measureUnit = "KWH", unitPrice = "0.13")))),
    list(scheme = "CURRENT", payerType = "RETAILER", timeVaryingTariffs = list(
      list(displayName = "Invented shared label", type = "SHOULDER",
        rates = list(list(measureUnit = "KWH", unitPrice = "0.04", volume = 25)),
        timeVariations = list(list(days = "TUE", startTime = "10:00", endTime = "14:00")))))))
  out <- fit_source$flatten_solar_fit(contract, "invented-mixed-fit", "ELECTRICITY")
  expect_equal(out$rates, tibble::tibble(
    planId = rep("invented-mixed-fit", 2), contract_type = rep("ELECTRICITY", 2),
    tariffUType = c("singleTariff", "timeVaryingTariffs"),
    displayName = rep("Invented shared label", 2), scheme = c("PREMIUM", "CURRENT"),
    payerType = c("GOVERNMENT", "RETAILER"), measure_unit = rep("KWH", 2),
    unit_price = c("0.13", "0.04"), volume = c(NA_real_, 25),
    usage_type = c(NA_character_, "SHOULDER")))
  expect_equal(out$windows, tibble::tibble(
    planId = "invented-mixed-fit", contract_type = "ELECTRICITY",
    displayName = "Invented shared label", usage_type = "SHOULDER", days = list("TUE"),
    start_time = "10:00", end_time = "14:00"))
})

test_that("absent or empty feed-in collections produce no invented rates or windows", {
  for (contract in list(list(), list(solarFeedInTariff = list()))) {
    out <- fit_source$flatten_solar_fit(contract, "invented-no-fit", "ELECTRICITY")
    expect_named(out, c("rates", "windows"))
    expect_s3_class(out$rates, "data.frame")
    expect_s3_class(out$windows, "data.frame")
    expect_equal(nrow(out$rates), 0L)
    expect_equal(nrow(out$windows), 0L)
  }
})
