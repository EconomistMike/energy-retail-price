test_that("invented tariff families pass without mutating inputs", {
  tables <- invented_tariffs()
  before <- serialize(tables, NULL)
  expect_equal(nrow(prd_tariff_issues(tables)), 0L)
  expect_true(assert_prd_tariffs(tables))
  expect_identical(serialize(tables, NULL), before)
  expect_named(prd_tariff_issues(tables), c("table", "row", "field", "message"))
})

test_that("empty families are explicit and accepted", {
  tables <- setNames(rep(list(data.frame()), 4), c("supply", "single", "tou", "demand"))
  expect_true(assert_prd_tariffs(tables))
  tables$single <- NULL
  expect_issue(tables, "_tables", "names")
})

test_that("table and column schemas fail closed", {
  x <- invented_tariffs(); x$single <- "not a table"
  expect_issue(x, "single", "schema")
  x <- invented_tariffs(); x$single$unit_price <- NULL
  expect_issue(x, "single", "unit_price")
  x <- invented_tariffs(); names(x)[2] <- "supply"
  expect_issue(x, "_tables", "names")
  x <- invented_tariffs(); names(x$single)[2] <- "planId"
  expect_issue(x, "single", "columns")
  x <- invented_tariffs(); x$single$planId <- list("one", "two")
  expect_issue(x, "single", "planId")
})

test_that("prices require complete finite numeric values", {
  for (bad in c("BAND", "$0.20", "0.20 per kWh", "1,200", "", "NA", "Inf", "NaN", "1e3")) {
    x <- invented_tariffs(); x$single$unit_price[1] <- bad
    expect_issue(x, "single", "unit_price")
  }
  for (bad in c(NA_real_, Inf, NaN)) {
    x <- invented_tariffs(); x$demand$amount <- bad
    expect_issue(x, "demand", "amount")
  }
  x <- invented_tariffs(); x$single$unit_price <- c(0, -0.05)
  expect_true(assert_prd_tariffs(x))
})

test_that("units stay attached to the contract and tariff family", {
  x <- invented_tariffs(); x$single$measure_unit[1] <- "MW"
  expect_issue(x, "single", "measure_unit")
  x <- invented_tariffs(); x$supply$measure_unit <- "KWH"
  expect_issue(x, "supply", "measure_unit")
  x <- invented_tariffs(); x$demand$measure_unit <- "DAYS"
  expect_issue(x, "demand", "measure_unit")
  x <- invented_tariffs(); x$single$contract_type <- "GAS"
  expect_issue(x, "single", "measure_unit")
  x$single$measure_unit <- "MJ"
  expect_true(assert_prd_tariffs(x))
})

test_that("missing identifiers and invalid contract names are diagnosed", {
  x <- invented_tariffs(); x$single$planId[1] <- NA_character_
  expect_issue(x, "single", "planId")
  x <- invented_tariffs(); x$tou$contract_type <- "unknown"
  expect_issue(x, "tou", "contract_type")
})

test_that("season bounds are real month-day values and may cross New Year", {
  x <- invented_tariffs()
  expect_true(assert_prd_tariffs(x))
  for (bad in c("02-30", "13-01", "1-01", "2025-01-01", NA_character_)) {
    x <- invented_tariffs(); x$single$start_date[1] <- bad
    expect_issue(x, "single", "start_date")
  }
  x <- invented_tariffs(); x$single$start_date <- "02-29"
  expect_true(assert_prd_tariffs(x))
})

test_that("volumes are numeric non-negative or explicitly unbounded", {
  for (bad in c(-1, Inf, NaN)) {
    x <- invented_tariffs(); x$single$volume_kwh[1] <- bad
    expect_issue(x, "single", "volume_kwh")
  }
  x <- invented_tariffs(); x$single$volume_kwh <- c("100", NA_character_)
  expect_issue(x, "single", "volume_kwh")
  x <- invented_tariffs(); x$single$volume_kwh <- c(0, NA_real_)
  expect_true(assert_prd_tariffs(x))
})

test_that("banded daily supply needs explicit positive band indices", {
  x <- invented_tariffs()
  x$supply <- x$supply[c(1, 1), ]
  x$supply$daily_type <- "BAND"; x$supply$band_idx <- 1:2
  x$supply$unit_price <- c("0.90", "1.10"); x$supply$volume <- c(20, NA_real_)
  expect_true(assert_prd_tariffs(x))
  x$supply$band_idx[1] <- NA_integer_
  expect_issue(x, "supply", "band_idx")
})

test_that("TOU clocks and days are syntactically complete without allocating intervals", {
  for (bad in c("25:00", "12:60", "7pm", NA_character_)) {
    x <- invented_tariffs(); x$tou$start_time <- bad
    expect_issue(x, "tou", "start_time")
  }
  for (bad in list(character(), "FUNDAY", NA_character_, 1)) {
    x <- invented_tariffs(); x$tou$days <- list(bad)
    expect_issue(x, "tou", "days")
  }
  x <- invented_tariffs(); x$tou$end_time <- "24:00"
  expect_true(assert_prd_tariffs(x))
  x$tou$start_time <- "22:00:00+10:00"; x$tou$end_time <- "07:00:00+10:00"
  expect_true(assert_prd_tariffs(x))
})

test_that("demand measurement and charge periods are validated independently", {
  x <- invented_tariffs(); x$demand$measurement_per <- "WEEK"
  expect_issue(x, "demand", "measurement_per")
  x <- invented_tariffs(); x$demand$charge_period <- NA_character_
  expect_issue(x, "demand", "charge_period")
  x <- invented_tariffs(); x$demand$charge_period <- "TARIFF_PERIOD"
  expect_true(assert_prd_tariffs(x))
})

test_that("duplicate flattened rows require an explicit component review", {
  x <- invented_tariffs(); x$tou <- x$tou[c(1, 1), ]
  expect_issue(x, "tou", "row")
})

test_that("expected contract coverage catches missing and unexpected contracts", {
  x <- invented_tariffs()
  expected <- data.frame(planId = "invented-electricity", contract_type = "ELECTRICITY")
  expect_true(assert_prd_tariffs(x, expected_contracts = expected))
  expected <- rbind(expected, data.frame(planId = "invented-gas", contract_type = "GAS"))
  out <- prd_tariff_issues(x, expected_contracts = expected)
  expect_true(any(out$table == "_contracts" & out$field == "coverage"))
  expect_error(assert_prd_tariffs(x, expected_contracts = expected), class = "prd_tariff_check_error")
  expected <- expected[2, , drop = FALSE]
  expect_true(any(prd_tariff_issues(x, expected_contracts = expected)$field == "coverage"))
})

test_that("amount and clock syntax consumes the complete string", {
  for (bad in c("0.20\n", "0.20\r\n")) {
    x <- invented_tariffs(); x$single$unit_price[1] <- bad
    expect_issue(x, "single", "unit_price")
  }
  for (field in c("start_time", "end_time")) {
    x <- invented_tariffs(); x$tou[[field]] <- "12:00\n"
    expect_issue(x, "tou", field)
  }
})

test_that("unexpected-only contracts and complex numerics fail with diagnostics", {
  x <- invented_tariffs()
  empty_expected <- data.frame(planId = character(), contract_type = character())
  expect_true(any(prd_tariff_issues(x, expected_contracts = empty_expected)$field == "coverage"))
  expect_error(assert_prd_tariffs(x, expected_contracts = empty_expected), class = "prd_tariff_check_error")
  x$single$unit_price <- c(1+1i, 2+0i)
  expect_issue(x, "single", "unit_price")
  x <- invented_tariffs(); x$single$volume_kwh <- c(1+1i, 2+0i)
  expect_issue(x, "single", "volume_kwh")
  x <- invented_tariffs(); x$supply$band_idx <- 1+1i
  expect_issue(x, "supply", "band_idx")
})


test_that("parser-shaped day lists reject malformed scalar tokens without coercion", {
  for (family in c("tou", "demand")) {
    x <- invented_tariffs(); x[[family]]$days <- list(list("MON", "TUE"))
    expect_true(assert_prd_tariffs(x))
    for (bad in list(list("FUNDAY"), list(NA_character_), list(1), list(TRUE),
        list(NULL), list(c("MON", "TUE")), list(list("MON")))) {
      x <- invented_tariffs(); x[[family]]$days <- list(bad)
      expect_issue(x, family, "days")
    }
  }
  x <- invented_tariffs(); x$tou$days <- list(list())
  expect_issue(x, "tou", "days")
  x <- invented_tariffs(); x$demand$days <- list(list())
  expect_true(assert_prd_tariffs(x))
})
