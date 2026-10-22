# Source definitions only: these public flatteners do not fetch plans or write data.
charges_source <- new.env(parent = globalenv())
sys.source(testthat::test_path("..", "..", "R", "functions_prd.R"), envir = charges_source)

test_that("fees keep fixed amounts and rates separate without converting raw strings", {
  contract <- list(fees = list(
    list(type = "DISCONNECTION", term = "FIXED", amount = "12.50",
      description = "Invented fixed fee"),
    list(type = "OTHER", term = "PERCENT_OF_BILL", rate = "0.025",
      description = "Invented percentage fee"),
    list(type = "CONNECTION", term = "FIXED", amount = "0.00")))
  out <- charges_source$flatten_fees(contract, "invented-fees", "ELECTRICITY")
  expect_equal(out, tibble::tibble(
    planId = rep("invented-fees", 3), contract_type = rep("ELECTRICITY", 3),
    type = c("DISCONNECTION", "OTHER", "CONNECTION"),
    term = c("FIXED", "PERCENT_OF_BILL", "FIXED"),
    amount = c("12.50", NA_character_, "0.00"),
    rate = c(NA_character_, "0.025", NA_character_),
    description = c("Invented fixed fee", "Invented percentage fee", NA_character_)))
})

test_that("all four discount methods retain their raw fields and item identity", {
  contract <- list(discounts = list(
    list(displayName = "Invented bill discount", type = "CONDITIONAL",
      methodUType = "percentOfBill", category = "PAY_ON_TIME",
      description = "Invented bill condition", percentOfBill = list(rate = "0.10"),
      startDate = "2026-01-01", endDate = "2026-12-31"),
    list(displayName = "Invented usage discount", type = "GUARANTEED",
      methodUType = "percentOfUse", category = "OTHER",
      percentOfUse = list(rate = "0.05")),
    list(displayName = "Invented fixed discount", type = "GUARANTEED",
      methodUType = "fixedAmount", category = "OTHER", fixedAmount = list(amount = "20.00")),
    list(displayName = "Invented threshold discount", type = "CONDITIONAL",
      methodUType = "percentOverThreshold", category = "OTHER",
      description = "Invented threshold condition",
      percentOverThreshold = list(rate = "0.15", usageAmount = "100"),
      startDate = "2026-07-01", endDate = "2027-06-30")))
  out <- charges_source$flatten_discounts(contract, "invented-discounts", "GAS")
  expect_equal(out, tibble::tibble(
    planId = rep("invented-discounts", 4), contract_type = rep("GAS", 4),
    displayName = c("Invented bill discount", "Invented usage discount",
      "Invented fixed discount", "Invented threshold discount"),
    type = c("CONDITIONAL", "GUARANTEED", "GUARANTEED", "CONDITIONAL"),
    method = c("percentOfBill", "percentOfUse", "fixedAmount", "percentOverThreshold"),
    category = c("PAY_ON_TIME", "OTHER", "OTHER", "OTHER"),
    description = c("Invented bill condition", NA_character_, NA_character_,
      "Invented threshold condition"),
    rate = c("0.10", "0.05", NA_character_, "0.15"),
    usageAmount = c(NA_character_, NA_character_, NA_character_, "100"),
    fixedAmount_amount = c(NA_character_, NA_character_, "20.00", NA_character_),
    startDate = c("2026-01-01", NA_character_, NA_character_, "2026-07-01"),
    endDate = c("2026-12-31", NA_character_, NA_character_, "2027-06-30")))
})

test_that("separate discounts sharing a label retain zero and formatted amounts", {
  contract <- list(discounts = list(
    list(displayName = "Invented shared discount", type = "GUARANTEED",
      methodUType = "fixedAmount", category = "OTHER", fixedAmount = list(amount = "0.00")),
    list(displayName = "Invented shared discount", type = "CONDITIONAL",
      methodUType = "fixedAmount", category = "PAY_ON_TIME", fixedAmount = list(amount = "05.00"))))
  out <- charges_source$flatten_discounts(contract, "invented-shared-discounts", "ELECTRICITY")
  expect_equal(out, tibble::tibble(
    planId = rep("invented-shared-discounts", 2), contract_type = rep("ELECTRICITY", 2),
    displayName = rep("Invented shared discount", 2), type = c("GUARANTEED", "CONDITIONAL"),
    method = rep("fixedAmount", 2), category = c("OTHER", "PAY_ON_TIME"),
    description = rep(NA_character_, 2), rate = rep(NA_character_, 2),
    usageAmount = rep(NA_character_, 2), fixedAmount_amount = c("0.00", "05.00"),
    startDate = rep(NA_character_, 2), endDate = rep(NA_character_, 2)))
})

test_that("absent, NULL and empty collections do not invent fees or discounts", {
  for (contract in list(list(), list(fees = NULL, discounts = NULL),
      list(fees = list(), discounts = list()))) {
    expect_identical(charges_source$flatten_fees(contract, "invented-empty", "GAS"),
      tibble::tibble())
    expect_identical(charges_source$flatten_discounts(contract, "invented-empty", "GAS"),
      tibble::tibble())
  }
})
