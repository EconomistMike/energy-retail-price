# Opt-in checks for the supported flattened tariff tables. No data is read,
# changed, converted or written here; callers decide whether to stop on issues.
prd_tariff_issues <- function(tables, expected_contracts = NULL) {
  issues <- data.frame(table = character(), row = integer(), field = character(), message = character())
  add <- function(table, row, field, message) {
    issues <<- rbind(issues, data.frame(table = table, row = as.integer(row), field = field, message = message))
  }
  families <- c("supply", "single", "tou", "demand")
  if (!is.list(tables) || is.data.frame(tables) || is.null(names(tables)) ||
      anyDuplicated(names(tables)) || !setequal(names(tables), families)) {
    add("_tables", NA_integer_, "names", "Supply exactly one named table for each supported family.")
    return(issues)
  }
  units <- list(supply = c("DAYS", "METER", "MONTH"), single = c("KWH", "MJ"),
    tou = c("KWH", "MJ"), demand = c("KW", "KVA", "KVAR", "KWH", "KVARH"))
  clock <- "(?:[01][0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9])?(?:Z|[+-](?:[01][0-9]|2[0-3]):[0-5][0-9])?"
  decimal <- "\\A[+-]?(?:[0-9]+(?:[.][0-9]+)?|[.][0-9]+)\\z"
  actual_contracts <- list()
  for (family in families) {
    df <- tables[[family]]
    if (!is.data.frame(df)) {
      add(family, NA_integer_, "schema", "Expected a data frame.")
      next
    }
    if (anyDuplicated(names(df)) || anyNA(names(df)) || any(names(df) == "")) {
      add(family, NA_integer_, "columns", "Column names must be present and unique.")
      next
    }
    # Empty families represent absent tariffs; this is not a completeness claim.
    if (!nrow(df)) next
    required <- c("planId", "contract_type", "start_date", "end_date", "measure_unit",
      if (family == "demand") c("amount", "measurement_per", "charge_period", "start_time", "end_time") else "unit_price",
      if (family %in% c("single", "tou")) "volume_kwh",
      if (family == "supply") c("daily_type", "band_idx"),
      if (family == "tou") c("days", "usage_type", "start_time", "end_time"))
    missing <- setdiff(required, names(df))
    if (length(missing)) {
      for (field in missing) add(family, NA_integer_, field, "Required flattened column is absent.")
      next
    }
    add_bad <- function(field, bad, message) {
      bad[is.na(bad)] <- TRUE
      for (row in which(bad)) add(family, row, field, message)
    }
    text_column <- function(field) {
      x <- df[[field]]
      ok <- is.character(x) && is.null(dim(x)) && length(x) == nrow(df)
      if (!ok) add(family, NA_integer_, field, "Expected a character vector.")
      ok
    }
    for (field in c("planId", "contract_type", "measure_unit")) {
      if (text_column(field)) add_bad(field, is.na(df[[field]]) | trimws(df[[field]]) == "", "A non-missing value is required.")
    }
    if (is.character(df$contract_type) && is.null(dim(df$contract_type))) {
      add_bad("contract_type", !df$contract_type %in% c("ELECTRICITY", "GAS"), "Unsupported contract type.")
    }
    if (is.character(df$measure_unit) && is.null(dim(df$measure_unit))) {
      add_bad("measure_unit", !df$measure_unit %in% units[[family]], "Unsupported unit for this tariff family.")
      if (family %in% c("single", "tou") && is.character(df$contract_type) && is.null(dim(df$contract_type))) {
        add_bad("measure_unit", (df$contract_type == "ELECTRICITY" & df$measure_unit != "KWH") |
          (df$contract_type == "GAS" & df$measure_unit != "MJ"), "Usage unit does not match the supported contract policy.")
      }
    }
    numeric_amount <- function(field, optional = FALSE) {
      x <- df[[field]]
      if (is.complex(x) || !is.null(dim(x)) || (!is.character(x) && !is.numeric(x))) {
        add(family, NA_integer_, field, "Expected numeric values or complete decimal strings.")
        return(invisible(NULL))
      }
      bad <- if (is.numeric(x)) !is.finite(x) else is.na(x) | !grepl(decimal, x, perl = TRUE) |
        !is.finite(suppressWarnings(as.numeric(x)))
      if (optional) bad[is.na(x) & !is.nan(x)] <- FALSE
      add_bad(field, bad, "Expected a finite amount, without currency/unit text or partial parsing.")
    }
    numeric_amount(if (family == "demand") "amount" else "unit_price")
    if ("general_unit_price" %in% names(df)) numeric_amount("general_unit_price", optional = TRUE)
    for (field in c("start_date", "end_date")) {
      if (text_column(field)) {
        x <- df[[field]]
        parsed <- as.Date(paste0("2000-", x), format = "%Y-%m-%d")
        add_bad(field, is.na(x) | !grepl("^[0-9]{2}-[0-9]{2}$", x) | is.na(parsed) |
          format(parsed, "%m-%d") != x, "Expected a real month-day bound; New Year crossing is allowed.")
      }
    }
    for (field in intersect(c("volume_kwh", "volume"), names(df))) {
      x <- df[[field]]
      if (!is.numeric(x) || is.complex(x) || !is.null(dim(x))) {
        add(family, NA_integer_, field, "Expected numeric volumes; NA denotes an unspecified/unbounded band.")
      } else {
        add_bad(field, is.nan(x) | (!is.na(x) & (!is.finite(x) | x < 0)), "Volume must be non-negative and finite, or NA.")
      }
    }
    if (family == "supply") {
      if (text_column("daily_type")) add_bad("daily_type", !df$daily_type %in% c("SINGLE", "BAND"), "Unsupported supply-charge type.")
      idx <- df$band_idx
      if (!is.numeric(idx) || is.complex(idx) || !is.null(dim(idx))) {
        add(family, NA_integer_, "band_idx", "Expected numeric band indices.")
      } else if (is.character(df$daily_type) && is.null(dim(df$daily_type))) {
        add_bad("band_idx", df$daily_type == "BAND" & (!is.finite(idx) | idx < 1 | idx != floor(idx)), "Banded supply requires positive whole-number indices.")
      }
    }
    if (family %in% c("tou", "demand")) {
      for (field in c("start_time", "end_time")) {
        if (text_column(field)) {
          pattern <- paste0("\\A(?:", clock, if (field == "end_time") "|24:00(?::00)?", ")\\z")
          add_bad(field, is.na(df[[field]]) | !grepl(pattern, df[[field]], perl = TRUE), "Expected a supported ISO-style clock value.")
        }
      }
      if ("days" %in% names(df)) {
        days <- df$days
        if (!is.list(days) || !is.null(dim(days)) || length(days) != nrow(df)) {
          add(family, NA_integer_, "days", "Expected a list of day-token vectors.")
        } else {
          bad <- vapply(days, function(x) !is.character(x) || anyNA(x) ||
            (family == "tou" && !length(x)) || !all(x %in% c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN", "PUBLIC_HOLIDAYS")), logical(1))
          add_bad("days", bad, "Expected supported day tokens; TOU windows require at least one day.")
        }
      }
    }
    if (family == "tou" && text_column("usage_type")) {
      add_bad("usage_type", !df$usage_type %in% c("PEAK", "OFF_PEAK", "SHOULDER", "SHOULDER1", "SHOULDER2"), "Unsupported TOU usage type.")
    }
    if (family == "demand") {
      for (field in c("measurement_per", "charge_period")) {
        if (text_column(field)) add_bad(field, !df[[field]] %in% c("DAY", "MONTH", "TARIFF_PERIOD"), "Unsupported demand application period.")
      }
    }
    duplicates <- tryCatch(duplicated(df), error = function(e) NULL)
    if (is.null(duplicates)) add(family, NA_integer_, "row", "Could not assess duplicate flattened rows.") else
      add_bad("row", duplicates, "Duplicate flattened rows need an explicit component/weighting review.")
    keys <- df[c("planId", "contract_type")]
    if (all(vapply(keys, is.character, logical(1))) && all(vapply(keys, function(x) is.null(dim(x)), logical(1)))) {
      actual_contracts[[family]] <- keys[complete.cases(keys), , drop = FALSE]
    }
  }
  if (!is.null(expected_contracts)) {
    keys <- c("planId", "contract_type")
    valid <- is.data.frame(expected_contracts) && !anyDuplicated(names(expected_contracts)) &&
      all(keys %in% names(expected_contracts))
    if (valid) valid <- all(vapply(expected_contracts[keys], function(x) is.character(x) && is.null(dim(x)), logical(1))) &&
      !anyNA(expected_contracts[keys]) && !any(vapply(expected_contracts[keys], function(x) any(trimws(x) == ""), logical(1)))
    if (!valid) {
      add("_contracts", NA_integer_, "schema", "Expected non-missing character planId/contract_type pairs.")
    } else {
      expected <- unique(expected_contracts[keys])
      actual <- if (length(actual_contracts)) unique(do.call(rbind, actual_contracts)) else expected[FALSE, , drop = FALSE]
      missing <- if (nrow(expected)) !duplicated(rbind(actual, expected))[nrow(actual) + seq_len(nrow(expected))] else logical()
      extra <- if (nrow(actual)) !duplicated(rbind(expected, actual))[nrow(expected) + seq_len(nrow(actual))] else logical()
      if (any(missing) || any(extra)) add("_contracts", NA_integer_, "coverage", "Observed and expected contract pairs differ; review completeness and joins.")
    }
  }
  rownames(issues) <- NULL
  issues
}

assert_prd_tariffs <- function(tables, expected_contracts = NULL) {
  issues <- prd_tariff_issues(tables, expected_contracts)
  if (nrow(issues)) {
    stop(structure(list(message = paste(nrow(issues), "tariff quality issue(s); inspect condition$issues."),
      call = NULL, issues = issues), class = c("prd_tariff_check_error", "error", "condition")))
  }
  invisible(TRUE)
}
