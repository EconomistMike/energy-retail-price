# R/functions_prd.R

`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------
# Config helpers
# -----------------------------
tz_loc   <- "Australia/Sydney"
data_dir <- "data_prd"

dir_create_silent <- function(path) {
  try({ fs::dir_create(path, recurse = TRUE) }, silent = TRUE)
}

get_run_date <- function() {
  format(lubridate::with_tz(Sys.time(), tz_loc), "%Y-%m-%d")
}

state_file_path <- function() file.path(data_dir, "state_last_run.txt")

read_last_run <- function() {
  sf <- state_file_path()
  if (fs::file_exists(sf)) readr::read_lines(sf, n_max = 1) else NA_character_
}

write_last_run <- function(when = Sys.time()) {
  dir_create_silent(data_dir)
  readr::write_lines(format(lubridate::with_tz(when, tz_loc), "%Y-%m-%d %H:%M:%S"), state_file_path())
  invisible(TRUE)
}

# -----------------------------
# Retailer discovery (no pdftools)
# -----------------------------
get_base_uris <- function() {
  dir_create_silent(data_dir)
  csv_path <- file.path(data_dir, "base_uris.csv")
  
  ensure_trailing_slash <- function(x) sub("/*$", "/", x)
  
  if (file.exists(csv_path)) {
    df <- readr::read_csv(csv_path, show_col_types = FALSE)
    
    # normalise names -> snake case
    names(df) <- tolower(gsub("[^a-z0-9]+", "_", names(df)))
    
    # case A: curated schema: brand_slug, base_uri
    if (all(c("brand_slug","base_uri") %in% names(df))) {
      out <- df %>%
        dplyr::transmute(
          brand_slug = tolower(.data$brand_slug),
          base_uri   = dplyr::if_else(
            grepl("/cds-au/v1/?$", .data$base_uri),
            .data$base_uri,
            paste0(ensure_trailing_slash(.data$base_uri), "cds-au/v1")
          )
        ) %>%
        dplyr::distinct()
      
      return(out)
    }
    
    # case B: AER export schema: Retailer Base URI + CDR Brand
    # (after normalisation: retailer_base_uri, cdr_brand)
    if (all(c("retailer_base_uri","cdr_brand") %in% names(df))) {
      out <- df %>%
        dplyr::transmute(
          brand_slug = tolower(.data$cdr_brand),
          base_uri   = paste0(ensure_trailing_slash(.data$retailer_base_uri), "cds-au/v1")
        ) %>%
        dplyr::distinct()
      
      return(out)
    }
    
    # helpful error if columns aren't recognised
    stop(
      "base_uris.csv must have either:\n",
      "  (a) columns: brand_slug, base_uri  OR\n",
      "  (b) columns: Retailer Base URI, CDR Brand (from the AER list)\n",
      "Found columns: ", paste(names(df), collapse = ", ")
    )
  }
  
  # fallback starter list (works immediately if CSV not present)
  tibble::tibble(
    brand_slug = c("agl","origin","energyaustralia","lumo","powershop"),
    base_uri   = paste0("https://cdr.energymadeeasy.gov.au/",
                        c("agl","origin","energyaustralia","lumo","powershop"),
                        "/cds-au/v1")
  )
}

# -----------------------------
# HTTP helpers
# -----------------------------
make_headers <- function(req, xv, xminv = xv) {
  req |>
    httr2::req_headers(
      "Accept" = "application/json",
      "x-v" = as.character(xv),
      "x-min-v" = as.character(xminv),
      "x-fapi-interaction-id" = uuid::UUIDgenerate()
    )
}

with_retry <- function(req) {
  req |>
    httr2::req_retry(
      max_tries = 5,
      backoff = ~ runif(1, 0.8, 1.6) * (2 ^ ..attempt),
      is_transient = function(resp) {
        s <- httr2::resp_status(resp)
        s >= 500 || s == 429
      }
    )
}

# -----------------------------
# Fetchers
# -----------------------------
get_generic_plans <- function(base_uri,
                              type = "ALL",
                              fuelType = NULL,
                              brand = NULL,
                              effective = "CURRENT",
                              updated_since = NULL,
                              page_size = 1000) {
  stopifnot(page_size <= 1000)
  page <- 1L; acc <- list()
  repeat {
    req <- httr2::request(paste0(base_uri, "/energy/plans")) |>
      httr2::req_url_query(
        type = type,
        `page-size` = page_size,
        page = page,
        fuelType = fuelType,
        brand = brand,
        effective = effective,
        `updated-since` = updated_since
      ) |>
      make_headers(xv = 1) |>
      with_retry()
    
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) >= 300) {
      warning("Non-success status: ", httr2::resp_status(resp), " @ ", base_uri)
      break
    }
    pl_raw <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    plans  <- pl_raw$data$plans %||% list()
    acc    <- c(acc, plans)
    if (length(plans) < page_size) break
    page <- page + 1L
  }
  
  tibble::tibble(
    planId      = purrr::map_chr(acc, "planId", .default = NA_character_),
    brand       = purrr::map_chr(acc, "brand", .default = NA_character_),
    brandName   = purrr::map_chr(acc, "brandName", .default = NA_character_),
    displayName = purrr::map_chr(acc, "displayName", .default = NA_character_),
    type        = purrr::map_chr(acc, "type", .default = NA_character_),
    fuelType    = purrr::map_chr(acc, "fuelType", .default = NA_character_),
    lastUpdated = purrr::map_chr(acc, "lastUpdated", .default = NA_character_),
    geography   = purrr::map(acc, "geography")
  )
}

get_generic_plan_detail <- function(base_uri, plan_id) {
  req <- httr2::request(paste0(base_uri, "/energy/plans/", plan_id)) |>
    make_headers(xv = 3) |>
    with_retry()
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 300)
    stop("Non-success for planId ", plan_id, " @ ", base_uri, " (",
         httr2::resp_status(resp), ")")
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

# -----------------------------
# Geography helpers
# -----------------------------
expand_postcode_ranges <- function(x) {
  x <- x %||% character()
  out <- list()
  for (tok in x) {
    if (stringr::str_detect(tok, "^[0-9]{4}-[0-9]{4}$")) {
      rng <- as.integer(stringr::str_split(tok, "-", simplify = TRUE))
      out[[length(out) + 1L]] <- seq.int(rng[1], rng[2])
    } else if (stringr::str_detect(tok, "^[0-9]{4}$")) {
      out[[length(out) + 1L]] <- as.integer(tok)
    }
  }
  if (length(out)) unique(unlist(out, use.names = FALSE)) else integer(0)
}

tidy_geography_from_plans <- function(plans_tbl) {
  plans_tbl %>%
    dplyr::mutate(geo = geography) %>%
    dplyr::select(planId, brandName, displayName, fuelType, type, geo) %>%
    dplyr::mutate(
      distributors      = purrr::map(geo, ~ .x$distributors %||% character()),
      included_tokens   = purrr::map(geo, ~ .x$includedPostcodes %||% character()),
      excluded_tokens   = purrr::map(geo, ~ .x$excludedPostcodes %||% character()),
      included_pc       = purrr::map(included_tokens, expand_postcode_ranges),
      excluded_pc       = purrr::map(excluded_tokens, expand_postcode_ranges)
    ) %>%
    dplyr::select(-geo) %>%
    tidyr::unnest_longer(distributors, values_to = "distributor")
}

# -----------------------------
# Tariff flatteners
# -----------------------------
.tidy_supply <- function(tp) {
  single_df <- if (!is.null(tp$dailySupplyCharge)) {
    tibble::tibble(
      period_display = tp$displayName %||% NA_character_,
      start_date     = tp$startDate   %||% NA_character_,
      end_date       = tp$endDate     %||% NA_character_,
      daily_type     = tp$dailySupplyChargeType %||% "SINGLE",
      band_idx       = NA_integer_,
      measure_unit   = "DAYS",
      unit_price     = tp$dailySupplyCharge %||% NA_character_
    )
  } else tibble::tibble(
    period_display = character(), start_date = character(), end_date = character(),
    daily_type = character(), band_idx = integer(), measure_unit = character(),
    unit_price = character()
  )
  
  banded <- tp$bandedDailySupplyCharges %||% list()
  banded_df <- if (length(banded)) {
    purrr::imap_dfr(banded, \(b,i) {
      tibble::tibble(
        period_display = tp$displayName %||% NA_character_,
        start_date     = tp$startDate   %||% NA_character_,
        end_date       = tp$endDate     %||% NA_character_,
        daily_type     = tp$dailySupplyChargeType %||% "BAND",
        band_idx       = as.integer(i),
        measure_unit   = b$measureUnit %||% NA_character_,
        unit_price     = b$unitPrice   %||% NA_character_,
        volume         = as.numeric(b$volume %||% NA_real_)
      )
    })
  } else tibble::tibble(
    period_display = character(), start_date = character(), end_date = character(),
    daily_type = character(), band_idx = integer(), measure_unit = character(),
    unit_price = character(), volume = numeric()
  )
  
  dplyr::bind_rows(single_df, banded_df)
}

.tidy_single_rate <- function(tp) {
  sr <- tp$singleRate; if (is.null(sr)) return(tibble::tibble())
  rates <- sr$rates %||% list(); if (!length(rates)) return(tibble::tibble())
  purrr::map_dfr(rates, function(r) {
    tibble::tibble(
      period_display     = tp$displayName %||% NA_character_,
      start_date         = tp$startDate   %||% NA_character_,
      end_date           = tp$endDate     %||% NA_character_,
      rate_block         = "SINGLE_RATE",
      measure_unit       = r$measureUnit  %||% NA_character_,
      unit_price         = r$unitPrice    %||% NA_character_,
      volume_kwh         = as.numeric(r$volume %||% NA_real_),
      general_unit_price = sr$generalUnitPrice %||% NA_character_
    )
  })
}

.tidy_time_of_use <- function(tp) {
  tous <- tp$timeOfUseRates %||% list(); if (!length(tous)) return(tibble::tibble())
  purrr::map_dfr(tous, function(tou) {
    rates   <- tou$rates     %||% list()
    windows <- tou$timeOfUse %||% list()
    if (!length(rates) || !length(windows)) return(tibble::tibble())
    
    tidyr::expand_grid(r = rates, w = windows) |>
      dplyr::mutate(
        period_display = tp$displayName %||% NA_character_,
        start_date     = tp$startDate   %||% NA_character_,
        end_date       = tp$endDate     %||% NA_character_,
        rate_block     = "TOU",
        usage_type     = tou$type %||% NA_character_,
        measure_unit   = purrr::map_chr(r, ~ .x$measureUnit %||% NA_character_),
        unit_price     = purrr::map_chr(r, ~ .x$unitPrice   %||% NA_character_),
        volume_kwh     = purrr::map_dbl(r, ~ as.numeric(.x$volume %||% NA_real_)),
        days           = purrr::map(w, ~ (.x$days %||% character())),
        start_time     = purrr::map_chr(w, ~ .x$startTime   %||% NA_character_),
        end_time       = purrr::map_chr(w, ~ .x$endTime     %||% NA_character_)
      ) |>
      dplyr::select(-r, -w)
  })
}

.tidy_demand <- function(tp) {
  dc <- tp$demandCharges %||% list(); if (!length(dc)) return(tibble::tibble())
  purrr::map_dfr(dc, function(x) {
    tibble::tibble(
      period_display   = tp$displayName %||% NA_character_,
      start_date       = tp$startDate   %||% NA_character_,
      end_date         = tp$endDate     %||% NA_character_,
      rate_block       = "DEMAND",
      display_name     = x$displayName  %||% NA_character_,
      description      = x$description  %||% NA_character_,
      amount           = x$amount       %||% NA_character_,
      measure_unit     = x$measureUnit  %||% NA_character_,
      measurement_per  = x$measurementPeriod %||% NA_character_,
      charge_period    = x$chargePeriod %||% NA_character_,
      days             = list(x$days %||% character()),
      start_time       = x$startTime    %||% NA_character_,
      end_time         = x$endTime      %||% NA_character_,
      min_demand_kw    = x$minDemand    %||% NA_character_,
      max_demand_kw    = x$maxDemand    %||% NA_character_
    )
  })
}

tidy_tariffs <- function(contract, planId, contract_type) {
  tps <- contract$tariffPeriod %||% list()
  if (!length(tps)) return(list(supply=tibble::tibble(), single=tibble::tibble(),
                                tou=tibble::tibble(), demand=tibble::tibble()))
  list(
    supply = purrr::map_dfr(tps, .tidy_supply)      |> dplyr::mutate(planId=planId, contract_type=contract_type),
    single = purrr::map_dfr(tps, .tidy_single_rate) |> dplyr::mutate(planId=planId, contract_type=contract_type),
    tou    = purrr::map_dfr(tps, .tidy_time_of_use) |> dplyr::mutate(planId=planId, contract_type=contract_type),
    demand = purrr::map_dfr(tps, .tidy_demand)      |> dplyr::mutate(planId=planId, contract_type=contract_type)
  )
}

# -----------------------------
# Other flatteners (fees, discounts, incentives, eligibility, FIT, green power, metering, payment options)
# -----------------------------

flatten_fees <- function(contract, planId, contract_type) {
  items <- contract$fees %||% list()
  if (!length(items)) return(tibble::tibble())
  purrr::map_dfr(items, function(x) {
    tibble::tibble(
      planId = planId, contract_type = contract_type,
      type = x$type %||% NA_character_,
      term = x$term %||% NA_character_,
      amount = x$amount %||% NA_character_,
      rate   = x$rate %||% NA_character_,
      description = x$description %||% NA_character_
    )
  })
}

flatten_discounts <- function(contract, planId, contract_type) {
  items <- contract$discounts %||% list()
  if (!length(items)) return(tibble::tibble())
  purrr::map_dfr(items, function(x) {
    tibble::tibble(
      planId = planId, contract_type = contract_type,
      displayName = x$displayName %||% NA_character_,
      type        = x$type %||% NA_character_,        # CONDITIONAL/GUARANTEED/OTHER
      method      = x$methodUType %||% NA_character_, # percentOfBill/percentOfUse/fixedAmount/percentOverThreshold
      category    = x$category %||% NA_character_,
      description = x$description %||% NA_character_,
      # normalise nested variants:
      rate               = (x$percentOfBill$rate %||% x$percentOfUse$rate %||% x$percentOverThreshold$rate %||% NA_character_),
      usageAmount        = (x$percentOverThreshold$usageAmount %||% NA_character_),
      fixedAmount_amount = (x$fixedAmount$amount %||% NA_character_),
      startDate          = x$startDate %||% NA_character_,
      endDate            = x$endDate %||% NA_character_
    )
  })
}

flatten_incentives <- function(contract, planId, contract_type) {
  items <- contract$incentives %||% list()
  if (!length(items)) return(tibble::tibble())
  purrr::map_dfr(items, function(x) {
    tibble::tibble(
      planId = planId, contract_type = contract_type,
      displayName = x$displayName %||% NA_character_,
      category    = x$category %||% NA_character_,
      description = x$description %||% NA_character_,
      eligibility = x$eligibility %||% NA_character_
    )
  })
}

flatten_eligibility <- function(contract, planId, contract_type) {
  items <- contract$eligibility %||% list()
  if (!length(items)) return(tibble::tibble())
  purrr::map_dfr(items, function(x) {
    tibble::tibble(
      planId = planId, contract_type = contract_type,
      type = x$type %||% NA_character_,
      information = x$information %||% NA_character_,
      description = x$description %||% NA_character_
    )
  })
}

flatten_green_power <- function(contract, planId, contract_type) {
  items <- contract$greenPowerCharges %||% list()
  if (!length(items)) return(tibble::tibble())
  purrr::map_dfr(items, function(gp) {
    tiers <- gp$tiers %||% list()
    if (!length(tiers)) return(tibble::tibble())
    purrr::map_dfr(tiers, function(t) {
      tibble::tibble(
        planId = planId, contract_type = contract_type,
        displayName = gp$displayName %||% NA_character_,
        scheme      = gp$scheme %||% NA_character_,      # GREENPOWER/OTHER
        type        = gp$type %||% NA_character_,        # FIXED_PER_DAY/PERCENT_OF_USE/etc.
        percentGreen = t$percentGreen %||% NA_character_,
        amount       = t$amount %||% NA_character_,
        rate         = t$rate %||% NA_character_
      )
    })
  })
}

flatten_solar_fit <- function(contract, planId, contract_type) {
  items <- contract$solarFeedInTariff %||% list()
  if (!length(items)) return(list(rates=tibble::tibble(), windows=tibble::tibble()))
  rates_list <- list(); windows_list <- list()
  
  for (sf in items) {
    # singleTariff
    if (!is.null(sf$singleTariff)) {
      rs <- sf$singleTariff$rates %||% list()
      for (r in rs) {
        rates_list[[length(rates_list)+1]] <- tibble::tibble(
          planId = planId, contract_type = contract_type,
          tariffUType = "singleTariff",
          displayName = sf$displayName %||% NA_character_,
          scheme      = sf$scheme %||% NA_character_,     # PREMIUM/CURRENT/VARIABLE/OTHER
          payerType   = sf$payerType %||% NA_character_,  # GOVERNMENT/RETAILER
          measure_unit = r$measureUnit %||% NA_character_,
          unit_price   = r$unitPrice %||% NA_character_,
          volume       = as.numeric(r$volume %||% NA_real_)
        )
      }
    }
    # timeVaryingTariffs
    tv <- sf$timeVaryingTariffs %||% list()
    if (length(tv)) {
      for (tvt in tv) {
        rs <- tvt$rates %||% list()
        wv <- tvt$timeVariations %||% list()
        for (r in rs) {
          rates_list[[length(rates_list)+1]] <- tibble::tibble(
            planId = planId, contract_type = contract_type,
            tariffUType = "timeVaryingTariffs",
            displayName = tvt$displayName %||% NA_character_,
            scheme      = sf$scheme %||% NA_character_,
            payerType   = sf$payerType %||% NA_character_,
            usage_type  = tvt$type %||% NA_character_,     # PEAK/OFF_PEAK/SHOULDER
            measure_unit = r$measureUnit %||% NA_character_,
            unit_price   = r$unitPrice %||% NA_character_,
            volume       = as.numeric(r$volume %||% NA_real_)
          )
        }
        for (w in wv) {
          windows_list[[length(windows_list)+1]] <- tibble::tibble(
            planId = planId, contract_type = contract_type,
            displayName = tvt$displayName %||% NA_character_,
            usage_type  = tvt$type %||% NA_character_,
            days        = list(w$days %||% character()),
            start_time  = w$startTime %||% NA_character_,
            end_time    = w$endTime %||% NA_character_
          )
        }
      }
    }
  }
  list(
    rates   = dplyr::bind_rows(rates_list),
    windows = dplyr::bind_rows(windows_list)
  )
}

flatten_metering_charges <- function(plan_detail, planId) {
  mc <- plan_detail$meteringCharges %||% list()
  if (!length(mc)) return(tibble::tibble())
  purrr::map_dfr(mc, function(x) {
    tibble::tibble(
      planId = planId,
      displayName   = x$displayName %||% NA_character_,
      description   = x$description %||% NA_character_,
      minimumValue  = x$minimumValue %||% NA_character_,
      maximumValue  = x$maximumValue %||% NA_character_,
      period        = x$period %||% NA_character_
    )
  })
}

flatten_payment_options <- function(contract, planId, contract_type) {
  opts <- contract$paymentOption %||% list()
  if (!length(opts)) return(tibble::tibble())
  tibble::tibble(planId = planId, contract_type = contract_type, payment_option = unlist(opts))
}

# -----------------------------
# Contracts + plan-level detail
# -----------------------------
normalise_contracts <- function(core) {
  out <- list()
  pid <- core$planId
  if (!is.null(core$electricityContract)) {
    c <- core$electricityContract
    out[[length(out)+1]] <- tibble::tibble(
      planId = pid, contract_type = "ELECTRICITY",
      isFixed = c$isFixed %||% NA,
      pricingModel = c$pricingModel %||% NA_character_,
      timeZone = c$timeZone %||% NA_character_,
      variation = c$variation %||% NA_character_,
      onExpiryDescription = c$onExpiryDescription %||% NA_character_,
      intrinsicGreenPower = (c$intrinsicGreenPower$greenPercentage %||% NA_character_)
    )
  }
  if (!is.null(core$gasContract)) {
    c <- core$gasContract
    out[[length(out)+1]] <- tibble::tibble(
      planId = pid, contract_type = "GAS",
      isFixed = c$isFixed %||% NA,
      pricingModel = c$pricingModel %||% NA_character_,
      timeZone = c$timeZone %||% NA_character_,
      variation = c$variation %||% NA_character_,
      onExpiryDescription = c$onExpiryDescription %||% NA_character_,
      intrinsicGreenPower = NA_character_  # not applicable to gas
    )
  }
  dplyr::bind_rows(out)
}

normalise_plan_detail_core <- function(core) {
  tibble::tibble(
    planId = core$planId,
    effectiveFrom = core$effectiveFrom %||% NA_character_,
    effectiveTo   = core$effectiveTo %||% NA_character_,
    applicationUri = core$applicationUri %||% NA_character_,
    additional_overviewUri   = core$additionalInformation$overviewUri %||% NA_character_,
    additional_termsUri      = core$additionalInformation$termsUri %||% NA_character_,
    additional_pricingUri    = core$additionalInformation$pricingUri %||% NA_character_,
    additional_eligibilityUri= core$additionalInformation$eligibilityUri %||% NA_character_,
    additional_bundleUri     = core$additionalInformation$bundleUri %||% NA_character_
  )
}

# -----------------------------
# Write Parquet (stringify list-cols first)
# -----------------------------
stringify_list_cols <- function(df) {
  df %>%
    dplyr::mutate(
      dplyr::across(
        where(is.list),
        ~ purrr::map_chr(.x, ~ jsonlite::toJSON(.x %||% list(), auto_unbox = TRUE))
      )
    )
}

write_parquet_tbl <- function(df, name, run_date) {
  dest <- file.path(data_dir, name, paste0("run_date=", run_date))
  dir_create_silent(dest)
  # Always stringify before write to avoid Arrow list-col issues
  df2 <- stringify_list_cols(df)
  arrow::write_parquet(df2, file.path(dest, paste0(name, ".parquet")))
  invisible(dest)
}
