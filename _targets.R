# _targets.R
source("R/packages.R")
source("R/functions_prd.R")

list(
  # 0. Ensure output dir
  tar_target(out_dir, { dir_create_silent(data_dir); data_dir }, cue = tar_cue("always")),
  
  # 1. Dates / state
  tar_target(run_ts,  lubridate::with_tz(Sys.time(), tz_loc)),
  tar_target(run_date, format(run_ts, "%Y-%m-%d")),
  tar_target(last_run_raw, read_last_run()),
  tar_target(updated_since, {
    if (!is.na(last_run_raw))
      format(lubridate::with_tz(lubridate::ymd_hms(last_run_raw, tz = tz_loc), "UTC"),
             "%Y-%m-%dT%H:%M:%SZ")
    else NULL
  }),
  
  # 2. Retailers
  tar_target(retailers, get_base_uris()),
  
  # 3. Plans list per retailer (parallel map) + fallback when updated-since yields 0
  tar_target(
    plans_per_retailer,
    {
      future::plan(future::multisession, workers = max(2, parallel::detectCores() - 2))
      on.exit(future::plan(future::sequential), add = TRUE)
      furrr::future_map(retailers$base_uri, ~{
        tryCatch({
          out <- get_generic_plans(.x, updated_since = updated_since, page_size = 1000)
          if (!is.null(updated_since) && nrow(out) == 0) {
            out <- get_generic_plans(.x, updated_since = NULL, page_size = 1000)  # fallback
          }
          dplyr::mutate(out, base_uri = .x)
        }, error = function(e) {
          warning("Plans fetch failed for ", .x, ": ", conditionMessage(e))
          tibble::tibble()
        })
      })
    }
  ),
  
  tar_target(plans_tbl, dplyr::bind_rows(plans_per_retailer) |> dplyr::distinct(planId, .keep_all = TRUE)),
  tar_target(plans_core, {
    plans_tbl %>%
      dplyr::mutate(lastUpdated = lubridate::ymd_hms(lastUpdated, quiet = TRUE)) %>%
      dplyr::select(planId, brand, brandName, displayName, type, fuelType, lastUpdated, base_uri)
  }),
  tar_target(geo_tbl, tidy_geography_from_plans(plans_tbl)),
  
  # 4. Decide planIds to update — fallback to ALL if filtered set is empty
  tar_target(plan_ids, {
    if (!is.na(last_run_raw)) {
      since_dt <- lubridate::ymd_hms(last_run_raw, tz = tz_loc, quiet = TRUE)
      ids <- plans_core %>%
        dplyr::filter(is.na(since_dt) | lastUpdated >= since_dt) %>%
        dplyr::pull(planId)
      if (length(ids) == 0) ids <- plans_core$planId   # fallback to all
      ids
    } else plans_core$planId
  }),
  
  # 5. Fetch details (parallel), save raw JSON
  tar_target(details_list, {
    if (!length(plan_ids)) return(list())
    id_to_base <- plans_core %>% dplyr::select(planId, base_uri)
    future::plan(future::multisession, workers = 6)
    on.exit(future::plan(future::sequential), add = TRUE)
    furrr::future_map(plan_ids, function(pid) {
      base_uri <- id_to_base$base_uri[id_to_base$planId == pid][1]
      tryCatch({
        d <- get_generic_plan_detail(base_uri, pid)
        raw_dir <- file.path(data_dir, "raw", paste0("run_date=", run_date), basename(base_uri))
        dir_create_silent(raw_dir)
        jsonlite::write_json(d, file.path(raw_dir, paste0(pid, ".json")), auto_unbox = TRUE, pretty = TRUE)
        d
      }, error = function(e) {
        warning("Detail failed for ", pid, ": ", conditionMessage(e))
        NULL
      })
    }) |> purrr::discard(is.null)
  }),
  
  # 6. Flatten all detail into tidy tables
  tar_target(contracts_df,        purrr::map_dfr(details_list, function(d) normalise_contracts(d$data))),
  tar_target(plan_detail_core_df, purrr::map_dfr(details_list, function(d) normalise_plan_detail_core(d$data))),
  
  tar_target(tariffs_all, {
    purrr::map(details_list, function(d) {
      core <- d$data; pid <- core$planId
      out <- list()
      if (!is.null(core$electricityContract)) out <- append(out, tidy_tariffs(core$electricityContract, pid, "ELECTRICITY"))
      if (!is.null(core$gasContract))         out <- append(out, tidy_tariffs(core$gasContract,         pid, "GAS"))
      out
    })
  }),
  tar_target(tariff_supply_df, purrr::map_dfr(tariffs_all, "supply")),
  tar_target(tariff_single_df, purrr::map_dfr(tariffs_all, "single")),
  tar_target(tariff_tou_df,    purrr::map_dfr(tariffs_all, "tou")),
  tar_target(tariff_demand_df, purrr::map_dfr(tariffs_all, "demand")),
  
  tar_target(fees_df, {
    purrr::map_dfr(details_list, function(d) {
      core <- d$data; pid <- core$planId
      out <- list()
      if (!is.null(core$electricityContract)) out[[length(out)+1]] <- flatten_fees(core$electricityContract, pid, "ELECTRICITY")
      if (!is.null(core$gasContract))         out[[length(out)+1]] <- flatten_fees(core$gasContract,         pid, "GAS")
      dplyr::bind_rows(out)
    })
  }),
  tar_target(discounts_df, {
    purrr::map_dfr(details_list, function(d) {
      core <- d$data; pid <- core$planId
      out <- list()
      if (!is.null(core$electricityContract)) out[[length(out)+1]] <- flatten_discounts(core$electricityContract, pid, "ELECTRICITY")
      if (!is.null(core$gasContract))         out[[length(out)+1]] <- flatten_discounts(core$gasContract,         pid, "GAS")
      dplyr::bind_rows(out)
    })
  }),
  tar_target(incentives_df, {
    purrr::map_dfr(details_list, function(d) {
      core <- d$data; pid <- core$planId
      out <- list()
      if (!is.null(core$electricityContract)) out[[length(out)+1]] <- flatten_incentives(core$electricityContract, pid, "ELECTRICITY")
      if (!is.null(core$gasContract))         out[[length(out)+1]] <- flatten_incentives(core$gasContract,         pid, "GAS")
      dplyr::bind_rows(out)
    })
  }),
  tar_target(eligibility_df, {
    purrr::map_dfr(details_list, function(d) {
      core <- d$data; pid <- core$planId
      out <- list()
      if (!is.null(core$electricityContract)) out[[length(out)+1]] <- flatten_eligibility(core$electricityContract, pid, "ELECTRICITY")
      if (!is.null(core$gasContract))         out[[length(out)+1]] <- flatten_eligibility(core$gasContract,         pid, "GAS")
      dplyr::bind_rows(out)
    })
  }),
  tar_target(green_power_df, {
    purrr::map_dfr(details_list, function(d) {
      core <- d$data; pid <- core$planId
      out <- list()
      if (!is.null(core$electricityContract)) out[[length(out)+1]] <- flatten_green_power(core$electricityContract, pid, "ELECTRICITY")
      if (!is.null(core$gasContract))         out[[length(out)+1]] <- flatten_green_power(core$gasContract,         pid, "GAS")
      dplyr::bind_rows(out)
    })
  }),
  
  tar_target(solar_fit_list, {
    purrr::map(details_list, function(d) {
      core <- d$data; pid <- core$planId
      out <- list(rates = tibble::tibble(), windows = tibble::tibble())
      if (!is.null(core$electricityContract)) out <- purrr::list_merge(out, flatten_solar_fit(core$electricityContract, pid, "ELECTRICITY"))
      out
    })
  }),
  tar_target(solar_fit_rates_df,   purrr::map_dfr(solar_fit_list, "rates")),
  tar_target(solar_fit_windows_df, purrr::map_dfr(solar_fit_list, "windows")),
  
  tar_target(metering_charges_df, {
    purrr::map_dfr(details_list, function(d) flatten_metering_charges(d$data, d$data$planId))
  }),
  
  tar_target(payment_options_df, {
    purrr::map_dfr(details_list, function(d) {
      core <- d$data; pid <- core$planId
      out <- list()
      if (!is.null(core$electricityContract)) out[[length(out)+1]] <- flatten_payment_options(core$electricityContract, pid, "ELECTRICITY")
      if (!is.null(core$gasContract))         out[[length(out)+1]] <- flatten_payment_options(core$gasContract,         pid, "GAS")
      dplyr::bind_rows(out)
    })
  }),
  
  # 7. Write Parquet partitions (list-cols stringified inside writer)
  tar_target(w_plans,            write_parquet_tbl(plans_core,          "plans",            run_date)),
  tar_target(w_plan_geo,         write_parquet_tbl(geo_tbl,             "plan_geography",   run_date)),
  tar_target(w_plan_detail_core, write_parquet_tbl(plan_detail_core_df, "plan_detail_core", run_date)),
  tar_target(w_contracts,        write_parquet_tbl(contracts_df,        "contracts",        run_date)),
  tar_target(w_tar_supply,       write_parquet_tbl(tariff_supply_df,    "tariff_supply",    run_date)),
  tar_target(w_tar_single,       write_parquet_tbl(tariff_single_df,    "tariff_single",    run_date)),
  tar_target(w_tar_tou,          write_parquet_tbl(tariff_tou_df,       "tariff_tou",       run_date)),
  tar_target(w_tar_demand,       write_parquet_tbl(tariff_demand_df,    "tariff_demand",    run_date)),
  tar_target(w_fees,             write_parquet_tbl(fees_df,             "fees",             run_date)),
  tar_target(w_discounts,        write_parquet_tbl(discounts_df,        "discounts",        run_date)),
  tar_target(w_incentives,       write_parquet_tbl(incentives_df,       "incentives",       run_date)),
  tar_target(w_eligibility,      write_parquet_tbl(eligibility_df,      "eligibility",      run_date)),
  tar_target(w_green_power,      write_parquet_tbl(green_power_df,      "green_power",      run_date)),
  tar_target(w_solar_fit_rates,  write_parquet_tbl(solar_fit_rates_df,  "solar_fit_rates",  run_date)),
  tar_target(w_solar_fit_windows,write_parquet_tbl(solar_fit_windows_df,"solar_fit_windows",run_date)),
  tar_target(w_metering,         write_parquet_tbl(metering_charges_df, "metering_charges", run_date)),
  tar_target(w_payment_options,  write_parquet_tbl(payment_options_df,  "payment_options",  run_date)),
  
  # 8. Update state (only after successful writes)
  tar_target(update_state, { write_last_run(run_ts); TRUE })
)
