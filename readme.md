# AER Product Reference Data Pipeline

**Purpose**  
Automated R pipeline to download, flatten, and archive the **Australian Energy Regulator (AER) Product Reference Data (PRD)**.  
It standardises all retailer plan data — tariffs, fees, discounts, eligibility, and solar FITs — into tidy Parquet tables for analysis and visualisation.

---

## 🧩 Overview

**Tech stack:** R (`{targets}`, `{httr2}`, `{furrr}`, `{arrow}`, `{dplyr}`)  
**Data source:** [AER Product Reference Data API](https://www.aer.gov.au/energy-product-reference-data)  
**Outputs:** Parquet tables under `data_prd/`, partitioned by `run_date=YYYY-MM-DD`.

```

data_prd/
├── plans/               # Plan metadata
├── plan_geography/      # DNSP & postcode coverage
├── contracts/           # Contract-level details
├── tariff_* /           # Supply / single / TOU / demand rates
├── fees/, discounts/, … # Ancillary data
└── raw/                 # Raw JSONs by retailer

````

---

## ⚙️ How it works

1. **Fetch plans**
   - Retrieves all retailer plans via the AER PRD API.
   - Uses `updated_since` (from `state_last_run.txt`) to pull only new or changed plans.

2. **Fetch plan details**
   - For each plan, downloads full details: contracts, tariffs, fees, eligibility, solar FIT, etc.

3. **Flatten & store**
   - Writes outputs as Parquet files (partitioned by date) for fast querying.

4. **State tracking**
   - Records last successful run timestamp in `data_prd/state_last_run.txt`.

5. **Parallel execution**
   - Uses `{furrr}` to parallelise retailer and plan detail fetches.

---

## 🚀 Running the pipeline

Run manually in R:

```r
library(targets)
tar_make()
````

Or parallelised:

```r
library(targets)
tar_make_future()
```

The run will:

* Create today’s Parquet partitions in `data_prd/`
* Update `state_last_run.txt`
* Skip unchanged targets automatically (built-in `{targets}` caching)

---

## 🧰 Project structure

```
R/
├── packages.R        # Required libraries & targets settings
├── functions_prd.R   # API fetchers + flatteners
_targets.R            # Main pipeline definition
data_prd/             # Output data (gitignored)
logs/                 # Optional logging
```

---

## 🧹 Git hygiene

`.gitignore` excludes generated and transient files:

```
_targets/
data_prd/
renv/library/
logs/
*.parquet
*.json
```

---

## 📊 Analysis examples

* **Single-rate electricity spread (brand-level)**

  ```r
  tar_read(tariff_single_df) %>%
    left_join(tar_read(plans_core), by = "planId") %>%
    filter(fuelType == "ELECTRICITY") %>%
    mutate(unit_price_c = as.numeric(unit_price) * 100) %>%
    ggplot(aes(x = brandName, y = unit_price_c)) +
    geom_boxplot() + coord_flip() +
    labs(y = "c/kWh (ex GST)", x = "Retailer")
  ```

* **AGL boxplot by DNSP (single-rate, residential)**
  See `analysis/agl_boxplots.R` for filtered plots.

---

## 🕓 Scheduling (optional)

Scheduling isn’t set up yet — run manually for now.
When ready, add a cron entry or Task Scheduler job to run daily, e.g.:

**Linux/macOS (cron):**

```bash
15 3 * * * Rscript -e "targets::tar_make_future()" >> logs/prd.log 2>&1
```

**Windows Task Scheduler:**

* *Program/script:* `Rscript.exe`
* *Arguments:* `-e "targets::tar_make_future()"`
* *Start in:* path to the project folder

---

## 🧮 Analyst workflow

Typical joins:

* `plans_core` ↔ `tariff_*` — link plan metadata to rates
* `plans_core` ↔ `plan_geography` — add DNSP / postcode
* `eligibility_df` or `solar_fit_rates_df` — segment by customer eligibility or FIT

Query with **DuckDB**, **dplyr**, or BI tools.

---

## 🧑‍🤝‍🧑 Team notes

* PRD prices are **ex-GST** unless otherwise noted.
* `data_prd/` is not versioned — rebuildable from the pipeline.
* `tar_make()` is idempotent — re-running daily is safe.
* For new brands, just update `data_prd/base_uris.csv` and re-run.

---

**Maintainer:** *Energy Data Team*
**Contact:** *Add your Slack channel or maintainer email here.*

