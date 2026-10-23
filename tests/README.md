# Synthetic checks

From the repository root, run the complete test suite:

```sh
nice -n 10 R --vanilla --slave --file=tests/run-tests.R
```

Dependencies for these checks are `testthat`, `tibble`, `purrr`, `dplyr`, `tidyr` and `jsonlite`, from an existing compatible R library. The runner does not restore or install packages. `--vanilla` prevents the project profile bootstrapping renv during verification.

The suite first specifies the opt-in checker in `checks/tariff-quality.R` using fabricated valid and invalid tables. It covers price parsing, supported units, identifiers, seasonal dates, quantities, band indices, clock/day syntax, demand periods, duplicate rows and optional contract coverage. An integration test round-trips fabricated JSON using the production parser settings (`simplifyVector = FALSE`), then calls the current supply, single-rate, TOU and demand flatteners with invented inputs and checks raw amount preservation and the emitted schema. A gas usage fixture keeps MJ distinct from KWH; the existing `volume_kwh` name does not change its unit.

All prices and volumes are fabricated. No live requests, collectors, targets, existing data or rendered outputs are used. The suite validates the new checker and the stated supported production examples, not the whole pipeline. The earlier banded-supply desired-behaviour regression remains a separate red artefact and the known defect remains disclosed in `checks/README.md`; no assertion has been redefined to call the bogus row correct. See that README for supported-policy boundaries and incomplete coverage.
