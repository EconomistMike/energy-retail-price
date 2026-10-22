# Synthetic checks

From the repository root, run the complete test suite:

```sh
nice -n 10 R --vanilla --slave --file=tests/run-tests.R
```

Dependencies for these checks are `testthat`, `tibble`, `purrr`, `dplyr` and `tidyr`, from an existing compatible R library. The runner does not restore or install packages. `--vanilla` prevents the project profile bootstrapping renv during verification.

The suite first specifies the opt-in checker in `checks/tariff-quality.R` using fabricated valid and invalid tables. It covers price parsing, supported units, identifiers, seasonal dates, quantities, band indices, clock/day syntax, demand periods, duplicate rows and optional contract coverage. An integration test calls the current supply, single-rate, TOU and demand flatteners with invented inputs and checks raw amount preservation and the emitted schema. A gas usage fixture keeps MJ distinct from KWH; the existing `volume_kwh` name does not change its unit.

All prices and volumes are fabricated. No live requests, collectors, targets, existing data or rendered outputs are used. The suite validates the new checker and the stated supported production examples, not the whole pipeline. The earlier banded-supply desired-behaviour regression remains a separate red artefact and the known defect remains disclosed in `checks/README.md`; no assertion has been redefined to call the bogus row correct. See that README for supported-policy boundaries and incomplete coverage.

`test-solar-fit.R` calls the public `flatten_solar_fit()` function with fabricated single and time-varying offers. It checks exact emitted tables: raw signed/zero prices without currency or GST conversion, KWH units, ordered band volumes (including zero and an unspecified final volume), scheme/payer labels, day lists, offset-bearing and overnight clock strings, and empty collections. Multiple bands and multiple windows remain separate tables, rather than multiplying each other. A mixed contract checks that single and timed offers sharing a display label both survive.

These tests do not validate a complete CDR payload, calculate export revenue, resolve daylight saving or establish that a window is correctly associated with its original parent offer. The current flattener drops parent identity/effective dates from the output and scheme/payer identity from windows. Two timed offers with the same display/usage labels can therefore have ambiguous window associations. Do not assume that joining rates and windows on their shared labels is safe. The opt-in tariff checker covers supply/usage/demand tables; it does not validate these solar feed-in tables.
