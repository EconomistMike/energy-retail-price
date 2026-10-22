# Opt-in tariff quality checks

Source `checks/tariff-quality.R`, then call `prd_tariff_issues(tables)` to obtain diagnostics, or `assert_prd_tariffs(tables)` to stop with a `prd_tariff_check_error` containing `$issues`. `tables` must be a named list containing `supply`, `single`, `tou` and `demand` data frames, as returned by `tidy_tariffs()`. The checker reads and writes no files, changes no values and is not wired into the collector, targets graph or plots.

This is a conservative analytical contract for the current **flattened** schema, not a complete CDR JSON Schema validator or a bill calculator. Unsupported values require review; a diagnostic does not by itself establish that the upstream API payload violates the CDR standard.

Supported checks:

- Required columns, character identifiers and contract types, and complete finite amounts. Numeric amounts and full decimal strings are supported; partial parsing, currency/unit suffixes and scientific-notation strings are rejected. Zero and negative prices are allowed. Optional quota `general_unit_price` is checked separately and never combined with ordinary usage prices or converted for GST.
- Explicit unit policy: supply `DAYS`, `METER` or `MONTH`; usage KWH for ELECTRICITY and MJ for GAS; demand `KW`, `KVA`, `KVAR`, `KWH` or `KVARH`. These are supported analytical units, not an exhaustive list of valid upstream units. The check cannot establish whether a price's declared currency scale is truthful.
- Real month-day bounds, including leap day and seasons crossing New Year; numeric non-negative volumes or NA for unspecified/unbounded quantities; positive whole band indices for banded daily supply.
- Complete TOU day lists, supported clock syntax and demand application periods. Overnight windows and offset-bearing clocks are accepted. This is syntax checking only: no DST conversion, interval allocation, boundary inclusivity, reset-period or coverage-overlap validation is performed.
- Exact duplicate flattened rows, flagged for component/weighting review. Different upstream components may become indistinguishable after flattening; resolve that identity before dismissing a diagnostic.

If independently established contract coverage is available, pass a data frame with character `planId` and `contract_type` columns as `expected_contracts`. The check compares expected and observed pairs in both directions. Deriving expectations from the same already-filtered tariff rows would not establish completeness. Empty tariff families are accepted, so successful validation without independent expectations is not proof of complete collection.

Known production risks remain open: banded-supply partial field matching can emit a `BAND` price row; dual-fuel target collection can lose gas rows; state advancement is not dependent on successful writes; tariff reset/time-zone/component metadata is lost. The original failing desired-behaviour regression is retained in the energy-r-repos job artefacts and remains red. These new opt-in checks do not replace or weaken it. The check catches invalid price/index values when explicitly applied to that output, but does not repair the extractor.
