# Run the complete suite from the repository root in a clean R session.
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Install testthat in a compatible R library before running the checks.")
}
results <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
counts <- as.data.frame(results)
cat(sprintf("\nBlocks: %d; passed expectations: %d; failures: %d; errors: %d; warnings: %d; skipped: %d\n",
  nrow(counts), sum(counts$passed), sum(counts$failed), sum(counts$error), sum(counts$warning), sum(counts$skipped)))
