# Shared setup for the "method = 'bayes'" sections of test-mldesc.R and
# test-within_between_correlations.R.
#
# Both sections fit real brms models, which is by far the slowest part of the
# test suite. To keep that bearable:
#
# 1. Both sections use the *same* cache folder and the *same* fixture data
#    objects (defined once, here). mldesc(method = "bayes") calls
#    within_between_correlations(method = "bayes") internally with the
#    data/vars/folder it was given, and brms::brm(file = ...) caches purely
#    on a hash of that data plus the sampling settings (see
#    `.brms_iter()`/`.brms_chains()` in R/utils.R). So whichever section runs
#    first fits the within/between correlation models, and the other reuses
#    them from disk instead of refitting -- as long as the fixture data is
#    byte-identical.
# 2. The fixture set is kept deliberately small: at most 2 variables per
#    case. The matrix-construction loops in both functions have no
#    special-casing for p > 2 (they're plain double loops over `vars`), so a
#    2-variable case already exercises the diagonal, within-group, and
#    between-group branches; a 3rd variable would add iterations of the same
#    branches, not new code paths.
# 3. Sampling settings are far below the package defaults (iter = 5000,
#    chains = 4): these tests only check output structure/formatting, not
#    posterior precision, and the data-hash in the cache key means raising
#    the settings later for a test that *does* check posterior values
#    closely would trigger a refit rather than silently reusing these fits.
#
# skip_on_cran()/skip_if_not_installed("brms") are intentionally NOT called
# at this file's top level: helper-*.R files are sourced once for the whole
# test run, and a skip() thrown there is not scoped to a single test file the
# way it is inside test_that(). Since this fixture data is shared with
# non-bayes tests living in the same files, each individual "method =
# 'bayes'" test calls skip_if_no_bayes() (defined below) itself instead, so
# only bayes-specific tests are skipped when brms is unavailable or on CRAN.
skip_if_no_bayes <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("brms")
}

bayes_cache_folder <- file.path(tools::R_user_dir("mlstats", "cache"), "testthat", "bayes_shared")
dir.create(bayes_cache_folder, recursive = TRUE, showWarnings = FALSE)
prune_old_brms_cache(bayes_cache_folder, days = 7)

options(mlstats.brms_iter = 200, mlstats.brms_chains = 1)

set.seed(123)
bayes_fixture_basic <- data.frame(
  group = rep(c("A", "B", "C"), each = 10),
  x = c(rnorm(10, 10, 2), rnorm(10, 15, 2), rnorm(10, 20, 2)),
  y = c(rnorm(10, 5, 1), rnorm(10, 10, 1), rnorm(10, 15, 1))
)

bayes_fixture_unbalanced <- data.frame(
  group = c(rep("A", 5), rep("B", 45)),
  x = c(rnorm(5, 0, 1), rnorm(45, 10, 1)),
  y = c(rnorm(5, 0, 1), rnorm(45, 10, 1))
)

bayes_fixture_na <- bayes_fixture_basic
bayes_fixture_na$x[c(1, 5, 15)] <- NA
bayes_fixture_na$y[c(2, 10, 20)] <- NA

# A constant variable paired with a varying one: cheap (no brms fit at all,
# since both functions special-case zero variance before calling brm()), but
# exercises a comparison-matrix branch ("NA" for undefined correlations) that
# the original test suite never reached.
bayes_fixture_zero_variance <- data.frame(
  group = rep(c("A", "B", "C"), each = 10),
  constant = 5,
  x = c(rnorm(10, 10, 2), rnorm(10, 15, 2), rnorm(10, 20, 2))
)
