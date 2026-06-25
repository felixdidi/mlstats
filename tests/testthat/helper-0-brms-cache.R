# Remove cached brms model files that haven't been (re)written in over
# `days` days. The persistent cache under tools::R_user_dir("mlstats",
# "cache") (see test-bayes_mldesc.R / test-bayes_within_between_correlations.R)
# is never overwritten in place -- each distinct data/iter/chains hash gets
# its own file -- so without this it would grow forever as test fixtures,
# package versions, or options(mlstats.brms_iter/chains) change over time.
prune_old_brms_cache <- function(dir, days = 7) {
  files <- base::list.files(dir, full.names = TRUE)
  if (base::length(files) == 0) {
    return(base::invisible())
  }
  cutoff <- base::Sys.time() - days * 24 * 60 * 60
  old <- files[base::file.mtime(files) < cutoff]
  if (base::length(old) > 0) {
    base::unlink(old)
  }
  base::invisible()
}
