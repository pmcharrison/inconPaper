do_stats <- function(df, opt) {
  cor <- cor_mat(df, opt)
  only_obs <- c(FALSE, TRUE)
  mods <- sapply(only_obs, function(x) do_mods(df, opt, only_observed = x),
                 simplify = FALSE)
  names(mods) <- paste("only-observed=", only_obs, sep = "")
  list(cor = cor, mods = mods)
}

do_mods <- function(df, opt, only_observed) {
  mods <- fit_poly_mods(df, opt, only_observed)
  plot_poly(mods, opt, only_observed)
  mods <- mods[, setdiff(names(mods), c("mod", "marginal_effects"))]
  path <- paste0("output/composition-analysis-mods-only-observed=", only_observed, ".rds")
  saveRDS(mods, path)
  mods
}

cor_mat <- function(df, opt) {
  cor(df[, opt$features])
}
