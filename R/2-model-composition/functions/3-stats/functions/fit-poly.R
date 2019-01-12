fit_poly_mods <- function(df, opt, only_observed) {
  message("Fitting polynomial models...")
  res <- data.frame()
  counter <- 0L
  pb <- txtProgressBar(max = length(opt$corpora) * length(opt$degrees), style = 3)
  for (corpus in opt$corpora) {
    for (degree in opt$degrees) {
      if (!(only_observed &&
            (corpus == "popular" && degree > 2) ||
            (corpus == "jazz" && degree > 3))) {
        x <- fit_poly_mod(df, corpus, degree, opt, only_observed)
        mod <- x$mod
        marginal_effects <- x$marginal_effects
        perm_imp <- x$perm_imp
        data <- x$data
        res <- rbind(res,
                     data.frame(
                       corpus = corpus,
                       degree = degree,
                       mod = I(list(mod)),
                       data = I(list(data)),
                       marginal_effects = I(list(marginal_effects)),
                       adj_r2 = summary(mod)$adj.r.squared,
                       perm_imp = I(list(perm_imp)),
                       AIC = AIC(mod),
                       AICc = AICcmodavg::AICc(mod),
                       BIC = BIC(mod),
                       conf_int = I(list(confint(mod))),
                       stringsAsFactors = FALSE
                     ))
      }
      counter <- counter + 1L
      setTxtProgressBar(pb, counter)
    }
  }
  close(pb)
  res
}

fit_poly_mod <- function(df, corpus, degree, opt, only_observed) {
  # Optionally only model non-zero counts
  tmp <- if (only_observed) df[df[[paste0(corpus, "_observed")]], ] else df
  # Add 1 to every count so we can subsequently log it
  tmp[[paste0(corpus, "_count")]] <- 1L + tmp[[paste0(corpus, "_count")]]
  # We log-transform the outcome and z-scale it
  tmp$scale_log_count <- as.numeric(scale(log(tmp[[paste0(corpus, "_count")]])))
  # We z-scale the features as part of the linear model fit
  for (feat in opt$features)
    tmp[[feat]] <- as.numeric(scale(tmp[[feat]]))
  
  outcome <- "scale_log_count"
  # tmp <- tmp[, c(outcome, opt$features)]
  
  f <- paste0(outcome, " ~ ", 
              paste(sprintf("poly(%s, degree = %i)", opt$features, degree), 
                    collapse = " + "))
  mod <- lm(as.formula(f), data = tmp)
  
  perm_imp <- boot_perm_int(formula = f, outcome = outcome, data = tmp, 
                            boot_rep = opt$boot_rep, features = opt$features,
                            skip = !opt$boot_perm_imp)
  
  sink(file = tempfile())
  marginal_effects <- lapply(opt$features, function(feature) {
    margins::cplot(mod, feature, what = "prediction", data = tmp,
                   level = 0.95, draw = FALSE,
                   xvals = if (is.null(opt$trim_marginals)) 
                     prediction::seq_range(tmp[[feature]], n = 100) else
                       seq(from = pmax(opt$trim_marginals[1], min(tmp[[feature]])),
                           to = pmin(opt$trim_marginals[2], max(tmp[[feature]])),
                           length.out = 100))
  })
  sink()
  names(marginal_effects) <- opt$features
  list(mod = mod, marginal_effects = marginal_effects, perm_imp = perm_imp,
       data = tmp[, c(outcome, opt$features)])
}
