library(boot)
library(memoise)

boot_perm_int <- function(formula, outcome, data, boot_rep, features, skip) {
  set.seed(1)
  b <- if (skip) {
    perm_int(data = data, indices = seq_len(nrow(data)),
             formula = formula, outcome = outcome, features = features)
  } else {
    boot(data = data, statistic = perm_int, R = boot_rep, 
         formula = formula, outcome = outcome, features = features)
  }
  ci <- lapply(seq_along(features), function(i) {
    x <- if (!skip) boot.ci(b, type = "bca", index = i)$bca[4:5]
    data.frame(feature = features[i], 
               imp = if (skip) b[i] else b$t0[i],
               imp_min_95 = if (skip) as.numeric(NA) else x[1],
               imp_max_95 = if (skip) as.numeric(NA) else x[2], 
               stringsAsFactors = FALSE)
  })
  do.call(rbind, ci)
}

boot_perm_int <- memoise::memoise(boot_perm_int, 
                                  cache = cache_filesystem("cache/boot_perm_int"))

perm_int <- function(data, indices, formula, outcome, features) {
  data <- data[indices, ]
  mod <- lm(as.formula(formula), data)
  actual <- data[[outcome]]
  pred <- predict(mod, data)
  r2_ref <- cor(pred, actual) ^ 2
  stopifnot(isTRUE(all.equal(r2_ref, summary(mod)$r.squared)))
  
  sapply(as.character(features), function(feature) {
    tmp <- data
    tmp[[feature]] <- tmp[[feature]][sample(nrow(tmp), size = nrow(tmp),
                                            replace = FALSE)]
    tmp_pred <- predict(mod, tmp)
    tmp_r2 <- cor(tmp_pred, actual) ^ 2
    r2_ref - tmp_r2
  })
}
