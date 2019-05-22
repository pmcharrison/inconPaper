plot_poly <- function(mods, opt, only_observed) {
  # Select best models
  best <- plyr::ldply(opt$corpora, function(corpus) {
    tmp <- mods[mods$corpus == corpus, ]
    z <- which.min(tmp$BIC)
    message("Plotting polynomials of degree = ", tmp$degree[z],
            " for the ", corpus, " corpus.")
    tmp[z, ]
  }, .id = NULL)
  
  a <- plot_perm_imp(best, opt, only_observed)
  b <- plot_marginals(best, opt, only_observed)
  c <- plot_predict(best, opt, only_observed)

  p <- cowplot::plot_grid(a, b, c, ncol = 1, rel_heights = c(0.77, 2.18, 1),
                     labels = "AUTO")
  ggsave(paste0("composition-analysis-only-observed=", only_observed, ".", opt$plot$type),
         path = "output",
         width = 5.5, height = 9,
         dpi = opt$plot$dpi)
}
plot_marginals <- function(best, opt, only_observed) {
  res <- data.frame()
  raw <- data.frame()
  
  for (i in seq_len(nrow(best))) {
    corpus_i <- best$corpus[[i]]
    margins_i <- best$marginal_effects[[i]]
    data_i <- best$data[[i]]
    for (j in seq_along(margins_i)) {
      feature_j <- names(margins_i)[j]
      df_j <- margins_i[[j]]
      df_j <- cbind(corpus = corpus_i,
                    feature = feature_j,
                    df_j,
                    stringsAsFactors = FALSE)
      res <- rbind(res, df_j)
      raw_j <- data.frame(corpus = corpus_i,
                          scale_log_count = data_i$scale_log_count,
                          feature = feature_j,
                          feature_val = data_i[[feature_j]],
                          stringsAsFactors = FALSE)
      if (feature_j == "chord_size") {
        step_size <- min(diff(sort(unique(raw_j$feature_val))))
        raw_j$feature_val <- raw_j$feature_val + runif(nrow(raw_j), 
                                                       - step_size / 2, 
                                                       step_size / 2)
      }
      raw <- rbind(raw, raw_j)
    }
  }
  res$corpus <- label_corpus(res$corpus, opt, detail = FALSE)
  res$feature <- label_feature(res$feature, opt)
  
  raw$corpus <- label_corpus(raw$corpus, opt, detail = FALSE)
  raw$feature <- label_feature(raw$feature, opt)
  
  ggplot() +
    geom_ribbon(data = res, 
                aes(x = xvals, ymin = lower, ymax = upper, fill = feature), 
                alpha = 0.25) +
    geom_line(data = res, aes(x = xvals, y = yvals)) + 
    geom_rug(mapping = aes(x = feature_val, colour = feature, alpha = corpus), 
             data = raw) +
    scale_alpha_manual(values = if (only_observed) 
      c(0.03, 0.15, 0.15) else rep(0.03, times = 3)) +
    facet_grid(feature ~ corpus, scales = "free") +
    scale_x_continuous("Feature value", limits = opt$trim_marginals) +
    scale_y_continuous("Chord frequency (marginal prediction)") +
    scale_color_manual(values = c("#B50000", "#11A3FF", "#000000"), guide = FALSE) +
    scale_fill_manual(values = c("#B50000", "#11A3FF", "#000000"), guide = FALSE) +
    theme(aspect.ratio = 1,
          panel.spacing = unit(0.8, "lines"),
          legend.position = "none")
}

plot_predict <- function(best, opt, only_observed) {
  res <- mapply(function(corpus, mod) {
    pred <- predict(mod)
    actual <- resid(mod) + pred
    data.frame(corpus = corpus, pred = pred, actual = actual, 
               stringsAsFactors = FALSE)
  }, best$corpus, best$mod, SIMPLIFY = FALSE)
  res <- do.call(rbind, res)
  row.names(res) <- NULL
  res$corpus <- label_corpus(res$corpus, opt)
  
  r_df <- res %>% as_tibble() %>% group_by(corpus) %>% 
    summarise(r = cor(pred, actual) %>% round(2))
  
  ggplot(res, aes(x = pred, y = actual)) + 
    geom_point(shape = 21) +
    scale_x_continuous("Predicted frequency", 
                       breaks = if (only_observed) 
                         seq(from = -2, to = 6) else waiver()) + 
    scale_y_continuous("Actual frequency") +
    geom_text(data = r_df, 
              aes(label = paste("~italic(r) ==", r)), 
              parse = TRUE,
              x = -Inf, y = Inf, 
              hjust = -0.1, vjust = 1.5) +
    facet_wrap(~ corpus, nrow = 1, scales = "free") +
    theme(aspect.ratio = 1)
}

plot_perm_imp <- function(best, opt, only_observed) {
  res <- data.frame()
  for (i in seq_len(nrow(best))) {
    corpus_i <- best$corpus[[i]]
    imp_i <- best$perm_imp[[i]]
    df_i <- cbind(corpus = corpus_i,
                  imp_i,
                  stringsAsFactors = FALSE)
    res <- rbind(res, df_i)
  }
  res$feature <- label_feature(res$feature, opt, reverse = TRUE)
  res$corpus <- label_corpus(res$corpus, opt)
  p <- ggplot(res, aes(x = feature, y = imp, fill = feature)) +
    geom_bar(stat = "identity", colour = "black") +
    scale_x_discrete("Feature") + 
    scale_y_continuous("Feature importance",
                       breaks = if (only_observed) (0:2 / 5) else waiver()) +
    scale_fill_manual(values = c("#FFFFFF", "#11A3FF", "#B50000"), guide = FALSE) +
    facet_wrap(~ corpus, nrow = 1) +
    coord_flip() + 
    theme(plot.margin = unit(c(12, 50, 12, 5.5), "points"))
  if (opt$boot_perm_imp) {
    p <- p + geom_errorbar(aes(ymin = imp_min_95, ymax = imp_max_95),
                           width = 0.3, colour = "black")
  }
  p
}
