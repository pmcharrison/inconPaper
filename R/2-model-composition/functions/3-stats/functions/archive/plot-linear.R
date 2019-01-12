plot_linear <- function(linear, corpora, features) {
  x <- data.frame()
  for (i in seq_len(nrow(linear))) {
    corpus_i <- linear$corpus[i]
    mod_i <- linear$mod[[i]]
    beta_i <- coef(mod_i)
    conf_i <- confint(mod_i)
    new_i <- plyr::ldply(features, function(feature) {
      beta_j <- beta_i[grepl(feature, names(beta_i))]
      conf_j <- conf_i[grepl(feature, rownames(conf_i)), ]
      stopifnot(length(beta_j) == 1L && length(as.numeric(conf_j) == 2L))
      data.frame(corpus = corpus_i, 
                 feature = feature,
                 beta = beta_j,
                 conf_95_min = conf_j[1],
                 conf_95_max = conf_j[2])
    }, .id = NULL)
    x <- rbind(x, new_i)
  }
  x$corpus <- label_corpus(x$corpus, corpora)
  x$feature <- label_feature(x$feature, features)
  ggplot(x, aes_string(x = "feature", y = "beta",
                       ymin = "conf_95_min", ymax = "conf_95_max")) +
    scale_x_discrete("Feature") +
    scale_y_continuous("Standardised regression coefficient") +
    geom_bar(stat = "identity", fill = "white", colour = "black") + 
    geom_errorbar(width = 0.25) +
    facet_wrap(~ corpus) + 
    theme(aspect.ratio = 1) +
    coord_flip()
  ggsave("output/predicting-corpora-lm-beta.pdf", width = 6.5, height = 2.75)
}
