library(tidyverse)
library(hrep)
library(inconData)
library(incon)
loadNamespace("plyr")

incon <- memoise::memoise(incon::incon, cache = memoise::cache_filesystem("cache/incon"))

generalise_lm <- function(mod, opt) {
  mod_perf <- get_generalise_data() %>% 
    plyr::llply(add_features, opt, .progress = "text") %>% 
    map(add_lm_pred, mod) %>% 
    get_all_generalise_performance(opt) %T>%
    saveRDS("output/perception-generalise-lm.rds")
  mod_perf %>% select(- cocor) %>% 
    write_csv("output/perception-generalise-lm.csv")
  plot <- plot_generalise_performance(mod_perf, opt)
  list(mod_perf, plot)
}

plot_generalise_performance <- function(x, opt) {
  x %>% filter(
    mod %in% c(opt$select, "combined", "combined_sans_chord_size") & 
      !(dataset == "sch03" & mod == "combined_sans_chord_size") &
      !(dataset == "jl12b" & mod == "combined_sans_chord_size") &
      !(dataset == "jl12a" & mod == "combined_sans_chord_size")
  ) %>% 
    mutate(
      dataset = recode_factor(
        dataset,
        sch03 = "Schwartz et al. (2003; 12 chords of size 2)",
        lah16 = "Lahdelma & Eerola (2016; 15 chords of size 3-6)",
        jl12b = "Exp 2. of Johnson-Laird et al. (2012; 48 chords of size 4)",
        jl12a = "Exp 1. of Johnson-Laird et al. (2012; 55 chords of size 3)",
        bowl18 = "Bowling et al. (2018; 298 chords of size 2-4)"
      ),
      mod = recode_factor(
        mod,
        combined_sans_chord_size = "Composite (no chord size)",
        combined = "Composite",
        har_19_corpus = "Culture",
        har_18_harmonicity = "Periodicity/harmonicity",
        hutch_78_roughness = "Interference",
      )) %>% 
    ggplot(aes(x = dataset,
               y = estimate,
               fill = mod,
               linetype = mod,
               ymin = conf.low,
               ymax = conf.high)) +
    geom_bar(stat = "identity",
             colour = "black",
             position = position_dodge(width = 0.9),
             width = 0.7) +
    geom_errorbar(position = position_dodge(width = 0.9),
                  width = 0.3, linetype = "solid") +
    scale_x_discrete("Dataset") +
    scale_y_continuous("Correlation with consonance ratings") +
    scale_fill_manual("Model",
                      values = c("#FFFFFF",
                                 "#FFFFFF", 
                                 "#E8E410",
                                 "#11A3FF",
                                 "#B50000")) +
    scale_linetype_manual("Model",
                          values = c(
                            "dotted",
                            "solid", 
                            "solid", 
                            "solid", 
                            "solid"
                          )) +
    coord_flip() +
    guides(fill = guide_legend(reverse = T),
           linetype = guide_legend(reverse = T)) +
    theme(aspect.ratio = 1)
}

get_all_generalise_performance <- function(data, opt) {
  map2(data, names(data), get_generalise_performance, opt) %>% 
    bind_rows()
}

get_generalise_performance <- function(x, x_label,  opt) {
  mods <- opt$selected %>% 
    union(opt$generalise_benchmark) %>% 
    union(c("combined", "combined_sans_chord_size")) %>% 
    sort
  df <- tibble(dataset = x_label,
               mod = mods,
               reverse = map_lgl(mod, 
                                 ~ cor(x[[.]], x$rating) < 0)
  ) %>% 
    mutate(res = map2(mod, reverse, 
                      function(mod, reverse) {
                        mult <- if (reverse) - 1 else 1
                        cor.test(x$rating, x[[mod]] * mult) %>% glance
                      })) %>% 
    unnest
  mutate(df, cocor = map(mod, compare_all_mods, x, mods, df))
}

compare_all_mods <- function(mod, x, mods, df) {
  mods %>% set_names(mods) %>% map(compare_mod, mod, x, df)
}

compare_mod <- function(mod_2, mod_1, x, df) {
  rev_1 <- filter(df, mod == mod_1) %>% pull(reverse)
  rev_2 <- filter(df, mod == mod_2) %>% pull(reverse)
  pred_1 <- x[[mod_1]] * (if (rev_1) -1 else 1)
  pred_2 <- x[[mod_2]] * (if (rev_2) -1 else 1)
  outcome <- x$rating
  suppressWarnings(
    cocor::cocor.dep.groups.overlap(r.jk = cor(outcome, pred_1), 
                                    r.jh = cor(outcome, pred_2),
                                    r.kh = cor(pred_1, pred_2), 
                                    nrow(x))
  )
}

get_generalise_data <- function() {
  bowl18 <- readRDS("output/perception-bowling.rds")
  jl12a <- inconData::jl12a
  jl12b <- inconData::jl12b
  lah16 <- inconData::lah16
  sch03 <- inconData::sch03 %>% mutate(pi_chord = map(pi_chord_type, pi_chord))
  as.list(environment())
}

add_lm_pred <- function(x, mod) {
  bind_cols(x, 
            get_lm_pred(mod, x) %>% transmute(combined = pred),
            get_lm_pred(mod, mutate(x, chord_size = 0)) %>% 
              transmute(combined_sans_chord_size = pred))
}

add_features <- function(x, opt) {
  consonance_models <- union(opt$selected, opt$generalise_benchmark)
  x$chord_size <- map_int(x$pi_chord, length)
  if (all(consonance_models %in% names(x))) return(x)
  bind_cols(
    x, 
    if (!"pi_chord_int" %in% names(x))
      map_dfr(x$pi_chord, ~ as.list(incon(., consonance_models))) else
        stop("this shouldn't happen")
    # pmap_dfr(
    #   list(x$pi_chord, x$pi_chord_int, x$tuning_tonic_pc_int),
    #   function(pi_chord, pi_chord_int, tuning_tonic_pc_int) {
    #     as.list(incon(
    #       x = pi_chord, 
    #       model = consonance_models,
    #       x_int = pi_chord_int,
    #       par = list(bowl_18_harmonicity = list(
    #         tonic = tuning_tonic_pc_int))))
    #   }
    # )
  )
}
