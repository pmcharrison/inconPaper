library(magrittr)
library(tidyverse)
library(purrrlyr)
library(glue)
library(broom)
library(zeallot)
loadNamespace("cocor")

theme_set(theme_classic() + 
            theme(axis.text.x = element_text(colour = "black"),
                  axis.text.y = element_text(colour = "black"),
                  axis.ticks = element_line(colour = "black")))

R.utils::mkdirs("cache/incon")

for (f in list.files("R/1-model-perception/2-analyse/functions", 
                     full.names = TRUE)) source(f)

opt <- list(
  selected = c(
    "hutch_78_roughness",
    "har_18_harmonicity",
    "har_19_corpus"
  ),
  exclude = "parn_94_mult",
  generalise_benchmark = c(
    "hutch_78_roughness",
    "har_18_harmonicity",
    "har_19_corpus",
    "stolz_15_periodicity",
    "huron_94_dyadic",
    "parn_94_pure",
    "gill_09_harmonicity",
    "jl_12_tonal",
    "parn_88_root_ambig"
  ),
  thesis = FALSE
)
opt$features <- c("chord_size", opt$selected)

dat_bowling <- readRDS("output/perception-bowling.rds")
c(all_mod_stats, p1) %<-% all_mods_bowling(dat_bowling, opt)
mod <- fit_lm(dat_bowling, opt$features)
p2 <- plot_lm_coef(mod, opt$features)
p3 <- plot_lm_pred(mod, dat_bowling)
c(mod_perf, p4) %<-% generalise_lm(mod, opt)
combine_panels(p1, p2, p3, p4)
