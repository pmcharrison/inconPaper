all_mods <- function() {
  bind_rows(incon_mods(), toolbox_mods())
}

incon_mods <- function() {
  incon::incon_models %>% 
    select(- f) %>% 
    filter(label %in% readLines("R/1-model-perception/all-incon-mods.txt")) %>% 
    mutate(citation = recode(citation,
                             `Wang et al. (2013)` = "Wang et al. (2013, new)",
                             `Harrison & Pearce (in prep.)` = "New corpus-based model"),
           input = "Symbolic")
}

toolbox_mods <- function() {
  tribble(~ label, ~ citation, ~ class, ~ consonance, ~ spectrum_sensitive, ~ continuous_pitch,
          "wang_13_roughness_orig", "Wang et al. (2013, original)", "Spectral interference", FALSE, TRUE, TRUE,
          "mir.seth", "MIRtoolbox (Sethares)", "Spectral interference", FALSE, TRUE, TRUE,
          "mir.seth.min_weighted", "MIRtoolbox (Sethares, v2)", "Spectral interference", FALSE, TRUE, TRUE,
          "mir.vass", "MIRtoolbox (Vassilakis)", "Spectral interference", FALSE, TRUE, TRUE,
          "mir.inharmonicity", "MIRtoolbox", "Periodicity", FALSE, TRUE, TRUE,
          "ess.roughness", "Essentia", "Spectral interference", FALSE, TRUE, TRUE,
  ) %>% 
    mutate(input = "Audio")
          # "ess.inharmonicity", "Essentia",
          # "mcgill.inharmonicity.mean", "Timbre Toolbox")
}
