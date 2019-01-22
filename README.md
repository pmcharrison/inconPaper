
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inconPaper

This repository, `inconPaper`, contains the analysis scripts for Peter
M. C. Harrison & Marcus T. Pearce’s manuscript “Instantaneous consonance
in the perception and composition of Western music”.

## Folders

  - `input` - Contains some of the input data used for the analyses.
  - `output` - All output files are written to this folder.
  - `R` - Contains the source code for the analyses

## Dependencies

The R scripts depend on several common open-source R packages, which can
be installed using the following commands in R (internet connection
required):

``` r
install.packages(c(
  "tidyverse", "boot", "memoise", "purrrlyr", "glue", "broom", "zeallot", 
  "withr", "jsonlite", "plyr", "cowplot", "AICcmodavg", "margins",
  "prediction", "cocor", "R.utils", "devtools"
))
devtools::install_github(paste("pmcharrison/", c(
  "hrep", "incon", "inconData"
), sep = ""))
```

We also used version 2.1 of Essentia and version 1.6.1 of MIRtoolbox.
For the former, it is necessary to install a Docker image for Essentia
(<https://hub.docker.com/r/mtgupf/essentia/>).

## Running

The main scripts are to be run within R. Each script can be run in a
fresh R session.

  - `R/1-model-perception/1-compile/1-compile.R` (re)compiles the
    perceptual data, saving results to the `output` folder.
  - `R/1-model-perception/2-analyse/analyse.R` runs the perceptual
    analyses, using compiled data present in the `output` folder, and
    saves final outputs including figures.
  - `R/2-model-composition/top-level.R` runs the composition analyses.
    By default, bootstrapped confidence intervals are disabled, because
    they take a long time (several days) to compute.

### Essentia

Essentia model outputs are compiled by running
`R/1-model-perception/1-compile/src/essentia.R` within R. Two steps are
necessary first, however:

  - Install the Essentia Docker image
    (<https://hub.docker.com/r/mtgupf/essentia/>);
  - Modify `in_dir` and `out_dir` variables in the script to match your
    current directory locations.

### MIRtoolbox

MIRtoolbox model outputs are compiled by running
`R/1-model-perception/1-compile/src/mirtoolbox` within MATLAB. First,
however, you must edit the following variables to match your current
directory locations:

  - `in_dir` - Directory containing the Bowling et al. (2018) .wav
    files;
  - `out_dir` - Output directory;
  - `pkg_path` - Directory of MIRtoolbox installation.

## Caching

Various slow operations are cached, with cached outputs saved to the
`cache` directory. Delete this directory to clear the cache.
