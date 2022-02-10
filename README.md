# Spectral binning as an approach to post-acquisition processing of high resolution FIE-MS metabolome fingerprinting data

[![Docker](https://github.com/jasenfinch/Spectral_binning_as_an_approach_to_post-acquisition_processing_of_FIE-HRMS_data/workflows/Docker/badge.svg?branch=devel)](https://github.com/jasenfinch/Spectral_binning_as_an_approach_to_post-acquisition_processing_of_FIE-HRMS_data/actions)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

This is the code and analysis repository for the article:

Finch, J.P., Wilson, T., Lyons, L., Phillips, H., Beckmann, M., and Draper, J., 2022. "Spectral binning as an approach to post-acquisition processing of high resolution FIE-MS metabolome fingerprinting data"

All code is written in R and the [drake](https://docs.ropensci.org/drake/) package has been used for workflow management.
The [renv](https://github.com/rstudio/renv) package has been used to ensure a reproducible R environment.

## Compile the manuscript

### Using docker

The manuscript can be compiled using a pre-built docker image, directly from GitHub:

``` sh
docker run -v $(pwd):/home/rstudio/Spectral_binning_as_an_approach_to_post-acquisition_processing_of_FIE-HRMS_data ghcr.io/jasenfinch/spectral-binning:latest
```

### Locally

To generate the manuscript, simply [clone the repository](https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository), open the R console, set the working directory to the repository clone using `setwd()` and run the command `drake::r_make()`.
