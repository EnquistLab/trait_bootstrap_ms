![Header](images/Traitstrap_vice.png)

# Sweet Dreams are Made of Bootstrapping: Bootstrapping outperforms community-weighted approaches for estimating the shapes of phenotypic distributions

[![Code](https://img.shields.io/badge/Code-10.5281/zenodo.153650790-f3b155)](https://zenodo.org/badge/latestdoi/153650790)
[![Datasets](https://img.shields.io/badge/Datasets-10.5281/zenodo.7876647-ff4ccc)](https://doi.org/10.5281/zenodo.7876647)
[![Manuscript](https://img.shields.io/badge/Preprint-10.22541/au.162196147.76797968/v1-90ff4c)](https://doi.org/10.1111/2041-210X.14160)

This project contains code associated with work that compares the performance of
parametric boostrapping, non-parametric bootstrapping and mean-field approaches
to inferring trait distribution.

## Repo Structure

- `data`: datasets used for analyses
- `figures`: figures and tables used in the manuscript and supplementary
  materials
- `images`: images used for figure or table insets
- `output_data`: processed data post simulations and bootstrapping. These are
  used for figure generation
- `r_functions`: functions for data analyses and cleaning that are used in the
  main script
- `scripts`: scripts used for implementation of {traitstrap} on the various
  datasets as well as figure generation


## Dependencies

Dependencies are managed and stored using {renv} so it is possible to restore
the last 'working state' of this project by calling `renv::restore()`. This will
install the correct package versions for this project.

### Version of `{traitstrap}`

Some of the code for generating distributions and simulations were built on an
older version of `{traitstrap}` (v 0.0.0.901). This legacy version is featured
in the repository as a .tar.gz and can be installed by running the following
code:

```
if(packageDescription("traitstrap", fields = "Version")!="0.0.0.901" |
     is.na(packageDescription("traitstrap", fields = "Version"))){
    install.packages("traitstrap_0.0.0.901.tar.gz",repos = NULL,method = "source") #this code is designed for version 0.0.0.901 of traitstrap, which we include in this Github repo
  }
```
