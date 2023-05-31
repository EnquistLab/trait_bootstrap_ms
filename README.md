![Header](images/Traitstrap_vice.png)

# On estimating the shape and dynamics of phenotypic distributions in ecology and evolution

This project contains code associated with work that compares the performance of
parametric boostrapping, non-parametric bootstrapping and mean-field approaches
to inferring trait distribution.

:newspaper: Associated preprint:
https://doi.org/10.22541/au.162196147.76797968/v1

:books: Associated datasets: https://doi.org/10.5281/zenodo.7876647

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
the last 'working state' of this project by calling `renv::restore()`. This
will install the correct package versions for this project.