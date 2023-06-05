# About this Readme

This README serves to provide a high level overview of the different files in
this folder and highlight their function.

## Internals

These scripts are called within workflows from other scripts.

- `_distribution_figure.R`: Creates distributions used for Figure S1.
- `_plotting_aesthetics.R`: Used for figure generation. This script standardises
  colour schemes and themes.

## Analyses

- `1_simulations_w_traitstrap.R`: Runs the sampling simulations used in the
  manuscript and SI. *Note: This is the slowest and most computationally
  intensive script.*
- `1.1_global_db_comparison.R`: Estimates trait distributions for the Colorado
  herbs dataset using a Global trait database (BIEN).
- `1.2_effect_sizes.R`: Calculates effect sizes (Cohen's d) between different
  methods for estimating the moments of trait distributions.  Not used in the
  manuscript.
- `1.3_pct_error_comparisons.R`: Generates a table showing the improvement in
  percent error relative to a cross-site, community-weighted approach for
  estimating the moments of trait distributions.

## Figure Generation

- `2.1_figures_main.R`: Creates all figures featured in the primary manuscript.
- `2.2_figures_supp.R`: Creates all figures featured in the supplementary
  material.
- `2.3_example_distributions`: Creates the inset images used in Box 1.
