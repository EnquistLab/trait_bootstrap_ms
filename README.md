![Header](images/Traitstrap_vice.png)

# On estimating the shape and dynamics of phenotypic distributions in ecology and evolution

This project contains code associated with work that compares 
the performance of parametric boostrapping, non-parametric 
bootstrapping and mean-field approaches to inferring trait 
distribution.

:tada: Associated preprint: https://doi.org/10.22541/au.162196147.76797968/v1

## Repo structure

:file_folder: /data

+ associated datsets

:file_folder: /figures

+ figures featured in the manuscript/supplementary documents

:file_folder: /images
+ additional images used within figures as insets

:file_folder:/output_data
+ 'traitstrapped' outputs of the datasets

:file_folder: /scritps_for_ms_with_traitstrap
+ scripts for simulations as well as figures generation

:page_facing_up: example_traitstrap.RMD
  + a vignette for {traitstrap}

## Why bootstrap?

Bootstrapping is a resampling method that uses random sampling 
with replacement to generate a set of replicated 
distributions, thereby characterising uncertainty. 
This leverages the variation within the data to better estimate trait distributions provides estimates of 
uncertainty. Bootstrapping also encourages us to conceptualize 
trait values as distributions, and in doing so highlights 
issues that are present in many common sampling protocols.

![](figures/Figure_1.png)