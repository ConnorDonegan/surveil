# surveil 0.2.1

The vignettes have been updated. The discussion of Markov chain Monte Carlo (MCMC) diagnostics, which was previously in the main package demo vignette, is now a stand-alone vignette with a bit of introductory discussion on MCMC analysis.

# surveil 0.2.0

### Feature updates

 - The `stan_rw` model-fitting function now supports binomial models.
 - There is a new vignette on age-standardization and comparing risk across two age-stratified populations: `vignette("age-standardization")`.
 - The `group_diff` function, for calculating pairwise measures of inequality, can now be used to compare age-stratified populations. This includes a calculation of total annual (and cumulative) excess cases and attributable risk derived from all of the age-specific rates and population sizes. See `vignette("age-standardization")`.
 - A new `plot` method now accepts a list of `stand_surveil` objects. This will allow multiple standardized rates to be visualized on the same plot.
 - All plots have a new default, custom ggplot theme.


# surveil 0.1.1

### Bug fixes

  - Previously, if multiple groups had been modeled, the cumulative percent change summary was printed incorrectly. Now, the print method will return a summary of the cumulative percent change for each group.

  - The plotting method for the group_diff object (pairwise inequality measures) has been adjusted so that the correct labels are used to identify the time periods on the x axis of the plots. Previously, if the `style = 'lines'` argument was used, the x axis used generic index values on the x axis instead of the labeled time periods provided by the user.

 
# surveil 0.1.0

November 2021: surveil's first release.

