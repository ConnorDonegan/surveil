# surveil 0.1.1

## Bug fixes

  - Previously, if multiple groups had been modeled, the cumulative percent change summary was printed incorrectly. Now, the print method will return a summary of the cumulative percent change for each group.

  - The plotting method for the group_diff object (pairwise inequality measures) has been adjusted so that the correct labels are used to identify the time periods on the x axis of the plots. Previously, if the `style = 'lines'` argument was used, the x axis used generic index values on the x axis instead of the labeled time periods provided by the user.

 
# surveil 0.1.0

November 6, 2021. surveil's first release.

