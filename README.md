
Contact: Katherine Hulting, <hultingk@msu.edu>

**Script name: 01_wrangling.R**\
Purpose: Wrangle data into format needed for analysis
Requires: CARBEL-arthropods.csv, CARBEL-floral.csv, CARBEL-seeds.csv, Patch_type.csv

**Script name: 02_defining_models.R**\
Purpose: Directed Acyclic Graphs to inform models
Requires: run 01_wrangling.R

**Script name: 03_arthropod_models.R**\
Purpose: Fitting GLMMs
Requires: run 01_wrangling.R

**Script name: 04_figures.R**\
Purpose: Create and export figures
Requires: run 03_arthropod_models.R
