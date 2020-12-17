This readme file summarizes the data and code in the *masd*-project directory. 

# `masd_reloaded.Rproj` (FILE)
Double-click this file to open the R-project. This will set your working directory to the project folder so that all relative paths should work.  

# `renv.lock` (FILE)
We used the `renv`-package for dependency management. The json-formatted `renv.lock` file lists all packages we used including their versions. When in the root-directory of the project, the packages can be installed using `renv::restore()`. 

# `results/` (FOLDER)
This folder stores intermediate and final results objects, such as dataframes stored as .Rda objects, Word tables, or exported figures. The names of results files usually correspond to the names of the R scripts that created them. 

# `code/` (FOLDER)
This folder contains all data processing and analysis code. The numbering from 01 to 04 indicates potential dependency: Scripts starting with 04 depend on output from scripts starting with 03, and so forth. Intermediate datasets are stored as .Rda files in the `results/` directory. Tables are stored as Word-documents in `results/figures and tables/`. Figures are stored as PNG files in the same folder. 

- `00_run.R`: This R script contains code to run all other R scripts as `jobs` in RStudio (see https://blog.rstudio.com/2019/03/14/rstudio-1-2-jobs/). This way, scripts can be run in parallel in separate R sessions, which speeds up development and avoids dependency issues with incorrectly loaded packages or objects floating around in the environment. Running the scripts that way is not strictly necessary though.
- `01_compute_es.R`: We collected all primary data (as either raw data or summary statistics) in Excel sheets with pre-defined formats. These Excel sheets are stored in the `data/prepared/` directory. However, note that these data are **not** uploaded to the OSF. We are not permitted to share the primary data we collected from original authors. We still include this script for transparency. The script loads the data from the Excel sheet and processes it according to the format. It outputs a dataframe with raw, outcome level effect sizes and some coding information on the outcome level. The output file is `results/es2_raw.Rda`. This file is included in the upload, so from this step on all analyses can be fully reproduced. 
- `02_prepare_df.R`: This script joins the raw effect size data with various additional coding data, cleans the dataset and computes additional variables. The key output of this script is the enriched meta-analytic dataset, stored in `results/es_prepared2.Rda`. This file is an R list, containing various subsets of the full dataset with or without effect size-level outliers. The scripts also prepares the correlational data for analysis and outputs the results in a dataframe in `results/es_cor.Rda`. 
- `03_all_items.R`: This scripts creates a table containing all questionnaire items included in the meta-analysis. This table is not included in the manuscript.  
- `03_correlation_analysis.R`: This script creates a figure for the meta-analytic correlation analysis. 
- `03_distribution_overlap.R`: This script creates a figure illustrating how the sex drive distributions of men and women overlap. 
- `03_inventory_overview.R`: This script creates a table listing all psychometric inventories that items were drawn from.
- `03_main_analysis_figure.R`: This script creates a figure for the main analysis. 
- `03_main_analysis_funnels.R`: This script creates funnel plots for the primary sex drive indicators. 
- `03_main_analysis_funnels_control.R`: This script creates funnel plots for the bias indicators. 
- `03_main_analysis_funnels_secondary.R`: This script creates funnel plots for the secondary sex drive indicators. 
- `03_main_analysis_histograms.R`: This script creates histograms illustrating the distribution of effect sizes for the primary indicators.
- `03_main_analysis_leave1out.R`: This script conducts a leave-on-out outlier analysis and creates a figures illustrating the results. 
- `03_main_analysis_table.R`: This script creates a table summarizing the main results of the meta-analysis. 
- `03_moderation_analysis.R`: This script conducts meta-regression analyses for the primary sex drive indicators. It creates two tables: One summarizing the results of global tests for moderation for each moderator, and one depicting the full regression tables. It also creates an intermediate results file `results/mod_results.Rda`. 
- `03_moderation_analysis_secondary.R`: This script conducts meta-regression analyses for the secondary sex drive indicators. It creates two tables: One summarizing the results of global tests for moderation for each moderator, and one depicting the full regression tables. It also creates an intermediate results file `results/mod_results_secondary.Rda`. 
- `03_moderator_overview.R`: This script produces a table containing describtive informative for all moderators across studies reporting primary sex drive indicators. 
- `03_moderator_overview_secondary.R`: This script produces a table containing describtive informative for all moderators across studies reporting secondary sex drive indicators. 
- `03_pubbias_3PSM.R`: This script tests for publication bias using Egger's regression test and the three-parameter selection model test. It creates an intermediate results object: `results/res_pubbias.Rda`. 
- `03_search_terms_item_overview.R`: This scripts creates a table containing examples for each item category and a table containg all search terms. 
- `03_theory_figure.R`: This script creates a figure that illustrates the theoretical conceptualization of sex drive we developed in this study. 
- `04_moderation_plots.R`: This script loads the intermediate data from the moderation analysis and creates a figure depicting scatter plots for all moderation analyses. These were used to visually detect outliers. The figures are not included in the manuscript. 
- `04_retrieve_results.R`: This script retrieves values from various intermediate results objects. It stores these results values in an Excel sheet. These are the values we report in text in the manuscript. The Word document of the manuscript is linked to the Excel results sheet. 
- `functions`: This folder contains R scripts with helper functions used throughout the project. 

# `data/` (FOLDER)
This folder stores all primary data sources. 

- `coding`: This folder contains various coding data. The file `item_coding.xlsx` contains codings for each questionnaire item. The `study_coding.xlsx` file contains codings one the study- and publication-level. The `mods_info.xlsx` file contains meta-information about the moderators. The `coder_reliability.xlsx` file contains codings by two raters for a set of studies. Some external datasets we used for enriching the dataset are not uploaded in this repository. These are all used in the `02_prepare_df.R` script. This script contains information about where to find the external datasets. 
- `templates`: This folder contains Excel sheet templates. These were used to put the primary data that we collected from original authors into predefined formats. We then processed these Excel sheets automatically according to the format. 
- `additional`, `prepared`, `removed`: These folder are **not** included in the repository, as they contain primary data. We do not have permission to share them. 

# `manuscript/` (FOLDER)
This folder contains the supplementary online materials in a Word document. 

# `full effect size data.csv` (FILE)
This is the full dataset containg all effect sizes after outlier removal. We added this file as a convenience for readers. The R scripts do not use this file, but use the intermediate results objects stored in `results/`.
