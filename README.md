Overview:
This project looks at the causal impact of the Ukraine War, on military expenditures among European NATO members. The analysis follows a two-step approach, which first seeks to 1) estimate the causal effect that the invasion had on military spending, and secondly attempts to 2) explain the variance in military spending as a result of the invasion. All coding and analysis is written in the programming language R and done in the RStudio IDE. 

SCRIPTS:
The results can be reproduced, by running the 7 scripts, one-by-one, in the correct order. The scripts are standalone, in the sense that they can be run individually, as long as the preceding scripts have been run at least once. E.g. the second script relies on data created in the first script in order to function properly, but the scripts do not need to be run in the same session. As long as 01_download_raw_data has been run at least once, then 02_fix_ideology_data should work fine a week later.

Names and order:
01_download_raw_data.R
02_fix_ideology_data.R
03_data_preparation.R
04_inspection_and_transformation.R
05_explorative_analysis_and_prelims.R
06_models.R
07_robustness.R

DATA:
The analysis uses data from a variety of sources. Due to it's size, the raw data isn't included in the repository, and has instead been uploaded to a Zenodo. The first script 01_download_raw_data will, as the name implies, download this raw data through a stable Zenodo link. This process may take a while, but it is a necessary step, and the simplest approach for accessing the data.

DATA SOURCES:
SIPRI, NATO, IEP GPI, IMF WEO, CEPII, ParlGov, The Manifesto Project, and WDI.

HOW TO REPRODUCE THE ANALYSIS:
1. Download and install R and RStudio
2. Open ba_project.Rproj
3. Open and run 01_download_raw_data.R
4. ...
