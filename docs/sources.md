# Data Sources for NATO Spending Project

This document lists the source and naming convention for all raw data files.


## Country Sample Overview
Source: Created by Author (Frederik Bender Bøeck-Nielsen)
Created on: 2025-09-25

Filename
------------------------------------
country_sample.xlsx


## Missing Government Data (ParlGov & The Manifesto Project integration)
Source: Created by Author (Frederik Bender Bøeck-Nielsen)
Created on: 2025-09-26

Filename
------------------------------------
missing_government_data.xlsx


## SIPRI Military Expenditure Data (2025)
Source URL: https://www.sipri.org/databases/milex
Downloaded on: 2025-09-07

Original Filename -> New Filename
------------------------------------
SIPRI-Milex-data-1949-2024_2.xlsx -> sipri_milex_2025p.xlsx


## NATO Defence Expenditure Data (2025)
Source URL: https://www.nato.int/cps/en/natohq/news_237171.htm
Downloaded on: 2025-09-17
Note: Estimates for 2025 military spending for Germany is missing from the NATO data. Therefore, an estimate of 2,4% has been sourced from IISS: https://www.iiss.org/online-analysis/military-balance/2025/062/nato-agrees-on-investment-pledge/

Original Filename -> New Filename
------------------------------------
250827-def-exp-en.xlsx -> nato_milex_2025-08-28p.xlsx


# CEPII GEODIST DATA (Dyadic file)
Source URL: https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=6
Downloaded on: 2025-09-18

Filename
------------------------------------
dist_cepii.dta


## IMF World Economic Outlook (WEO) Data (April 2025 Edition)
Source URL: https://data.imf.org/en/datasets/IMF.RES:WEO
Downloaded on: 2025-09-24

Download Options Selected:
- Download Full Dataset
- File Format: CSV
- Data Format: Observation per row
- Metadata (Identifier): ID and Name
- Metadata (Fields selected): Country, Indicator, Frequency, Unit of Measure

Original Filename -> New Filename
------------------------------------
dataset_2025-09-24T16_30_06.786292732Z_DEFAULT_INTEGRATION_IMF.RES_WEO_6.0.0.csv -> imf_weo_2025-04-22p.csv


## CEPII BACI HS12 DATA
Source URL: https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37
Downloaded on: 2025-09-25
Note: Click the link "HS12" to download the ZIP folder called "BACI_HS12_V202501". The annual trade data is located in separate .csv files within the folder. Files from the ZIP that are not listed below, are not used in the project.

Original Filename -> New Filename
------------------------------------
Readme.txt -> cepii_baci_readme.txt
country_codes_V202501.csv -> cepii_baci_country_codes.csv
BACI_HS12_Y2014_V202501.csv -> cepii_baci_2014.csv
BACI_HS12_Y2015_V202501.csv -> cepii_baci_2015.csv
BACI_HS12_Y2016_V202501.csv -> cepii_baci_2016.csv
BACI_HS12_Y2017_V202501.csv -> cepii_baci_2017.csv
BACI_HS12_Y2018_V202501.csv -> cepii_baci_2018.csv
BACI_HS12_Y2019_V202501.csv -> cepii_baci_2019.csv
BACI_HS12_Y2020_V202501.csv -> cepii_baci_2020.csv
BACI_HS12_Y2021_V202501.csv -> cepii_baci_2021.csv
BACI_HS12_Y2022_V202501.csv -> cepii_baci_2022.csv


## IEP Global Peace Index 2025
Source URL: https://www.economicsandpeace.org/consulting/data-licensing/
Downloaded on: 2025-09-16
Note: Accessed via submission of form for non-commercial download

Original Filename -> New Filename
------------------------------------
GPI_public_release_2025.xlsx -> iep_gpi_2025p.xlsx


## ParlGov 2024 Release
Source URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC
Downloaded on: 2025-09-23

Original Filename -> New Filename
------------------------------------
view_cabinet.csv
view_party.csv


# DATA ACCESSED THROUGH R PACKAGES:


## The Manifesto Project
R Package: manifestoR
API Key: b2f2e5d48f009fac1d6cc0b1b97b8dcc


## V-DEM
Package: vdemdata


## WDI
Package: 