# Fall 2021 PRIME: AQIxCovid19 outcome analysis


# Template structure

* All data goes into the subfolders inside the `data` folder.
* All code goes into the `code` folder or subfolders. `readme.md` provides more detail on data
* All results (figures, tables, computed values) go into `results` folder or subfolders.
* Manuscript is housed int the `products` subfolder.

# Order of operations for reproducable analysis

1) Raw data obtained from various sources is housed in the `raw_data` folder. 
2) Code/processing_code folder contains:
  - `AQIcleaning.R` which processes data from EPA
  - 
  Processed data is save to `processed_data`
3) Code/analysis_code/`exploratoryscript` contains an R script which loads the processed data and does some exploratory data analysis
  Tables and figures are saved to results/`exploratoryfigures` and used in the manuscript. Exploratory modeling are done in here as well
4) Code/analysis_code/`analysisscript`is the final model 
  Tabled and figures saved in the `results` folder.
5) Products/`manuscript` folder contains main Rmd document for the manuscript. 


