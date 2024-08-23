This directory contains the code to create reports.
A few files are shared among all reports. 

* `bibliography.bib`
   : References to cite in the reports
* `styles.css`
   : CSS to format fonts of various styles

#### R markdown

* `default_rmd_setup.R`
   : A usual 'setup' chunk in R markdown
* `_output.yml`
   : The yaml to contol output, in the header of all R markdown

* `gen-<sample>-<contents>.Rmd`
   : This contains a report of exploratory data analysis (EDA) but also data preprocessing and quality control steps. 
   As the steps are frequently adjusted by new observation, they were combined into one report.
   And, the report is designed to be converted to `.R` script by `knitr::purl` function. 
