### Survey Data

This directory contains the data collected by our post-experimental questionnaire. The file `survey_data.RData` contains two data sets: `dta_receipt_matched` and `dta_survey_matched`.

`dta_receipt_matched`: Is organized at the receipt-day level. It results from merging the dish choice data, after aggregating it at the receipt-day level with the survey response to identify transactions which we can allocate to a specific survey response. 

`dta_survey_matched`: Contains survey responses and their potential match to a specific dish choice, including the respective dish choice data for convenience. 

Please note: `survey_data.RData` is stored in a binary format that is R-specific. However, you can easily convert the data sets to CSV by running the following snippet of R code (simply change the file names as desired):

```
load("data/survey/survey_data.RData")
write.csv(dta_receipt_matched, "data/survey/data_receipt_matched.csv", row.names = FALSE)
write.csv(dta_survey_matched, "data/survey/data_survey_matched.csv", row.names = FALSE)
```

  
  
