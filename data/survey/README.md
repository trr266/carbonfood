### Survey Data

This directory contains the data collected by our post-experimental questionnaire. The file `survey_data.RData` contains two data sets: `dta_receipt_matched` and `dta_survey_matched`. 

`dta_receipt_matched`: Is organized at the receipt-day level. It results from merging the dish choice data, after aggregating it at the receipt-day level with the survey response to identify transactions which we can allocate to a specific survey response. Variables are defined as follows:

- `eday`: The experimental day in consecutive numbering.
- `pcard_id`: A pseudonymized ID identifying a single payment card. While each transaction is linked to such an ID, participants can (and at times do) pay for multiple diners in one transaction. Also, sometimes participants split up their food choice in multiple transactions for technical reasons.
- `customer_group`: A discrete variable ('Bedienstete', 'Gäste' and 'Studierende'), indicating whether the payment card used for the transactions belongs to a faculty/staff member, a guest, or a student.
- `receipt_id`: The receipt number of a diner's purchase. Necessary to match survey data and checkout data.
- `qhour`: Time of checkout, measured in quarter hour slots past 11am.
- `minute`: Time of checkout, measured in minutes past 11am. 
- `tment_start`: The start of the first treatment, measured in minutes past 11am.
- `tment_end`: As discussed in the paper, on most experimental days some main dishes ran out prior to the end of our experimental period (2pm). On these days,`tment_end` indicates the point of time in minutes past 11am where the first main dish component ran out on both counters.
- `tslot`: Treatment slot the participant was in.
- `tment`: Treatment condition the participant was in.
- `time`: The time of day of the transaction, measured in hours and minutes.
- `mtfsh`: Binary variable equal to 1 if the chosen dish by the diner contained meat or fish, 0 otherwise.
- `weight`: The amount of food contained in the main dish container measured in grams.
- `co2eg`: The carbon footprint information for the respective main dish. The calculation of this measure is detailed in the paper.
- `first_time`: Whether the observation stems from the first experimental day a the respective paying card was used to purchase a meal.
- `visits`: Number of distinct experimental days a paying card has been used.
- `took_survey`: Binary variable equal to true if a diner took the survey, false otherwise.
- `dining_data_match`: Binary variable equal to 1 if survey response could be matched to checkout data.

`dta_survey_matched`: Contains survey responses and their potential match to a specific dish choice, including the respective dish choice data for convenience. Variables are defined as follows:

- `eday`: The experimental day in consecutive numbering.
- `survey_minute`: The time when the survey response was submitted, measured in minutes past 11am.  
- `receipt_id`: The receipt number of a diner's purchase. Necessary to match survey data and checkout data.
- `meal_happiness`: Categorical variable: Self-reported happiness of a survey participant with their food choice ranging from unhappy to happy (5 point scale, Unzufrieden (1); Eher unzufrieden (2); Weder zufrieden, noch unzufrieden (3); Eher zufrieden (4); Zufrieden (5)).
- `number_people`: Categorical variable: Self-reported number of people who dine together (including the participant, Ich esse heute allein (1); Wir sind zu zweit (2); Wir sind zu dritt (3); Wir sind mehr als drei Personen (>3)).
- `avoid_usually`: A categorical variable indicating which foods participants usually avoid (Fleisch (Meat); Fisch (Fish); Sonstige tierische Produkte (Other animal products); Allergie- und Unverträglichkeiten auslösende Lbensmittel (z.B. Erdnüsse, Histamin, etc.) (Food causing allergies or intolerances (e.g. peanuts, histamine)); Sonstige (Other); Keine (None)).
- `select_usually`: A categorical variable indicating which criteria a participant usually applies to base their dish selection on (see survey question 4 in OA.C.1, Aussehen (Appearance); Geschmack (Taste); Gesundheitliche Aspekte (Health considerations); Länge der Warteschlange für das Gericht (Length of the queue at the counter); Nährwerte (z.B. Proteine, Kalorien) (Nutritional value (e.g. proteins, calories)); Ökologischer Fußabdruck (Ecological footprint); Preis (Price); Sonstiges (Other), Tierwohl (Animal welfare)).
- `select_today`: A categorical variable indicating which criteria a participant applied on the experimental day to base their dish selection on (see survey question 5 in OA.C.1 and `select_usally`).
- `terms_accepted`: A binary variable equal to 1 if participants agreed to the terms of the survey and raffle, 0 otherwise.
- `language`: Language the participant took the survey in (de (German) or en (English)).
- `surveys_per_receipt_id`: Number of survey responses that were submitted per receipt ID.
- `pcard_id`: A pseudonymized ID identifying a single payment card. While each transaction is linked to such an ID, participants can (and at times do) pay for multiple diners in one transaction. Also, sometimes participants split up their food choice in multiple transactions for technical reasons.
- `customer_group`: A discrete variable ('Bedienstete', 'Gäste' and 'Studierende'), indicating whether the payment card used for the transactions belongs to a faculty/staff member, a guest, or a student.
- `qhour`: Time of checkout, measured in quarter hour slots past 11am.
- `minute`: Time of checkout, measured in minutes past 11am. 
- `tment_start`: The start of the first treatment, measured in minutes past 11am.
- `tment_end`: As discussed in the paper, on most experimental days some main dishes ran out prior to the end of our experimental period (2pm). On these days,`tment_end` indicates the point of time in minutes past 11am where the first main dish component ran out on both counters.
- `tslot`: Treatment slot the participant was in.
- `tment`: Treatment condition the participant was in.
- `time`: The time of day of the transaction, measured in hours and minutes.
- `mtfsh`: Binary variable equal to 1 if the chosen dish by the diner contained meat or fish, 0 otherwise.
- `weight`: The amount of food contained in the main dish container measured in grams.
- `co2eg`: The carbon footprint information for the respective main dish. The calculation of this measure is detailed in the paper.
- `first_time`: Whether the observation stems from the first experimental day a the respective paying card was used to purchase a meal.
- `visits`: Number of distinct experimental days a paying card has been used.
- `dining_data_match`: Binary variable equal to 1 if survey response could be matched to checkout data.
- `took_survey`: Binary variable equal to true if a diner took the survey, false otherwise.

Please note: `survey_data.RData` is stored in a binary format that is R-specific. However, you can easily convert the data sets to CSV by running the following snippet of R code (simply change the file names as desired):

```
load("data/survey/survey_data.RData")
write.csv(dta_receipt_matched, "data/survey/data_receipt_matched.csv", row.names = FALSE)
write.csv(dta_survey_matched, "data/survey/data_survey_matched.csv", row.names = FALSE)
```

  
  
