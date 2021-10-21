### Some Codebook Rules

As said in 'data_readme.md', external data should always be accompanied by a data codebook. Data codebooks vary according to the nature of the data that they are subscribed. Normally, they a organized as a (nested) table with a row for each variable in the data and contain at least the following information

- The variable name
- A short explanatory label that can be used in tables etc.
- The nature of the data (factorial, numerical(integer, real), datetime, logical)
- For factorial variables:
    - For string codes: List of strings that identify the factors, definitions what they mean if they are not self-explanatory
    - For numerical codes: A translation of numerical codes into their meaning
    - Number of codes
    - Some rough descriptives (e.g., frequency counts per code)
- For numerical variables
    - The unit of measurement (Currency, physical units)
    - The range
    - Where appropriate: The precision of measurement
- For datetime variables
    - Information on how datetime strings are formatted 
    - For time values: Timezone?
- If appropriate: What identifies a missing data point and what does this imply?
- A variable definition that explains in clear and concise language what the variable represents
- Source where the data have been collected
- If appropriate: A time for which the data can be assumed to be correct


### Further Information

Data management and maintenance can be a very daunting task and there are various initiatives, institutions and service providers in this field. Most of them are more or less domain-specific. The [Guide to Social Science Data Preparation and Archiving](https://www.icpsr.umich.edu/files/deposit/dataprep.pdf) published by the Inter-university Consortium for Political and Social Research (ICPSR) might be a good starting point for those interested in social science data
