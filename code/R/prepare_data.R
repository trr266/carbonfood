# --- Header -------------------------------------------------------------------
# Prepares a sample based on the pulled World Bank data
#
# (C) TRR 266 -  See LICENSE file for details
# ------------------------------------------------------------------------------
library(dplyr)
library(tidyr)

load("data/pulled/worldbank.rda")

smp <- worldbank_panel %>%
  mutate(ln_gdp_capita = log(gdp_capita)) %>%
  drop_na()

smp_def <-  bind_rows(
    worldbank_def,
    tibble(
      var_name = "ln_gdp_capita",
      var_def = "Natural log of gdp_capita",
      type = "numeric"
    )
  )

# As the below will be used in Latex, make sure that special characters
# are properly escaped

smp_def$label <-  c(
  "Country (ISO3c)",
  "Country (ISO32)",
  "Country",
  "World Bank Geographic Region",
  "World Bank Income Group",
  "Calendar Year",
  "National Income per Capita",
  "Unemployment (in \\%)",
  "Life Expectancy (in years)",
  "ln(National Income per Capita)"
)

save(smp, smp_def, file = "data/generated/sample.rda")
