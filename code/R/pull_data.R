# ------------------------------------------------------------------------------
# This pulls some World Bank data
# (c) TRR 266 - Read LICENSE for details
# ------------------------------------------------------------------------------

library(dplyr)
library(wbstats)

pull_worldbank_data <- function(vars) {
  new_cache <- wb_cache()
  all_vars <- as.character(unique(new_cache$indicators$indicator_id))
  data_wide <- wb_data(indicator = vars, mrv = 70, return_wide = TRUE)
  new_cache$indicators[new_cache$indicators$indicator_id %in% vars, ] %>%
    rename(var_name = indicator_id) %>%
    mutate(var_def = paste(indicator, "\nNote:",
                           indicator_desc, "\nSource:", source_org)) %>%
    select(var_name, var_def) -> wb_data_def

  new_cache$countries %>%
    select(iso3c, iso2c, country, region, income_level) -> ctries

  left_join(data_wide, ctries, by = "iso3c") %>%
    rename(year = date,
           iso2c = iso2c.y,
           country = country.y) %>%
    select(iso3c, iso2c, country, region, income_level, everything()) %>%
    select(-iso2c.x, -country.x) %>%
    filter(!is.na(NY.GDP.PCAP.KD),
           region != "Aggregates") -> wb_data

  wb_data$year <- as.numeric(wb_data$year)

  wb_data_def<- left_join(data.frame(var_name = names(wb_data),
                                     stringsAsFactors = FALSE),
                          wb_data_def, by = "var_name")
  wb_data_def$var_def[1:6] <- c(
    "Three letter ISO country code as used by World Bank",
    "Two letter ISO country code as used by World Bank",
    "Country name as used by World Bank",
    "World Bank regional country classification",
    "World Bank income level classification",
    "Calendar year of observation"
  )
  wb_data_def$type = c(rep("cs_id", 3), rep("factor",  2), "ts_id",
                       rep("numeric", 3))
  return(list(wb_data, wb_data_def))
}

message(sprintf("Pulling World Bank data: %s", Sys.time()))

vars <- c("SP.DYN.LE00.IN", "NY.GDP.PCAP.KD", "SL.UEM.TOTL.ZS")
var_names <- c("gdp_capita", "unemployment", "life_expectancy")

wb_list <- pull_worldbank_data(vars)
worldbank_panel <- wb_list[[1]]
worldbank_def <- wb_list[[2]]

colnames(worldbank_panel)[c(7:9)] <- var_names
worldbank_def$var_name[c(7:9)] <- var_names

save(worldbank_panel, worldbank_def, file = "data/pulled/worldbank.rda")

message("done")
