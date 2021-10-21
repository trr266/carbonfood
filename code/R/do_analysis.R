library(dplyr)
library(ggplot2)
library(ExPanDaR)

load("data/generated/sample.rda")

fig_scatter <- ggplot(
  smp, aes(x = ln_gdp_capita, y = life_expectancy, color = region)
) +
  geom_point(alpha = 0.3) +
  labs(
    color = "World Bank Region",
    x = "Ln(Income per capita in thsd. 2010 US-$)",
    y = "Life expectancy in years"
  ) +
  theme_minimal()

tab_desc_stat <- prepare_descriptive_table(
  smp %>% select(-year, -ln_gdp_capita)
)

tab_corr <- prepare_correlation_table(
  smp %>% select(-year, -gdp_capita),
  format = "latex", booktabs = TRUE, linesep = ""
)

tab_regression <-  prepare_regression_table(
  smp,
  dvs = rep("life_expectancy", 4),
  idvs = list(
    c("ln_gdp_capita"),
    c("ln_gdp_capita", "unemployment"),
    c("ln_gdp_capita", "unemployment"),
    c("ln_gdp_capita", "unemployment")
  ),
  feffects = list("", "", "year", c("country", "year")),
  cluster = list("", "",  "year", c("country", "year")),
  format = "latex"
)


save(
  list = c(ls(pattern = "fig_*"), ls(pattern = "tab_*")),
  file = "output/results.rda"
)
