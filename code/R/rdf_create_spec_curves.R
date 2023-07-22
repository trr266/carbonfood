# devtools::install_github("joachim-gassen/rdfanalysis")

suppressMessages({
  library(tidyverse)
  library(rdfanalysis)
})

PREREG_CHOICES <- list(
  unit_of_observation = "dish",
  edays_include = "no_eday9",
  only_first_timers = "no",
  use_counter_info = "no",
  limit_twindows_to_stable_choice_sets = "yes",
  identify_tment_change = "on_counter",
  mtfsh_model = "logit",
  dep_vars = "log",
  fixed_effects_choice_set = "edaycounter",
  fixed_effects_time_of_day = "tslot",
  standard_errors = "cluster edaycounter_tslot",
  controls = "none"
)

rdf_outcomes <- readRDS("data/generated/rdf_outcomes.rds")

df <- rdf_outcomes %>% filter(
  standard_errors == "cluster edaycounter_tslot",
  fixed_effects_choice_set == "edaycounter",
  fixed_effects_time_of_day == "tslot",
  mtfsh_model == "logit"
) %>%
  select(
    -standard_errors, -fixed_effects_choice_set, -fixed_effects_time_of_day,
    -mtfsh_model
  )
attr(df, "choices") <- 1:8

PRE_REG_DC = PREREG_CHOICES[c(1:6, 8, 12)]

labels_dc <- rev(c(
  "Observational unit: dishes",
  "Observational unit: transactions",
  "Include all experimental days",
  "Exclude day 9, include day 11",
  "Include day 9, exclude day 11",
  "Limit sample to first-time diners",
  "Include also repeat diners",
  "Determine treatment times separately by counter",
  "Common treatment times for both counters",
  "Limit treatment times to stable food choices",
  "Full treatment windows with varying food choices",
  "Treatment change determined by point of sale",
  "Treatment change determined by info change",
  "Treatment change by median of pos and info",
  "Treatment change period excluded",
  "CO₂e as dependent variable",
  "log(CO₂e) as dependent variable",
  "No additional control variables in regression",
  "Additional control variables in regression"
))


create_spec_curve <- function(df, h) {
  suppressWarnings(plot_rdf_spec_curve(
    df, sprintf("co2eg_h%d_est", h), 
    sprintf("co2eg_h%d_lb", h), sprintf("co2eg_h%d_ub", h), 
    highlight = data.frame(PRE_REG_DC), pt_size_highlight = 2,
    addon_sc = list(
      labs(y = ""),
      scale_y_continuous(label = scales::label_percent()),
      scale_x_continuous(expand = c(NA, 0))
    ),
    addon_dc = list(
      scale_y_discrete(labels = labels_dc),
      labs(x = "Research Design Variants"),
      scale_x_continuous(expand = c(NA, 0))
    ),
    file = sprintf(paste0(ofolder, "/fig4_panel_h%d.svg"), h), 
    width = 8, height = 10 
  ))
  message(
    sprintf(
      "Average est: %.3f, %% sign. neg: %.1f, sig. pos: %.1f", 
      mean(df[, sprintf("co2eg_h%d_est", h)]),
      100*mean(df[, sprintf("co2eg_h%d_ub", h)] <  0),
      100*mean(df[, sprintf("co2eg_h%d_lb", h)] >  0)
    )
  )
}

dir.create(ofolder <- tempfile())

create_spec_curve(df, 1)
create_spec_curve(df, 2)
create_spec_curve(df, 3)
create_spec_curve(df, 4)

projectwd <- getwd()
setwd(ofolder)
zip(
  paste0(projectwd, "/output/rdf_spec_curves.zip"), 
  list.files()
)
setwd(projectwd)

message(sprintf("%s: Done", Sys.time()))
