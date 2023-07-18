# devtools::install_github("joachim-gassen/rdfanalysis")

suppressMessages({
  source("code/R/rdf_design.R")
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

create_scp_list <- function(sc) {
  rv <- as.list(paste0(sc, c("_lb", "_est", "_ub")))
  names(rv) <- c("lb", "est", "ub")
  sc_parts <- str_split_fixed(sc, fixed("_"), 2)
  rv$title <- sprintf("Effect of %s on %s", sc_parts[,2], sc_parts[,1])
  rv$est_label <- ""
  rv$label <- rv$title
  rv$addon_sc = scale_y_continuous(label = scales::label_percent())
  rv
}

spec_curves <- 
  unique(str_remove(
    names(rdf_outcomes)[-attr(rdf_outcomes, "choices")][-1],
    "(_lb|_est|_ub|_pvalue)"
  ))

scparms <- lapply(spec_curves, create_scp_list)
names(scparms) <- spec_curves

my_model_render_func <- function(m) {
  sc <- input$selected_spec_curve
  mod <- str_split_fixed(sc, fixed("_"), 2)
  coi <- mod[, 2]
  
  dep_var <- mod[, 1]
  mod_pos <- match(dep_var, c("mtfsh", "weight", "co2eg"))
  coi_pos <- match(
    coi, c("co2neutral", "co2colorcoded", "budgetcolorcoded", "moneycolorcoded")
  )*2
  if (identical(names(m), c("mod_mtfsh", "mod_weight", "mod_co2eg")))
    rv <- m[[mod_pos]]
  else rv <- lapply(m, function(x) x[[mod_pos]])

  names(rv) <- unlist(lapply(
    rv, 
    function(x) as.character(x$fml)[2]
  ))
  
  max_coefs <- max(unlist(lapply(rv, function(x) length(coef(x)))))
  
  htests <- c(
    paste0(dep_var, "_h1"), paste0(dep_var, "_h2"),
    paste0(dep_var, "_h3"), paste0(dep_var, "_h4")
  )
  htests_pvalues <- as_tibble(t(plot_df()[, paste0(htests, "_pvalue")]))
  htests_est <- as_tibble(t(plot_df()[, paste0(htests, "_est")]))
  htests_est[] <- lapply(
    seq_len(ncol(htests_est)), 
    function(x) star(htests_est %>% pull(x), htests_pvalues %>% pull(x))
  )
  names(htests_est) <- paste0("mod", 1:length(rv))
  
  exc_choices <- c(
    "dep_vars", "controls"
  )
  choices <- names(data)[attr(data, "choices")]
  incl_choices <- choices[!choices %in% exc_choices] 
  add_rows_choices <- as_tibble(t(plot_df()[, incl_choices]))
  names(add_rows_choices) <- paste0("mod", 1:length(rv))

  mod_add_rows <- tibble(bind_cols(col1 = c(
    "H1 (Main Effect)", "H2 (Contextualization)", 
    "H3 (Budget Context)", "H4 (Color Coding)"
  ), htests_est, .name_repair = "unique") %>%
    bind_rows(bind_cols(col1 = incl_choices, add_rows_choices, .name_repair = "unique")))
  
  add_rows_start <- max_coefs*2 + 1
  add_rows_end <- add_rows_start + nrow(mod_add_rows) - 1
  attr(mod_add_rows, 'position') <- add_rows_start:add_rows_end

  coi_pos <- case_when(
    coi == "h1" ~ add_rows_start,
    coi == "h2" ~ add_rows_start + 1,
    coi == "h3" ~ add_rows_start + 2,
    coi == "h4" ~ add_rows_start + 3,
    TRUE ~ coi_pos
  )
    
  tab <- modelsummary(
    rv, estimate = "{estimate}{stars}", output = "kableExtra",
    coef_map = c(
      "tmentCO2Neutral" = "CO2 Neutral",
      "tmentCO2ColorCoded" = "CO2 Color Coded",
      "tmentBudgetColorCoded" = "Budgetary Context",
      "tmentMoneyColorCoded" = "Monetary Context",
      "customer_groupGuests" = "Diner is Guest",
      "customer_groupStudents" = "Diner is Student",
      "visits > 1TRUE" = "Repeated Diner",
      "visits" = "Number of Visits"
    ),
    gof_map = list(
      list(raw = "adj.r.squared", clean = "Adjusted R2", fmt = 3),
      list(
        raw = "nobs", clean = "Number of observations", 
        fmt = function(x) format(x, big.mark=",")
      )
    ),
    add_rows = mod_add_rows,
    stars = c('*' = .1, '**' = 0.05, '***' = .01)
  )  %>% 
    kableExtra::row_spec(coi_pos, background = 'lightblue') 
  
  if (!str_detect(coi, "^h")) tab <- tab %>% 
    kableExtra::row_spec(coi_pos - 1, background = 'lightblue') 
  
  tab_html <- tab %>%
    str_replace_all(fixed("box-shadow: 0px 1px"), "") 
    
  htmltools::HTML(tab_html) 
}

choice_labels <- c(
  "What should be the unit of observation?",
  "Which experiment days should be included?",
  "Should the data be limited to first-time diners?",
  "Should the treatment slots be determined separately for both canteen counters?",
  "Should the treatment slots be limited to stable choice sets?",
  "How should the treatment change be identified?",
  "How should the food choice model be estimated?",
  "How should the continuous dependent variable models be estimated?",
  "Which fixed effects should be included to capture choice set variation?",
  "Which fixed effect should be included to capture day-time effects?",
  "How should the standard errors be clustered?",
  "Do you want to include control variables?"
)

abstract <- read_file("doc/rdf_display_abstract.txt")

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

shiny_rdf_spec_curve(
  title = "Do CO\u2082 labels affect consumer choice?", 
  abstract = abstract,
  libs = c("tidyverse", "lubridate", "fixest", "marginaleffects"),
  choice_labels = choice_labels,
  rdf_outcomes, default_choices = PREREG_CHOICES,
  design = design, start_input = list(base_smp_dish, base_smp_taction),  
  model_render_func = my_model_render_func,
  restore_button = TRUE, with_spinner = TRUE,
  spec_curve_parms = scparms, spec_curve_selected = "co2eg_moneycolorcoded"
)


