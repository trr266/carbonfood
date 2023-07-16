# devtools::install_github("joachim-gassen/rdfanalysis")

suppressMessages({
  library(tidyverse)
  library(rdfanalysis)
  library(fixest)
  library(marginaleffects)
  
  source("code/R/exp_utils.R")
})

GCGROUPS <- c("Bedienstete", "Gäste", "Studierende")
ECGROUPS <- c("Faculty and staff", "Guests", "Students")

fdc <- readRDS("data/experiment/dish_choices.rds")
tment_data <- readRDS("data/experiment/tment_data.rds")
dish_plan <- read_csv("data/experiment/dish_plan.csv", show_col_types = FALSE)

avg_co2e100g <- median(dish_plan$co2e100g, na.rm = TRUE)

df1 <- fdc %>% 
  left_join(dish_plan, by = c("eday", "dish"))

base_smp_dish <- df1 %>%
  filter(!is.na(mtfsh)) %>%
  mutate(
    co2eg = (co2e100g*0.5*weight + 0.5*weight*avg_co2e100g) /100,
    n_dishes = 1
  ) %>% 
  select(-co2e100g) %>% 
  group_by(pcard_id) %>%
  arrange(eday, time) %>% 
  mutate(
    first_time = eday == min(eday),
    visits = n_distinct(eday)
  ) %>%
  ungroup() %>%
  left_join(tment_data, by = "eday") %>%
  mutate(
    minute = 60 * time@hour + time@minute - 660,
    qhour = minute %/% 15 + 1,
    customer_group = ECGROUPS[match(customer_group, GCGROUPS)]
  ) 

base_smp_taction <- df1 %>%
  group_by(
    eday, counter, pcard_id, register_id, sheet_id, trans_id, time,
    customer_group
  ) %>%
  summarise(
    to_go = any(to_go),
    dish = ifelse(
      any(!is.na(mtfsh)) & any(is.na(mtfsh)), "Main and Sides",
      ifelse(all(!is.na(mtfsh)), "Main only", "Sides only"
      )),
    co2_md = sum(co2e100g*0.5*weight, na.rm = TRUE),
    co2_mdsd = sum(0.5*weight*avg_co2e100g*(!is.na(mtfsh)), na.rm = TRUE),
    co2_sd = sum(weight*avg_co2e100g*(is.na(mtfsh)), na.rm = TRUE),
    co2eg = (co2_md + co2_mdsd + co2_sd)/100,
    weight = sum(weight),
    mtfsh = any(mtfsh, na.rm = TRUE),
    n_dishes = n(),
    .groups = "drop"
  ) %>% 
  group_by(pcard_id) %>%
  arrange(eday, time) %>% 
  mutate(
    first_time = eday == min(eday),
    visits = n_distinct(eday)
  ) %>%
  ungroup() %>%
  left_join(tment_data, by = "eday") %>%
  mutate(
    minute = 60 * time@hour + time@minute - 660,
    qhour = minute %/% 15 + 1,
    customer_group = ECGROUPS[match(customer_group, GCGROUPS)]
  ) 

dups <- base_smp_taction %>% group_by(eday, register_id, sheet_id, trans_id) %>%
  filter(n() > 1)

if (nrow(dups) > 0) stop (
  "Transcaction level data created duplicates. This should not happen"
)


design <- c(
  "select_sample", "estimate_effects"
)

select_sample <- function(input = NULL, choice = NULL) {
  step_description <- c(
    "## Select experimental sample",
    "### Content",
    "",
    "Reads data, selects the observations for anaylsis and identifies", 
    "the treatment windows." 
  )
  choice_description <- c(
    "### Choice",
    "",
    "`unit_of_observation`: A character value containing one of the",
    "following values:",
    "",
    "- `dish`: The outcome variables are constructed at the main dish choice level.", 
    "Not-treated side dishes are excluded from the analysis.",
    "- `transaction`: The outcome variables are constructed at the transaction level.",
    "Non-treated side dishes are included.",
    "",
    "`edays_include`: A character value containing one of the",
    "following values:",
    "",
    "- `all_days`: Include all experimental days. Please note that this overweights two treatments as these were included in eday 9 and 11.",
    "- `no_eday9`: Exclude the low-powered eday 9. Instead include the replacement eday 11.",
    "- `no_eday11`: Include the low-powered eday 9 and exclude the replacement day.",
    "",
    "`only_first_timers`: A character value containing one of the",
    "following values:",
    "",
    "- `yes`: Limit the sample to observations where the according participant visited the canteen for the first time.",
    "- `no`: Use all observations.",
    "",
    "`use_counter_info`: A character value containing one of the",
    "following values:",
    "",
    "- `yes`: Determine the treatment windows sepearately per counter.",
    "- `no`: Determine identical treatment windows regardless of the counter.",
    "",
    "`limit_twindows_to_stable_choice_sets`: A character value containing one of the",
    "following values:",
    "",
    "- `yes`: Make sure that no choice set changes happened during the", 
    "treatment windows. This reduces the number of observations.",
    "- `yes_no_stop_at_2pm`: Make sure that no choice set changes happened.", 
    "during the treatment windows until 2pm. Include post 2pm observations.",
    "After 2pm, the number of diners decreases and small choice set changes", 
    "become very frequent.",
    "- `no_stop_at_2pm`: Include all observations regardless of choice set", 
    "changes but stop at 2pm.",
    "- `no`: Include all dining observations regardless of choice set changes",
    "",
    "`identify_tment_change`: A character value containing one of the",
    "following values:",
    "",
    "- `on_counter`: Change treatment when diners that",
    "received the new treatment likely reached the check-out registers",
    "(latest point in time)",
    "- `on_info`: Change treatment on when the first CO2 displays were",
    "changed (earliest point in time).",
    "- `median`: Change treatment on the median of all treatment change events",
    "- `exclude`: Exclude all observations within the treatment change period"
  )
  
  # Specify your valid choices below. Format will be checked by test_design()
  # for consistency
  
  choice_type <- list(
    list(name = "unit_of_observation",
         type = "character",
         valid_values = c("dish", "transaction"),
         weights = c(0.5, 0.5)),
    list(name = "edays_include",
         type = "character",
         valid_values = c("all", "no_eday9", "no_eday11"),
         weights = c(0.1, 0.8, 0.1)),
    list(name = "only_first_timers",
         type = "character",
         valid_values = c("yes", "no"),
         weights = c(0.2, 0.8)),
    list(name = "use_counter_info",
         type = "character",
         valid_values = c("yes", "no"),
         weights = c(0.5, 0.5)),
    list(name = "limit_twindows_to_stable_choice_sets",
         type = "character",
         valid_values = c("yes", "yes_no_stop_at_2pm", "no_stop_at_2pm", "no"),
         weights = c(0.8, 0, 0, 0.2)),
    list(name = "identify_tment_change",
         type = "character",
         valid_values = c("on_counter", "on_info", "median", "exclude"),
         weights = c(0.5, 0.1, 0.1, 0.3))
  )
  
  if (is.null(choice)) return(list(
    step_description = step_description,
    choice_description = choice_description,
    choice_type = choice_type
  )) else check_choice(choice, choice_type)
  
  # ___ Analysis code starts below ___
  
  if (choice[[1]] == "dish") df <- input[[1]] else df <- input[[2]]

  if (choice[[2]] == "no_eday9") {df <- df %>% filter(eday != 9)}
  if (choice[[2]] == "no_eday11") {df <- df %>% filter(eday != 11)}
  
  if (choice[[3]] == "yes") {df <- df %>% filter(first_time)} 
  
  if (choice[[4]] == "yes") {
    df <- df %>%
      mutate(
        tment_change_max = ifelse(
          counter == "yellow", tment_change_max_yellow, tment_change_max_red
        ),
        tment_change_min = ifelse(
          counter == "yellow", tment_change_min_yellow, tment_change_min_red
        ) ,
        tment_end = ifelse(counter == "yellow", tment_end_yellow, tment_end_red)
      )
  } else {
    df <- df %>%
      mutate(
        tment_change_max = tment_change,
        tment_change_min = pmin(tment_change_min_yellow, tment_change_min_red)
      )
  } 
  
  df <- df %>% mutate(
    tment_end = ifelse(is.finite(tment_end), tment_end, Inf)
  )
  
  if (choice[[5]] == "yes") {
    df <- df %>% filter(
      minute  >= tment_start, 
      minute < ifelse(is.finite(tment_end), tment_end, 180)
    )
  }
  
  if (choice[[5]] == "yes_no_stop_at_2pm") {
    df <- df %>% filter(minute  >= tment_start, minute < tment_end)
  } 

  if (choice[[5]] == "no_stop_at_2pm") {
    df <- df %>% filter(minute  < 180)
  } 
  
  switch (choice[[6]],
    "on_counter" = df <- df %>% mutate(tment_change = tment_change_max),
    "on_info" = df <- df %>% mutate(tment_change = tment_change_min), 
    "median" = df <- df %>% mutate(
      tment_change = (tment_change_max + tment_change_min)/2
      ), 
    "exclude" = {
      df <- df %>% filter(
        minute < tment_change_min | minute > tment_change_max 
      ) %>% mutate(tment_change = tment_change_max)
    }
  )
  
  smp <- df %>% 
    mutate(
      tslot = ifelse(minute < tment_change, 1, 2),
      tment = factor(ifelse(minute < tment_change, tslot1, tslot2), TMENTS)
    ) %>%
    filter(
      ! dish %in% "Stuffed Peppers",
      !is.na(customer_group)
    ) %>%
    arrange(eday, minute, register_id, trans_id) %>%
    select(
      eday, counter, qhour, minute, pcard_id, register_id, trans_id,
      customer_group, visits, tslot, tment, dish, n_dishes, to_go, 
      mtfsh, weight, co2eg
    )

  return(list(
    data = smp,
    protocol = list(choice)
  ))
}


estimate_effects <- function(input = NULL, choice = NULL) {
  step_description <- c(
    "## Specify models and estimate treatment effects",
    "### Content",
    "",
    "You can select the fixed effect structure, the type of the",
    "standard errors, whether you want to include covariate controls",
    "and the estimtion type for the meat/fish choice."
  )
  choice_description <- c(
    "### Choice",
    "",
    "`mtfsh_model`: A character value containing one of the",
    "following values:",
    "",
    "- `ols`: A linear probability model is being estimated.",
    "- `logit`: A logit model is being estimated",
    "",
    "`dep_vars`: A character value containing one of the",
    "following values:",
    "",
    "- `level`: The non-binary dependent variables `weight` and `co2e` are",
    "estimated at their levels",
    "- `log`: he non-binary dependent variables `weight` and `co2e` are",
    "estimated at their logged values",
    "",
    "`fixed_effects_choice_set`: A character value containing one of the",
    "following values:",
    "",
    "- `none`: No choice set fixed effects are included. Please note that in", 
    "this case the variance in the dish choice sets over experimental days", 
    "will liekly swamp out most of the treament effects.",
    "- `edaycounter`: Fixed effects for each experimental day are included to", 
    "control for the variance in the dish choice sets over experimental days.",
    "If treatments are determined separately for both counters, these are", 
    "interacted with the two counters yielding 20 fixed effects,",
    "",
    "`fixed_effects_time_of_day`: A character value containing one of the",
    "following values:",
    "",
    "- `none`: No time of day fixed effects are included.", 
    "- `tslot`: Treatment slot fixed effects are included.", 
    "- `qhour`: Quarter hour fixed effects are included.", 
    "- `minute`: Minute fixed effects are included.", 
    "- `tslot x wday`: Separate treatement slot fixed effects for each day of", 
    "the week are included.", 
    "- `qhour x wday`: Seperate quarter hour fixed effects for each day of", 
    "the week are included.", 
    "- `minute x wday`: Minute fixed effects for each day of", 
    "the week are included.",
    "",
    "`standard_errors`: A character value containing one of the",
    "following values:",
    "",
    "- `plain`: Do report plain unclustered standard errors",
    "- `robust`: Do report robust unclustered standard errors",
    "- `cluster edaycounter_tslot`: Cluster by choice set (20 clusters,", 
    "or 40 if choiced sets are determined separately for both counters)", 
    "- `cluster edaycounter_qhour`: Cluster by experimental day (times counter) and quarter hour", 
    "(~ 120 cluster or ~ 240 clusters when determined separately by counter)", 
    "- `cluster edaycounter x qhour`: Two-way cluster by experimental day (times counter) and", 
    "quarter hour (10 x ~12 clusters or 20 x ~12 clusters when determined separately by counter)" ,
    "",
    "`controls`: A character value containing one of the",
    "following values:",
    "- `none`: Include no covariate controls (besides the fixed effects)",
    "- `customer_group`: Include indicator variables for the three customer", 
    "groups.",
    "- `returning_customer`: Include indicator variable whether a customer",
    "is a repeated diner",
    "- `nobs`: A count variable capturing the number of dining observations",
    "- `all`: All suitable conrrol variables",
    ""
  )
  
  # Specify your valid choices below. Format will be checked by test_design()
  # for consistency
  
  choice_type <- list(
    list(name = "mtfsh_model",
         type = "character",
         valid_values = c("ols", "logit"),
         weights = c(0.5, 0.5)),
    list(name = "dep_vars",
         type = "character",
         valid_values = c("level", "log"),
         weights = c(0.25, 0.75)),
    list(name = "fixed_effects_choice_set",
         type = "character",
         valid_values = c("none", "edaycounter"),
         weights = c(0.1, 0.9)),
    list(name = "fixed_effects_time_of_day",
         type = "character",
         valid_values = c("none", "tslot", "qhour", "minute", 
                          "tslot x wday", "qhour x wday", "minute x wday"),
         weights = c(0.1, 0.6, 0.1, 0, 0.1, 0.1, 0)),
    list(name = "standard_errors",
         type = "character",
         valid_values = c("plain", "robust", "cluster edaycounter_tslot",
                          "cluster edaycounter_qhour", 
                          "cluster edaycounter x qhour"),
         weights = c(0.1, 0.1, 0.7, 0.1, 0)),
    list(name = "controls",
         type = "character",
         valid_values = c("none", "customer_group", "returning_customer", "nobs", 
                          "all"),
         weights = c(0.5, 0, 0, 0, 0.5))
  )
  
  if (is.null(choice)) return(list(
    step_description = step_description,
    choice_description = choice_description,
    choice_type = choice_type
  )) else check_choice(choice, choice_type)
  
  # ___ Analysis code starts below ___
  
  df <- input$data %>%
    mutate(
      eday_counter = factor(10 * eday + (counter == "yellow")),
      wday = factor(ifelse(eday == 11, 4, eday %% 5))
    )
  
  use_counter <- (input$protocol[[1]]$use_counter_info == "yes")
  
  add_fe <- function(str) {
    fe_str <<- ifelse(fe_str == "", paste("|", str), paste(fe_str, "+", str))
  }
  
  parms <- list(data = df)
  
  if (choice[[1]] == "ols") {
    estfun_mtfsh <- feols
    parms_mtfsh <- list(data = df)
  } else {
    estfun_mtfsh <- feglm
    parms_mtfsh <- list(data = df, family = binomial(link = "logit"))
  }
  
  fstr_mtfsh <- "mtfsh ~ tment "

  if (choice[[2]] == "log") {
    fstr_weight <- "log(weight) ~ tment "
    fstr_co2eg <- "log(co2eg) ~ tment "
  } else {
    fstr_weight <- "weight ~ tment "
    fstr_co2eg<- "co2eg ~ tment "
  }
  
  switch(
    choice[[3]],
    "none" = fe_str <- "", 
    "edaycounter" = fe_str <- ifelse(use_counter, "| eday_counter", "| eday")
  )

  switch(
    choice[[4]],
    "tslot" =  add_fe("tslot"), 
    "qhour" = add_fe("qhour"),
    "minute" = add_fe("minute"),
    "tslot x wday" = add_fe("tslot^wday"),
    "qhour x wday" = add_fe("qhour^wday"),
    "minute x wday" = add_fe("minute^wday")
  )

  switch(
    choice[[5]],
    "plain" = vcov <- "iid", 
    "robust" = vcov <- "hetero", 
    "cluster edaycounter_tslot" = {
      if (use_counter) {vcov <- cluster ~ eday^counter^tslot}
      else {vcov <- cluster ~ eday^tslot}
    },
    "cluster edaycounter_qhour" = {
      if (use_counter) {vcov <- cluster ~ eday^counter^qhour}
      else {vcov <- cluster ~ eday^qhour}
    },
    "cluster edaycounter x qhour" = {
      if (use_counter) {vcov <- cluster ~ eday^counter + qhour}
      else {vcov <- cluster ~ eday + qhour}
    }
  )
  
  switch(
    choice[[6]],
    "none" = ctrls_str <- "",
    "customer_group" = ctrls_str <- "+ customer_group ", 
    "returning_customer" = ctrls_str <- "+ (visits > 1) ", 
    "nobs" = ctrls_str <- "+ visits  ", 
    "all" = ctrls_str <- " + customer_group + (visits > 1) + visits "
  )
    
  fml_mtfsh <- as.formula(paste0(fstr_mtfsh, ctrls_str, fe_str))
  fml_weight <- as.formula(paste0(fstr_weight, ctrls_str, fe_str))
  fml_co2eg <- as.formula(paste0(fstr_co2eg, ctrls_str, fe_str))
  
  suppressMessages({
    mod_mtfsh <- do.call(estfun_mtfsh, c(parms_mtfsh, fml = fml_mtfsh, vcov = vcov))
    mod_weight <- do.call(feols, c(parms, fml = fml_weight, vcov = vcov))
    mod_co2eg <- do.call(feols, c(parms, fml = fml_co2eg, vcov = vcov))
  })
  
  get_ci <- function(mod_str, mult = 1) {
    mod <- get(paste0("mod_", mod_str))
    coef_str <- paste0("tment", TMENTS[2:5])
    if (mod_str == "mtfsh" & choice[[1]] == "logit") {
      rv <- avg_slopes(mod) 
      return(tibble(
        model = mod_str,
        tment = TMENTS[2:5],
        lb = rv$conf.low[rv$term == "tment"],
        est = rv$estimate[rv$term == "tment"],
        ub = rv$conf.high[rv$term == "tment"]
      ))
    }
    tibble(
      model = mod_str,
      tment = TMENTS[2:5],
      lb = confint(mod)[coef_str, 1] * mult,
      est = unname(mod$coefficients[coef_str] * mult),
      ub = confint(mod)[coef_str, 2] * mult
    )
  }
  
  cis <- bind_rows(
    get_ci("mtfsh"),
    get_ci("weight"),
    get_ci("co2eg")
  )
  
  if (choice[[2]] == "log") {
    cis <- cis %>%
      mutate(
        lb = ifelse(model == "mtfsh", lb, exp(lb) - 1),
        est = ifelse(model == "mtfsh", est, exp(est) - 1),
        ub = ifelse(model == "mtfsh", ub, exp(ub) - 1),
      )
  } else {
    cis <- cis %>%
      mutate(
        lb = case_when(
          model == "weight" ~ lb/mean(df$weight), 
          model == "co2eg" ~ lb/mean(df$co2eg), 
          model == "mtfsh" ~lb
        ),
        est = case_when(
          model == "weight" ~ est/mean(df$weight), 
          model == "co2eg" ~ est/mean(df$co2eg), 
          model == "mtfsh" ~est
        ),
        ub = case_when(
          model == "weight" ~ ub/mean(df$weight), 
          model == "co2eg" ~ ub/mean(df$co2eg), 
          model == "mtfsh" ~ub
        )
      )
  }
  
  vn <- expand_grid(
    model = c("mtfsh", "weight", "co2eg"),
    tment = tolower(TMENTS[2:5]),
    coef = c("lb", "est", "ub"),
  )  
  var_names = paste(vn$model, vn$tment, vn$coef, sep = "_")
  cis_wide <- cis %>% 
    pivot_wider(
      names_from = c(model, tment), values_from = c(lb, est, ub),
      names_glue = "{model}_{tolower(tment)}_{.value}", names_sort = TRUE
    ) %>%
    select(all_of(var_names))

  lh <- construct_lin_hypotheses(df)
  
  get_hypotheses_ests <- function(mod_str) {
    get_hypothesis_est <- function(mod_str, no) {
      mod <- get(paste0("mod_", mod_str))
      rv <- test_hypothesis(mod, lh[no]) %>% select(lb, est, ub, pvalue)
      if (mod_str == "mtfsh" & choice[[1]] == "logit") {
        if (fe_str == "") {
          mn_fe <- mod$coeftable[1,1] 
        } else {
          fes <- fixef(mod)
          mn_fe <- mean(unlist(lapply(fes, mean)))
        }
        exp_coefs <- function(c) {
          exp(mn_fe + c)/(1 + exp(mn_fe + c)) - exp(mn_fe)/(1 + exp(mn_fe)) 
        }
        rv <- tibble(
          lb = exp_coefs(rv$lb),
          est = exp_coefs(rv$est),
          ub = exp_coefs(rv$ub),
          pvalue = rv$pvalue
        )
      } else {
        if (mod_str != "mtfsh") {
          if (choice[2] == "level") {
            if (mod_str == "weight") avg_y <- mean(df$weight)
            else avg_y <- mean(df$co2eg)
            rv <- rv %>% mutate(
              lb = lb/avg_y,
              est = est/avg_y,
              ub = ub/avg_y
            )
          } else {
            rv <- rv %>% mutate(
              lb = exp(lb) - 1,
              est = exp(est) - 1,
              ub = exp(ub) - 1
            )
          }
        }        
      }
      
      names(rv) <- paste0(mod_str, "_h", no, "_", names(rv))
      rv
    }
    
    bind_cols(
      get_hypothesis_est(mod_str, 1),
      get_hypothesis_est(mod_str, 2),
      get_hypothesis_est(mod_str, 3),
      get_hypothesis_est(mod_str, 4)
    )
  }
  
  tests_wide <- bind_cols(
    get_hypotheses_ests("mtfsh"),
    get_hypotheses_ests("weight"),
    get_hypotheses_ests("co2eg")
  )
  
  results <- bind_cols(cis_wide, tests_wide)

  protocol <- input$protocol
  protocol[[length(protocol) + 1]] <-  choice
  
  return(list(
    data = results,
    protocol = protocol,
    models = list(
      mod_mtfsh = mod_mtfsh, mod_weight = mod_weight, mod_co2eg = mod_co2eg
    )
  ))
}


if (FALSE) {
  # Code for a single design choice plus documentation and testing
  
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
  
  smp_step <- select_sample(
    list(base_smp_dish, base_smp_taction), 
    choice = PREREG_CHOICES[1:6]
  )
  
  smp <- smp_step$data
  
  est_step <- estimate_effects(smp_step, choice = PREREG_CHOICES[7:12])
  
  summary(est_step$models[[3]])
  
  test_design(design, input = list(base_smp_dish, base_smp_taction))
}
