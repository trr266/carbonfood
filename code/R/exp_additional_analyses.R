# ------------------------------------------------------------------------------
# Some additional experimental analysis (as announced in the prereg report)
# ------------------------------------------------------------------------------

suppressMessages({
  library(tidyverse)
  library(fixest)
  library(knitr)
  library(kableExtra)
  library(modelsummary)
  library(ggridges)
  library(emojifont)
  
  load.fontawesome()
  source("code/R/exp_utils.R")
  
  devtools::source_url(
    "https://raw.githubusercontent.com/trr266/treat/main/code/R/theme_trr.R"
  )
})


run_add_analyses <- function(unit_obs) {
  dir.create(ofolder <- tempfile())
  
  smp <- readRDS(paste0("data/generated/exp_sample_", unit_obs, ".rds"))
  
  df <- smp %>%
    mutate(
      edayqhour = 100*eday + qhour,
      edaytslot = 10*eday + tslot,
      wday = ifelse(eday == 11, 4, eday %% 5) ,
      customer_group = factor(
        customer_group, 
        c("Students", "Faculty and staff", "Guests")
      ),
      repeated_diner = visits > 1 
    ) %>%
    mutate(day = ifelse(eday == 11, 9, eday)) %>%
    rename(
      food_weight = weight, meat_fish = mtfsh, co2e = co2eg,
      slot = tslot
    ) 
  
  
  df <- df %>%
    mutate(
      dayslot = factor(day*10 + slot),
      day = as.factor(day), slot = as.factor(slot)
    )
  
  # --- Slot descriptives ------------------------------------------------------
  
  tab <- kable(
    df %>% group_by(dayslot) %>%
      summarise(
        tment = unique(tment),
        diners = n(),
        meat_fish = mean(meat_fish),
        mn_food_weight = mean(food_weight),
        sd_food_weight = sd(food_weight),
        min_food_weight = min(food_weight),
        max_food_weight = max(food_weight),
        mn_co2e = mean(co2e),
        sd_co2e = sd(co2e),
        min_co2e = min(co2e),
        max_co2e = max(co2e)
      ), digits = c(0, 0, 0, 3, rep(0, 8)), format.args = list(big.mark = ",")
  ) %>% kable_styling() 
  
  cat (
    tab, 
    file = paste0(ofolder, "/exp_t2p2_descriptives_daytslot_", unit_obs, ".html")
  )
  
  
  # --- Additional statistics for main results ---------------------------------
  
  weight_mod <- feols(
    log(food_weight) ~ tment| day + slot, 
    df, cluster = c("dayslot")
  )
  mtfsh_mod <- femlm(
    meat_fish ~ tment| day + slot, 
    family = "logit", df, cluster = c("dayslot")
  )
  mtfsh_mod_ols <- feols(
    meat_fish ~ tment| day + slot, 
    df, cluster = c("dayslot")
  )
  co2e_mod <- feols(
    log(co2e) ~ tment| day + slot, 
    df, cluster = c("dayslot")
  )
   
  tab <- exp(cbind(coef(co2e_mod), confint(co2e_mod))) - 1
  cat(
    kable(tab),
    file = paste0(
      ofolder, "/exp_t3_add_analyses_exp_cis_reg_co2e_", unit_obs, ".html"
    )
  )
  
  tab <- exp(cbind(coef(weight_mod), confint(weight_mod))) - 1
  cat(
    kable(tab),
    file = paste0(
      ofolder, "/exp_t3_add_analyses_exp_cis_reg_weight_", unit_obs, ".html"
    )
  )
  
  mn_mf <- mean(df$meat_fish)
  b0 <- log(mn_mf/(1- mn_mf))
  lexp <- function(c) {exp(b0 + c)/(1 + exp(b0 + c))}
  
  tab <- lexp(cbind(coef(mtfsh_mod), confint(mtfsh_mod))) - mn_mf
  cat(
    kable(tab),
    file = paste0(
      ofolder, "/exp_t3_add_analyses_exp_cis_reg_mtfsh_", unit_obs, ".html"
    )
  )
  
  
  lh <- construct_lin_hypotheses(df)
  exp_cis_hypotheses <- function(h, mod) {
    hr <- test_hypothesis(mod, lh[h])
    cbind(exp(hr[,1:3]) - 1, hr[,4])
  }
  tab <- bind_rows(lapply(c("h1", "h2", "h3", "h4"), exp_cis_hypotheses, mod = co2e_mod))
  cat(
    kable(tab),
    file = paste0(
      ofolder, "/exp_t3_add_analyses_exp_cis_hyp_co2e_", unit_obs, ".html"
    )
  )
  
  tab <- bind_rows(lapply(c("h1", "h2", "h3", "h4"), exp_cis_hypotheses, mod = weight_mod))
  cat(
    kable(tab),
    file = paste0(
      ofolder, "/exp_t3_add_analyses_exp_cis_hyp_weight_", unit_obs, ".html"
    )
  )
  
  lexp_cis_hypotheses <- function(h, mod) {
    hr <- test_hypothesis(mod, lh[h])
    cbind(lexp(hr[,1:3]) - mn_mf, hr[,4])
  }
  tab <- bind_rows(lapply(c("h1", "h2", "h3", "h4"), lexp_cis_hypotheses, mod = mtfsh_mod))
  cat(
    kable(tab),
    file = paste0(
      ofolder, "/exp_t3_add_analyses_exp_cis_hyp_mtfsh_", unit_obs, ".html"
    )
  )
  
  # --- Regressions with controls ----------------------------------------------
  
  mtfsh_modc <- femlm(
    meat_fish ~ tment + customer_group + repeated_diner + visits| day + slot, 
    family = "logit", df, cluster = c("dayslot")
  )
  mtfsh_modc_ols <- feols(
    meat_fish ~ tment + customer_group + repeated_diner + visits| day + slot, 
    df, cluster = c("dayslot")
  )
  weight_modc <- feols(
    log(food_weight) ~ tment + customer_group + repeated_diner + visits | 
      day + slot, 
    df, cluster = c("dayslot")
  )
  co2e_modc <- feols(
    log(co2e) ~ tment + customer_group + repeated_diner + visits| day + slot, 
    df, cluster = c("dayslot")
  )
  
  tab <- modelsummary(
    list(
      meat_fish_logit = mtfsh_modc, weight = weight_modc, co2e = co2e_modc
    ), stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1)
  )
  
  cat(
    tab, 
    file = paste0(
      ofolder, "/exp_t4p1a_add_analyses_controls_reg_", unit_obs, ".html"
    )
  )
  
  hresultsc <- create_hypotheses_table(df, mtfsh_modc, weight_modc, co2e_modc)
  
  cat(
    hresultsc$tab, 
    file = paste0(
      ofolder, "/exp_t4p1b_add_analyses_controls_hyp_tests_", unit_obs, ".html"
    )
  )
  
  
  # --- Sub sample regressions -------------------------------------------------
  
  sdf <- df %>% filter(first_time)
  
  mtfsh_modf <- femlm(
    meat_fish ~ tment | day + slot, 
    family = "logit", sdf, cluster = c("dayslot")
  )
  mtfsh_modf_ols <- feols(
    meat_fish ~ tment | day + slot, 
    sdf, cluster = c("dayslot")
  )
  weight_modf <- feols(
    log(food_weight) ~ tment  | day + slot, 
    sdf, cluster = c("dayslot")
  )
  co2e_modf <- feols(
    log(co2e) ~ tment | day + slot, 
    sdf, cluster = c("dayslot")
  )
  
  tab <- modelsummary(
    list(
      meat_fish_logit = mtfsh_modf, weight = weight_modf, co2e = co2e_modf
    ), stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1)
  )
  
  cat(
    tab, 
    file = paste0(
      ofolder, "/exp_t4p2a_add_analyses_fst_time_diners_reg_", unit_obs, ".html"
    )
  )
  
  hresultsf <- create_hypotheses_table(sdf, mtfsh_modf, weight_modf, co2e_modf)
  
  cat(
    hresultsf$tab, 
    file = paste0(
      ofolder, "/exp_t4p2b_add_analyses_fst_time_diners_hyp_tests_", 
      unit_obs, ".html"
    )
  )

  
  # Save all results for later use
  
  save(
    weight_modc, mtfsh_modc, co2e_modc, hresultsc, 
    weight_modf, mtfsh_modf, co2e_modf, hresultsf, 
    file = paste0(ofolder, "/exp_additional_results_", unit_obs, ".rdata")
  )
  
  projectwd <- getwd()
  setwd(ofolder)
  zip(
    paste0(projectwd, "/output/exp_additional_output_", unit_obs, ".zip"), 
    list.files()
  )
  setwd(projectwd)
}

run_add_analyses("dish")
run_add_analyses("taction")