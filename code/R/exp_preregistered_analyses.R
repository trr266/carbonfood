# ------------------------------------------------------------------------------
# This code follows the protocol that we preregister with JAR line by line
# It is mostly a copy & past exercise building of the relevant parts of
# doc/power_analysis.Rmd
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
  
#  devtools::source_url(
#    "https://raw.githubusercontent.com/trr266/treat/main/code/R/theme_trr.R"
#  )
})

# Style color and theme of ridge plots
style_ridges_plot <- function(p) {
  p + 
    scale_color_manual(
      values = c("ColorCoded" = "limegreen", "Neutral" = "black"),
      guide = "none"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

prepare_prereg_analyses <- function(unit_obs) {
  save_plot <- function(name, h, w){
    ggsave(paste0(name, "_", unit_obs, ".svg"), height = h, width = w, units = "in")
    ggsave(paste0(name, "_", unit_obs, ".pdf"), height = h, width = w, units = "in")
    ggsave(paste0(name, "_", unit_obs, ".png"), height = h, width = w, units = "in")
  }
  
  dir.create(ofolder <- tempfile())
  
  smp <- readRDS(paste0("data/generated/exp_sample_", unit_obs, ".rds"))
  tplan <- readRDS("data/generated/old/tment_data.rds") %>%
    select(eday, cday, tslot1, tslot2, exp_eff_size) %>%
    mutate(day = c(1:8, NA, 10, 9)) %>% filter(eday != 9) %>% arrange(day)
  
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
    rename(
      food_weight = weight, day = eday, meat_fish = mtfsh, co2e = co2eg,
      slot = tslot
    ) 
  
  tab <- kable(
    df %>% group_by(day) %>%
      summarise(
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
      ), digits = c(0, 0, 3, rep(0, 8)), format.args = list(big.mark = ",")
  ) %>% kable_styling() 
  
  cat(tab, file = paste0(ofolder, "/exp_t2_descriptives_", unit_obs, ".html"))
  
  if (FALSE) {
    # Some additional descriptives
    ggplot(df) + geom_density(aes(x  = food_weight, color = meat_fish))+
      scale_color_trr266_d() +
      theme_trr() +
      theme(legend.position = "bottom")
    
    ggplot(df) + geom_density(aes(x  = co2e, color = meat_fish)) + 
      scale_color_trr266_d() +
      scale_x_log10() +
      theme_trr() +
      theme(legend.position = "bottom")
    
    modelsummary(
      feols(log(co2e) ~ food_weight + meat_fish | day + slot, data = df),
      stars = TRUE
    )
  }
  
  df <- df %>%
    mutate(
      dayslot = factor(day*10 + slot),
      day = as.factor(day), slot = as.factor(slot)
    )
  
  df_viz <- df %>% 
    mutate(
      day_tment = factor(
        (as.numeric(day) - 1) * 3+ 
          as.numeric(slot),
        levels = 1:29
      ),
      color_cond = ifelse(str_detect(tment, "Color"), "ColorCoded", "Neutral")
    )
  
  df_viz2 <- df_viz %>% 
    group_by(day, tment, color_cond) %>%
    summarise(
      meat_fish = mean(meat_fish),
      .groups = "drop"
    ) %>%
    mutate(
      label = case_when(
        tment == "NoInfo" ~  fontawesome("fa-question"),
        tment == "CO2Neutral" ~ fontawesome("fa-cloud"),
        tment == "CO2ColorCoded" ~ fontawesome("fa-cloud"),
        tment == "BudgetColorCoded" ~ fontawesome("fa-pie-chart"),
        tment == "MoneyColorCoded" ~ fontawesome("fa-eur")
      ),
      nudge_left = case_when(
        day == 1 & tment == "NoContext" ~ TRUE,
        TRUE ~ FALSE
      ),
      nudge_right = case_when(
        day == 1 & tment == "NoContext" ~ TRUE,
        TRUE ~ FALSE
      )
    )

  fig <- ggplot() + 
    geom_text(
      data = df_viz2 %>% filter(!nudge_left & !nudge_right),
      aes(x = day, y = meat_fish, color = color_cond, label = label), 
      family='fontawesome-webfont', size = 6
    ) +
    geom_text(
      data = df_viz2 %>% filter(nudge_left),
      aes(x = day, y = meat_fish, color = color_cond, label = label), 
      family='fontawesome-webfont', size = 6,
      position = position_nudge(x = -0.15)
    ) +
    geom_text(
      data = df_viz2 %>% filter(nudge_right),
      aes(x = day, y = meat_fish, color = color_cond, label = label), 
      family='fontawesome-webfont', size = 6,
      position = position_nudge(x = +0.15)
    ) +
    scale_color_manual(
      values = c("ColorCoded" = "limegreen", "Neutral" = "black"),
      guide = "none"
    ) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) + 
    labs(x = "Day", y = "Share of meat or fish dishes") +
    theme_minimal()
  
  save_plot(paste0(ofolder, "/exp_f1_food_choice"), 6, 6)
  
  ylabs <- NULL
  for (i in 1:10) {
    tp <- tplan %>% filter(day == i)
    tments_str <- c(tp$tslot1, tp$tslot2)
    ylabs <- c(ylabs, tments_str)
    if (i < 10) ylabs <- c(ylabs, "")
  }
  
  xlabs <- case_when(
    ylabs == "NoInfo" ~  fontawesome("fa-question"),
    ylabs == "CO2Neutral" ~ fontawesome("fa-cloud"),
    ylabs == "CO2ColorCoded" ~ fontawesome("fa-cloud"),
    ylabs == "BudgetColorCoded" ~ fontawesome("fa-pie-chart"),
    ylabs == "MoneyColorCoded" ~ fontawesome("fa-eur"),
    TRUE ~ ""
  )
  
  fig <- ggplot(df_viz, aes(x = food_weight, color = color_cond, y = day_tment)) + 
    geom_density_ridges(fill = NA, ) + 
    labs(y = "", x = "Food purchased [kg]") +
    scale_y_discrete(labels = ylabs, drop = FALSE) + 
    scale_x_log10(breaks = c(250, 500, 750))
  
  suppressMessages({
    style_ridges_plot(fig)
    save_plot(paste0(ofolder, "/exp_f2a_food_weight"), 8, 6)
  })

  fig <- ggplot(df_viz, aes(x = day_tment, color = color_cond, y = food_weight/1000)) + 
    geom_boxplot(outlier.shape = NA) + 
    ggbeeswarm::geom_beeswarm(alpha = 0.1, size = 0.1, cex = 0.2) +
    labs(x = "", y = "Food purchased [kg]") +
    scale_x_discrete(labels = xlabs, drop = FALSE) + 
    scale_y_log10() + 
    theme_minimal() +
    scale_color_manual(
      values = c("ColorCoded" = "limegreen", "Neutral" = "black"),
      guide = "none"
    )  +
    theme(
      axis.text.x = element_text(family='fontawesome-webfont', size = 8),
      panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()
    )
  
  suppressMessages({
    save_plot(paste0(ofolder, "/exp_f2b_food_weight"), 5, 10)
  })
  
    
  fig <- ggplot(df_viz, aes(x = co2e/1000, color = color_cond, y = day_tment)) + 
    geom_density_ridges(fill = NA) + 
    labs(y = "", x = "Carbon footprint [kgCO\u2082e]") +
    scale_y_discrete(labels = xlabs, drop = FALSE) + 
    scale_x_log10()
  
  suppressMessages({
    style_ridges_plot(fig)
    save_plot(paste0(ofolder, "/exp_f3a_food_co2e"), 8, 6)
  })
  
  fig <- ggplot(df_viz, aes(x = day_tment, color = color_cond, y = co2e/1000)) + 
    geom_boxplot(outlier.shape = NA) + 
    ggbeeswarm::geom_beeswarm(alpha = 0.1, size = 0.1, cex = 0.2) +
    labs(x = "", y = "Carbon footprint [kgCO\u2082e]") +
    scale_x_discrete(labels = xlabs, drop = FALSE) + 
    scale_y_log10() + 
    theme_minimal() +
    scale_color_manual(
      values = c("ColorCoded" = "limegreen", "Neutral" = "black"),
      guide = "none"
    )  +
    theme(
      axis.text.x = element_text(family='fontawesome-webfont', size = 8),
      panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()
    )
  
  suppressMessages({
    save_plot(paste0(ofolder, "/exp_f3b_food_co2e"), 5, 10)
  })
  
  mtfsh_mod <- femlm(
    meat_fish ~ tment| day + slot, 
    family = "logit", df, cluster = c("dayslot")
  )
  mtfsh_mod_ols <- feols(
    meat_fish ~ tment| day + slot, 
    df, cluster = c("dayslot")
  )
  weight_mod <- feols(
    log(food_weight) ~ tment| day + slot, 
    df, cluster = c("dayslot")
  )
  co2e_mod <- feols(
    log(co2e) ~ tment| day + slot, 
    df, cluster = c("dayslot")
  )
  
  tab <- modelsummary(
    list(
      meat_fish_logit = mtfsh_mod, weight = weight_mod, co2e = co2e_mod
    ), stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1)
  )
  
  cat(tab, file = paste0(ofolder, "/exp_t3p1_reg_results_",unit_obs, ".html"))
  
  hresults <- create_hypotheses_table(df, mtfsh_mod, weight_mod, co2e_mod)
  
  cat(
    hresults$tab, 
    file = paste0(ofolder, "/exp_t3p2_hyp_tests_", unit_obs, ".html")
  )

  # Save all results for potential slide deck
  save(
    weight_mod, mtfsh_mod, co2e_mod, hresults, 
    file = paste0(ofolder, "/exp_prereg_results_", unit_obs, ".rdata")
  )
  
  # Zip all output from temp folder to output folder
  
  projectwd <- getwd()
  setwd(ofolder)
  zip(
    paste0(projectwd, "/output/exp_prereg_output_", unit_obs, ".zip"), 
    list.files()
  )
  setwd(projectwd)
  
}

prepare_prereg_analyses("dish")
prepare_prereg_analyses("taction")