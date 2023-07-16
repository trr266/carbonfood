suppressMessages({
  library(tidyverse)
  library(ggforce)
  library(fixest)
  devtools::source_url(
    "https://raw.githubusercontent.com/trr266/treat/main/code/R/theme_trr.R"
  )
})

# Ensure that the parameter is set consistently in prepare_exp_sample.R
USE_COUNTER_INFO <- FALSE

smp <- readRDS("data/generated/exp_sample_dish.rds")
dish_plan <- read_csv("data/experiment/dish_plan.csv", show_col_types = FALSE)

df <- smp %>%
  mutate(
    cset = {if (USE_COUNTER_INFO) eday*10 + (counter == "yellow")  else  eday},
    csetqhour = factor(100*cset + qhour),
    csettslot = factor(10*cset + tslot),
    cset = factor(cset),
    wday = factor(ifelse(eday == 11, 4, eday %% 5)),
    customer_group = factor(
      customer_group, 
      c("Students", "Faculty and staff", "Guests")
    ),
    repeated_diner = visits > 1 
  ) 

est_models <- function(data, dv, cluster = c("csettslot"), method = "ols") {
  estfun <- ifelse(
    method == "logit", 
    function(...) femlm(family = 'logit', ...),
    function(...) feols(...)
  )
  list(
    tfe_none = estfun(
      as.formula(sprintf("%s ~ tment| cset", dv)), data, cluster = cluster
    ),
    tfe_tslot = estfun(
      as.formula(sprintf("%s ~ tment| cset + tslot", dv)) , 
      data, cluster = cluster
    ),
    tfe_wdaytslot = estfun(
      as.formula(sprintf("%s ~ tment| cset + wday^tslot", dv)) , 
      data, cluster = cluster
    ),
    tfe_wdayqhour = estfun(
      as.formula(sprintf("%s ~ tment| cset + wday^qhour", dv)) , 
      data, cluster = cluster
    ),
    tfe_wdayminute = estfun(
      as.formula(sprintf("%s ~ tment| cset + wday^minute", dv)) , 
      data, cluster = cluster
    )
  )
}

get_teffect_cis <- function(mods) {
  bind_rows(lapply(mods, confint), .id = "mod") %>%
    remove_rownames() %>%
    rename(lb = `2.5 %`, ub = `97.5 %`) %>%
    mutate(
      tment = unlist(lapply(
        mods, 
        function(x) {
          tments <- str_remove(names(x$coefficients), fixed("tment"))
          factor(tments, tments)
        }
      )),
      mod = factor(mod, unique(mod)),
      est = unlist(lapply(
        mods, 
        function(x) unname(x$coefficients)
      ))
    ) %>% select(mod, tment, lb, est, ub)
}


ci_plot <- function(df, title = "", mods = unique(df$mod)) {
  ellipse_df <- df %>% 
    pivot_longer(
      cols = c("lb", "ub", "est"), names_to = "type", values_to = "est"
    ) %>%
    group_by(tment) %>%
    summarise(
      x0 = as.numeric(unique(tment)), y0 = 0.5*(min(est) + max(est)),
      a = length(mods)*0.04, b = (max(est) - min(est))/2,                             
      .groups = "drop"
    ) %>% ungroup()
  
  df <- df %>% filter(mod %in% mods)
  
  ggplot() +
    geom_ellipse(
      data = ellipse_df, 
      aes(x0 = x0, y0 = y0, a = a, b = b, fill = tment, angle = 0), 
      color = NA, alpha = 0.1
    ) +
    geom_pointrange(
      data = df, 
      aes(
        x = as.numeric(tment), 
        ymin = lb, y = est, ymax = ub, group = mod, color = tment
      ), 
      position = position_dodge(w = 0.2),
      fatten = .75
    ) +
    geom_hline(yintercept = 0, lty = 2, col = "red") +
    scale_x_continuous(label = ellipse_df$tment) +
    scale_color_trr266_d() +
    scale_fill_trr266_d() +
    labs(
      x = "", y = "", title = title
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

mods_mtfsh_ols <- est_models(df, "mtfsh")
mods_co2 <- est_models(df, "log(co2eg)")

mtfsh_ols_cis <- get_teffect_cis(mods_mtfsh_ols)
co2_cis <- get_teffect_cis(mods_co2)

title <- "Carbon footprint labels reduce your carbon footprint"
title_de <- "CO2-Information reduzieren Ihren CO2-Fußabdruck"

caption_str <- str_wrap(paste(
  "This figure reports our treatment effect estimates based on a sample of", 
  sprintf("%s meal transactions", format(nrow(df), big.mark = ",")),
  "recorded during the 10-day experimental window.", 
  "Each treatment, indicated on the x axis, was administered for four half",
  "days, with a baseline of four additional half days without any treatment.",
  "The effect sizes are measured as percentage changes in",
  "the carbon footprint of the chosen main dishes.",
  "The ranges reflect 95% confidence intervals and are based on OLS regression",
  "estimates including daily and treatment slot fixed effects. The shaded areas",
  "around the confidence intervals indicate model uncertainty by reflecting",
  "the confidence intervals from alternative fixed effects structures."
), width = 120)

caption_str_de <- str_wrap(paste(
  "Diese Abbildung zeigt unsere Effektschätzungen auf der Grundlage einer",
  sprintf("Untersuchungsgesamtheit von %s", format(nrow(df), big.mark = ",")),
  "Gerichten die während des 10-tägigen Experiments in der Mensa erworben",
  "wurden. Jede CO2-Darstellungsform wurde vier halbe Tage lang präsentiert,", 
  "mit einer Baseline von vier weiteren halben Tagen ohne CO2-Informationen.", 
  "Die Effektgrößen werden als prozentuale Veränderungen des CO2-Fußabdrucks",
  "der gewählten Gerichte gemessen. Die Bereiche spiegeln",
  "95%-Konfidenzintervalle wider und basieren auf OLS-Regressionsschätzungen", 
  "mit täglichen und Zeitfenster fixed effects. Die schattierten Bereiche", 
  "um die Konfidenzintervalle zeigen die Modellunsicherheit an und geben", 
  "die Konfidenzintervalle alternativer fixed-effects-Strukturen wieder."
), width = 120)


exponate <- function(x) {exp(x) - 1}
co2_cis_exp <- co2_cis %>%
  mutate(across(c("lb", "est", "ub"), exponate))

plot_cis_co2 <- ci_plot(co2_cis_exp, title, mods = "tfe_tslot") + 
  scale_y_continuous(label = scales::label_percent()) +
  labs(caption = caption_str)

ggsave("output/debriefing_res_exp.svg", width = 7.5, height = 5, units = "in")
ggsave("output/debriefing_res_exp.pdf", width = 7.5, height = 5, units = "in")

