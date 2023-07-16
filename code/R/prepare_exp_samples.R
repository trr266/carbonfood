suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(kableExtra)
})

options(knitr.kable.NA = '')
dir.create(ofolder <- tempfile())

GCGROUPS <- c("Bedienstete", "Gäste", "Studierende")
ECGROUPS <- c("Faculty and staff", "Guests", "Students")

fdc <- readRDS("data/experiment/dish_choices.rds")
tment_data <- readRDS("data/experiment/tment_data.rds")
dish_plan <- read_csv("data/experiment/dish_plan.csv", show_col_types = FALSE)

avg_co2e100g <- median(dish_plan$co2e100g, na.rm = TRUE)

df1 <- fdc %>% 
  left_join(dish_plan, by = c("eday", "dish"))

df_dish <- df1 %>%
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
  filter(eday != 9) %>% 
  ungroup()

df_taction <- df1 %>%
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
  filter(eday != 9) %>% 
  ungroup()

  
dups <- df_taction %>% group_by(eday, register_id, sheet_id, trans_id) %>%
  filter(n() > 1)

if (nrow(dups) > 0) stop (
  "Transcaction level data created duplicates. This should not happen"
)

check_missing_obs <- function(df) {
  df_missing_dvs <- df %>%
    filter(is.na(mtfsh) | is.na(weight) | is.na(co2eg))
  
  if (nrow(df_missing_dvs) > 0) stop(
    "Data aggregation created missing dependent variables. This should not happen"
  )
} 

check_missing_obs(df_dish)
check_missing_obs(df_taction)

sample_sel_table <- function(df) {
  ssel <- df %>% 
    left_join(tment_data, by = "eday") %>%
    rename(ftime = first_time) %>%
    mutate(
      day = ifelse(eday == 11, 9, eday),
      tment_end = ifelse(is.na(tment_end), Inf, tment_end),
      minute = 60 * time@hour + time@minute - 660,
      post2pm_or_pre11am = (minute >= 180 | minute <= 0),
      pre_tment = minute <= tment_start,
      post_tment = minute >= tment_end,
      treated = !post2pm_or_pre11am & !pre_tment & !post_tment,
      tslot1 = treated & minute < tment_change,
      tslot2 = treated & minute >= tment_change
    )  %>%
    summarise(
      `Dish choices` = n(),
      `- Choices based on different food options` = n() - sum(treated),
      `= Experimental sample` = sum(treated),
      `Students` = sum(treated & customer_group == "Studierende"),
      `Staff` = sum(treated & customer_group == "Bedienstete"),
      `Guests` = sum(treated & customer_group == "Gäste"),
      `Choices by first-time visitors` = sum(treated & ftime),
      `Choices by returning visitors` = sum(treated & ! ftime),
      `Total visits = 1` = sum(treated & visits == 1),
      `2 <= total visits < 5` = sum(treated & (visits > 1) & (visits < 5)),
      `Total visits >= 5` = sum(treated & (visits >= 5)),
      .groups = "drop"
    ) %>% t()
  
  colnames(ssel) <- "# of observations"
  if (max(df$n_dishes) > 1) {
    rownames(ssel)[1] <- "Dining transactions" 
    rownames(ssel)[7] <- "Transactions by first-time visitors" 
    rownames(ssel)[8] <- "Transactions by returning visitors" 
  }
  kable(
    ssel , format.args = list(big.mark = ",")
  ) %>% kable_styling() 
}

tab <- sample_sel_table(df_dish)
cat(tab, file = paste0(ofolder, "/exp_t1pa_sample_sel_dish.html"))

tab <- sample_sel_table(df_taction)
cat(tab, file = paste0(ofolder, "/exp_t1pa_sample_sel_taction.html"))

finalize_sample <- function(df) {
  df %>% 
    left_join(tment_data, by = "eday") %>%
    mutate(
      day = ifelse(eday == 11, 9, eday),
      tment_end = ifelse(!is.finite(tment_end), 180, tment_end),
      minute = 60 * time@hour + time@minute - 660,    
      qhour = minute %/% 15 + 1,
      customer_group = ECGROUPS[match(customer_group, GCGROUPS)],
      tslot = ifelse(minute < tment_change, 1, 2),
      tment = factor(
        ifelse(minute < tment_change, tslot1, tslot2),
        c(
          "NoInfo", "CO2Neutral", "CO2ColorCoded", 
          "MoneyColorCoded","BudgetColorCoded"
        )
      )
    ) %>%
    filter(
      minute >= tment_start,
      minute < tment_end
    ) %>%
    arrange(eday, minute, register_id, trans_id) %>%
    select(
      eday, counter, qhour, minute, pcard_id, register_id, sheet_id, trans_id,
      customer_group, tment_start, tment_change, tment_end, tslot, tment, 
      dish, n_dishes, to_go, first_time, visits, weight, mtfsh, co2eg
    )
}

smp_dish <- finalize_sample(df_dish)
smp_taction <- finalize_sample(df_taction)

saveRDS(smp_dish, "data/generated/exp_sample_dish.rds")
saveRDS(smp_taction, "data/generated/exp_sample_taction.rds")

tstr <- function(m) sprintf("%02d:%02d", 11 + m %/% 60, m %% 60)

desc_table_tslot <- function(smp) {
  tslot <- smp %>%
    mutate(
      Day = ifelse(eday == 11, 9, eday),
      Slot = as.character(tslot),
      Treatment = tment,
      `Duration [min]`  = ifelse(
        tslot == 1, tment_change - tment_start, tment_end - tment_change
      )
    ) %>%
    group_by(Day, Slot, Treatment, `Duration [min]`) %>%
    summarise(
      `Treated observations` = n(),
      `Meat or fish` = sprintf("%.1f%%", 100*mean(mtfsh)),
      `Mean weight [g]` = mean(weight), 
      `SD weight [g]` = sd(weight), 
      `Min weight [g]` = min(weight), 
      `Max weight [g]` = max(weight), 
      `Mean CO₂e [g]` = mean(co2eg), 
      `SD CO₂e [g]` = sd(co2eg), 
      `Min CO₂e [g]` = min(co2eg), 
      `Max CO₂e [g]` = max(co2eg), 
      .groups = "drop"
    ) %>% mutate(Day = as.character(Day))
  
  bind_rows(
    tslot, 
    tibble(
      Day = "Experimental Sample", 
      Slot = NA_character_, 
      Treatment = NA_character_, 
      `Duration [min]` = mean(tslot$`Duration [min]`),
      `Treated observations` = nrow(smp),
      `Meat or fish` = sprintf("%.1f%%", 100*mean(smp$mtfsh)),
      `Mean weight [g]` = mean(smp$weight), 
      `SD weight [g]` = sd(smp$weight), 
      `Min weight [g]` = min(smp$weight), 
      `Max weight [g]` = max(smp$weight), 
      `Mean CO₂e [g]` = mean(smp$co2eg), 
      `SD CO₂e [g]` = sd(smp$co2eg), 
      `Min CO₂e [g]` = min(smp$co2eg), 
      `Max CO₂e [g]` = max(smp$co2eg)
    )
  ) %>%
    kable(
      format.args = list(big.mark = ","), digits = 0,
      align = "lllrrrrrrrrrrrrrr"
    ) %>% kable_styling() 
}

tab <- desc_table_tslot(smp_dish)
cat(tab, file = paste0(ofolder, "/exp_t1pb_tslot_desc_dish.html"))
tab <- desc_table_tslot(smp_taction)
cat(tab, file = paste0(ofolder, "/exp_t1pb_tslot_desc_taction.html"))

projectwd <- getwd()
setwd(ofolder)
zip(
  paste0(projectwd, "/output/exp_samples_desc_output.zip"), 
  list.files()
)
setwd(projectwd)

