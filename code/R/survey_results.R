suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(fixest)
  library(modelsummary)
  library(tibble)
  library(binom)
})

save_plot <- function(name, h, w){
 ggsave(paste0(name,".pdf"), height = h, width = w)
 ggsave(paste0(name,".png"), height = h, width = w)
}

dir.create(ofolder <- tempfile())

load("data/survey/survey_data.RData")

################################################################################
##### Create data sets for analyses ############################################
################################################################################

smp_survey <- dta_survey_matched %>%
  mutate(survey_id = row_number()) %>%
  mutate(meal_happiness_numeric = case_when(
    meal_happiness == "Zufrieden" ~ 5,
    meal_happiness == "Eher zufrieden" ~ 4,
    meal_happiness == "Weder zufrieden, noch unzufrieden" ~ 3,
    meal_happiness == "Eher unzufrieden" ~ 2,
    meal_happiness == "Unzufrieden" ~ 1
  )) %>%
  mutate(number_people_numeric = case_when(
    number_people == "Wir sind mehr als drei Personen" ~ 4,
    number_people == "Wir sind zu dritt" ~ 3,
    number_people == "Wir sind zu zweit" ~ 2,
    number_people == "Ich esse heute allein" ~ 1
  ))

smp_avoid <- smp_survey %>%
  select(survey_id, pcard_id, avoid_usually, eday, tslot, tment, dining_data_match) %>%
  separate_rows(avoid_usually, sep = ", ") %>%
  filter(avoid_usually != "Histamin" & avoid_usually != "etc.)")%>%
  group_by(survey_id) %>%
  mutate(number_of_criteria_mentioned = n())

smp_select_usually <- smp_survey %>%
  select(survey_id, pcard_id, select_usually, eday, tslot, tment, dining_data_match) %>%
  separate_rows(select_usually, sep = ", ") %>%
  filter(select_usually != "Kalorien)") %>%
  mutate(select_usually = ifelse(select_usually == "Sonstige", "Sonstiges", select_usually)) %>%
  group_by(survey_id) %>%
  mutate(number_of_criteria_mentioned = n())

smp_select_today <- smp_survey %>%
  select(survey_id, pcard_id, select_today, eday, tslot, tment, dining_data_match) %>%
  separate_rows(select_today, sep = ", ") %>%
  filter(select_today != "Kalorien)") %>%
  mutate(select_today = ifelse(select_today == "Sonstige", "Sonstiges", select_today))%>%
  group_by(survey_id) %>%
  mutate(number_of_criteria_mentioned = n())

smp_select_merge <- smp_avoid %>%
  rename(item = avoid_usually) %>%
  mutate(feature = "avoid") %>%
  rbind(., smp_select_usually %>%
          rename(item = select_usually) %>%
          mutate(feature = "select_usually")) %>%
  rbind(., smp_select_today %>%
          rename(item = select_today) %>%
          mutate(feature = "select_today")) %>%
  mutate(item = case_when(
    item == "Aussehen" ~ "Appearance",
    item == "Geschmack" ~ "Taste",
    item == "Preis" ~ "Price",
    item == "Gesundheitliche Aspekte" ~ "Health considerations",
    item == "Nährwerte (z.B. Proteine" ~ "Nutritional value (e.g. proteins, calories)",
    item == "Tierwohl" ~ "Animal welfare",
    item == "Länge der Warteschlange für das Gericht" ~ "Length of the queue at the counter",
    item == "Länge der Warteschlagen für das Gericht" ~ "Length of the queue at the counter",
    item == "Ökologischer Fußabdruck" ~ "Ecological footprint",
    item == "Sonstiges" ~ "Other",
    item == "Fisch" ~ "Fish",
    item == "Fleisch" ~ "Meat",
    item == "Sonstige tierische Produkte" ~ "Other Animal Products",
    item == "Keine" ~ "None",
    item == "Sonstige" ~ "Other",
    item == "Allergie- und Unverträglichkeiten auslösende Lbensmittel (z.B. Erdnüsse" ~ "Allergens")) %>%
  mutate(eco = ifelse(item == "Ecological footprint", 1, 0))

smp_select_usually_merge <- smp_select_usually %>%
  select(survey_id, select_usually, number_of_criteria_mentioned) %>%
  mutate(eco = ifelse(select_usually == "Ökologischer Fußabdruck", 1, 0)) %>%
  group_by(survey_id) %>%
  mutate(eco = sum(eco)) %>%
  select(survey_id, eco, number_of_criteria_mentioned) %>%
  distinct()
  
smp_survey <- smp_survey %>%
  left_join(., (smp_select_usually_merge), by = "survey_id") %>%
  mutate(footprint_importance = eco/number_of_criteria_mentioned)

################################################################################
##### Online Appendix OA.C.3 ###################################################
################################################################################

#----- Panel A: Survey vs. experimental sample

oac3_panel_a <- datasummary(data = dta_receipt_matched %>%
                                      filter(is.na(dining_data_match) | dining_data_match == 1) %>%
                                      mutate(mtfsh = ifelse(mtfsh == T, 1, 0),
                                             took_survey = ifelse(took_survey == T,
                                                                  "took survey",
                                                                  "did not take survey")),
                                    mtfsh + weight + co2eg  ~
                                      took_survey * (N + min + median + max + mean + sd),
                                    stars = c('*' = .1, '**' = .05, '***' = .01),
                                    fmt = 2,
                                    title = "Descriptive Statistics (Survey): Panel D",
                                    notes = paste0("The number of survey participants in this
                                   table is ", 
                                                   nrow(dta_receipt_matched %>%
                                                          filter(took_survey == T,
                                                                 dining_data_match == 1)),
                                                   ". It is based on all survey responses that
                                   could be uniquely matched to one receipt. As
                                   some people participated more than once by
                                   using the same receipt (albeit a different
                                   customer card) to submit their response, this
                                   number is lower than the total number of 
                                   survey responses (",
                                                   nrow(dta_survey_matched),
                                                   ") used in other survey-related analyses"))

cat(oac3_panel_a, file = paste0(ofolder, "/table_oac3_panel_a.html"))

#----- Panel B: Survey vs. Germany/USA (eating habits)

smp_habits <- smp_avoid %>%
  mutate(value = 1) %>%
  pivot_wider(
    id_cols = "survey_id",
    values_from = "value",
    names_from = "avoid_usually"
  ) %>%
  mutate_at(vars(-group_cols()), ~replace(., is.na(.), 0)) %>%
  mutate(vegan = ifelse(Fleisch == 1 & 
                          Fisch == 1 & 
                          `Sonstige tierische Produkte` == 1, 1, 0),
         vegetarian = ifelse(Fleisch == 1 &
                               Fisch == 1 & 
                               `Sonstige tierische Produkte` == 0, 1, 0),
         pescetarian = ifelse(Fleisch == 1 &
                                Fisch == 0 &
                                `Sonstige tierische Produkte` == 0, 1, 0),
         omnivore = ifelse (vegan == 1 |
                              vegetarian == 1 |
                              pescetarian == 1, 0, 1)) %>% # Basically contains the rest
  ungroup() %>%
  select(vegan, vegetarian, pescetarian, omnivore) %>%
  summarize_all(list(~sum(.)/n())) %>%
  rbind(., 
        c(0.031, 0.069, 0.024, 0.876),
        c(0.007, 0.075, 0.047, 0.872)) %>% # Flexitarier counted as omnivores, German data from: https://veganz.de/blog/veganz-ernaehrungsstudie-2022/
  mutate(datasource = c("Survey sample", 
                        "Germany",
                        "USA")) %>%
  pivot_longer(cols = c(vegan, vegetarian, pescetarian, omnivore)) %>%
  mutate(
    name = factor(
      name, level = c("vegan", "vegetarian", "pescetarian", "omnivore")
    )
  ) %>%
  rename(percentage = value) %>%
  mutate(datasource = factor(datasource, level = c("Survey sample",
                                                   "Germany",
                                                   "USA")))

oac3_panel_b <- datasummary_df(smp_habits %>%
                                         mutate(percentage = paste0(round(percentage*100,1), " %")) %>%
                                         pivot_wider(values_from = percentage,
                                                     names_from = datasource),
                                       notes = paste0("This table is based on all ",
                                                      nrow(smp_survey),
                                                      " survey responses. It rests on the following
                               assumptions: Survey participants who state to
                               regularly avoid meat, fish, and other animal
                               products are classified as vegans. Participants 
                               who state to avoid meat and fish are classified
                               as vegetarians. Participants who state to
                               avoid meat only are classified as pescetarians. All
                               other participants are classified as omnivores.
                               Data for Germany is taken from 
                               https://veganz.de/blog/veganz-ernaehrungsstudie-2022/. Data
                               for the US is taken from O'Malley, Smith, & Rose (2023), 
                               https://www.sciencedirect.com/science/article/pii/S0002916523005117#tbl1"),
                                       align = "lrrr")

cat(oac3_panel_b, file = paste0(ofolder, "/table_oac3_panel_b.html"))


#----- Panel C: Happiness and group size

oac3_panel_c <- datasummary(data = smp_survey,
                                    formula = number_people_numeric +
                                      meal_happiness_numeric ~
                                      N + min + P25 + median + P75 + max + mean + sd,
                                    fmt = 2,
                                    title = "Descriptive Statistics (Survey): Panel A",
                                    notes = paste0("This table is based on all ",
                                                   nrow(smp_survey),
                                                   " survey responses." ))

cat(oac3_panel_c, file = paste0(ofolder, "/table_oac3_panel_c.html"))


#----- Panel D: Usually select (categorical)

selection_summary <- smp_select_merge %>%
  mutate(responses = nrow(smp_survey)) %>%
  group_by(feature, item) %>%
  mutate(n_items = n()) %>%
  ungroup() %>%
  select(responses, n_items, item, feature) %>%
  distinct() %>%
  mutate(share = n_items/responses)

oac3_panel_d <- datasummary_df(data = selection_summary %>%
                                         filter(feature == "select_usually") %>%
                                         select(feature, item, share) %>% 
                                         pivot_wider(names_from = "feature", id_cols = c("item"), values_from = "share") %>%
                                         arrange(desc(select_usually)) %>%
                                         mutate(select_usually = paste0(round(select_usually*100,1), " %")),
                                       title = "Descriptive Statistics (Survey): Panel C",
                                       notes = paste0("This table is based on all ",
                                                      nrow(smp_survey),
                                                      " survey responses. It was mandatory to select 
                              foods that participants usually avoid. It was 
                              optional to select criteria that participants
                              usually or today base their meal choice on. Note 
                              that participants had the possibility to 
                              participate once per day. This table is based on 
                              unique survey responses, not unique participants.
                              The percentages do not add up to one
                              since it was possible to select multiple
                              criteria."),
                                       align = "lr")

cat(oac3_panel_d, file = paste0(ofolder, "/table_oac3_panel_d.html"))


#----- Panel E: Avoid (categorical)

oac3_panel_e <- datasummary_df(data = selection_summary %>%
                                         filter(feature == "avoid") %>%
                 select(feature, item, share) %>% 
                 pivot_wider(names_from = "feature", id_cols = c("item"), values_from = "share") %>%
                   arrange(desc(avoid)) %>%
                   mutate(avoid = paste0(round(avoid*100,1), " %")),
               title = "Descriptive Statistics (Survey): Panel B",
               notes = paste0("This table is based on all ",
                              nrow(smp_survey),
                              " survey responses. It was mandatory to select 
                              foods that participants usually avoid. It was 
                              optional to select criteria that participants
                              usually or today base their meal choice on. Note 
                              that participants had the possibility to 
                              participate once per day. This table is based on 
                              unique survey responses, not unique participants.
                              The percentages do not add up to one
                              since it was possible to select multiple
                              criteria."),
               align = "lr")


cat(oac3_panel_e, file = paste0(ofolder, "/table_oac3_panel_e.html"))

#----- Panel F: Happiness by information condition

smp_survey_happiness <- smp_survey %>%
  filter(dining_data_match == 1 | is.na(dining_data_match)) %>%
  mutate(meal_happiness = factor(meal_happiness, level = c("Unzufrieden",
                                                           "Eher unzufrieden",
                                                           "Weder zufrieden, noch unzufrieden",
                                                           "Eher zufrieden",
                                                           "Zufrieden")))

happy_col <- smp_survey_happiness %>%
  select(tment, meal_happiness_numeric, meal_happiness) %>%
  group_by(tment) %>%
  mutate(nobs = n()) %>%
  group_by(tment, meal_happiness_numeric) %>%
  mutate(percentage = round(n()/nobs*100,1)) %>%
  group_by(tment) %>%
  reframe(n = n(),
          mean = round(mean(meal_happiness_numeric),2),
          sd = round(sd(meal_happiness_numeric),2),
          percentage = percentage,
          meal_happiness = meal_happiness,
          tment = tment) %>%
  distinct() %>%
  pivot_wider(names_from = meal_happiness, 
              values_from = percentage) %>%
  select("tment",
         "Unzufrieden",
         "Eher unzufrieden",
         "Weder zufrieden, noch unzufrieden",
         "Eher zufrieden",
         "Zufrieden",
         "n",
         "mean",
         "sd")

happy_col2 <- smp_survey_happiness %>%
  filter(tment != "NoInfo") %>%
  mutate(nobs = n()) %>%
  select(tment, meal_happiness_numeric, meal_happiness, nobs) %>%
  mutate(tment = as.character(tment)) %>%
  group_by(meal_happiness) %>%
  reframe(percentage = round(n()/nobs*100,1)) %>%
  distinct() %>%
  pivot_wider(names_from = meal_happiness, 
              values_from = percentage) %>%
  mutate(n = nrow(smp_survey_happiness %>% filter(tment != "NoInfo")),
         mean = round(mean(unlist(smp_survey_happiness %>% 
                                    filter(tment != "NoInfo") %>% 
                                    select(meal_happiness_numeric))),2),
         sd = round(sd(unlist(smp_survey_happiness %>% 
                                filter(tment != "NoInfo") %>% 
                                select(meal_happiness_numeric))),2),
         tment = "Treatment") %>%
  select("tment",
         "Unzufrieden",
         "Eher unzufrieden",
         "Weder zufrieden, noch unzufrieden",
         "Eher zufrieden",
         "Zufrieden",
         "n",
         "mean",
         "sd")

happy_col3 <- smp_survey_happiness %>%
  mutate(nobs = n()) %>%
  select(tment, meal_happiness_numeric, meal_happiness, nobs) %>%
  mutate(tment = as.character(tment)) %>%
  group_by(meal_happiness) %>%
  reframe(percentage = round(n()/nobs*100,1)) %>%
  distinct() %>%
  pivot_wider(names_from = meal_happiness, 
              values_from = percentage) %>%
  mutate(n = nrow(smp_survey_happiness),
         mean = round(mean(unlist(smp_survey_happiness %>% 
                                    select(meal_happiness_numeric))),2),
         sd = round(sd(unlist(smp_survey_happiness %>% 
                                select(meal_happiness_numeric))),2),
         tment = "All") %>%
  select("tment",
         "Unzufrieden",
         "Eher unzufrieden",
         "Weder zufrieden, noch unzufrieden",
         "Eher zufrieden",
         "Zufrieden",
         "n",
         "mean",
         "sd")

happy_col <- rbind(happy_col, happy_col2, happy_col3)

oac3_panel_f <- datasummary_df(happy_col,
                                  notes = paste0("This table is based on the total number of survey responses (", nrow(smp_survey),")."))

cat(oac3_panel_f, file = paste0(ofolder, "/table_oac3_panel_f.html"))


################################################################################
##### Online Appendix OA.C.4 ###################################################
################################################################################

# Survey Response Bias test

mod_resp_bias_ols <- feols(
  took_survey ~ tment + customer_group + mtfsh + log(weight) + log(co2eg) | eday + tslot, 
  data=dta_receipt_matched %>% filter(is.na(dining_data_match) | dining_data_match == 1),
  cluster = "eday^tslot"
)
mod_resp_bias_logit <- feglm(
  took_survey ~ tment + customer_group + mtfsh + log(weight) + log(co2eg) | eday + tslot, 
  family = binomial,
  data = dta_receipt_matched %>% filter(is.na(dining_data_match) | dining_data_match == 1),
  cluster = "eday^tslot"
)

oac4 <- modelsummary(models = list(
  "took_survey ols" = mod_resp_bias_ols,
  "took_survey logit" = mod_resp_bias_logit
), stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3,
notes = paste0("This table is based on all ",
               nrow(dta_receipt_matched %>% filter(is.na(dining_data_match) | dining_data_match == 1)), " observations for which survey 
               responses could be uniquely matched to receipt data. The number
               of observations in the logit regression is lower since the given 
               fixed effects structure leads to some outcomes being only 0, 
               or only 1."))

cat(oac4, file = paste0(ofolder, "/table_oac4.html"))


################################################################################
##### Online Appendix OA.C.5 ###################################################
################################################################################

#----- Part 1

smp_survey$tment_anyco2 <- smp_survey$tment != "NoInfo"

smp_survey_matched <- smp_survey[smp_survey$dining_data_match == 1,]

reg_mtfsh_fe <- feglm(data = smp_survey_matched %>%
                        filter(surveys_per_receipt_id == 1),
                      mtfsh ~ tment,
                      family = "binomial",
                      fixef = c("eday", "tslot"),
                      cluster = "eday^tslot")

reg_mtfsh_fe_h1 <- feglm(data = smp_survey_matched %>%
                           filter(surveys_per_receipt_id == 1),
                      mtfsh ~ tment_anyco2,
                      family = "binomial",
                      fixef = c("eday", "tslot"),
                      cluster = "eday^tslot")

reg_happiness_fe <- feols(data = smp_survey_matched %>%
                            filter(surveys_per_receipt_id == 1),
                          meal_happiness_numeric ~ tment,
                          fixef = c("eday", "tslot"),
                          cluster = "eday^tslot")

reg_happiness_fe_h1 <- feols(data = smp_survey_matched %>%
                               filter(surveys_per_receipt_id == 1),
                          meal_happiness_numeric ~ tment_anyco2,
                          fixef = c("eday", "tslot"),
                          cluster = "eday^tslot")

oac5_1 <- modelsummary(models = list(
  "mtfsh logit" = reg_mtfsh_fe,
  "happiness" = reg_happiness_fe,
  "mtfsh logit" = reg_mtfsh_fe_h1,
  "happiness" = reg_happiness_fe_h1), 
  stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3,
  notes = paste0("This table is based on all observations for which a survey response could be matched to
               a dish choice."))

cat(oac5_1, file = paste0(ofolder, "/table_oac5_1.html"))

#----- Part 2

smp_survey$two_or_more <- smp_survey$number_people_numeric %in% c(2, 3, 4)

smp_survey_matched <- smp_survey[smp_survey$dining_data_match == 1,]

reg_press_fe <- feglm(
  data = smp_survey_matched %>%
    filter(surveys_per_receipt_id == 1),
  mtfsh ~ tment * two_or_more,
  family = "binomial",
  fixef = c("eday", "tslot"),
  cluster = "eday^tslot"
)

reg_press_fe_h1 <- feglm(
  data = smp_survey_matched %>%
    filter(surveys_per_receipt_id == 1),
  mtfsh ~ tment_anyco2 * two_or_more,
  family = "binomial",
  fixef = c("eday", "tslot"),
  cluster = "eday^tslot"
)

reg_press_fe_happy <- feols(
  data = smp_survey_matched %>%
    filter(surveys_per_receipt_id == 1),
  meal_happiness_numeric ~ tment * two_or_more,
  fixef = c("eday", "tslot"),
  cluster = "eday^tslot"
)

reg_press_fe_happy_h1 <- feols(
  data = smp_survey_matched %>%
    filter(surveys_per_receipt_id == 1),
  meal_happiness_numeric ~ tment_anyco2 * two_or_more,
  fixef = c("eday", "tslot"),
  cluster = "eday^tslot"
)

oac5_2 <- modelsummary(models = list(
  "mtfsh logit" = reg_press_fe,
  "happiness" = reg_press_fe_happy,
  "mtfsh logit" = reg_press_fe_h1,
  "happiness" = reg_press_fe_happy_h1
), stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3,
notes = paste0("This table is based on all observations for which a survey response could be matched to
               a dish choice. The independent variable two_or_moreTRUE
               is an indicator variable equal to one if the diner filling out 
               the survey indicated the were accompanied by at least one person
               for their meal."))

cat(oac5_2, file = paste0(ofolder, "/table_oac5_2.html"))

################################################################################
##### Online Appendix OA.C.6 ###################################################
################################################################################

df_select_reg <- smp_select_merge %>%
  filter(!is.na(tment)) %>%
  filter(is.na(dining_data_match) | dining_data_match == 1) %>%
  group_by(eday, tment, feature, survey_id, tslot) %>%
  summarise(eco = sum(eco), .groups = "drop") %>%
  pivot_wider(names_from = "feature", values_from = "eco") %>%
  mutate(eco_usually = case_when(select_usually == 1 ~ 1,
                                 TRUE ~ 0)) %>%
  mutate(AnyTreatment = ifelse(tment != "NoInfo", 1, 0))

reg_usually <- feols(data = df_select_reg,
                     eco_usually ~ tment)

reg_usually_fe <- feols(data = df_select_reg,
                        eco_usually ~ tment,
                        fixef = c("eday", "tslot"),
                        cluster = "eday^tslot")

reg_usually_logit <- feglm(data = df_select_reg,
                           eco_usually ~ tment,
                           family = "binomial")

reg_usually_anytment <- feols(data = df_select_reg,
                              eco_usually ~ AnyTreatment,
                              fixef = c("eday", "tslot"),
                              cluster = "eday^tslot")

rows <- tribble(~type, 
                ~reg_usually, ~reg_usually_logit, ~reg_usually_fe, ~reg_usually_anytment,
                'Type', 
                'OLS', 'Logit', 'OLS', 'OLS')
attr(rows, 'position') <- c(13)

oac6 <- modelsummary(models = list(
  "eco_usually" = reg_usually,
  "eco_usually logit" = reg_usually_logit,
  "eco_usually" = reg_usually_fe,
  "eco_usually anytment" = reg_usually_anytment),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 3,
  add_rows = rows,
  notes = paste0("This table is based on ", nrow(df_select_reg),
                 " observations for which it was possible to match
                 the survey response to the receipt ID of the participant."))

cat(oac6, file = paste0(ofolder, "/table_oac6.html"))


################################################################################
##### Online Appendix OA.C.7 ###################################################
################################################################################

smp_eco <- smp_select_merge %>%
  group_by(eday) %>%
  mutate(daily_obs = n_distinct(survey_id)) %>%
  filter(feature == "select_usually") %>%
  group_by(eday) %>%
  reframe(sum_eco = sum(eco),
          eco_share = sum(eco)/daily_obs,
          daily_obs = daily_obs) %>%
  mutate(ci_low = binom.confint(sum_eco, daily_obs, method = "exact")[,"lower"],
         ci_high = binom.confint(sum_eco, daily_obs, method = "exact")[,"upper"]) %>%
  distinct()

# Create the ggplot object
oac7 <- ggplot(data = smp_eco,
                         aes(x = as.factor(eday), y = eco_share, ymin = 0)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  geom_text(aes(y = 0.25), label = paste0("N = ", smp_eco$sum_eco), size = 2.5) +
  ggtitle("") +
  ylab(label = "") +
  xlab(label = "") +
  theme_bw() +
  labs(caption = paste0("This graph shows the share of survey responses indicating that
       the respondent applied the ecological footprint as a criterion for their dish selection
       relative to the total number of survey responses per day, incl. 95pc confidence intervals. The underlying survey
       question is 'What criteria do you usually apply to select your dishes at the canteen?'
       The total number of survey responses for the whole sample period is ", nrow(smp_survey), ".")) +
  theme_classic()

save_plot(paste0(ofolder, "/graph_oac7"), 6, 12)


##### SAVE
projectwd <- getwd()
setwd(ofolder)
zip(
  file.path(projectwd, "output/survey_output.zip"), 
  list.files()
)
setwd(projectwd)

