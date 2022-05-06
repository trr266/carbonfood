# ------------------------------------------------------------------------------
# Author: Joachim Gassen, 2022, gassen@wiwi.hu-berlin.de
# License: MIT, see LICENSE file for details
#
# The code below runs a simulation that compares the fixed-effect approach
# of Bayer et al. (TBD) with varying discrete choice model approaches
# ------------------------------------------------------------------------------

library(tidyverse)
library(fixest)
library(mlogit)


N_DINERS <- 2000
N_DISHES <- 2
N_DAYS <- 10

N_UNOBS_ATTRIBUTES <- 2
MN_PREF_CO2E <- -1
MUTE_PREF_CO2E_NO_TMENT <- 0.1
MN_PREF_UNOBS_ATTRIBUTES <- 1

N_SIM_RUNS <- 1000

run_count <- 0

sim_run <- function() {
  set.seed(run_count)
  diners <- tibble(
    diner_id = replicate(
      N_DINERS, paste(sample(LETTERS, 8, replace = TRUE), collapse = "")
    ),
    pref_co2e = rnorm(N_DINERS, MN_PREF_CO2E)
  )
  
  for (v in seq_len(N_UNOBS_ATTRIBUTES)) {
    diners$z <- rnorm(N_DINERS, MN_PREF_UNOBS_ATTRIBUTES)
    names(diners)[ncol(diners)] <- sprintf("pref_z%d", v)
  }
  
  dishes <- tibble(
    day = rep(seq_len(N_DAYS), each = N_DISHES),
    dish_id = replicate(
      N_DAYS * N_DISHES, paste(sample(LETTERS, 8, replace = TRUE), collapse = "")
    ),
    co2e = rnorm(N_DAYS * N_DISHES)
  ) %>%
    group_by(day) %>%
    arrange(day, -co2e) %>%
    mutate(mtfsh = row_number() == 1) %>%
    select(day, dish_id, mtfsh, co2e) %>%
    ungroup()
  
  
  for (v in seq_len(N_UNOBS_ATTRIBUTES)) {
    dishes$z <- rnorm(N_DAYS * N_DISHES)
    names(dishes)[ncol(dishes)] <- sprintf("z%d", v)
  }
  
  choices <- NULL
  dish_vars <- c('co2e', paste0('z', seq_len(N_UNOBS_ATTRIBUTES)))
  
  for (d in seq_len(N_DAYS)) {
    dish_options <- left_join(
      diners %>% 
        mutate(
          day = d, 
          tment = sample(c(rep(T, N_DINERS/2), rep(F, N_DINERS/2)))
        ), 
      dishes, by = "day"
    ) %>%
      mutate(co2e = ifelse(!tment, MUTE_PREF_CO2E_NO_TMENT*co2e, co2e))
    
    dish_options$utility <- rowSums(
      dish_options[dish_vars] * dish_options[paste0('pref_', dish_vars)]
    ) + rnorm(N_DINERS * N_DISHES)
    choices <-bind_rows(
      choices,
      dish_options %>%
        group_by(diner_id) %>%
        arrange(diner_id, -utility) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        select(day, diner_id, tment, dish_id)
    )
  }
  
  mlogit_smp <- expand_grid(dishes, diners) %>%
    left_join(
      choices %>% mutate(choice = TRUE), by = c("day", "diner_id", "dish_id")
    ) %>%
    group_by(day, diner_id) %>%
    mutate(
      dish = ifelse(mtfsh, "Meat/Fish", "Veggie"),
      diner = paste0(day, "-", diner_id),
      tment = as.logical(sum(tment, na.rm = TRUE)),
      choice = as.factor(ifelse(!is.na(choice), "yes", "no"))
    ) %>%
    ungroup() %>%
    arrange(day, diner_id, dish_id)
  
  mod_mlogit_full_nr <- mlogit(
    choice ~ co2e:tment +  z1 + z2, 
    data = mlogit_smp,
    idx = c("diner", "dish")
  )

  mod_mlogit_full_rd <- mlogit(
    choice ~ co2e:tment + z1 + z2, 
    data = mlogit_smp,
    method='bhhh', 
    rpar = c("co2e:tmentFALSE" = 'n', "co2e:tmentTRUE" = 'n', z1 = 'n', z2 = 'n'),
    idx = c("diner", "dish"),
    seed = 123
  )

  mod_mlogit_tment_iacted <- mlogit(
    choice ~ co2e:tment, 
    data = mlogit_smp,
    idx = c("diner", "dish")
  )
  mod_mlogit_tment_only <- mlogit(
    choice ~ 0 | tment, 
    data = mlogit_smp,
    idx = c("diner", "dish")
  )
  
  days <- dishes %>% 
    group_by(day) %>%
    summarise(
      delta_co2e = co2e[2] - co2e[1],
      delta_z1 = z1[2] - z1[1],
      delta_z2 = z2[2] - z2[1]
    )
  
  smp <- choices %>%
    left_join(days, by = "day") %>% 
    left_join(diners, by = "diner_id") %>% 
    left_join(dishes, by = c("day", "dish_id")) %>%
    mutate(veggie = !mtfsh)
  
  mod_ofe_full <- feglm(
    veggie ~ delta_co2e:tment + delta_z1 + delta_z2, 
    data = smp, 
    family = binomial(link = "logit")
  )
  
  mod_fe_tment_iacted <- feglm(
    veggie ~ delta_c02e_tment | day, 
    data = smp %>% mutate(delta_c02e_tment = delta_co2e*tment), 
    family = binomial(link = "logit")
  )

  mod_fe_tment_only <- feglm(
    veggie ~ tment | day, 
    data = smp %>% mutate(delta_c02e_tment = delta_co2e*tment), 
    family = binomial(link = "logit")
  )
  
  mods <- list(
    mod_mlogit_full_nr, mod_mlogit_full_rd,
    mod_mlogit_tment_iacted, mod_mlogit_tment_only,
    # mod_ofe_full, mod_ofe_tment_iacted, mod_ofe_tment_only,
    mod_fe_tment_iacted, mod_fe_tment_only
  )
  
  mod_names <- c(
    "mod_mlogit_full_nr", "mod_mlogit_full_rd",
    "mod_mlogit_tment_iacted", "mod_mlogit_tment_only",
    # "mod_ofe_full", "mod_ofe_tment_iacted", "mod_ofe_tment_only",
    "mod_fe_tment_iacted", "mod_fe_tment_only"
  )
  
  extract_coefs <- function(mod) {
    # Now this is hacky but gets the model variable name
    mod_name <- mod_names[
      as.numeric(as.character(substitute(.x[[i]], env = parent.frame(1)))[3])
    ]
    broom::tidy(mod, conf.int = TRUE) %>% 
      filter(str_detect(term, "co2e|tment")) %>%
      filter(!str_detect(term, fixed("sd."))) %>%
      mutate(
        model = mod_name
      ) %>%
      select(model, everything())
  } 
  
  run_count <<- run_count + 1
  if (run_count %% 10 == 0) message(
    sprintf("%s: Completed %d of %d sim runs", Sys.time(), run_count, N_SIM_RUNS))

  bind_rows(map(mods, extract_coefs))
}

fname <- sprintf("data/generated/disc_choice_rd_%d_sim_runs.rds", N_SIM_RUNS)

if (!file.exists(fname)) {
  message(sprintf("%s: Starting %d sim runs", Sys.time(), N_SIM_RUNS))  
  sim_runs <- bind_cols(
    tibble(run = rep(seq_len(N_SIM_RUNS), each = 9)),
    bind_rows(replicate(N_SIM_RUNS, sim_run(), simplify = FALSE))
  )
  saveRDS(sim_runs, fname)
  message (sprintf("%s: Done!", Sys.time()))
} else sim_runs <- readRDS(fname)

ests_dchoice <- sim_runs %>%
  filter(model %in% c(
    "mod_mlogit_full_nr", "mod_mlogit_full_rd", "mod_mlogit_tment_iacted"
  )) %>% 
  group_by(run, model) %>%
  summarise(estimate = -(estimate[2] - estimate[1]), .groups = "drop")

ests <- bind_rows(
  sim_runs %>%
    filter(model %in% c(
      "mod_mlogit_tment_only", "mod_fe_tment_iacted", "mod_fe_tment_only"
    )) %>%
    mutate(estimate = ifelse(
      model == "mod_fe_tment_iacted", -estimate, estimate
    )) %>%
    select(run, model, estimate),
  ests_dchoice
) %>%
  arrange(run, model)

ggplot(ests, aes (x = estimate, color = model)) + 
  geom_density() + 
  theme_classic() 

ggplot(
  ests %>% filter(model %in% c(
    "mod_mlogit_full_nr", "mod_mlogit_full_rd",
    "mod_mlogit_tment_iacted",
    "mod_fe_tment_iacted"
  )), 
  aes (x = estimate, color = model)
) + 
  geom_density() + 
  theme_classic() 

ests_wide <- pivot_wider(ests, names_from = "model", values_from = "estimate")

ggplot(ests_wide) +
  geom_point(aes(x = mod_mlogit_full_nr, y = mod_fe_tment_iacted)) +
  theme_minimal()

ggplot(ests_wide) +
  geom_point(aes(x = mod_mlogit_full_nr, y = mod_mlogit_full_rd)) +
  theme_minimal()


