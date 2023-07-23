# ------------------------------------------------------------------------------
# Utility functions that are used in the experimental analyses
# ------------------------------------------------------------------------------

TMENTS <- c(
  "NoInfo", "CO2Neutral", "CO2ColorCoded", "BudgetColorCoded", "MoneyColorCoded"
)

star <- function(x, p) {
  sprintf(
    "%.3f%s", x, 
    case_when(
      p < 0.01 ~ "***",
      p < 0.05 ~ "**",
      p < 0.1 ~ "*",
      TRUE ~ ""
    )
  )
}

test_hypothesis <- function(mod, test) {
  rv <- marginaleffects::hypotheses(mod, test)
  rv[] <- lapply(rv, function(x) {attributes(x) <- NULL; x})
  tibble(
    lb = rv$conf.low,
    est = rv$estimate,
    ub = rv$conf.high,
    pvalue = rv$p.value,
    starred = star(rv$estimate, rv$p.value)
  )
}

construct_lin_hypotheses <- function(df) {
  no_info <- TMENTS[1]
  weights <- pull(
    df %>% filter(tment != no_info) %>% group_by(tment) %>% 
      summarise(share = n()/nrow(df))
  ) 

  h1_test <- paste0(
    paste0(
      weights/sum(weights), " * tment", TMENTS[-1], collapse = " + "
    ),
    " = 0"
  )
  
  h2_test <- paste0(
    "-tment", TMENTS[3], " + ",  
    paste0(
      weights[3:4]/sum(weights[3:4]), " * tment", TMENTS[4:5], 
      collapse = " + "
    ),
    " = 0"
  )
  
  h3_test <- paste0("tment", TMENTS[4], " -tment", TMENTS[5], " = 0")
  
  h4_test <- paste0("tment", TMENTS[3], " -tment", TMENTS[2], " = 0")
  
  return(c(
    h1 = h1_test,
    h2 = h2_test,
    h3 = h3_test,
    h4 = h4_test
  ))
} 

create_hypotheses_table <- function(df, mm, mw, mc) {
  lh <- construct_lin_hypotheses(df)
  
  hresults <- matrix(NA_character_, 4, 3)
  
  col <- 0
  for (mod in list(mm, mw, mc)) {
    col <- col + 1
    hresults[1, col] <- test_hypothesis(mod, lh["h1"])$starred
    hresults[2, col] <- test_hypothesis(mod, lh["h2"])$starred
    hresults[3, col] <- test_hypothesis(mod, lh["h3"])$starred
    hresults[4, col] <- test_hypothesis(mod, lh["h4"])$starred
  }
  rownames(hresults) <- c("H1", "H2", "H3", "H4")
  colnames(hresults) <- c("mtfsh_mod", "weight_mod", "co2e_mod")
  
  return(list(
    df = hresults,
    tab = kable(hresults) %>% kable_styling()
  ))
} 

if (FALSE) {
  # Example code
  
  library(dplyr)
  library(readr)
  
  smp <- readRDS("data/generated/exp_sample.rds")
  dish_plan <- read_csv("data/experiment/dish_plan.csv", show_col_types = FALSE)
  
  df <- smp %>%
    mutate(
      edaytslot = factor(10*eday + tslot),
      co2eg = (co2e100g*0.5*weight + 
                 0.5*weight*median(dish_plan$co2e100g, na.rm = TRUE))/100,
    ) %>%
    filter(eday != 9)
  
  lh <- construct_lin_hypotheses(smp)
  
  mod_co2eg <- fixest::feols(
    log(co2eg) ~ tment| eday + tslot, 
    df, cluster = c("edaytslot")
  )
  test_hypothesis(mod_co2eg, lh[1])
  
  
  mod_co2eg_plain <- fixest::feols(
    log(co2eg) ~ tment| eday + tslot, 
    df, vcov = "iid"
  )
  test_hypothesis(mod_co2eg_plain, lh[1])
}

save_plot <- function(name, h, w){
  ggsave(paste0(name,".svg"), height = h, width = w)
  ggsave(paste0(name,".pdf"), height = h, width = w)
  ggsave(paste0(name,".png"), height = h, width = w)
}