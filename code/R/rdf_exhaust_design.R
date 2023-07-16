# devtools::install_github("joachim-gassen/rdfanalysis")

suppressMessages({
  source("code/R/rdf_design.R", keep.source = TRUE)
})

RDF_FILE <- "data/generated/rdf_outcomes.rds"
USE_PARALLEL_CORES <- 8 # Set to 1 to not use parallel processing
EXHAUST_ONLY_POS_WEIGHTS <- TRUE

# Set the below to TRUE when you want to use a database
# for exhausting the design choice combinations. This might be useful
# when you want to exhaust maaany combinations as you can use multiple 
# worker processes from different clients concurrently then. Also,
# you are then able to stop the process at any time.

USE_DB <- FALSE


prepare_design_documentation(design, "output/rdf_code_doc.pdf")
prepare_design_flow_chart(design, file_name = "output/rdf_flowchart.pdf")

if (USE_PARALLEL_CORES > 1) {
  cl <- parallel::makeCluster(USE_PARALLEL_CORES)
} else cl <- NULL

if (!interactive()) pbapply::pboptions(type = "timer")

if (USE_DB) {
  db_conn_func <-  function() {
    conn <- DBI::dbConnect(
      RPostgres::Postgres(), "rdf", host = "localhost",
      user = "joga", password = "hrjeu8f", port = 5432
    )    
    return(conn)
  } 
} else db_conn_func <- NULL
  
choice_df <- generate_choice_df(
  design, weight = EXHAUST_ONLY_POS_WEIGHTS, verbose = TRUE
)

rdf_outcomes <- exhaust_design(
    design, cl = cl, db_conn_func = db_conn_func,
    libs = c("tidyverse", "rdfanalysis", "fixest", "marginaleffects", "DBI", "RPostgres"),
    start_input = list(base_smp_dish, base_smp_taction),
    choice_df = choice_df,
    weight = EXHAUST_ONLY_POS_WEIGHTS, 
    verbose = TRUE
  )

saveRDS(rdf_outcomes, RDF_FILE)

if (USE_PARALLEL_CORES > 1) parallel::stopCluster(cl)

message(sprintf("%s: Done exhausting designs", Sys.time()))
