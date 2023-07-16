# Adapted from car::linearHypothesis.default
# This code has been used for the power simulation during
# the pre registration. The code for evaluating the experimental
# evidence uses the 'marginaleffects' package yielding
# identical insights. 

library(car)

linearHypothesis.fixest <- function (
  model, hypothesis.matrix, rhs = NULL, test = c("F", "Chisq"), 
  vcov. = NULL, singular.ok = FALSE, verbose = FALSE, 
  coef. = coef(model), suppress.vcov.msg = FALSE, ...
) 
{
  df <- fixest::degrees_freedom(model, type = "t", ...)
  if (is.null(df)) 
    stop("fixest did not return a residual df = 0")
  V <- if (is.null(vcov.)) 
    vcov(model, ...)
  else if (is.function(vcov.)) 
    vcov.(model)
  else vcov.
  b <- coef.
  if (any(aliased <- is.na(b)) && !singular.ok) 
    stop("there are aliased coefficients in the model")
  b <- b[!aliased]
  if (is.null(b)) 
    stop(paste("there is no coef() method for models of class", 
               paste(class(model), collapse = ", ")))
  if (is.character(hypothesis.matrix)) {
    L <- makeHypothesis(names(b), hypothesis.matrix, rhs)
    if (is.null(dim(L))) 
      L <- t(L)
    rhs <- L[, NCOL(L)]
    L <- L[, -NCOL(L), drop = FALSE]
    rownames(L) <- hypothesis.matrix
  }
  else {
    L <- if (is.null(dim(hypothesis.matrix))) 
      t(hypothesis.matrix)
    else hypothesis.matrix
    if (is.null(rhs)) 
      rhs <- rep(0, nrow(L))
  }
  q <- NROW(L)
  value.hyp <- L %*% b - rhs
  vcov.hyp <- L %*% V %*% t(L)
  if (verbose) {
    cat("\nHypothesis matrix:\n")
    print(L)
    cat("\nRight-hand-side vector:\n")
    print(rhs)
    cat("\nEstimated linear function (hypothesis.matrix %*% coef - rhs)\n")
    print(drop(value.hyp))
    cat("\n")
    if (length(vcov.hyp) == 1) 
      cat("\nEstimated variance of linear function\n")
    else cat("\nEstimated variance/covariance matrix for linear function\n")
    print(drop(vcov.hyp))
    cat("\n")
  }
  SSH <- as.vector(t(value.hyp) %*% solve(vcov.hyp) %*% value.hyp)
  test <- match.arg(test)
  if (!(is.finite(df) && df > 0)) 
    test <- "Chisq"
  name <- try(formula(model), silent = TRUE)
  if (inherits(name, "try-error")) 
    name <- substitute(model)
  title <- "Linear hypothesis test\n\nHypothesis:"
  topnote <- paste("Model 1: restricted model", "\n", 
                   "Model 2: ", paste(deparse(name), collapse = "\n"), 
                   sep = "")
  note <- if (is.null(vcov.) || suppress.vcov.msg) 
    ""
  else "\nNote: Coefficient covariance matrix supplied.\n"
  rval <- matrix(rep(NA, 8), ncol = 4)
  colnames(rval) <- c("Res.Df", "Df", test, paste("Pr(>", 
                                                  test, ")", sep = ""))
  rownames(rval) <- 1:2
  rval[, 1] <- c(df + q, df)
  if (test == "F") {
    f <- SSH/q
    p <- pf(f, q, df, lower.tail = FALSE)
    rval[2, 2:4] <- c(q, f, p)
  }
  else {
    p <- pchisq(SSH, q, lower.tail = FALSE)
    rval[2, 2:4] <- c(q, SSH, p)
  }
  if (!(is.finite(df) && df > 0)) 
    rval <- rval[, -1]
  result <- structure(as.data.frame(rval), heading = c(title, 
                                                       printHypothesis(L, rhs, names(b)), "", topnote, 
                                                       note), class = c("anova", "data.frame"))
  attr(result, "value") <- value.hyp
  attr(result, "vcov") <- vcov.hyp
  result
}



if (FALSE) {
  # Some testing
  library(tidyverse)
  library(car)
  library(lmtest)
  library(fixest)
  
  check_fixest_wald <- function(firms = 10, years = 3, se = NULL) {
    df <- expand_grid(
      firmid = 1:firms,
      year = 1:years
    ) %>%
      mutate(
        x = rnorm(n())
      ) %>%
      group_by(firmid) %>%
      mutate(
        y = firmid + year + x + rnorm(n(), 0, year + firmid)
      ) %>%
      ungroup() %>%
      mutate(
        firmid = as.factor(firmid),
        year = as.factor(year)
      )
    
    ols <- lm(y ~ x + firmid + year, data = df)
    lh_ols <- linearHypothesis(ols, "x = 0")
    
    fedm <- feols(y ~ x | firmid + year, se = se, data = df)
    lh_fedm <- linearHypothesis(fedm, "x = 0")
    
    feest <- feols(y ~ x + firmid + year, se = se, data = df)
    lh_feest <-linearHypothesis(feest, "x = 0")
    
    tibble(
      ols_p_coef = coeftest(ols)[2, 4],
      ols_p_wald = lh_ols[2, 6],
      fedm_p_coef = coeftest(fedm, df = degrees_freedom(fedm, "t"))[1, 4],
      fedm_p_wald = lh_fedm[2, 4],
      feest_p_coef = coeftest(feest, df = degrees_freedom(feest, "t"))[2, 4],
      feest_p_wald = lh_feest[2, 4]
    )
  }
  
  df <- bind_rows(replicate(10, check_fixest_wald(), simplify = FALSE))
}
