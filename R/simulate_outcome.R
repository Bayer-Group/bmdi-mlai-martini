#' Simulate outcome for MLAI pipeline
#'
#' Simulate regression, classification  or survival outcome based on
#' feature matrix and effect vector
#'
#' @param X Prepared feature matrix (as tibble or matrix) with standardized and 
#' potentially log-transformed numeric variables and dummy-coded categorical 
#' variables. If interaction effects are desired, a corresponding column has to
#' be present in \code{X} (e.g. \code{X$interaction_A_B = X$A*X$B}). Needs to 
#' contain an id column named ".id".
#' @param beta Named effect vector with the corresponding effects for the 
#' columns in \code{X}. Only non-zero effects have to be specified
#' @param type type of the simulated outcome: "regression", "classification" or 
#' "survival"
#' @param ctrl_regr list with the settings for regression outcome, simulated by
#' a linear regression model
#' \describe{
#'   \item{`b0`}{model intercept}
#'   \item{`sd`}{standard deviation of the model error term}
#' }
#' @param ctrl_class list with settings for classification outcome 
#' (event vs. no event), simulated by a logistic regression model
#' \describe{
#'   \item{`prob_ev`}{event probability}
#'   \item{`mult_beta`}{enhancement factor for the beta coefficient to control 
#'   signal-to-noise ratio}
#' }
#' @param ctrl_surv list with settings for right-censored survival outcome, 
#' simulated by a proportional hazard model with time-constant baseline hazard
#' \describe{
#'   \item{`surv_mean`}{mean survival time}
#'   \item{`cens_mean`}{mean censoring time, NULL for no censoring}
#'   \item{`cens_max`}{max censoring time, NULL for no censoring}
#'   \item{`mult_beta`}{enhancement factor for the beta coefficient to control 
#'   signal-to-noise ratio}
#'   \item{`int`}{boolean, round the survival times to the next highest integer}
#' }
#'
#' @return
#' A tibble with `nrow(X)` rows and columns `.id` and `.out` for regression or 
#' classification outcome or columns `.id`, `.time` and `.status` for survival
#' outcome.
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#'

simulate_outcome <- function(
  
  X,
  beta = NULL,
  type = c("regression", "classification", "survival"),
  
  ctrl_regr  = list(b0 = 0,
                    sd = .4),
  ctrl_class = list(prob_ev   = .5,
                    mult_beta = 1),
  ctrl_surv  = list(surv_mean = 18,
                    cens_mean = 18,
                    cens_max  = 36,
                    mult_beta = 1,
                    int       = FALSE)
  
){

  type <- match.arg(type)

  b <- ncol(X) %>% 
    numeric() %>% 
    stats::setNames(colnames(X))
  
  if (!is.null(beta)){
    
    if (is.null(names(beta))) usethis::ui_stop("'beta' needs to be a named vector.")
    
    beta           <- beta[intersect(names(beta), names(X))]
    b[names(beta)] <- beta
    
  }

  if (type == "regression"){
    
    outc <- tibble::tibble(
      X %>% dplyr::select(.id),
      .out = (ctrl_regr$b0 + as.matrix(X) %*% b + stats::rnorm(nrow(X), sd = ctrl_regr$sd)) %>% 
        round(2) %>% 
        .[,1]
    )
    
  } else if (type == "classification"){
    
    logistic_b0 <- -log(1/ctrl_class$prob_ev - 1)
    
    outc <- tibble::tibble(
      X %>% dplyr::select(.id),
      .out = stats::rbinom(
        n    = nrow(X),
        size = 1,
        prob = 1/(1 + exp(- logistic_b0 - as.matrix(X) %*% (ctrl_class$mult_b*b)))
      )
    ) %>% 
      dplyr::mutate(.out = factor(.out, labels = c("no event", "event")))
    
    
  } else if (type == "survival"){
    
    time_ev <- stats::rexp(
      n    = nrow(X),
      rate = 1/ctrl_surv$surv_mean*exp(as.matrix(X) %*% (ctrl_surv$mult_b*b))
    )
    
    if (is.null(ctrl_surv$cens_max)) ctrl_surv$cens_max <- Inf
    
    if (is.null(ctrl_surv$cens_mean)){
      time_cens <- Inf
    } else {
      time_cens <- stats::rexp(
        n    = nrow(X),
        rate = 1/ctrl_surv$cens_mean
      ) %>% 
        pmin(ctrl_surv$cens_max)
    }
    
    outc <- tibble::tibble(
      X %>% dplyr::select(.id),
      .time   = pmin(time_ev, time_cens),
      .status = as.numeric(time_ev <= time_cens)
    ) %>% 
      {if (ctrl_surv$int){
        dplyr::mutate(., .time = ceiling(.time))
      } else {.}}
    
  }
  
  outc
  
}
