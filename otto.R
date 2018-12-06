# ================================================================== #
# Functions for automated process for fitting and testing models
# using the auto.arima function in the forecast package
# ================================================================== #

# Setup
# ================================================================== #

load_pkgs <- function(pkgs){
  for(pkg in pkgs){
    if(!(require(pkg, character.only=TRUE))){
      install.packages(pkg)
      library(pkg, character.only=TRUE)
    }
  }
}

load_pkgs(c('bit64', 'data.table', 'dplyr', 'forecast', 'seasonal', 'stringr'))

# Functions
# ================================================================== #

extract_params <- function(model_string){
  #' Turns a model specification string in format printed by
  #' auto.arima(trace = T) into a named list, i.e.
  #' ARIMA(p, d, q)(P, D, Q)[s]
  #' =============================================================== #
  model_string %>%
    str_split(pattern = fixed(')')) %>%
    unlist() %>%
    paste(collapse = ',') %>%
    str_remove_all(pattern = '[ARIMA()\\[\\]]') %>%
    str_split(pattern = fixed(',')) %>%
    unlist() %>%
    structure(names = c('p', 'd', 'q', 'P', 'D', 'Q', 's')) %>%
    as.list()
}

otto.arima <- function(y, ic = 'aicc', all_models = T, cutoff = 5, return_best = F, ...){
  #' returns an object of class otto with attributes model and table
  #' by default, the best model found by auto.arima is not returned;
  #' set return_best = T in order to do so
  #' by default, all models are returned; set all_models = F along
  #' with a cutoff value to keep only those models within a certain
  #' range of lowest information criterion used
  #' =============================================================== #
  # run auto.arima with most precise options enabled
  # capture output (string specifying model and information criterion
  # calculated for that model) as data.table
  out <- capture.output({
    m <- auto.arima(y = y, ic = ic, stepwise = F, approximation = F, trace = T, ...)
  })
  out_data <- setDT(read.table(con <- textConnection(out), sep = ':', as.is = T))
  close(con)
  ic <- toupper(ic)
  setnames(out_data, 'V1', 'Model')
  out_data <- out_data[!(Model %like% 'Best'), ]
  out_data[, Model := str_remove_all(Model, pattern = fixed(' '))]
  out_data[, ic := as.numeric(V2)]
  setnames(out_data, 'ic', ic)
  out_data[, V2 := NULL]
  # concatenate return object
  ret <- list('model' = NULL, 'table' = NULL)
  if(return_best) ret$model <- m
  if(!all_models){
    if(ic == 'AIC') m_ic <- m$aic
    if(ic == 'AICC') m_ic <- m$aicc
    if(ic == 'BIC') m_ic <- m$bic
    ret$table <- out_data[abs(m_ic - get(ic)) < cutoff, ]
  } else{
    ret$table <- out_data
  }
  class(ret) <- c('otto', class(ret))
  return(ret)
}

# TODO - automated search for regressors
#   -for Arima and tslm

# TODO - automated search for Fourier terms
#   -for Arima and tslm
