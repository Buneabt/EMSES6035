# Estimate mixed logit (MXL) models

# Load libraries
library(logitr)
library(tidyverse)
library(here)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here('data', 'rfid_simulated_choices_base.csv'))
head(data)


# -----------------------------------------------------------------------------
# Estimate preference space MXL model with linear price, fuelEconomy, and accelTime

# Estimate the model
model_mxl_pref <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c('type','price', 'compatability', 'capacity', 'range',"choice"),
  randPars = c(type = "n")
)

# View summary of results
summary(model_mxl_pref)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mxl_pref$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mxl_pref$hessian)$values

# -----------------------------------------------------------------------------
# Estimate WTP space MXL model with linear price

# Estimate the model
model_mxl_wtp <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c('type', 'compatability', 'capacity', 'range',"choice"),
  scalePar = 'price',
  randPars = c(type = "n")
)

# View summary of results
summary(model_mxl_wtp)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mxl_wtp$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mxl_wtp$hessian)$values

# -----------------------------------------------------------------------------
# Save model objects

save(
  model_mxl_pref,
  model_mxl_wtp,
  file = here("final-analysis", "model_mxl.RData")
)
