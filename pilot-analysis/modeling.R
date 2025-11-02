# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(cbcTools)
library(janitor)
library(here)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here("data", "choice_data.csv"))
head(data)

# Estimate MNL model
model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "capacity", "range", "type", "compatability")
)

# View summary of results
summary(model)

saveRDS(model, file = "Model1.rds")

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values
