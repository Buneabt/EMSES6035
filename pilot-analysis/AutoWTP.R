# Directly estimate WTP using a "WTP Space" model

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(fastDummies)

options(dplyr.width = Inf) # So you can see all of the columns

# Load the data set:
data <- read_csv(here('data', 'choice_data.csv'))
head(data)

# Estimate the model
model_mnl_wtp <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c("capacity", "range", "type", "compatability"),
    scalePar = 'price',
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
)

# View summary of results
summary(model_mnl_wtp)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mnl_wtp$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl_wtp$hessian)$values

# Compare computed versus estimated WTP
model1 <- readRDS("Model1.rds")
wtpCompare(model1, model_mnl_wtp, scalePar = 'price')

# Save model
save(
    model_mnl_wtp,
    file = here("data", "model_mnl_wtp.RData")
)
