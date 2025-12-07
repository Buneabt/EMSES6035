# You should write code here to analyze your model results, e.g. computing WTP,
# market simulations, sensitivity analyses, etc.
library(logitr)
library(tidyverse)
library(here)
library(fastDummies)
# Compute WTP

model_base <- readRDS("Model1.rds")
model_afraid <- readRDS("Model_afraid.rds")
model_unafraid <- readRDS("Model_unafraid.rds")

# Get the model coefficients
coefs <- coef(model_base)
coefs

# Compute WTP estimates
wtp <- coefs / (-1 * coefs['price'])

# Compute WTP with uncertainty:

# Get the model coefficients and covariance matrix
covariance <- vcov(model_base)

# Take 10,000 draws of the coefficients
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))

# Compute WTP for each coefficient draw
wtp_draws = -1 * (coef_draws[, 2:8] / coef_draws[, 1])
head(wtp_draws)

# For each coefficient, get the mean and 95% confidence interval of WTP
wtp_ci <- ci(wtp_draws, level = 0.95)
wtp_ci
