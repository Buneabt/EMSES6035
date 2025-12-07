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
data <- read_csv(here("data", "rfid_simulated_choices_afraid.csv")) %>% 
    select(-session_id)
head(data)

#data <- read_csv(here("data", "choice_data.csv")) %>% 
#    select(-session_id)
#head(data)



data <- cbcTools::cbc_encode(data, coding = 'dummy')

data <- clean_names(data)


# Estimate MNL model
model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obs_id",
    pars = c("price", "capacity", "range", "type_ring", "type_bracelet", "type_implant", "compatability_android", "compatability_both")
)

# View summary of results
summary(model)

saveRDS(model, file = "Model_afraid.rds")

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values
