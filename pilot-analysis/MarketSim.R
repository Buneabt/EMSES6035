# Compute expected probabilities of different alternatives

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)

# Load estimated models
model1 <- readRDS("Model1.rds")

# -----------------------------------------------------------------------------
# Single market simulation using the mnl model

summary(model1)

# Create a set of alternatives for which to simulate shares
baseline <- data.frame(
    altID = c(1, 2, 3),
    obsID = c(1, 1, 1),
    price = c(25, 25, 25),
    capacity = c(1, 1, 1),
    compatabilityiOS = c(1,1,1),
    compatabilityi0S/Android = c(0,0,0),    
    range = c(3,3,3),
    typeRing = c(0,0,1),
    typeCard = c(0,1,0),
    typeImplantable = c(1,0,0)
)

#pars = c("capacity", "range", "type", "compatability")

# Columns are attributes, rows are alternatives
baseline

# Use the predict() function to compute the probabilities
sim_mnl <- predict(
    model1,
    newdata = baseline,
    obsID = 'obsID',
    level = 0.95,
    interval = 'confidence',
    returnData = TRUE # This returns your data along with predicted values
)

sim_mnl

# -----------------------------------------------------------------------------
## Multiple simulations using the mnl model
#
## Read in market scenarios
#scenarios <- read_csv(here('data', 'scenarios.csv'))
#head(scenarios)
#
## Use the predict() function to compute the probabilities
#sim_mnl_multi <- predict(
#    model1,
#    newdata = scenarios,
#    obsID = 'obsID',
#    level = 0.95,
#    interval = 'confidence',
#    returnData = TRUE
#)
#
#head(sim_mnl_multi)
#
## Save simulations
#save(
#    sim_mnl,
#    sim_mnl_multi,
#    file = here("sims", "mnl.RData")
#)
#