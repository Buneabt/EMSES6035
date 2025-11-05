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
    alt_id = c(1, 2, 3),
    obs_id = c(1, 1, 1),
    price = c(50,50,50),
    capacity  = c(1,1,1),
    range  = c(1,1,1),
    type_implantable  = c(1,0,0),
    type_ring  = c(0,1,0),
    type_bracelet  = c(0,0,0),
    compatabilityi_os_android  = c(1,1,1),
    compatabilityi_os = c(0,0,0)
)


# Check the column names match your model
names(baseline)

# Use the predict() function to compute the probabilities
sim_mnl <- predict(
    model1,
    newdata = baseline,
    obsID = 'obs_id',
    level = 0.95,
    interval = 'confidence',
    returnData = TRUE
)

sim_mnl
