# RFID Device Conjoint Survey Simulation
library(here)
library(cbcTools)
library(tidyverse)

set.seed(123)

# Define profiles
profiles <- cbc_profiles(
    type         = c('Implant', 'Ring', 'Bracelet', 'Card'),
    price        = c(25, 50, 100), # $
    compatability = c('iOS', 'Android', 'Both'),
    capacity     = c(1, 3, 5), # kilobytes
    range        = c(1, 3, 5)  # feet
)

# Define priors
priors <- cbc_priors(
    profiles = profiles,
    price = -0.05,
    type = c(-1, 2, 1),  # Ring, Implant, Card vs Bracelet
    compatability = c(-1, 1),  # Android, iOS vs Both
    capacity = 1.5,  # 3KB, 5KB vs 1KB
    range = 1  # 3ft, 5ft vs 1ft
)

# Generate design
design <- cbc_design(
    profiles = profiles,
    n_resp   = 500,
    n_alts   = 3,
    n_q      = 6
)

# Simulate choices
simulated_choices <- cbc_choices(design, priors = priors)


# Final dataset with session_id
simulated_choices_final <- simulated_choices %>% 
    mutate(session_id = paste0("session_", respID))


# Save data
write_csv(simulated_choices_final, "data/rfid_simulated_choices.csv")