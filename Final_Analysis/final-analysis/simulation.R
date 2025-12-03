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
    price = -0.02,
    type = c(-2.8, 0.4, 0.9),  # Implant, Ring, Card vs Bracelet
    compatability = c(-0.3, 1.9),  # iOS, Both vs Android
    capacity = c(0.7, 1.1),  # 3KB, 5KB vs 1KB
    range = c(0.9, 1.4)  # 3ft, 5ft vs 1ft
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