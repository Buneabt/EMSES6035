# RFID Device Conjoint Survey Simulation
library(here)
library(cbcTools)
library(tidyverse)

set.seed(123)

# Define profiles
profiles <- cbc_profiles(
    type         = c('Bracelet', 'Ring', 'Card', 'Implant'),
    price        = c(25, 50, 100), # $
    compatability = c('Both', 'Android', 'iOS'),
    capacity     = c(1, 3, 5), # kilobytes
    range        = c(1, 3, 5)  # feet
)

# Define priors
priors <- cbc_priors(
    profiles = profiles,
    price = -0.5,
    type = c(1.75, 1, 0.5),  # Ring, Bracelet, Implant vs Card
    compatability = c(-2, 0.5),  # Android, Both vs iOS
    capacity = 1.5,  
    range = 1  
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