# Make conjoint surveys using the cbcTools package

# Install packages
# install.packages("remotes")
# install.packages("tidyverse")
# remotes::install_github("jhelvy/cbcTools")

# Load libraries
library(here)
library(cbcTools)
library(tidyverse)

# Define profiles with attributes and levels
profiles <- cbc_profiles(
  type      = c('Implant', 'Ring', 'Bracelet', 'Card'),
  price     = c(25,50,100), # $
  compatability = c('iOS', 'Android', 'Both'),
  capacity = c(1,3,5),# kilobytes
  range = c(1,3,5) # feet
)

# Make a basic survey using the full factorial of all profiles
design <- cbc_design(
  profiles = profiles,
  n_resp   = 500, # Number of respondents
  n_alts   = 3,    # Number of alternatives per question
  n_q      = 6     # Number of questions per respondent
)

head(design) # preview

# Add image names matched to the apple type
# (we'll use these to display images in the survey)
image_names <- data.frame(
  type = c('Bracelet','Card','Ring','Implant'),
  image = c(
    'Bracelet.jpg',
    'Card.jpg',
    'Ring.jpg',
    'Implant.jpg')
)
design <- design %>%
  left_join(image_names, by = "type")

head(design) # preview

# Save design
write_csv(design, here("data", "choice_questions.csv"))



