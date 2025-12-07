# You should write code here to analyze your model results, e.g. computing WTP,
# market simulations, sensitivity analyses, etc.
library(logitr)
library(tidyverse)
library(here)
library(fastDummies)
library(cowplot)
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

# Separate coefficient CIs by attribute

wtp_ci$par <- row.names(wtp_ci)
wtp_capacity <- wtp_ci %>% filter(par == 'capacity')
wtp_range <- wtp_ci %>% filter(par == 'range')
wtp_ring <- wtp_ci %>% filter(par == 'type_ring')
wtp_bracelet <- wtp_ci %>% filter(par == 'type_bracelet')
wtp_implant <- wtp_ci %>% filter(par == 'type_implant')
wtp_android <- wtp_ci %>% filter(par == 'compatability_android')
wtp_both <- wtp_ci %>% filter(par == 'compatability_both')

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_capacity <- data.frame(level = c(1,3,5)) %>%
    mutate(
        diff = level - min(level),
        mean = diff * wtp_capacity$mean,
        lower = diff * wtp_capacity$lower,
        upper = diff * wtp_capacity$upper
    )

df_range <- data.frame(level = c(1,3,5)) %>%
    mutate(
        diff = level - min(level),
        mean = diff * wtp_range$mean,
        lower = diff * wtp_range$lower,
        upper = diff * wtp_range$upper
    )

df_type <- data.frame(level = c("Card", "Ring","Bracelet","Implant")) %>%
    mutate(
        mean = c(0, wtp_ring$mean, wtp_bracelet$mean,wtp_implant$mean),
        lower = c(0, wtp_ring$lower, wtp_bracelet$lower,wtp_implant$lower),
        upper = c(0, wtp_ring$upper, wtp_bracelet$upper,wtp_implant$upper)
    )

df_compatability <- data.frame(level = c("iOS", "Android","Both")) %>%
    mutate(
        mean = c(0, wtp_android$mean, wtp_both$mean),
        lower = c(0, wtp_android$lower, wtp_both$lower),
        upper = c(0, wtp_android$upper, wtp_both$upper)
    )

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(c(
    df_capacity$lower,
    df_range$lower,
    df_type$lower,
    df_compatability$lower
)))
ymax <- ceiling(max(c(
    df_capacity$upper,
    df_range$upper,
    df_type$upper,
    df_compatability$upper
)))

# Plot the WTP for each attribute *with 95% CI*
plot_capacity <- df_capacity %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) +
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Product Capacity (KB)', y = 'WTP ($10)') +
    theme_bw()

plot_range <- df_range %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) +
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Product Range (feet)', y = 'WTP ($10)') +
    theme_bw()

plot_type <- df_type %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Product Type', y = 'WTP ($10)') +
    theme_bw()

plot_compatability <- df_compatability %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Operating System Compatability', y = 'WTP ($10)') +
    theme_bw()

plot_mnl_wtp <- plot_grid(
    plot_range,
    plot_capacity,
    plot_type,
    plot_compatability,
    nrow = 2
)
