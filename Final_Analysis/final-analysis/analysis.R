# You should write code here to analyze your model results, e.g. computing WTP,
# market simulations, sensitivity analyses, etc.
library(logitr)
library(tidyverse)
library(here)
library(fastDummies)
library(cowplot)

respondent <- read.csv(here('data','FinalSurveyV2_2025-12-03.csv'))

gender <- respondent %>% 
    filter(!is.na(gender)) %>% 
    filter(gender == "man")
nrow(gender)

needles <- respondent %>% 
    filter(needles == "blue")
nrow(needles)

rfid <- respondent %>% 
    filter(rfid_familiar == "yes")
nrow(rfid)

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
    labs(x = 'Product Capacity (KB)', y = 'WTP ($)') +
    theme_bw()

plot_range <- df_range %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) +
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Product Range (feet)', y = 'WTP ($)') +
    theme_bw()

plot_type <- df_type %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Product Type', y = 'WTP ($)') +
    theme_bw()

plot_compatability <- df_compatability %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'OS Compatibility', y = 'WTP ($)') +
    theme_bw()

plot_mnl_wtp <- plot_grid(
    plot_range,
    plot_capacity,
    plot_type,
    plot_compatability,
    nrow = 2
)

ggsave(
    filename = here('images', 'plot_mnl_wtp.jpeg'),
    plot = plot_mnl_wtp,
    width = 5,
    height = 3
)


# Market Simulation
# Single market simulation using the mnl model

summary(model_base)

# Create a set of alternatives for which to simulate shares
baseline <- data.frame(
    altID = c(1, 2, 3, 4),
    obsID = c(1, 1, 1, 1),
    price = c(20, 16, 25, 15),
    range = c(1,1,1,1),
    capacity = c(3,1,5,1),
    type_ring = c(1, 0, 0,0),
    type_bracelet = c(0, 1, 0,0),
    type_implant = c(0, 0, 1,0),
    compatability_android = c(0,0,0,0),
    compatability_both = c(1,1,1,1)
)

# Columns are attributes, rows are alternatives
baseline

# Use the predict() function to compute the probabilities
sim_mnl <- predict(
    model_base,
    newdata = baseline,
    obsID = 'obsID',
    level = 0.95,
    interval = 'confidence',
    returnData = TRUE # This returns your data along with predicted values
)

sim_mnl

## Plot Market Simulation
plot_sim_mnl <- sim_mnl %>%
    mutate(label = c("Jakcom Ring", "Fobster Bracelet", "RFIDinMe", "Jiaxing Card")) %>%
    ggplot(aes(
        x = label,
        y = predicted_prob,
        ymin = predicted_prob_lower,
        ymax = predicted_prob_upper
    )) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Alternative', y = 'Market Share') +
    theme_bw()
plot_sim_mnl
ggsave(
    filename = here('images', 'sim_mnl.jpeg'),
    plot = plot_sim_mnl,
    width = 5,
    height = 3
)

## Sensitivity for Price

# Define the sensitivity cases

prices <- seq(10, 30) # Define sensitivity price levels
n <- length(prices) # Number of simulations 
rep_df <- function(df, n) {
    result <- df[rep(seq_len(nrow(df)), n), ]
    row.names(result) <- NULL
    return(result)
}
scenarios_price <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_price$obsID <- rep(seq(n), each = 4) # Reset obsIDs

# Set the price for each scenario
scenarios_price$price[which(scenarios_price$altID == 3)] <- prices
head(scenarios_price)

# For each case, simulate the market share predictions
sens_price <- predict(
    model_base,
    newdata = scenarios_price,
    obsID = 'obsID',
    level = 0.95,
    interval = 'confidence',
    returnData = TRUE
) %>%
    # Keep only RFIDinMe alternative
    filter(altID == 3) %>%
    # Keep only prices and predictions
    select(price, starts_with("predicted_"))

sens_price

## Sensitivity to Other Attributes

# "high" means they result in higher market shares
# "low"  means they result in lower market shares
cases <- tribble(
    ~obsID, ~altID, ~attribute,    ~case,  ~value,
    2,      3,     'price',       'high',  20*0.8,
    3,      3,     'price',       'low',   20*1.2,
    4,      3,     'range',       'high',  2*1.2,
    5,      3,     'range',       'low',   2*0.8,
    6,      3,     'capacity',    'high',  2*0.8,
    7,      3,     'capacity',    'low',   2*1.2
)

cases

# Define scenarios
n <- 7 # baseline + high & low for each attribute
scenarios_atts <- rep_df(baseline, n)
scenarios_atts$obsID <- rep(seq(n), each = 4) # Reset obsIDs

# Replace scenarios with case values

scenarios_atts <- scenarios_atts %>%
    left_join(cases, by = c("altID", "obsID")) %>%
    mutate(
        attribute = ifelse(is.na(attribute), "other", attribute),
        case = ifelse(is.na(case), "base", case),
        price = ifelse(!is.na(value) & attribute == 'price', value, price),
        range = ifelse(!is.na(value) & attribute == 'range', value, range),
        capacity = ifelse(!is.na(value) & attribute == 'capacity', value, capacity)
    )

scenarios_atts

# For each case, simulate the market share predictions
sens_atts <- predict(
    model_base,
    newdata = scenarios_atts,
    obsID = 'obsID',
    level = 0.95,
    interval = 'confidence',
    returnData = TRUE
) %>%
    # Keep only RFIDinMe alternative
    filter(altID == 3) %>%
    # Keep only attributes and predictions
    select(attribute, case, value, predicted_prob)

sens_atts


# Make a tornado diagram to show market sensitivity to multiple
ggtornado <- function(
        data,
        baseline,
        var,
        level,
        value,
        result
) {
    
    # Create a new data frame for plotting
    df <- data[c(var, level, value, result)]
    colnames(df) <- c('var', 'level', 'value', 'result')
    
    # Add hust based on the level
    df$hjust <- rep(c(0, 1), nrow(df) / 2)
    
    # "Center" the result around the baseline result (so baseline is at 0)
    df$result <- df$result - baseline
    
    # Compute the range in change from low to high levels for sorting
    df$resultRange <- stats::ave(abs(df$result), df$var, FUN = sum)
    
    # dplyr solution
    # df <- df %>%
    #     # "Center" the result around the baseline result (so baseline is at 0)
    #     mutate(result = result - baseline) %>%
    #     # Compute the range in change from low to high levels for sorting
    #     group_by(var) %>%
    #     mutate(resultRange = sum(abs(result)))
    
    # Compute labels for the x-axis
    lb        <- floor(10*min(df$result))/10
    ub        <- ceiling(10*max(df$result))/10
    breaks    <- seq(lb, ub, (ub - lb) / 5)
    breakLabs <- round(breaks + baseline, 2)
    
    # Make the tornado diagram
    plot <- ggplot(df,
                   aes(
                       x = .data$result,
                       y = stats::reorder(.data$var, .data$resultRange),
                       fill = level)
    ) +
        geom_col(width = 0.6) +
        # Add labels on bars
        geom_text(aes(label = .data$value, hjust = .data$hjust), vjust = 0.5) +
        scale_x_continuous(
            limits = c(lb, ub),
            breaks = breaks,
            labels = breakLabs) +
        labs(x = 'Result', y = 'Parameter') +
        theme_bw() +
        theme(legend.position = 'none') # Remove legend
    
    return(plot)
}
labels <- data.frame(
    attribute = c('price', 'range', 'capacity'),
    label = c(
        'Price ($)',
        'Range (Ft)',
        'Capacity (kB)'
    )
)

tornado_data <- sens_atts %>%
    filter(case != 'base') %>%
    # Rename variables for plotting labels
    left_join(labels, by = 'attribute')

tornado_base <- ggtornado(
    data = tornado_data,
    baseline = sens_atts$predicted_prob[1],
    var = 'label',
    level = 'case',
    value = 'value',
    result = 'predicted_prob'
)

# Change the fill colors, adjust labels
tornado_plot <- tornado_base +
    scale_fill_manual(values = c("#67a9cf", "#ef8a62")) +
    labs(x = 'Market Share', y = 'Attribute')

tornado_plot

ggsave(
    filename = here('images', 'tornado.jpeg'),
    plot = tornado_plot,
    width = 5,
    height = 3
)
