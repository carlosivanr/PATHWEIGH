# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. Cu Anschutz Dept. of Family Medicine
# Generate model plot:
# This function was designed to generate model plots

# Inputs:
# data - the data used in model building
# m - the model object output from the model

# Output:
# Generates a plot with a trendline overlaid the observed and predicted weight
#   values.
# Plot x axis is the month after the index date, y axis is weight in kgs

# Notes: This function is designed to work with a model object that includes the
# terms N_days_post_id and N_days_post_180.

# The model must include terms for the first 6 months of weight loss and then the 
# weight loss after 6 months and is bound by 18 months

# Two separate, but contiguous trendlines are overlaid the actual and predicted
# values

# Trend line segment 1 - models the first 6 months
# Consists of two points and one slope for the first 6 months
# a. the y intercept is the average of all observations/predictions for 18 
# months of data. 
# b. slope for the first 6 months is calculated from n_days_post_id
# c. the x value of the first segment is the 6month time point, but the y value
# is calculated from the slope

# Trend line segment 2 - models months 7 through 18
# y intercept is the endpoint of line segment 1 and uses the slope for the
# remaining time period


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_model_plot <-  function(mod_data, m){
  # Inputs:
  # mod_data - data set used to generate the model
  # m - model object
  
  # Generate predicted values ----
  predicted_vals <- predict(m, mod_data)
  
  # Merge predicted values into the analysis data set ----
  m_plot_data <- 
    bind_cols(mod_data, data.frame(predicted_vals)) %>%
    rename(Weight_pred = "predicted_vals") %>% 
    select(Arb_PersonId, Weight_dv, Weight_pred, N_months_post_id) %>%
    group_by(Arb_PersonId, N_months_post_id) %>%
    summarise(Weight_dv = mean(Weight_dv),
              Weight_pred = mean(Weight_pred, na.rm = TRUE), # na.rm to remove the missing values of predicted_vals
              .groups = "drop") %>%
    pivot_longer(cols = Weight_dv:Weight_pred, values_to = "Weight", names_to = "Type") %>%
    group_by(N_months_post_id, Type) %>%
    summarize(Avg_Weight_kgs = mean(Weight),
              n = n(), 
              .groups = "drop")
  
  # Capture the coefficients for the ndays post id and ndays post 180 terms ----
  coefficients <- broom.mixed::tidy(m) %>%
    select(term, estimate) %>%
    mutate_if(is.numeric, ~round(.,4)) %>%
    filter(term %in% c("N_days_post_id", "N_days_post_180"))
  
  # Estimate for N days post id
  est_post_id <- coefficients %>%
    filter(term == "N_days_post_id") %>%
    pull(estimate)
  
  # Estimate for n days post id for 1 month
  est_post_id_30d <- est_post_id * 30

  
  # Estimate for N days post 180 which is the sum of post_id and post_180
  est_post_180 <- sum(coefficients$estimate)
  
  # Estimate for post 180 for 1 month
  est_post_180_30d <- est_post_180 * 30
  
  # Slope for predicted change at 6 months
  a <-  est_post_id_30d * 6

  # Slope for predicted change at 18 months
  b <- est_post_180_30d * 12
  
  # Average of all 18 monthly values on the green line (predicted)
  predicted_avg <- m_plot_data %>%
    filter(Type == "Weight_pred") %>%
    pull(Avg_Weight_kgs) %>%
    mean()
  
  # Trendline y intercept at 0 months
  y_int <- predicted_avg - a 
  
  # Trendline y intercept at 6 months
  month_6_y_int <- y_int + (6 * est_post_id_30d) 
  # month_6_y_int <- y_int - (6 * .13) # may have been .12 instead of .13
  
  # Trendline y intercept at 18 months
  month_18_y_int <- month_6_y_int + (12 * est_post_180_30d)
  # month_18_y_int <- month_6_y_int - (12 * .03)
  
  # Base plot
  p <- m_plot_data %>%
    ggplot(., aes(x = N_months_post_id, y = Avg_Weight_kgs, color = Type, linetype = "Trendline")) +
    geom_line() +
    
    # Trendline segment 1:
    # from 0 to 6 months on x axis
    # from y intercept to month 6 y intercept
    geom_segment(aes(x = 0, xend = 6, y = y_int, yend = month_6_y_int), color = "blue") +
    
    # Trendline segment 2:
    # from 6 to 18 months on x axis
    # from y intercept at 6 months to y intercept at 18 months
    geom_segment(aes(x = 6, xend = 18, y = month_6_y_int, yend = month_18_y_int), color = "blue") + 
    theme(legend.title=element_blank()) + 
    scale_color_discrete(labels = c("Actual", "Predicted")) +
    ylab("Avg. weight (kgs)") +
    xlab("Months after index visit") +
    theme_minimal() +
    theme(legend.title= element_blank(),
          legend.text=element_text(size=14),
          axis.line = element_line(color = "grey70"),
          axis.text = element_text(size=14), # Controls the axis text size
          axis.title = element_text(size=14)) # Controls the axis label size
  
  # out <- list(p, y_int, month_6_y_int, month_18_y_int)
  # 
  # return(out)
  
  return(p)
  
}

generate_model_plot_lmer <-  function(mod_data, m){
  # Inputs:
  # mod_data - data set used to generate the model
  # m - model object
  
  # Generate predicted values ----
  predicted_vals <- predict(m, mod_data)
  
  # Merge predicted values into the analysis data set ----
  m_plot_data <- 
    bind_cols(mod_data, data.frame(predicted_vals)) %>%
    rename(Weight_pred = "predicted_vals") %>% 
    select(Arb_PersonId, Weight_dv, Weight_pred, N_months_post_id) %>%
    group_by(Arb_PersonId, N_months_post_id) %>%
    summarise(Weight_dv = mean(Weight_dv),
              Weight_pred = mean(Weight_pred, na.rm = TRUE), # na.rm to remove the missing values of predicted_vals
              .groups = "drop") %>%
    pivot_longer(cols = Weight_dv:Weight_pred, values_to = "Weight", names_to = "Type") %>%
    group_by(N_months_post_id, Type) %>%
    summarize(Avg_Weight_kgs = mean(Weight),
              n = n(), 
              .groups = "drop")
  
  # Capture the coefficients for the ndays post id and ndays post 180 terms ----
  coefficients <- broom.mixed::tidy(m) %>%
    select(term, estimate) %>%
    mutate_if(is.numeric, ~round(.,4)) %>%
    filter(term %in% c("N_days_post_id", "N_days_post_180"))
  
  # Estimate for N days post id
  est_post_id <- coefficients %>%
    filter(term == "N_days_post_id") %>%
    pull(estimate)
  
  # Estimate for n days post id for 1 month
  est_post_id_30d <- est_post_id * 30
  
  
  # Estimate for N days post 180 which is the sum of post_id and post_180
  est_post_180 <- sum(coefficients$estimate)
  
  # Estimate for post 180 for 1 month
  est_post_180_30d <- est_post_180 * 30
  
  # Slope for predicted change at 6 months
  a <-  est_post_id_30d * 6
  
  # Slope for predicted change at 18 months
  b <- est_post_180_30d * 12
  
  # Average of all 18 monthly values on the green line (predicted)
  predicted_avg <- m_plot_data %>%
    filter(Type == "Weight_pred") %>%
    pull(Avg_Weight_kgs) %>%
    mean()
  
  # Trendline y intercept at 0 months
  y_int <- predicted_avg - a 
  
  # Trendline y intercept at 6 months
  month_6_y_int <- y_int + (6 * est_post_id_30d) 
  # month_6_y_int <- y_int - (6 * .13) # may have been .12 instead of .13
  
  # Trendline y intercept at 18 months
  month_18_y_int <- month_6_y_int + (12 * est_post_180_30d)
  # month_18_y_int <- month_6_y_int - (12 * .03)
  
  # Base plot
  p <- m_plot_data %>%
    ggplot(., aes(x = N_months_post_id, y = Avg_Weight_kgs, color = Type, linetype = "Trendline")) +
    geom_line() +
    
    # Trendline segment 1:
    # from 0 to 6 months on x axis
    # from y intercept to month 6 y intercept
    # geom_segment(aes(x = 0, xend = 6, y = y_int, yend = month_6_y_int), color = "blue") +
    
    # Trendline segment 2:
    # from 6 to 18 months on x axis
    # from y intercept at 6 months to y intercept at 18 months
    # geom_segment(aes(x = 6, xend = 18, y = month_6_y_int, yend = month_18_y_int), color = "blue") + 
    
    
    theme(legend.title=element_blank()) + 
    scale_color_discrete(labels = c("Actual", "Predicted")) +
    ylab("Avg. weight (kgs)") +
    xlab("Months after index visit") +
    theme_minimal() +
    theme(legend.title= element_blank(),
          legend.text=element_text(size=14),
          axis.line = element_line(color = "grey70"),
          axis.text = element_text(size=14), # Controls the axis text size
          axis.title = element_text(size=14)) # Controls the axis label size
  
  # out <- list(p, y_int, month_6_y_int, month_18_y_int)
  # 
  # return(out)
  
  return(p)
  
}
  
  