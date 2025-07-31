<<<<<<< HEAD
setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

library(parameters)
library(dplyr)
library(ggplot2)
library(ggeffects)

df <- read.csv("wls_merged_250719.csv")
df2 <- df

# DepGPS_composite2 
df2$row_mean <- rowMeans(df2[, c("Depression_GPS", "Neuroticism_GPS")], na.rm = TRUE)
df2$DepGPS_composite2 <- scale(df2$row_mean)
df2$DepGPS_composite2 <- as.numeric(scale(df2$row_mean))




################################# Variables ################################################

# moderators (prs)
mod <- c('DepGPS_composite',
           'DepGPS_composite2',
           'IntelligenceGPS_composite',
           'Depression_GPS', 'MDD_GPS', 'Neuroticism_GPS',
           'EA_GPS','IQ_GPS','CP_GPS')

outcome <- c('Depression_score_R6')

# Covariates
cov_cont <- c('age_R5', 'Personal_Income_R6', 'Education_years_R6')
cov_factor <- c('sex', 'Marital_status_R6_new')
df2[cov_factor] <- lapply(df2[cov_factor], factor)

cov_list <- c(cov_cont, cov_factor)
COVARS <- paste(cov_list, collapse = " + ")

# independent variables
PersonalResource <- c('Autonomy_R5', 'EnvironmentalMastery_R5', 'PersonalGrowth_R5',
                      'PositiveRelationship_R5', 'SelfAcceptance_R5', 'PurposeinLife_R5',
                      'Optimism_R5', 'Mattering_R5', 'PersonalResource_composite')

SocialSupport1 <- c('Support_money_R5', 'Support_problem_R5', 'Support_sick_R5', 'SocialSupport1_composite')
SocialSupport2 <- c('Love_social_R5','Listen_social_R5','SocialSupport2_composite')
SocialParticipation1 <- c('Social_friends_R5', 'Social_relatives_R5', 'SocialParticipation1_composite')
Social_involvement_R5 <- c('Social_involvement_R5')
indep <- c(PersonalResource, SocialSupport1, SocialSupport2, SocialParticipation1, Social_involvement_R5)


############################## straight line plot 1 ############################################3


library(dplyr)
library(ggplot2)
library(ggeffects)

# function for interaction plot

make_interaction_plot_grouped <- function(indep_var, mod_var, data, outcome_var, COVARS) {
  # Create grouping (10% and 90% quantiles)
  quantiles <- quantile(data[[mod_var]], probs = c(0.10, 0.90), na.rm = TRUE)
  group_var <- paste0(mod_var, "_group")
  
  data[[group_var]] <- cut(data[[mod_var]],
                           breaks = c(-Inf, quantiles[1], quantiles[2], Inf),
                           labels = c("low", "middle", "high"),
                           right = TRUE)
  
  # Build formula
  formula_str <- paste(
    outcome_var, "~", group_var, "*", indep_var, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + ")
  )
  
  model_formula <- as.formula(formula_str)
  model <- lm(model_formula, data = data)
  
  # Get predicted values for plotting interaction
  pred_df <- ggpredict(model, terms = c(indep_var, group_var))
  
  # Plot
  p <- ggplot(pred_df, aes(x = x, y = predicted, color = group, fill = group, linetype = group)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
    scale_color_manual(values = c("green", "blue", "red")) +
    scale_fill_manual(values = c("green", "blue", "red")) +
    labs(
      title = paste("Moderation Effect"),
      x = indep_var,
      y = paste("Predicted", outcome_var),
      color = group_var,
      fill = group_var,
      linetype = group_var
    ) +
    theme_minimal(base_size = 12)
  
  return(p)
}

# use function to plot
plot1 <- make_interaction_plot_grouped(
  indep_var = "SelfAcceptance_R5",
  mod_var = "DepGPS_composite2",
  data = df2,
  outcome_var = "Depression_score_R6",
  COVARS = COVARS
)

print(plot1)




########################## Curved Interaction Plot ########################################3



library(dplyr)
library(ggplot2)


mode_factor <- function(x) {
  ux <- na.omit(x)
  levels(ux)[which.max(tabulate(ux))]
}

make_curve_plot <- function(data,                       
                            indep_var,                    
                            mod_var,                    
                            outcome_var,                
                            cov_cont,                   
                            cov_factor) {               
  
  ## helper: modal level for factors (for representative covariate values)
  mode_factor <- function(x) {
    ux <- na.omit(x)
    levels(ux)[which.max(tabulate(ux))]
  }
  
  
  ## 1) split moderator into Low / Mid / High (10 / 80 / 10 %)
  q <- quantile(data[[mod_var]], probs = c(.10, .90), na.rm = TRUE)
  data <- data %>% 
    mutate(
      Mod_grp = cut(
        .data[[mod_var]],
        breaks = c(-Inf, q[1], q[2], Inf),
        labels = c("Low (≤10th)", "Middle (10-90th)", "High (≥90th)"),
        right  = TRUE
      )
    )
  
  
  ## 2) fit linear model with interaction and covariates
  form <- reformulate(
    c(indep_var, "Mod_grp", paste0(indep_var, ":Mod_grp"), cov_cont, cov_factor),
    response = outcome_var
  )
  
  fit <- lm(form, data = data)
  
  ## 3) build prediction grid: 100 indep_var percentiles × 3 prs groups
  indep_grid <- tibble(
    indep_pct  = 1:100,
    !!indep_var := quantile(data[[indep_var]],
                          probs = seq(.01, 1, .01),
                          na.rm = TRUE)
  )
  
  cov_means <- bind_cols(
    data %>% summarise(across(all_of(cov_cont), ~ mean(.x, na.rm = TRUE))),
    data %>% summarise(across(all_of(cov_factor), ~ mode_factor(.x)))
  )
  
  newdat <- tidyr::crossing(
    indep_grid,
    tibble(Mod_grp = levels(data$Mod_grp))
  ) %>% 
    bind_cols(cov_means[rep(1, nrow(.)), ])   # recycle covariate row
  
  ## 4) predict outcome + CI
  pr <- predict(fit, newdata = newdat, interval = "confidence")
  newdat <- newdat %>%
    mutate(Pred = pr[, "fit"],
           CI_lo = pr[, "lwr"],
           CI_hi = pr[, "upr"])
  
  ## 5) compute High vs Low moderator difference at PGS 10th & 90th
  arrow_df <- newdat %>%
    filter(indep_pct %in% c(10, 90),
           Mod_grp %in% c("Low (≤10th)", "High (≥90th)")) %>%
    dplyr::select(indep_pct, Mod_grp, Pred) %>%
    tidyr::pivot_wider(names_from = Mod_grp,
                       values_from = Pred) %>%
    mutate(
      diff  = round(`Low (≤10th)` - `High (≥90th)`, 2),
      x     = indep_pct,
      xend  = indep_pct,
      yend  = `Low (≤10th)`,
      y     = `High (≥90th)`,
      label = paste0("Δ = ", diff),
      text_x = ifelse(indep_pct == 10, x + 2 , x + 2)
    )
  
  ## 6) plot
  ggplot(newdat,
         aes(x = indep_pct, y = Pred,
             colour = Mod_grp, fill = Mod_grp)) +
    geom_line(linewidth = 1.2) +
    geom_ribbon(aes(ymin = CI_lo, ymax = CI_hi),
                alpha = .15, colour = NA) +
    scale_colour_manual(values = c("#2b83ba", "#d7191c", "#abdda4")) +
    scale_fill_manual(values   = c("#2b83ba", "#d7191c", "#abdda4")) +
    labs(
      title  = paste0("Interaction: ", indep_var, " × ", mod_var),
      x      = paste("Percentile of ", indep_var),
      y      = paste("Predicted", outcome_var),
      colour = mod_var,
      fill   = mod_var
    ) +
    theme_minimal(base_size = 12) +
    
    ## dashed arrows
    geom_segment(data = arrow_df,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 inherit.aes = FALSE,
                 linetype = "dashed",
                 colour   = "black",
                 arrow    = arrow(length = unit(0.22, "cm"),
                                  ends   = "both",
                                  type   = "closed")) +
    
    ## labels
    geom_text(data = arrow_df,
              aes(x = text_x, y = (y + yend)/2, label = label),
              inherit.aes = FALSE,
              size = 3,
              hjust = 0)
}




plot1 <- make_curve_plot(
  data        = df2,
  indep_var   = "SelfAcceptance_R5",
  mod_var     = "DepGPS_composite2",
  outcome_var = "Depression_score_R6",
  cov_cont    = cov_cont,
  cov_factor  = cov_factor
)

print(plot1)


############################# straight line plot 2 ##############################################


library(dplyr)
library(ggplot2)


mode_factor <- function(x) {
  ux <- na.omit(x)
  levels(ux)[which.max(tabulate(ux))]
}

make_curve_plot <- function(data,                       
                            indep_var,                    
                            mod_var,                    
                            outcome_var,                
                            cov_cont,                   
                            cov_factor) {               
  
  ## helper: modal level for factors (for representative covariate values)
  mode_factor <- function(x) {
    ux <- na.omit(x)
    levels(ux)[which.max(tabulate(ux))]
  }
  
  
  ## 1) split moderator into Low / Mid / High (10 / 80 / 10 %)
  q <- quantile(data[[mod_var]], probs = c(.10, .90), na.rm = TRUE)
  data <- data %>% 
    mutate(
      Mod_grp = cut(
        .data[[mod_var]],
        breaks = c(-Inf, q[1], q[2], Inf),
        labels = c("Low (≤10th)", "Middle (10-90th)", "High (≥90th)"),
        right  = TRUE
      )
    )
  
  
  ## 2) fit linear model with interaction and covariates
  form <- reformulate(
    c(indep_var, "Mod_grp", paste0(indep_var, ":Mod_grp"), cov_cont, cov_factor),
    response = outcome_var
  )
  
  fit <- lm(form, data = data)
  
  ## 3) build prediction grid: 100 indep_var percentiles × 3 prs groups
  indep_seq <- seq(min(data[[indep_var]], na.rm = TRUE),
                   max(data[[indep_var]], na.rm = TRUE),
                   length.out = 100)
  
  indep_grid <- tibble(
    indep_pct = seq(1, 100),
    !!indep_var := indep_seq
  )
  
  
  cov_means <- bind_cols(
    data %>% summarise(across(all_of(cov_cont), ~ mean(.x, na.rm = TRUE))),
    data %>% summarise(across(all_of(cov_factor), ~ mode_factor(.x)))
  )
  
  newdat <- tidyr::crossing(
    indep_grid,
    tibble(Mod_grp = levels(data$Mod_grp))
  ) %>% 
    bind_cols(cov_means[rep(1, nrow(.)), ])   # recycle covariate row
  
  ## 4) predict outcome + CI
  pr <- predict(fit, newdata = newdat, interval = "confidence")
  newdat <- newdat %>%
    mutate(Pred = pr[, "fit"],
           CI_lo = pr[, "lwr"],
           CI_hi = pr[, "upr"])
  
  ## 5) compute High vs Low moderator difference at PGS 10th & 90th
  arrow_df <- newdat %>%
    filter(indep_pct %in% c(10, 90),
           Mod_grp %in% c("Low (≤10th)", "High (≥90th)")) %>%
    dplyr::select(indep_pct, Mod_grp, Pred) %>%
    tidyr::pivot_wider(names_from = Mod_grp,
                       values_from = Pred) %>%
    mutate(
      diff  = round(`Low (≤10th)` - `High (≥90th)`, 2),
      x     = indep_pct,
      xend  = indep_pct,
      yend  = `Low (≤10th)`,
      y     = `High (≥90th)`,
      label = paste0("Δ = ", diff),
      text_x = ifelse(indep_pct == 10, x + 2 , x + 2)
    )
  
  ## 6) plot
  ggplot(newdat,
         aes(x = indep_pct, y = Pred,
             colour = Mod_grp, fill = Mod_grp)) +
    geom_line(linewidth = 1.2) +
    geom_ribbon(aes(ymin = CI_lo, ymax = CI_hi),
                alpha = .15, colour = NA) +
    scale_colour_manual(values = c("#2b83ba", "#d7191c", "#abdda4")) +
    scale_fill_manual(values   = c("#2b83ba", "#d7191c", "#abdda4")) +
    labs(
      title  = paste0("Interaction: ", indep_var, " × ", mod_var),
      x      = indep_var,
      y      = paste("Predicted", outcome_var),
      colour = mod_var,
      fill   = mod_var
    ) +
    theme_minimal(base_size = 12) +
    
    ## dashed arrows
    geom_segment(data = arrow_df,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 inherit.aes = FALSE,
                 linetype = "dashed",
                 colour   = "black",
                 arrow    = arrow(length = unit(0.22, "cm"),
                                  ends   = "both",
                                  type   = "closed")) +
    
    ## labels
    geom_text(data = arrow_df,
              aes(x = text_x, y = (y + yend)/2, label = label),
              inherit.aes = FALSE,
              size = 3,
              hjust = 0)
}




plot1 <- make_curve_plot(
  data        = df2,
  indep_var   = "SelfAcceptance_R5",
  mod_var     = "DepGPS_composite2",
  outcome_var = "Depression_score_R6",
  cov_cont    = cov_cont,
  cov_factor  = cov_factor
)

print(plot1)

=======
setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

library(parameters)
library(dplyr)
library(ggplot2)
library(ggeffects)

df <- read.csv("wls_merged_250719.csv")
df2 <- df

# DepGPS_composite2 
df2$row_mean <- rowMeans(df2[, c("Depression_GPS", "Neuroticism_GPS")], na.rm = TRUE)
df2$DepGPS_composite2 <- scale(df2$row_mean)
df2$DepGPS_composite2 <- as.numeric(scale(df2$row_mean))




################################# Variables ################################################

# moderators (prs)
mod <- c('DepGPS_composite',
           'DepGPS_composite2',
           'IntelligenceGPS_composite',
           'Depression_GPS', 'MDD_GPS', 'Neuroticism_GPS',
           'EA_GPS','IQ_GPS','CP_GPS')

outcome <- c('Depression_score_R6')

# Covariates
cov_cont <- c('age_R5', 'Personal_Income_R6', 'Education_years_R6')
cov_factor <- c('sex', 'Marital_status_R6_new')
df2[cov_factor] <- lapply(df2[cov_factor], factor)

cov_list <- c(cov_cont, cov_factor)
COVARS <- paste(cov_list, collapse = " + ")

# independent variables
PersonalResource <- c('Autonomy_R5', 'EnvironmentalMastery_R5', 'PersonalGrowth_R5',
                      'PositiveRelationship_R5', 'SelfAcceptance_R5', 'PurposeinLife_R5',
                      'Optimism_R5', 'Mattering_R5', 'PersonalResource_composite')

SocialSupport1 <- c('Support_money_R5', 'Support_problem_R5', 'Support_sick_R5', 'SocialSupport1_composite')
SocialSupport2 <- c('Love_social_R5','Listen_social_R5','SocialSupport2_composite')
SocialParticipation1 <- c('Social_friends_R5', 'Social_relatives_R5', 'SocialParticipation1_composite')
Social_involvement_R5 <- c('Social_involvement_R5')
indep <- c(PersonalResource, SocialSupport1, SocialSupport2, SocialParticipation1, Social_involvement_R5)


############################## straight line plot 1 ############################################3


library(dplyr)
library(ggplot2)
library(ggeffects)

# function for interaction plot

make_interaction_plot_grouped <- function(indep_var, mod_var, data, outcome_var, COVARS) {
  # Create grouping (10% and 90% quantiles)
  quantiles <- quantile(data[[mod_var]], probs = c(0.10, 0.90), na.rm = TRUE)
  group_var <- paste0(mod_var, "_group")
  
  data[[group_var]] <- cut(data[[mod_var]],
                           breaks = c(-Inf, quantiles[1], quantiles[2], Inf),
                           labels = c("low", "middle", "high"),
                           right = TRUE)
  
  # Build formula
  formula_str <- paste(
    outcome_var, "~", group_var, "*", indep_var, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + ")
  )
  
  model_formula <- as.formula(formula_str)
  model <- lm(model_formula, data = data)
  
  # Get predicted values for plotting interaction
  pred_df <- ggpredict(model, terms = c(indep_var, group_var))
  
  # Plot
  p <- ggplot(pred_df, aes(x = x, y = predicted, color = group, fill = group, linetype = group)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
    scale_color_manual(values = c("green", "blue", "red")) +
    scale_fill_manual(values = c("green", "blue", "red")) +
    labs(
      title = paste("Moderation Effect"),
      x = indep_var,
      y = paste("Predicted", outcome_var),
      color = group_var,
      fill = group_var,
      linetype = group_var
    ) +
    theme_minimal(base_size = 12)
  
  return(p)
}

# use function to plot
plot1 <- make_interaction_plot_grouped(
  indep_var = "SelfAcceptance_R5",
  mod_var = "DepGPS_composite2",
  data = df2,
  outcome_var = "Depression_score_R6",
  COVARS = COVARS
)

print(plot1)




########################## Curved Interaction Plot ########################################3



library(dplyr)
library(ggplot2)


mode_factor <- function(x) {
  ux <- na.omit(x)
  levels(ux)[which.max(tabulate(ux))]
}

make_curve_plot <- function(data,                       
                            indep_var,                    
                            mod_var,                    
                            outcome_var,                
                            cov_cont,                   
                            cov_factor) {               
  
  ## helper: modal level for factors (for representative covariate values)
  mode_factor <- function(x) {
    ux <- na.omit(x)
    levels(ux)[which.max(tabulate(ux))]
  }
  
  
  ## 1) split moderator into Low / Mid / High (10 / 80 / 10 %)
  q <- quantile(data[[mod_var]], probs = c(.10, .90), na.rm = TRUE)
  data <- data %>% 
    mutate(
      Mod_grp = cut(
        .data[[mod_var]],
        breaks = c(-Inf, q[1], q[2], Inf),
        labels = c("Low (≤10th)", "Middle (10-90th)", "High (≥90th)"),
        right  = TRUE
      )
    )
  
  
  ## 2) fit linear model with interaction and covariates
  form <- reformulate(
    c(indep_var, "Mod_grp", paste0(indep_var, ":Mod_grp"), cov_cont, cov_factor),
    response = outcome_var
  )
  
  fit <- lm(form, data = data)
  
  ## 3) build prediction grid: 100 indep_var percentiles × 3 prs groups
  indep_grid <- tibble(
    indep_pct  = 1:100,
    !!indep_var := quantile(data[[indep_var]],
                          probs = seq(.01, 1, .01),
                          na.rm = TRUE)
  )
  
  cov_means <- bind_cols(
    data %>% summarise(across(all_of(cov_cont), ~ mean(.x, na.rm = TRUE))),
    data %>% summarise(across(all_of(cov_factor), ~ mode_factor(.x)))
  )
  
  newdat <- tidyr::crossing(
    indep_grid,
    tibble(Mod_grp = levels(data$Mod_grp))
  ) %>% 
    bind_cols(cov_means[rep(1, nrow(.)), ])   # recycle covariate row
  
  ## 4) predict outcome + CI
  pr <- predict(fit, newdata = newdat, interval = "confidence")
  newdat <- newdat %>%
    mutate(Pred = pr[, "fit"],
           CI_lo = pr[, "lwr"],
           CI_hi = pr[, "upr"])
  
  ## 5) compute High vs Low moderator difference at PGS 10th & 90th
  arrow_df <- newdat %>%
    filter(indep_pct %in% c(10, 90),
           Mod_grp %in% c("Low (≤10th)", "High (≥90th)")) %>%
    dplyr::select(indep_pct, Mod_grp, Pred) %>%
    tidyr::pivot_wider(names_from = Mod_grp,
                       values_from = Pred) %>%
    mutate(
      diff  = round(`Low (≤10th)` - `High (≥90th)`, 2),
      x     = indep_pct,
      xend  = indep_pct,
      yend  = `Low (≤10th)`,
      y     = `High (≥90th)`,
      label = paste0("Δ = ", diff),
      text_x = ifelse(indep_pct == 10, x + 2 , x + 2)
    )
  
  ## 6) plot
  ggplot(newdat,
         aes(x = indep_pct, y = Pred,
             colour = Mod_grp, fill = Mod_grp)) +
    geom_line(linewidth = 1.2) +
    geom_ribbon(aes(ymin = CI_lo, ymax = CI_hi),
                alpha = .15, colour = NA) +
    scale_colour_manual(values = c("#2b83ba", "#d7191c", "#abdda4")) +
    scale_fill_manual(values   = c("#2b83ba", "#d7191c", "#abdda4")) +
    labs(
      title  = paste0("Interaction: ", indep_var, " × ", mod_var),
      x      = paste("Percentile of ", indep_var),
      y      = paste("Predicted", outcome_var),
      colour = mod_var,
      fill   = mod_var
    ) +
    theme_minimal(base_size = 12) +
    
    ## dashed arrows
    geom_segment(data = arrow_df,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 inherit.aes = FALSE,
                 linetype = "dashed",
                 colour   = "black",
                 arrow    = arrow(length = unit(0.22, "cm"),
                                  ends   = "both",
                                  type   = "closed")) +
    
    ## labels
    geom_text(data = arrow_df,
              aes(x = text_x, y = (y + yend)/2, label = label),
              inherit.aes = FALSE,
              size = 3,
              hjust = 0)
}




plot1 <- make_curve_plot(
  data        = df2,
  indep_var   = "SelfAcceptance_R5",
  mod_var     = "DepGPS_composite2",
  outcome_var = "Depression_score_R6",
  cov_cont    = cov_cont,
  cov_factor  = cov_factor
)

print(plot1)


############################# straight line plot 2 ##############################################


library(dplyr)
library(ggplot2)


mode_factor <- function(x) {
  ux <- na.omit(x)
  levels(ux)[which.max(tabulate(ux))]
}

make_curve_plot <- function(data,                       
                            indep_var,                    
                            mod_var,                    
                            outcome_var,                
                            cov_cont,                   
                            cov_factor) {               
  
  ## helper: modal level for factors (for representative covariate values)
  mode_factor <- function(x) {
    ux <- na.omit(x)
    levels(ux)[which.max(tabulate(ux))]
  }
  
  
  ## 1) split moderator into Low / Mid / High (10 / 80 / 10 %)
  q <- quantile(data[[mod_var]], probs = c(.10, .90), na.rm = TRUE)
  data <- data %>% 
    mutate(
      Mod_grp = cut(
        .data[[mod_var]],
        breaks = c(-Inf, q[1], q[2], Inf),
        labels = c("Low (≤10th)", "Middle (10-90th)", "High (≥90th)"),
        right  = TRUE
      )
    )
  
  
  ## 2) fit linear model with interaction and covariates
  form <- reformulate(
    c(indep_var, "Mod_grp", paste0(indep_var, ":Mod_grp"), cov_cont, cov_factor),
    response = outcome_var
  )
  
  fit <- lm(form, data = data)
  
  ## 3) build prediction grid: 100 indep_var percentiles × 3 prs groups
  indep_seq <- seq(min(data[[indep_var]], na.rm = TRUE),
                   max(data[[indep_var]], na.rm = TRUE),
                   length.out = 100)
  
  indep_grid <- tibble(
    indep_pct = seq(1, 100),
    !!indep_var := indep_seq
  )
  
  
  cov_means <- bind_cols(
    data %>% summarise(across(all_of(cov_cont), ~ mean(.x, na.rm = TRUE))),
    data %>% summarise(across(all_of(cov_factor), ~ mode_factor(.x)))
  )
  
  newdat <- tidyr::crossing(
    indep_grid,
    tibble(Mod_grp = levels(data$Mod_grp))
  ) %>% 
    bind_cols(cov_means[rep(1, nrow(.)), ])   # recycle covariate row
  
  ## 4) predict outcome + CI
  pr <- predict(fit, newdata = newdat, interval = "confidence")
  newdat <- newdat %>%
    mutate(Pred = pr[, "fit"],
           CI_lo = pr[, "lwr"],
           CI_hi = pr[, "upr"])
  
  ## 5) compute High vs Low moderator difference at PGS 10th & 90th
  arrow_df <- newdat %>%
    filter(indep_pct %in% c(10, 90),
           Mod_grp %in% c("Low (≤10th)", "High (≥90th)")) %>%
    dplyr::select(indep_pct, Mod_grp, Pred) %>%
    tidyr::pivot_wider(names_from = Mod_grp,
                       values_from = Pred) %>%
    mutate(
      diff  = round(`Low (≤10th)` - `High (≥90th)`, 2),
      x     = indep_pct,
      xend  = indep_pct,
      yend  = `Low (≤10th)`,
      y     = `High (≥90th)`,
      label = paste0("Δ = ", diff),
      text_x = ifelse(indep_pct == 10, x + 2 , x + 2)
    )
  
  ## 6) plot
  ggplot(newdat,
         aes(x = indep_pct, y = Pred,
             colour = Mod_grp, fill = Mod_grp)) +
    geom_line(linewidth = 1.2) +
    geom_ribbon(aes(ymin = CI_lo, ymax = CI_hi),
                alpha = .15, colour = NA) +
    scale_colour_manual(values = c("#2b83ba", "#d7191c", "#abdda4")) +
    scale_fill_manual(values   = c("#2b83ba", "#d7191c", "#abdda4")) +
    labs(
      title  = paste0("Interaction: ", indep_var, " × ", mod_var),
      x      = indep_var,
      y      = paste("Predicted", outcome_var),
      colour = mod_var,
      fill   = mod_var
    ) +
    theme_minimal(base_size = 12) +
    
    ## dashed arrows
    geom_segment(data = arrow_df,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 inherit.aes = FALSE,
                 linetype = "dashed",
                 colour   = "black",
                 arrow    = arrow(length = unit(0.22, "cm"),
                                  ends   = "both",
                                  type   = "closed")) +
    
    ## labels
    geom_text(data = arrow_df,
              aes(x = text_x, y = (y + yend)/2, label = label),
              inherit.aes = FALSE,
              size = 3,
              hjust = 0)
}




plot1 <- make_curve_plot(
  data        = df2,
  indep_var   = "SelfAcceptance_R5",
  mod_var     = "DepGPS_composite2",
  outcome_var = "Depression_score_R6",
  cov_cont    = cov_cont,
  cov_factor  = cov_factor
)

print(plot1)

>>>>>>> fca84fb (first commit)
