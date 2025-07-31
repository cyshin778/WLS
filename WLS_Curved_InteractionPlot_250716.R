# curved interaction plot

setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

library(parameters)
library(dplyr)
library(ggplot2)
library(ggeffects)

df <- read.csv("wls_merged_250716.csv")
df2 <- df

# DepGPS_composite2 
df2$row_mean <- rowMeans(df2[, c("Depression_GPS", "Neuroticism_GPS")], na.rm = TRUE)
df2$DepGPS_composite2 <- scale(df2$row_mean)
df2$DepGPS_composite2 <- as.numeric(scale(df2$row_mean))

################################# Variables ################################################

# Main Variables
indep <- c('DepGPS_composite',
           'DepGPS_composite2',
           'IntelligenceGPS_composite',
           'Depression_GPS','MDD_GPS', 'Neuroticism_GPS',
           'EA_GPS','IQ_GPS','CP_GPS')

outcome <- c('Depression_score_R6')

# Covariates
cov_cont <- c('age_R5', 'Personal_Income_R6', 'Education_years_R6', paste0('EV',1:10))
cov_factor <- c('sex', 'Marital_status_R6_new')
df2[cov_factor] <- lapply(df2[cov_factor], factor)

cov_list <- c(cov_cont, cov_factor)
COVARS <- paste(cov_list, collapse = " + ")


library(dplyr)
library(ggplot2)


mode_factor <- function(x) {
  ux <- na.omit(x)
  levels(ux)[which.max(tabulate(ux))]
}

make_curve_plot <- function(data,                       
                            pgs_var,                    
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
    c(pgs_var, "Mod_grp", paste0(pgs_var, ":Mod_grp"), cov_cont, cov_factor),
    response = outcome_var
  )
  
  fit <- lm(form, data = data)
  
  ## 3) build prediction grid: 100 PGS percentiles × 3 moderator groups
  pgs_grid <- tibble(
    PGS_pct  = 1:100,
    !!pgs_var := quantile(data[[pgs_var]],
                          probs = seq(.01, 1, .01),
                          na.rm = TRUE)
  )
  
  cov_means <- bind_cols(
    data %>% summarise(across(all_of(cov_cont), ~ mean(.x, na.rm = TRUE))),
    data %>% summarise(across(all_of(cov_factor), ~ mode_factor(.x)))
  )
  
  newdat <- tidyr::crossing(
    pgs_grid,
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
    filter(PGS_pct %in% c(10, 90),
           Mod_grp %in% c("Low (≤10th)", "High (≥90th)")) %>%
    dplyr::select(PGS_pct, Mod_grp, Pred) %>%
    tidyr::pivot_wider(names_from = Mod_grp,
                       values_from = Pred) %>%
    mutate(
      diff  = round(`Low (≤10th)` - `High (≥90th)`, 2),
      x     = PGS_pct,
      xend  = PGS_pct,
      yend  = `Low (≤10th)`,
      y     = `High (≥90th)`,
      label = paste0("Δ = ", diff),
      text_x = ifelse(PGS_pct == 10, x + 2 , x + 2)
    )
  
  ## 6) plot
  ggplot(newdat,
         aes(x = PGS_pct, y = Pred,
             colour = Mod_grp, fill = Mod_grp)) +
    geom_line(linewidth = 1.2) +
    geom_ribbon(aes(ymin = CI_lo, ymax = CI_hi),
                alpha = .15, colour = NA) +
    scale_colour_manual(values = c("#2b83ba", "#d7191c", "#abdda4")) +
    scale_fill_manual(values   = c("#2b83ba", "#d7191c", "#abdda4")) +
    labs(
      title  = paste0("Interaction: ", pgs_var, " × ", mod_var),
      x      = "Percentile of polygenic score",
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
  pgs_var     = "DepGPS_composite2",
  mod_var     = "SelfAcceptance_R5",
  outcome_var = "Depression_score_R6",
  cov_cont    = cov_cont,
  cov_factor  = cov_factor
)

print(plot1)


# Count sample size per PGS percentile bin
library(dplyr)

pgs_var <- "DepGPS_composite"

pgs_bin_counts <- df2 %>%
  filter(!is.na(.data[[pgs_var]])) %>%
  mutate(PGS_pct_bin = ntile(.data[[pgs_var]], 100)) %>%
  group_by(PGS_pct_bin) %>%
  summarise(n = n()) %>%
  arrange(PGS_pct_bin)

print(pgs_bin_counts)

library(ggplot2)

ggplot(pgs_bin_counts, aes(x = PGS_pct_bin, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Sample size per PGS percentile bin",
    x = "PGS Percentile Bin (1–100)",
    y = "Sample Size"
  ) +
  theme_minimal()


library(dplyr)
library(ggplot2)

# Count sample size per PGS percentile bin in each moderator group

library(dplyr)

pgs_var <- "DepGPS_composite"
mod_var <- "PositiveRelationship_R5"

pgs_mod_counts <- df2 %>%
  filter(!is.na(.data[[pgs_var]]), !is.na(.data[[mod_var]])) %>%
  mutate(
    PGS_pct_bin = ntile(.data[[pgs_var]], 100),
    Mod_grp = cut(
      .data[[mod_var]],
      breaks = quantile(.data[[mod_var]], probs = c(0, 0.10, 0.90, 1), na.rm = TRUE),
      labels = c("Low (≤10th)", "Middle (10–90th)", "High (≥90th)"),
      include.lowest = TRUE
    )
  ) %>%
  dplyr::count(PGS_pct_bin, Mod_grp) %>%
  arrange(PGS_pct_bin, Mod_grp)

print(pgs_mod_counts)

ggplot(pgs_mod_counts, aes(x = PGS_pct_bin, y = n, fill = Mod_grp)) +
  geom_col(position = "stack") +
  labs(
    title = "Sample size per PGS percentile bin by Moderator group",
    x = "PGS Percentile Bin (1–100)",
    y = "Sample Size",
    fill = "Moderator Group"
  ) +
  theme_minimal()




