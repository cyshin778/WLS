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
cov_ev <- paste0('EV', 1:10)

cov_list <- c(cov_cont, cov_factor, cov_ev)
COVARS <- paste(cov_list, collapse = " + ")

# Independent variables
PersonalResource <- c('Autonomy_R5', 'EnvironmentalMastery_R5', 'PersonalGrowth_R5',
                      'PositiveRelationship_R5', 'SelfAcceptance_R5', 'PurposeinLife_R5',
                      'Optimism_R5', 'Mattering_R5', 'PersonalResource_composite')

SocialSupport1 <- c('Support_money_R5', 'Support_problem_R5', 'Support_sick_R5', 'SocialSupport1_composite')
SocialSupport2 <- c('Love_social_R5','Listen_social_R5','SocialSupport2_composite')
SocialParticipation1 <- c('Social_friends_R5', 'Social_relatives_R5', 'SocialParticipation1_composite')
Social_involvement_R5 <- c('Social_involvement_R5')
indep <- c(PersonalResource, SocialSupport1, SocialSupport2, SocialParticipation1, Social_involvement_R5)


# ------------------------- Main (Additive) Effects -------------------------

library(dplyr)
library(car)

results_main <- list()
vif_list <- list()
vif_summary <- list()

for (indep_var in indep) {
  for (mod_var in mod) {
    
    formula_str <- paste(outcome, "~", indep_var, "+", mod_var, "+", COVARS)
    
    model <- lm(as.formula(formula_str), data = df2)
    
    vif_values <- vif(model)
    vif_list[[paste(indep_var, mod_var, sep = "_x_")]] <- vif_values
    
    vif_summary[[length(vif_summary) + 1]] <- data.frame(
      Independent = indep_var,
      Moderator = mod_var,
      Variable = names(vif_values),
      VIF = as.numeric(vif_values)
    )
    
    coefs <- summary(model)$coefficients
    confint_df <- confint(model, level = 0.95)
    
    for (term in c(indep_var, mod_var)) {
      if (term %in% rownames(coefs)) {
        results_main[[length(results_main) + 1]] <- data.frame(
          Independent = indep_var,
          Moderator = mod_var,
          Term = term,
          Estimate = round(coefs[term, "Estimate"], 4),
          Std_Error = round(coefs[term, "Std. Error"], 4),
          CI_95_Low = round(confint_df[term, 1], 4),
          CI_95_High = round(confint_df[term, 2], 4),
          P_value = signif(coefs[term, "Pr(>|t|)"], 4)
        )
      }
    }
  }
}

# Bind and process results
results_df <- bind_rows(results_main)

# VIF summary
vif_df <- bind_rows(vif_summary)
filter(vif_df, VIF > 5)

# FDR correction per independent variable
results_df$P_FDR <- NA
results_df$FDR_sig <- ""

for (indep_var in unique(results_df$Independent)) {
  idx <- results_df$Independent == indep_var
  fdr_p <- p.adjust(results_df$P_value[idx], method = "fdr")
  results_df$P_FDR[idx] <- fdr_p
  
  results_df$FDR_sig[idx] <- sapply(fdr_p, function(pval) {
    if (is.na(pval)) return("")
    else if (pval < 0.001) return("***")
    else if (pval < 0.01) return("**")
    else if (pval < 0.05) return("*")
    else if (pval < 0.1) return(".")
    else return("")
  })
}


# Output
print(results_df)
write.csv(results_df, "Results/main_effect_results_250731.csv", row.names = FALSE)


# ------------------------- Moderation Effects -------------------------

results_list <- list()
vif_list <- list()
vif_summary <- list()

for (indep_var in indep) {
  for (mod_var in mod) {
    
    interaction_term <- paste0(indep_var, ":", mod_var)
    formula_str <- paste(outcome, "~", indep_var, "*", mod_var, "+", COVARS)
    
    model <- lm(as.formula(formula_str), data = df2)
    
    # check multicollinearity
    vif_values <- vif(model)
    vif_list[[paste(indep_var, mod_var, sep = "_x_")]] <- vif_values
    
    vif_summary[[length(vif_summary) + 1]] <- data.frame(
      Independent = indep_var,
      Moderator = mod_var,
      Variable = names(vif_values),
      VIF = as.numeric(vif_values)
    )
    
    terms_to_extract <- c(indep_var, mod_var, interaction_term)
    coefs <- summary(model)$coefficients
    confint_df <- confint(model, level = 0.95)  # Wald confidence intervals
    
    available_terms <- intersect(terms_to_extract, rownames(coefs))
    
    # Extract values
    beta <- round(coefs[available_terms, "Estimate"], 4)
    se <- round(coefs[available_terms, "Std. Error"], 4)
    p <- signif(coefs[available_terms, "Pr(>|t|)"], 4)
    lowci <- round(confint_df[available_terms, 1], 4)
    highci <- round(confint_df[available_terms, 2], 4)
    
    # Combine into dataframe
    res_df <- data.frame(
      Model = paste(indep_var, "x", mod_var),
      Independent = indep_var,
      Moderator = mod_var,
      Term = available_terms,
      Estimate = beta,
      Std_Error = se,
      CI_95_Low = lowci,
      CI_95_High = highci,
      P_value = p,
      stringsAsFactors = FALSE
    )
    
    results_list[[paste(indep_var, mod_var, sep = "_x_")]] <- res_df
  }
}

# Combine all results
final_results <- bind_rows(results_list)
vif_df <- bind_rows(vif_summary)
filter(vif_df, VIF > 5)

# FDR correction (interaction terms only)
final_results$P_FDR <- NA
final_results$FDR_sig <- ""

for (indep_var in unique(final_results$Independent)) {
  interaction_mask <- final_results$Independent == indep_var &
    final_results$Term == paste0(indep_var, ":", final_results$Moderator)
  
  # Get raw p-values for interaction terms
  pvals <- final_results$P_value[interaction_mask]
  
  # Apply FDR correction
  fdr_pvals <- round(p.adjust(pvals, method = "fdr"), 4)
  final_results$P_FDR[interaction_mask] <- fdr_pvals
  
  final_results$FDR_sig[interaction_mask] <- sapply(fdr_pvals, function(pval) {
    if (is.na(pval)) return("")
    else if (pval < 0.001) return("***")
    else if (pval < 0.01) return("**")
    else if (pval < 0.05) return("*")
    else if (pval < 0.1) return(".")
    else return("")
  })
}

write.csv(final_results, "Results/moderation_results_250731.csv", row.names = FALSE)



# ------------------------- Forest Plot -------------------------

library(dplyr)
library(ggplot2)
library(ggeffects)

target_mod <- "DepGPS_composite2"

# 1. Filter interaction terms
interaction_data <- final_results %>%
  filter(Moderator == target_mod,
         Term == paste0(Independent, ":", Moderator)) %>%
  mutate(
    Significance = cut(
      P_FDR,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", "")
    ),
    SignifLevel = as.numeric(factor(Significance, levels = c("***", "**", "*", ".", ""), labels = c(1, 2, 3, 4, 5))),
    Beta_CI_Label = sprintf("%.3f [%.3f, %.3f]", Estimate, CI_95_Low, CI_95_High)
  )


# 2. Sort by significance and effect size
interaction_data_sorted <- interaction_data %>%
  arrange(SignifLevel, desc(abs(Estimate))) %>%
  mutate(Independent = factor(Independent, levels = rev(unique(Independent))))


# 3. Plot
ggplot(interaction_data_sorted, aes(x = Estimate, y = Independent)) +
  geom_point(shape = 15, size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = CI_95_Low, xmax = CI_95_High), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_text(aes(x = max(CI_95_High) + 0.005, label = Significance), size = 3, hjust = 0) +
  geom_text(aes(x = max(CI_95_High) + 0.008, label = Beta_CI_Label), size = 3, hjust = 0) +
  theme_minimal() +
  labs(
    title = paste("Moderation Effects (", target_mod, ")", sep = ""),
    subtitle = "FDR-corrected: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
    x = "Beta Value of Interaction Term",
    y = "Independent Variable"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_cartesian(xlim = c(min(interaction_data_sorted$CI_95_Low) - 0.01,
                           max(interaction_data_sorted$CI_95_High) + 0.04))



################################# Interaction Plot ################################################

library(dplyr)
library(ggplot2)
library(ggeffects)


make_interaction_plot_grouped <- function(indep_var, mod_var, data, outcome_var, COVARS) {
  quantiles <- quantile(data[[mod_var]], probs = c(0.10, 0.90), na.rm = TRUE)
  group_var <- paste0(mod_var, "_group")
  
  data[[group_var]] <- cut(data[[mod_var]],
                           breaks = c(-Inf, quantiles[1], quantiles[2], Inf),
                           labels = c("low", "middle", "high"),
                           right = TRUE)
  
  # Build formula
  formula_str <- paste(outcome_var, "~", group_var, "*", indep_var, "+", COVARS)
  
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

plot1 <- make_interaction_plot_grouped(
  indep_var = "SelfAcceptance_R5",
  mod_var = "DepGPS_composite2",
  data = df2,
  outcome_var = "Depression_score_R6",
  COVARS = COVARS
)

print(plot1)


################################# Mediation ################################################
library(mediation)
library(dplyr)

indep_var <- "DepGPS_composite2" # manually 

results_list <- list()

for (med in moderators) {
  
  # Mediator model
  med_formula <- as.formula(
    paste(med, "~", indep_var, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + "))
  )
  med_model <- lm(med_formula, data = df2)
  
  # Outcome model
  out_formula <- as.formula(
    paste(outcome, "~", indep_var, "+", med, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + "))
  )
  out_model <- lm(out_formula, data = df2)
  
  # Mediation analysis
  med_out <- mediate(
    med_model, out_model,
    treat = indep_var,
    mediator = med,
    boot = TRUE, sims = 1000
  )
  
  res_df <- data.frame(
    Independent = indep_var,
    Mediator = med,
    ACME = round(med_out$d0, 4),
    ACME_p = round(med_out$d0.p, 4),
    ADE = round(med_out$z0, 4),
    ADE_p = round(med_out$z0.p, 4),
    Total_Effect = round(med_out$tau.coef, 4),
    Total_p = round(med_out$tau.p, 4),
    Prop_Mediated = round(med_out$n0, 4),
    Prop_Mediated_p = round(med_out$n0.p, 4),
    stringsAsFactors = FALSE
  )
  
  results_list[[med]] <- res_df
}

# Combine results
mediation_results <- bind_rows(results_list)

# FDR correction
mediation_results$ACME_p_fdr <- p.adjust(mediation_results$ACME_p, method = "fdr")
mediation_results$ADE_p_fdr <- p.adjust(mediation_results$ADE_p, method = "fdr")
mediation_results$Prop_Mediated_p_fdr <- p.adjust(mediation_results$Prop_Mediated_p, method = "fdr")

# Sort by 'ACME_p_fdr'
mediation_results <- mediation_results[order(mediation_results$ACME_p_fdr), ]

#print(mediation_results)
#write.csv(mediation_results, paste0("Results/mediation_results_", indep_var, "_250723.csv"), row.names = FALSE)

