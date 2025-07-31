<<<<<<< HEAD
setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

library(parameters)
library(dplyr)
library(ggplot2)
library(ggeffects)

df <- read.csv("wls_merged_250716.csv")
df2 <- df

# DepGPS_composite2 
df2$row_mean <- rowMeans(df2[, c("Depression_GPS", "Neuroticism_GPS")], na.rm = TRUE)
df2$DepGPS_composite2 <- as.numeric(scale(df2$row_mean))

df_male <- df2 %>% filter(sex == 1)
df_female <- df2 %>% filter(sex == 2)



################################# Variables ################################################

# Main Variables
indep <- c('DepGPS_composite',
           'DepGPS_composite2',
           'IntelligenceGPS_composite',
           'Depression_GPS', 'MDD_GPS', 'Neuroticism_GPS',
           'EA_GPS','IQ_GPS','CP_GPS')

outcome <- c('Depression_score_R6')

# Covariates
cov_cont <- c('age_R5', 'Personal_Income_R6', 'Education_years_R6')
cov_factor <- c('Marital_status_R6_new')
df2[cov_factor] <- lapply(df2[cov_factor], factor)

cov_list <- c(cov_cont, cov_factor)
COVARS <- paste(cov_list, collapse = " + ")

# Moderators
PersonalResource <- c('Autonomy_R5', 'EnvironmentalMastery_R5', 'PersonalGrowth_R5',
                      'PositiveRelationship_R5', 'SelfAcceptance_R5', 'PurposeinLife_R5',
                      'Optimism_R5', 'Mattering_R5', 'PersonalResource_composite')

SocialSupport1 <- c('Support_money_R5', 'Support_problem_R5', 'Support_sick_R5', 'SocialSupport1_composite')
SocialSupport2 <- c('Love_social_R5','Listen_social_R5','SocialSupport2_composite')
SocialParticipation1 <- c('Social_friends_R5', 'Social_relatives_R5', 'SocialParticipation1_composite')
Social_involvement_R5 <- c('Social_involvement_R5')
moderators <- c(PersonalResource, SocialSupport1, SocialSupport2, SocialParticipation1, Social_involvement_R5)


################################# Main(Additive) Effects ################################################

library(dplyr)

run_main_effect <- function(data, label) {
  results_main <- list()
  
  for (indep_var in indep) {
    for (mod in moderators) {
      formula_str <- paste(
        outcome, "~", indep_var, "+", mod, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + ")
      )
      
      model <- lm(as.formula(formula_str), data = data)
      coefs <- summary(model)$coefficients
      confint_df <- confint(model, level = 0.95)
      
      for (term in c(indep_var, mod)) {
        if (term %in% rownames(coefs)) {
          results_main[[length(results_main) + 1]] <- data.frame(
            Sex = label,
            Independent = indep_var,
            Moderator = mod,
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
  
  results_df <- bind_rows(results_main)
  
  # FDR correction
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
  
  return(results_df)
}

# 실행
main_male <- run_main_effect(df_male, "Male")
main_female <- run_main_effect(df_female, "Female")

# 저장
write.csv(main_male, "Results/main_effect_male_250723.csv", row.names = FALSE)
write.csv(main_female, "Results/main_effect_female_250723.csv", row.names = FALSE)


################################# Moderation Effects ################################################
run_moderation_effect <- function(data, label) {
  results_list <- list()
  
  for (indep_var in indep) {
    for (mod_var in moderators) {
      interaction_term <- paste0(indep_var, ":", mod_var)
      formula_str <- paste(
        outcome, "~", indep_var, "*", mod_var, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + ")
      )
      
      model <- lm(as.formula(formula_str), data = data)
      coefs <- summary(model)$coefficients
      confint_df <- confint(model, level = 0.95)
      
      terms_to_extract <- c(indep_var, mod_var, interaction_term)
      available_terms <- intersect(terms_to_extract, rownames(coefs))
      
      res_df <- data.frame(
        Sex = label,
        Model = paste(indep_var, "x", mod_var),
        Independent = indep_var,
        Moderator = mod_var,
        Term = available_terms,
        Estimate = round(coefs[available_terms, "Estimate"], 4),
        Std_Error = round(coefs[available_terms, "Std. Error"], 4),
        CI_95_Low = round(confint_df[available_terms, 1], 4),
        CI_95_High = round(confint_df[available_terms, 2], 4),
        P_value = signif(coefs[available_terms, "Pr(>|t|)"], 4),
        stringsAsFactors = FALSE
      )
      
      results_list[[paste(indep_var, mod_var, sep = "_x_")]] <- res_df
    }
  }
  
  final_results <- bind_rows(results_list)
  
  # FDR correction for interaction terms
  final_results$P_FDR <- NA
  final_results$FDR_sig <- ""
  
  for (indep_var in unique(final_results$Independent)) {
    interaction_mask <- final_results$Independent == indep_var &
      final_results$Term == paste0(indep_var, ":", final_results$Moderator)
    
    pvals <- final_results$P_value[interaction_mask]
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
  
  return(final_results)
}

# 실행
mod_male <- run_moderation_effect(df_male, "Male")
mod_female <- run_moderation_effect(df_female, "Female")

# 저장
write.csv(mod_male, "Results/moderation_effect_male_250723.csv", row.names = FALSE)
write.csv(mod_female, "Results/moderation_effect_female_250723.csv", row.names = FALSE)



################################# Forest Plot ################################################

library(dplyr)
library(ggplot2)

target_indep <- "DepGPS_composite"  # Your IV of interest

plot_forest <- function(data, sex_label) {
  interaction_data <- data %>%
    filter(Independent == target_indep,
           Term == paste0(Independent, ":", Moderator)) %>%
    mutate(
      Significance = cut(
        P_value,
        breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
        labels = c("***", "**", "*", ".", "")
      ),
      SignifLevel = as.numeric(factor(Significance, levels = c("***", "**", "*", ".", ""), labels = c(1, 2, 3, 4, 5))),
      Beta_CI_Label = sprintf("%.3f [%.3f, %.3f]", Estimate, CI_95_Low, CI_95_High)
    ) %>%
    arrange(SignifLevel, desc(abs(Estimate))) %>%
    mutate(Moderator = factor(Moderator, levels = rev(unique(Moderator))))
  
  ggplot(interaction_data, aes(x = Estimate, y = Moderator)) +
    geom_point(shape = 15, size = 3, color = "steelblue") +
    geom_errorbarh(aes(xmin = CI_95_Low, xmax = CI_95_High), height = 0.2, color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    geom_text(aes(x = max(CI_95_High, na.rm = TRUE) + 0.005, label = Significance), size = 3, hjust = 0) +
    geom_text(aes(x = max(CI_95_High, na.rm = TRUE) + 0.011, label = Beta_CI_Label), size = 3, hjust = 0) +
    theme_minimal() +
    labs(
      title = paste("Moderation Effects of", target_indep, "in", sex_label),
      subtitle = "Raw p-value: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
      x = "Beta Value of Interaction Term",
      y = "Moderator"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid = element_blank()
    ) +
    coord_cartesian(
      xlim = c(min(interaction_data$CI_95_Low, na.rm = TRUE) - 0.01,
               max(interaction_data$CI_95_High, na.rm = TRUE) + 0.04)
    )
}

# Plot for Male
plot_forest(mod_male, "Male")

# Plot for Female
plot_forest(mod_female, "Female")



=======
setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

library(parameters)
library(dplyr)
library(ggplot2)
library(ggeffects)

df <- read.csv("wls_merged_250716.csv")
df2 <- df

# DepGPS_composite2 
df2$row_mean <- rowMeans(df2[, c("Depression_GPS", "Neuroticism_GPS")], na.rm = TRUE)
df2$DepGPS_composite2 <- as.numeric(scale(df2$row_mean))

df_male <- df2 %>% filter(sex == 1)
df_female <- df2 %>% filter(sex == 2)



################################# Variables ################################################

# Main Variables
indep <- c('DepGPS_composite',
           'DepGPS_composite2',
           'IntelligenceGPS_composite',
           'Depression_GPS', 'MDD_GPS', 'Neuroticism_GPS',
           'EA_GPS','IQ_GPS','CP_GPS')

outcome <- c('Depression_score_R6')

# Covariates
cov_cont <- c('age_R5', 'Personal_Income_R6', 'Education_years_R6')
cov_factor <- c('Marital_status_R6_new')
df2[cov_factor] <- lapply(df2[cov_factor], factor)

cov_list <- c(cov_cont, cov_factor)
COVARS <- paste(cov_list, collapse = " + ")

# Moderators
PersonalResource <- c('Autonomy_R5', 'EnvironmentalMastery_R5', 'PersonalGrowth_R5',
                      'PositiveRelationship_R5', 'SelfAcceptance_R5', 'PurposeinLife_R5',
                      'Optimism_R5', 'Mattering_R5', 'PersonalResource_composite')

SocialSupport1 <- c('Support_money_R5', 'Support_problem_R5', 'Support_sick_R5', 'SocialSupport1_composite')
SocialSupport2 <- c('Love_social_R5','Listen_social_R5','SocialSupport2_composite')
SocialParticipation1 <- c('Social_friends_R5', 'Social_relatives_R5', 'SocialParticipation1_composite')
Social_involvement_R5 <- c('Social_involvement_R5')
moderators <- c(PersonalResource, SocialSupport1, SocialSupport2, SocialParticipation1, Social_involvement_R5)


################################# Main(Additive) Effects ################################################

library(dplyr)

run_main_effect <- function(data, label) {
  results_main <- list()
  
  for (indep_var in indep) {
    for (mod in moderators) {
      formula_str <- paste(
        outcome, "~", indep_var, "+", mod, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + ")
      )
      
      model <- lm(as.formula(formula_str), data = data)
      coefs <- summary(model)$coefficients
      confint_df <- confint(model, level = 0.95)
      
      for (term in c(indep_var, mod)) {
        if (term %in% rownames(coefs)) {
          results_main[[length(results_main) + 1]] <- data.frame(
            Sex = label,
            Independent = indep_var,
            Moderator = mod,
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
  
  results_df <- bind_rows(results_main)
  
  # FDR correction
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
  
  return(results_df)
}

# 실행
main_male <- run_main_effect(df_male, "Male")
main_female <- run_main_effect(df_female, "Female")

# 저장
write.csv(main_male, "Results/main_effect_male_250723.csv", row.names = FALSE)
write.csv(main_female, "Results/main_effect_female_250723.csv", row.names = FALSE)


################################# Moderation Effects ################################################
run_moderation_effect <- function(data, label) {
  results_list <- list()
  
  for (indep_var in indep) {
    for (mod_var in moderators) {
      interaction_term <- paste0(indep_var, ":", mod_var)
      formula_str <- paste(
        outcome, "~", indep_var, "*", mod_var, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + ")
      )
      
      model <- lm(as.formula(formula_str), data = data)
      coefs <- summary(model)$coefficients
      confint_df <- confint(model, level = 0.95)
      
      terms_to_extract <- c(indep_var, mod_var, interaction_term)
      available_terms <- intersect(terms_to_extract, rownames(coefs))
      
      res_df <- data.frame(
        Sex = label,
        Model = paste(indep_var, "x", mod_var),
        Independent = indep_var,
        Moderator = mod_var,
        Term = available_terms,
        Estimate = round(coefs[available_terms, "Estimate"], 4),
        Std_Error = round(coefs[available_terms, "Std. Error"], 4),
        CI_95_Low = round(confint_df[available_terms, 1], 4),
        CI_95_High = round(confint_df[available_terms, 2], 4),
        P_value = signif(coefs[available_terms, "Pr(>|t|)"], 4),
        stringsAsFactors = FALSE
      )
      
      results_list[[paste(indep_var, mod_var, sep = "_x_")]] <- res_df
    }
  }
  
  final_results <- bind_rows(results_list)
  
  # FDR correction for interaction terms
  final_results$P_FDR <- NA
  final_results$FDR_sig <- ""
  
  for (indep_var in unique(final_results$Independent)) {
    interaction_mask <- final_results$Independent == indep_var &
      final_results$Term == paste0(indep_var, ":", final_results$Moderator)
    
    pvals <- final_results$P_value[interaction_mask]
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
  
  return(final_results)
}

# 실행
mod_male <- run_moderation_effect(df_male, "Male")
mod_female <- run_moderation_effect(df_female, "Female")

# 저장
write.csv(mod_male, "Results/moderation_effect_male_250723.csv", row.names = FALSE)
write.csv(mod_female, "Results/moderation_effect_female_250723.csv", row.names = FALSE)



################################# Forest Plot ################################################

library(dplyr)
library(ggplot2)

target_indep <- "DepGPS_composite"  # Your IV of interest

plot_forest <- function(data, sex_label) {
  interaction_data <- data %>%
    filter(Independent == target_indep,
           Term == paste0(Independent, ":", Moderator)) %>%
    mutate(
      Significance = cut(
        P_value,
        breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
        labels = c("***", "**", "*", ".", "")
      ),
      SignifLevel = as.numeric(factor(Significance, levels = c("***", "**", "*", ".", ""), labels = c(1, 2, 3, 4, 5))),
      Beta_CI_Label = sprintf("%.3f [%.3f, %.3f]", Estimate, CI_95_Low, CI_95_High)
    ) %>%
    arrange(SignifLevel, desc(abs(Estimate))) %>%
    mutate(Moderator = factor(Moderator, levels = rev(unique(Moderator))))
  
  ggplot(interaction_data, aes(x = Estimate, y = Moderator)) +
    geom_point(shape = 15, size = 3, color = "steelblue") +
    geom_errorbarh(aes(xmin = CI_95_Low, xmax = CI_95_High), height = 0.2, color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    geom_text(aes(x = max(CI_95_High, na.rm = TRUE) + 0.005, label = Significance), size = 3, hjust = 0) +
    geom_text(aes(x = max(CI_95_High, na.rm = TRUE) + 0.011, label = Beta_CI_Label), size = 3, hjust = 0) +
    theme_minimal() +
    labs(
      title = paste("Moderation Effects of", target_indep, "in", sex_label),
      subtitle = "Raw p-value: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
      x = "Beta Value of Interaction Term",
      y = "Moderator"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid = element_blank()
    ) +
    coord_cartesian(
      xlim = c(min(interaction_data$CI_95_Low, na.rm = TRUE) - 0.01,
               max(interaction_data$CI_95_High, na.rm = TRUE) + 0.04)
    )
}

# Plot for Male
plot_forest(mod_male, "Male")

# Plot for Female
plot_forest(mod_female, "Female")



>>>>>>> fca84fb (first commit)
