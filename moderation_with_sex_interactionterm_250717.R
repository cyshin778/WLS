# 남녀차이

setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

library(parameters)
library(dplyr)
library(ggplot2)
library(ggeffects)

df <- read.csv("wls_merged_250716.csv")
df2 <- df


################################# Variables ################################################

# Main Variables
indep <- c('DepGPS_composite',
           'IntelligenceGPS_composite',
           'Depression_GPS', 'MDD_GPS', 'Neuroticism_GPS',
           'EA_GPS','IQ_GPS','CP_GPS')

outcome <- c('Depression_score_R6')

# Covariates
cov_cont <- c('age_R5', 'Personal_Income_R6', 'Education_years_R6')
cov_factor <- c('sex', 'Marital_status_R6_new')
df2[cov_factor] <- lapply(df2[cov_factor], factor)

cov_list <- c(cov_cont, 'Marital_status_R6_new')
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


################################# Moderation with 'sex' interaction term ################################################

results_list <- list()

for (indep_var in indep) {
  for (mod_var in moderators) {
    
    # Build formula string
    formula_str <- paste(
      outcome, "~", paste0(indep_var, "*", mod_var, "*sex"), "+", 
      COVARS, "+", paste0("EV", 1:10, collapse = " + ")
    )
    
    # Fit model
    model <- lm(as.formula(formula_str), data = df2)
    coefs <- summary(model)$coefficients
    confint_df <- confint(model, level = 0.95)
    
    # Terms to extract
    terms_to_extract <- c(
      indep_var,
      mod_var,
      "sex2",
      paste0(indep_var, ":sex2"),
      paste0(mod_var, ":sex2"),
      paste0(indep_var, ":", mod_var),
      paste0(indep_var, ":", mod_var, ":sex2")
    )
    
    available_terms <- intersect(terms_to_extract, rownames(coefs))
    
    # Save results
    res_df <- data.frame(
      Model = paste0(indep_var, ":", mod_var, ":sex"),
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

# Combine all model results
final_results <- bind_rows(results_list)


# Extract the independent variable name from the Model column
final_results$Independent <- sapply(strsplit(final_results$Model, ":"), `[`, 1)

# Initialize new columns
final_results$P_FDR <- NA
final_results$FDR_sig <- ""

# FDR per independent variable, for 3-way terms ending in :sex2
for (indep_var in unique(final_results$Independent)) {
  
  # Filter 3-way interaction terms like indep:mod:sex2
  mask <- final_results$Independent == indep_var &
    grepl(":", final_results$Term) &
    grepl("sex2$", final_results$Term) &
    (lengths(regmatches(final_results$Term, gregexpr(":", final_results$Term))) == 2)
  
  pvals <- final_results$P_value[mask]
  
  if (length(pvals) > 0) {
    fdr_pvals <- round(p.adjust(pvals, method = "fdr"), 4)
    final_results$P_FDR[mask] <- fdr_pvals
    
    final_results$FDR_sig[mask] <- vapply(fdr_pvals, function(pval) {
      if (is.na(pval)) return("")
      else if (pval < 0.001) return("***")
      else if (pval < 0.01) return("**")
      else if (pval < 0.05) return("*")
      else if (pval < 0.1) return(".")
      else return("")
    }, character(1))
  }
}

write.csv(final_results, "moderation_with_sex_interactionterm_250717.csv", row.names = FALSE)
