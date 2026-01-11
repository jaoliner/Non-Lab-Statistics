#This file displays a complete workflow for factor analysis regression analysis within R
#This workflow covers factor score creation, heteroskedasticity management, and mixed-effects regression analyses/testing.

#Factor loadings used in score construction are derived from the exploratory factor analysis described in the accompanying EFA workflow file.

#Factor sccores are created through the summation of relevant varibles multiplied by their respective factor loadings.
#However, for equal weighting these variables need to be on the same scale, or standardized. 
#Standardization is typically done via z-score normalization or min-max normalization. Assuming the presence of binary variables, 
#min–max normalization is preferred since Z-score standardization can disproportionately inflate the contribution of such variables.

#Creating a function to standardize the variables via min-max normalization
scale_score <- function(df) {
  
  #Designating which variables weren't included in the factor analysis, since we don't want to standardize these.
  covariates <- c("dependent_var", "control_var_1", "control_var_2", "control_var_3", "control_var_4", "location_var")
  
  for (variable in names(df)) {
    if (!(variable %in% covariates)) {
      if (!(all(unique(variable) %in% c(0,1)))) {
        df[[variable]] <- df[[variable]] / max(df[[variable]])
      }
    }
  }
  return(df)
}
regress_df_scaled <- scale_score(regress_df)

#Since all of the variables are now standardized, we can multiply them by the loadings for the factor scores. 
#To do this, we need to first extract the loadings from our prior factor analysis.

#We need to manually order the factor loadings because they don't always match with the data frame. 
#Every time you change the factor analysis by adding new variables, make sure to change the order too!

f_one <- (fa_result$loadings[, 1])
f_one <- f_one[c("var_1", "var_2", "var_3", "var_4", "var_5", "var_6", "var_7","var_8", "var_9", "var_10")]

f_two <- (fa_result$loadings[, 2])
f_two <- f_two[c("var_1", "var_2", "var_3", "var_4", "var_5", "var_6", "var_7","var_8", "var_9", "var_10")]

#Now that we have our loadings, we need to extract the relevant variables to create our scores. We need our matrices to 
#match for multiplication, so let's get rid of the other variables such as our dependent variable and control covariates.

factor_vars_scaled <- regress_df_scaled %>%
  select(-c("dependent_var", "control_var_1", "control_var_2", "control_var_3", "control_var_4", "location_var")) %>%
  mutate_all(~ as.numeric(.))

#Now, we can create the scores! Here, we're just multiplying the variables by their matching loadings to create the factor scores. 
#This is then added to the ORIGINAL data frame (with the dependent/covariates) to make sure everything is prepped for analysis.

regress_df_scaled$Factor1Score <- rowSums(sweep(factor_vars_scaled, 2, f_one, "*"))
regress_df_scaled$Factor2Score <- rowSums(sweep(factor_vars_scaled, 2, f_two, "*"))

#Now scores should be created within the data frame!

#Assuming that "location_var" may impact estimates, let's set up a linear, mixed-effects regression model on our dependent outcome,
#with location as our random effect.

#Creating the model
base_model <- lmer(dependent_var ~ control_var_1 + control_var_2 + control_var_3 + control_var_4 + (1 |location_var), 
                   data = regress_df_scaled)

fixef(base_model)  #To view the fixed effects
ranef(base_model) #To view the random effects (of location)

#Residual / Heteroskedasticity Analysis

#Following model estimation, we analyze residual behavior to evaluate assumptions of normality and homoskedasticity within the model.

#To plot the residuals
plot(fitted(base_model), residuals(base_model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot",
     pch = 1, 
     col = "blue",
     abline(h = 0, col = "red", lty = 2))

#To plot a Q-Q visualization
qqnorm(base_model$residuals, pch = 1, col = "blue") 
qqline(base_model$residuals, col = "red")

#To assess homoskedasticity
lmtest::bptest(base_model) # Breusch-Pagan test
car::ncvTest(base_model)  # NCV test

#All of the tests above provide indicators of normality and homoskedasticity in the model residuals. Assuming heteroskedasticity 
#is present (which is common in biological research), a robust standard error technique can be used to obtain reliable inference.

#Calculate robust standard errors

rse_model <- coef_test(base_model, vcov = "CR2")
