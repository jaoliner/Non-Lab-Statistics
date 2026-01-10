#This file displays a complete worklow for completing an Exploratory Factor Analysis (EFA) within R.
#This 10-variable EFA assumes that 5 variables are binary and 5 are ordered, thus applying heterogenous correlations.
#-----------------Data loading------------------
#clear current memory
rm(list = ls()) 

#load packages
library(dplyr)
library(psych)
library(polycor)
library(nFactors) 
library(GPArotation)

#set working directory
setwd("directory")

#load in dataset
factor_df <- read.csv("dataset.csv") 

#convert dichotomous variables to factors (for heterogenous correlation)
binary_vars <- c("var_1", "var_2", "var_3", "var_4", "var_5")
factor_df[binary_vars] <- lapply(factor_df[binary_vars], as.factor)

#convert ordinal variables to ordered variables
ordinal_vars <- c("var_6", "var_7","var_8", "var_9", "var_10")
factor_df[ordinal_vars] <- lapply(factor_df[ordinal_vars], factor, ordered = TRUE)

#running a simple (heterogenous) correlation
polychoric_cor <- hetcor(factor_df, ML = TRUE)

#creating a matrix of such correlation (saving the values for use)
correlation_matrix <- polychoric_cor$correlations
correlation_df <- as.data.frame(correlation_matrix)             

#bartlett's test; a formal statistic to see if the variables are sufficiently correlated to justify EFA
bartlett_test <- cortest.bartlett(correlation_matrix, n = nrow(factor_df))
print(bartlett_test) #a low value provides sufficient evidence

#KMO test; another statsitic to justify EFA
kmo_result <- KMO(correlation_matrix)
print(kmo_result$MSA) #results in a value between 0 and 1, with a higher value indicating a higher fit

#calling the eigenvalues of the correlation matrix
eigenvalues <- eigen(correlation_matrix)$values

#plotting a scree plot, which shows the recommended factors to specify the EFA by
nScree_result <- nScree(eigenvalues)
#plotnScree(nScree_result)

#setting the number of factors to the scree output, in this case assuming 2 factors
num_factors <- 2

#running the factor analysis, assumong correlation between factors (oblimin), and normal distributions (ml)
fa_result <- fa(correlation_matrix, nfactors = num_factors, fm = "ml", rotate = "oblimin")

#testig the communalities
print(fa_result$communalities)

#calling the loadings from the factor analysis
loadings <- fa_result$loadings

#calling the variables from the loadings
variables <- rownames(loadings)

#creating a data frame of loadings for each factor (assuming 2 factors)
fa_load1 <- data.frame(variables, loadings = loadings [, 'ML1'])
fa_load2 <- data.frame(variables, loadings = loadings [, 'ML2'])

#plotting the loadings of factor 1
ggplot(fa_load1, aes(variables, loadings)) + 
  geom_bar(stat = 'identity', fill = ifelse(fa_load1$loadings < 0, 'red', 'blue')) +
  labs(title = 'Factor 1 Loadings',
       y = 'Loadings',
       x = 'Variables') +
  ylim(-1,1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
        panel.border = element_rect(color = 'black', fill = NA))

#plotting the loadings of factor 2
ggplot(fa_load2, aes(variables, loadings)) + 
  geom_bar(stat = 'identity', fill = ifelse(fa_load2$loadings < 0, 'red', 'blue')) +
  labs(title = 'Factor 2 Loadings',
       y = 'Loadings',
       x = 'Variables') +
  ylim(-1,1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
        panel.border = element_rect(color = 'black', fill = NA))
