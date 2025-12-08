#tableau de contingences
rm(list=ls())

setwd("/home/oryx/Documents/r_dataviz/chi2")

data = matrix(c(55,110,90,52,39,40),byrow=TRUE,ncol=3)
colnames(data)=c("Faible","Moyen","Elevé")
rownames(data)=c("Jeune","Agée")

chisq.test(data,correct=FALSE)
chisq.test(data)


#https://www.datacamp.com/tutorial/chi-square-test-r
library(readr)
library(dplyr)

dataset <- read_csv("children anemia.csv")
head(dataset)
colnames(dataset)[colnames(dataset) == "Anemia level...8"] <- "Anemia level"
colnames(dataset)

selected_data <- dataset %>% select("Highest educational level", "Anemia level")
contingency_table <- table(selected_data$"Highest educational level", selected_data$"Anemia level")
print(contingency_table)
print(selected_data)

# Perform chi-square test
chi_square_test <- chisq.test(contingency_table)

# View the results
print(chi_square_test)

# Observed counts
observed_counts <- chi_square_test$observed

print(observed_counts)

# Expected counts
#These counts are calculated under the assumption that there is no association between the mother’s education level and the child’s anemia status. 
#The expected counts can be retrieved from the following code:

expected_counts <- chi_square_test$expected
print(round(expected_counts, 2))


# Pearson residuals
#These residuals help identify the largest discrepancies between observed and expected counts, indicating which cells contribute most to the chi-square statistic. 
#The Pearson residuals can be retrieved from the following code:


pearson_residuals <- chi_square_test$residuals
print(round(pearson_residuals, 2))
