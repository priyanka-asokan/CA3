# Loading the datasets

#Dataset 1 constains the details about the production of different diary products such as cheese,butter and skimmed milk.
Diray_products_production_details <- read.csv("Production of Dairy Products (000 Tonnes) by Product and Month.csv")

#Dataset 2 constains the details about the Diary Industrial Turnover Index.
Diray_products_turnover_details<- read.csv("Diary Industrial Production and Turnover Index.csv")

# The research question is to identify which diary products production 
# affect the diary industry turnover for the past 10 years in Ireland
# Hence, A Hypothesis testing is done for below conditions
# H0 =  The production of butter does not contribute more to the diary industry turnover than cheese and skimmed milk  
# H1 =  The production of butter contributers more to the diary industry turnover than cheese and skimmed milk

# Inorder to identifiy the realtion between varibles the following steps are carried out  

# 1. used shapiro.test to check if the data set is normally distributed or not. 

# There are three different diary products, all the products are checked against the turnover in testing.
# Hence,the normality test is done for all the three Diary products as below

normality_test_cheese <- shapiro.test(Diray_products_production_details$Cheese)
normality_test_cheese$p.value

normality_test_butter <- shapiro.test(Diray_products_production_details$Butter)
normality_test_butter$p.value

normality_test_Skimmed_Milk <- shapiro.test(Diray_products_production_details$Skimmed_Milk)
normality_test_Skimmed_Milk$p.value

# Normality test for Diary Industry trunover 
normality_test_Turnover <- shapiro.test(Diray_products_turnover_details$Diary_Industrial_Turnover_Index)
normality_test_Turnover$p.value

# Implemeting the pwr package for power analysis 
install.packages("pwr")
library(pwr)
library(dplyr)

# 2. calculating the effect size.
effetive_size <- cohen.ES(test = "r", size = "medium")
effetive_size

# 3. Finding the relationship using corelation pwr.r.test() test. 
sample_size <- pwr.r.test(r = effetive_size$effect.size, sig.level = 0.05, power = 0.8)
sample_size
plot(sample_size)

# Merging the two different dataset.
Merge_data <- merge(Diray_products_production_details,Diray_products_turnover_details)

# 4.passing the sample number of records,
sample_data <- sample_n(Merge_data, 85)
head(sample_data)
str(sample_data)
nrow(sample_data)

# Finding the p-value and co-relation values for all three products against the trunover individually 


corr_test_cheese <- cor.test(sample_data$Cheese, sample_data$Diary_Industrial_Turnover_Index, method = "spearman" )
corr_test_cheese

corr_test_Butter <- cor.test(sample_data$Butter, sample_data$Diary_Industrial_Turnover_Index, method = "spearman")
corr_test_Butter

corr_test_skimmed_milk <- cor.test(sample_data$Skimmed_Milk, sample_data$Diary_Industrial_Turnover_Index, method = "spearman")
corr_test_skimmed_milk
