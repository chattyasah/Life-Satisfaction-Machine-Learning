## setting work directory
setwd("C:/Users/asahc/Desktop/work dir")

# Loading Required Libraries

# Data manipulation and transformation
library(dplyr)      
library(skimr)      
library(readr)      

# Data visualization
library(ggplot2)    
library(corrplot)   
library(gridExtra)  
library(RColorBrewer)  

# Statistical analysis
library(Hmisc)      
library(corrr)      
library(psych)      
library(broom)      
library(margins)    
library(pROC)       

# Model building and evaluation
library(caret)  
library(rpart)      
library(rattle)     

# Additional functionality
library(haven)        # For reading dta files
library(erer)         



## Data Importation
Eqls <- read_dta("eqls_integrated_trend_2003-2016.dta")



#Creating composite variables by merging related questions by mean as justified by their alpha values 
#The approximation of the mean to the nearest whole number is;
#Justified by the need for uniformity with the original scale and strong alpha values
Eqls <- Eqls %>%
  mutate(Y16_Q61composite = round(rowMeans(across(Y16_Q61a:Y16_Q61d), na.rm = FALSE)),
         Y16_Q52composite = round(rowMeans(across(Y16_Q52a:Y16_Q52c), na.rm = FALSE)),
         Y16_Q89composite = round(rowMeans(across(Y16_Q89a:Y16_Q89f), na.rm = FALSE)),
         Y16_Q36composite = round(rowMeans(across(Y16_Q36a:Y16_Q36d), na.rm = FALSE)),
         Y16_Q56composite = round(rowMeans(across(Y16_Q56a:Y16_Q56d), na.rm = FALSE)),
         Y16_Q55composite = round(rowMeans(across(Y16_Q55a:Y16_Q55b), na.rm = FALSE)),
         Y16_Q20composite = round(rowMeans(across(Y16_Q20a:Y16_Q20c), na.rm = FALSE)),
         Y16_Q7composite = round(rowMeans(across(c(Y16_Q7a, Y16_Q7c,Y16_Q7d)), na.rm = FALSE)),
         Y16_Q47composite = round(rowMeans(across(Y16_Q47a:Y16_Q47d), na.rm = FALSE)))


#Cronbach alpha values.
#the NAs and outliers are removed for alignment purposes 

##Health Care access 
cron_health <- Eqls %>% select(Y16_Q61a,Y16_Q61b,Y16_Q61c, Y16_Q61d)
cron_health <- na.omit(cron_health)
cron_health<-cron_health %>% filter_all(all_vars(. <= 96))
alpha(cron_health)
##mental health 
cron_mhealth <- Eqls %>% select(Y16_Q52a,Y16_Q52b,Y16_Q52c)
cron_mhealth <- na.omit(cron_health)
cron_mhealth<-cron_mhealth %>% filter_all(all_vars(. <= 96))
alpha(cron_mhealth)
##Basic need affordability per household
cron_economy <- Eqls %>% select(Y16_Q89a,Y16_Q89b,Y16_Q89c,Y16_Q89d,Y16_Q89e, Y16_Q89f )
cron_economy <- na.omit(cron_economy)
cron_economy<-cron_economy %>% filter_all(all_vars(. <= 96))
alpha(cron_economy)
##time for social activities
cron_social_1 <- Eqls %>% select(Y16_Q47a, Y16_Q47b, Y16_Q47c, Y16_Q47d )
cron_social_1 <- na.omit(cron_social_1)
cron_social_1<-cron_social_1 %>% filter_all(all_vars(. <= 96))
alpha(cron_social_1)
##Social Exclusion
cron_social_2 <- Eqls %>% select(Y16_Q36a, Y16_Q36b, Y16_Q36c, Y16_Q36d)
cron_social_2 <- na.omit(cron_social_2)
cron_social_2<-cron_social_2 %>% filter_all(all_vars(. <= 96))
alpha(cron_social_2)
##access to local ameneties
cron_env <- Eqls %>% select(Y16_Q56a,Y16_Q56b,Y16_Q56c,Y16_Q56d, Y16_Q56e, Y16_Q56f )
cron_env <- na.omit(cron_env)
cron_env<-cron_env %>% filter_all(all_vars(. <= 96))
alpha(cron_env)
##neigbourhood safety
cron_env1 <- Eqls %>% select(Y16_Q55a,Y16_Q55b )
cron_env1 <- na.omit(cron_env1)
cron_env1<-cron_env1 %>% filter_all(all_vars(. <= 96))
alpha(cron_env1)
##work life balance
cron_psycho <- Eqls %>% select(Y16_Q20a,Y16_Q20b,Y16_Q20c)
cron_psycho <- na.omit(cron_psycho)
cron_psycho<-cron_psycho %>% filter_all(all_vars(. <= 96))
alpha(cron_psycho)
##subjective well being
cron_psycho_1 <- Eqls %>% select(Y16_Q7a,Y16_Q7c,Y16_Q7d )
cron_psycho_1 <- na.omit(cron_psycho_1)
cron_psycho_1<-cron_psycho_1 %>% filter_all(all_vars(. <= 96))
alpha(cron_psycho_1)

# Selecting variables relevant to the analysis based on their theoretical importance 
# and creating a new dataset for further analysis.
EqlsA <- Eqls %>% select(Y16_Q4,Y16_Agegroup,Y16_Empstatus_8categories_TREND,
                         Y16_HH2a, Y16_Q37,Y16_Education_3categories,Y16_Q13,Y16_Q21,
                         Y16_Q88,Y16_Q6b,Y16_Q32,Y16_Q48,Y16_Q49, Y16_Q61composite,Y16_Q52composite, Y16_Q58a,
                         Y16_Q40d,Y16_Q27d, Y16_Q47composite,Y16_Q6e,Y16_Q36composite,Y16_Q6d,
                         Y16_Q55composite,Y16_Q56composite,Y16_Q20composite,Y16_Q7composite,Y16_Q89composite,
                         Y16_Q19)




# Cleaning the data by removing NAs and filtering out outliers to ensure data quality.

EqlsA <- na.omit(EqlsA)

EqlsA <- EqlsA %>% filter_all(all_vars(. <= 10))

summary(EqlsA)


# Data Transformation - Base Dataset
BaseData <- EqlsA %>% transmute(
  life_satisfaction = ifelse(Y16_Q4 >= 7, 1, 0),
  gender = as.factor(ifelse(Y16_HH2a == 1, 1, 0)),
  marital_status = as.factor(Y16_Q37),
  age_category = as.factor(Y16_Agegroup),
  education_level = as.factor(Y16_Education_3categories),
  work_sector = as.factor(Y16_Q13),
  job_security = Y16_Q21,
  Household_financial_wellbeing = Y16_Q88,
  job_satisfaction = Y16_Q6b,
  economy_satisfaction = Y16_Q32,
  health_status = Y16_Q48,
  chronic_health_issues = ifelse(Y16_Q49 == 1, 1, 0),
  healthcare_access = Y16_Q61composite,
  healthservices_quality = Y16_Q58a,
  mental_wellbeing = Y16_Q52composite,
  social_support = as.factor(Y16_Q40d),
  community_engagement = Y16_Q27d,
  time_for_social_activities = Y16_Q47composite,
  familylife_satisfaction = Y16_Q6e,
  social_exclusion = Y16_Q36composite,
  housing_satisfaction = Y16_Q6d,
  Neigbourhood_safety = Y16_Q55composite,
  access_to_local_amenities = Y16_Q56composite,
  basic_need_affordability = ifelse(Y16_Q89composite == 1, 1, 0),
  work_life_balance = Y16_Q20composite,
  subjective_wellbeing = Y16_Q7composite,
  work_hours_lifebalance = Y16_Q19,
  cid = row_number() 
)

# Descriptive Statistics for Base Data
summary(BaseData)
skim(BaseData)

# Data Transformation - Transformed Dataset
TransformedData <- BaseData %>% transmute(
  life_satisfaction = ifelse(life_satisfaction == 0, "Not satisfied", "Satisfied"),
  gender = ifelse(gender == 1, "Male", "Female"),
  marital_status = recode_factor(as.factor(marital_status),
                                 `1` = "Never Married",
                                 `2` = "Married",
                                 `3` = "Separated",
                                 `4` = "Widowed",
                                 `5` = "Divorced"),
  age_category = recode_factor(as.factor(age_category),
                               `1` = "18 - 24",
                               `2` = "25 - 34",
                               `3` = "35 - 49",
                               `4` = "50 - 64",
                               `5` = "65 +"),
  education_level = recode_factor(as.factor(education_level),
                                  `1` = "Lower Secondary",
                                  `2` = "Upper Secondary",
                                  `3` = "Tertiary"),
  work_sector = recode_factor(as.factor(work_sector),
                              `1` = "Govt Admin",
                              `2` = "Other Public Sector",
                              `3` = "Private Sector",
                              `4` = "Others"),
  chronic_health_issues = ifelse(chronic_health_issues == 1, "Yes", "No"),
  healthcare_access = recode_factor(as.factor(healthcare_access),
                                    `1` = "Very Difficult",
                                    `2` = "Little Difficult",
                                    `3` = "Not Difficult"),
  time_for_social_activities = recode_factor(as.factor(time_for_social_activities),
                                             `1` = "Less Time",
                                             `2` = "Normal Time",
                                             `3` = "More Time"),
  Neigbourhood_safety = recode_factor(as.factor(Neigbourhood_safety),
                                      `1` = "Strongly Agree",
                                      `2` = "Agree",
                                      `3` = "Neutral",
                                      `4` = "Disagree",
                                      `5` = "Strongly Disagree"),
  access_to_local_amenities = recode_factor(as.factor(access_to_local_amenities),
                                            `1` = "Very Difficult",
                                            `2` = "Rather Difficult",
                                            `3` = "Rather Easy",
                                            `4` = "Very Easy"),
  basic_need_affordability = ifelse(basic_need_affordability == 1, "Yes", "No"),
  subjective_wellbeing = recode_factor(as.factor(subjective_wellbeing),
                                       `1` = "Strongly Agree",
                                       `2` = "Agree",
                                       `3` = "Neutral",
                                       `4` = "Disagree",
                                       `5` = "Strongly Disagree"),
  social_support = recode_factor(as.factor(social_support),
                                 `1` = "Family Member",
                                 `2` = "Non-Family Member",
                                 `3` = "Service Provider",
                                 `4` = "Nobody"),
  job_security,
  Household_financial_wellbeing,
  job_satisfaction,
  economy_satisfaction,
  health_status,
  healthservices_quality,
  mental_wellbeing,
  community_engagement,
  familylife_satisfaction,
  social_exclusion,
  housing_satisfaction,
  work_life_balance,
  work_hours_lifebalance
)

# Descriptive Statistics for Transformed Data
summary(TransformedData)
skim(TransformedData)


#Pearson Correlation Matrix
# Select relevant Quality of Life (QoL) factors for correlation analysis
QoL_factors <- BaseData %>%
  select(life_satisfaction,
         job_security,
         Household_financial_wellbeing,
         job_satisfaction,
         economy_satisfaction,
         health_status,
         chronic_health_issues,
         healthcare_access,
         healthservices_quality,
         mental_wellbeing,
         community_engagement,
         time_for_social_activities,
         familylife_satisfaction,
         social_exclusion,
         housing_satisfaction,
         Neigbourhood_safety,
         access_to_local_amenities,
         basic_need_affordability,
         work_life_balance,
         subjective_wellbeing,
         work_hours_lifebalance)

# Calculate the Pearson correlation matrix for the selected factors
# 'use = "complete.obs"' ensures that only complete cases are used, i.e no NAs
cor_matrix <- cor(QoL_factors, use = "complete.obs")

# Round the correlation values to 1 decimal place for better readability
cor_matrix <- round(cor_matrix, 1)

# Display the rounded correlation matrix
print(cor_matrix)

# Visualize the correlation matrix using 'corrplot'
# 'type = "lower"' displays only the lower triangular matrix
# Text labels ('tl') are rotated and coloured for clarity
# Color limits ('cl.lim') are set to range from -1 to 1 to represent the full correlation range
corrplot(cor_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "white", number.cex = 0.4, 
         number.digits = 1, 
         col = colorRampPalette(c("#FF5722", "grey", "#4CAF50"))(200),
         cl.lim = c(-1, 1), 
         mar = c(1, 1, 1, 1), 
         tl.cex = 0.6, 
         title = "Correlation Matrix of QoL Factors")

# Compute the correlation matrix with p-values to assess significance
# 'rcorr' from the 'Hmisc' package provides both correlation coefficients and p-values
correlation_results <- rcorr(as.matrix(QoL_factors))

# Extract and round p-values to 2 decimal places for easier interpretation
p_matrix <- round(correlation_results$P, 2)

# Display the p-values matrix 
print(p_matrix)



#Logit prediction model
logit_prediction_model <- glm(data = BaseData, life_satisfaction ~ 
                                job_security +
                                Household_financial_wellbeing +
                                job_satisfaction +
                                economy_satisfaction +
                                health_status +
                                chronic_health_issues +
                                healthcare_access +
                                healthservices_quality +
                                mental_wellbeing +
                                social_support +
                                community_engagement +
                                time_for_social_activities +
                                familylife_satisfaction +
                                social_exclusion +
                                housing_satisfaction +
                                Neigbourhood_safety +
                                access_to_local_amenities +
                                basic_need_affordability +
                                work_life_balance +
                                subjective_wellbeing +
                                work_hours_lifebalance +
                                age_category +
                                gender +
                                marital_status +
                                work_sector +
                                education_level, 
                              family = binomial(link = "logit"), x = TRUE)

summary(logit_prediction_model)

#Showing marginal effects
margins_summary(logit_prediction_model)

#Assessing Predictive Performance of the Logistic Regression Model

#Data Splitting
# Set seed for reproducibility
set.seed(2543)

# Split the data into training (80%) and testing (20%) sets
train <- BaseData %>% sample_frac(0.8)
test  <- anti_join(BaseData, train, by = "cid")

#Preparing Training and Testing Data
# Define features and target for training and testing sets
x_train <- train %>% select(-life_satisfaction, -cid)
y_train <- train$life_satisfaction
x_test <- test %>% select(-life_satisfaction, -cid)
y_test <- test$life_satisfaction

# Convert life_satisfaction to a factor
train$life_satisfaction <- as.factor(train$life_satisfaction)
test$life_satisfaction <- as.factor(test$life_satisfaction)

#Training the Logistic Regression Model
# Set seed for reproducibility
set.seed(1783)

# Train Logistic Regression model with Cross-Validation
logit_train_model <- train(life_satisfaction ~ ., data = train, method = "glm", 
                           family = binomial, trControl = trainControl(method = "cv", number = 10))

# Print the model to see the results of cross-validation
print(logit_train_model)

# Making Predictions
# Generate predictions on the test set
logit_predictions <- predict(logit_train_model, newdata = test)

# Evaluating Model Performance
# Confusion Matrix and Accuracy
conf_matrix_logit <- confusionMatrix(logit_predictions, test$life_satisfaction)
print(conf_matrix_logit)

# ROC Curve and AUC
# Get predicted probabilities for the positive class
logit_probs <- predict(logit_train_model, newdata = test, type = "prob")[,2]
summary(logit_probs)
# Compute ROC curve and AUC
roc_logit <- roc(as.numeric(test$life_satisfaction) - 1, logit_probs) # Convert factor levels to 0/1
plot(roc_logit)
print(auc(roc_logit))


# Moderation Analysis
# This script examines the interaction effects between various predictors and age category on life satisfaction.
# logistic moderation for each variables is performed
# Significant interactions indicate that the interaction term is significantly different from the reference group, hence moderation
# significant interaction terms are further probed via marginal effect or simple slope analysis


# Analyzing Interaction Effects Between Health and Age Category

# Model H6a: Interaction between Health Status and Age Category
H6a <- glm(data = BaseData, life_satisfaction ~ health_status * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H6a)

# Model H6b: Interaction between Mental Wellbeing and Age Category
H6b <- glm(data = BaseData, life_satisfaction ~ mental_wellbeing * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H6b)

# Model H6c: Interaction between Chronic Health Issues and Age Category
H6c <- glm(data = BaseData, life_satisfaction ~ chronic_health_issues * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H6c)

# Model H6d: Interaction between Healthcare Access and Age Category
H6d <- glm(data = BaseData, life_satisfaction ~ healthcare_access * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H6d)

# Since the interaction in Model H6d (Healthcare Access * Age Category) is significant, the marginal effect is computed and plotted.

marginal_effects_H6d <- margins(H6d, variables = "healthcare_access", 
                                at = list(age_category = levels(BaseData$age_category)))
marginal_effects_H6d <- summary(marginal_effects_H6d)
marginal_effects_H6d <- as.data.frame(marginal_effects_H6d)

# Adding age_category as a factor for plotting
marginal_effects_H6d$age_category <- as.factor(marginal_effects_H6d$age_category)

# Plotting the marginal effects for Healthcare Access * Age Category
ggplot(marginal_effects_H6d, aes(x = age_category, y = AME, ymin = lower, ymax = upper, color = age_category)) +
  geom_pointrange() +
  labs(title = "Marginal Effect of Healthcare Access & Age Interaction on Life Satisfaction",
       x = "Age Category",
       y = "Average Marginal Effect (AME) of Healthcare Access on Life Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed")


# Model H6e: Interaction between Health Services Quality and Age Category
H6e <- glm(data = BaseData, life_satisfaction ~ healthservices_quality * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H6e)

# since significant, calculate and plot the marginal effects
marginal_effects_H6e <- margins(H6e, variables = "healthservices_quality", 
                                at = list(age_category = levels(BaseData$age_category)))
marginal_effects_H6e <- summary(marginal_effects_H6e)
marginal_effects_H6e <- as.data.frame(marginal_effects_H6e)
marginal_effects_H6e$age_category <- as.factor(marginal_effects_H6e$age_category)

ggplot(marginal_effects_H6e, aes(x = age_category, y = AME, ymin = lower, ymax = upper, color = age_category)) +
  geom_pointrange() +
  labs(title = "Marginal Effect of Health Services Quality & Age Interaction on Life Satisfaction",
       x = "Age Category",
       y = "Average Marginal Effect (AME) of Health Services Quality on Life Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed")

# Analyzing Social Interaction Effects

# Model H7a: Interaction between Community Engagement and Age Category
H7a <- glm(data = BaseData, life_satisfaction ~ community_engagement * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H7a)

# since significant, calculate and plot the marginal effects
marginal_effects_H7a <- margins(H7a, variables = "community_engagement", 
                                at = list(age_category = levels(BaseData$age_category)))
marginal_effects_H7a <- summary(marginal_effects_H7a)
marginal_effects_H7a <- as.data.frame(marginal_effects_H7a)
marginal_effects_H7a$age_category <- as.factor(marginal_effects_H7a$age_category)

ggplot(marginal_effects_H7a, aes(x = age_category, y = AME, ymin = lower, ymax = upper, color = age_category)) +
  geom_pointrange() +
  labs(title = "Marginal Effect of Community Engagement & Age Interaction on Life Satisfaction",
       x = "Age Category",
       y = "Average Marginal Effect (AME) of Community Engagement on Life Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed")


# Model H7b: Interaction between Time for Social Activities and Age Category
H7b <- glm(data = BaseData, life_satisfaction ~ time_for_social_activities * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H7b)

# Model H7c: Interaction between Family Life Satisfaction and Age Category
H7c <- glm(data = BaseData, life_satisfaction ~ familylife_satisfaction * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H7c)

# since significant, calculate and plot the marginal effects
marginal_effects_H7c <- margins(H7c, variables = "familylife_satisfaction", 
                                at = list(age_category = levels(BaseData$age_category)))
marginal_effects_H7c <- summary(marginal_effects_H7c)
marginal_effects_H7c <- as.data.frame(marginal_effects_H7c)
marginal_effects_H7c$age_category <- as.factor(marginal_effects_H7c$age_category)

ggplot(marginal_effects_H7c, aes(x = age_category, y = AME, ymin = lower, ymax = upper, color = age_category)) +
  geom_pointrange() +
  labs(title = "Marginal Effect of Family Life Satisfaction & Age Interaction on Life Satisfaction",
       x = "Age Category",
       y = "Average Marginal Effect (AME) of Family Life Satisfaction on Life Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed")


# Model H7d: Interaction between Social Exclusion and Age Category
H7d <- glm(data = BaseData, life_satisfaction ~ social_exclusion * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H7d)

#Analyzing Economic Interaction Effects

# Model H8a: Interaction between Job Security and Age Category
H8a <- glm(data = BaseData, life_satisfaction ~ job_security * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H8a)

# since significant, calculate and plot the marginal effects
marginal_effects_H8a <- margins(H8a, variables = "job_security", 
                                at = list(age_category = levels(BaseData$age_category)))
marginal_effects_H8a <- summary(marginal_effects_H8a)
marginal_effects_H8a <- as.data.frame(marginal_effects_H8a)
marginal_effects_H8a$age_category <- as.factor(marginal_effects_H8a$age_category)

ggplot(marginal_effects_H8a, aes(x = age_category, y = AME, ymin = lower, ymax = upper, color = age_category)) +
  geom_pointrange() +
  labs(title = "Marginal Effect of Job Security & Age Interaction on Life Satisfaction",
       x = "Age Category",
       y = "Average Marginal Effect (AME) of Job Security on Life Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed")

# Model H8b: Interaction between Job Satisfaction and Age Category
H8b <- glm(data = BaseData, life_satisfaction ~ job_satisfaction * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H8b)

# since significant, calculate and plot the marginal effects
marginal_effects_H8b <- margins(H8b, variables = "job_satisfaction", 
                                at = list(age_category = levels(BaseData$age_category)))
marginal_effects_H8b <- summary(marginal_effects_H8b)
marginal_effects_H8b <- as.data.frame(marginal_effects_H8b)
marginal_effects_H8b$age_category <- as.factor(marginal_effects_H8b$age_category)

ggplot(marginal_effects_H8b, aes(x = age_category, y = AME, ymin = lower, ymax = upper, color = age_category)) +
  geom_pointrange() +
  labs(title = "Marginal Effect of Job Satisfaction & Age Interaction on Life Satisfaction",
       x = "Age Category",
       y = "Average Marginal Effect (AME) of Job Satisfaction on Life Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed")


# Model H8c: Interaction between Economy Satisfaction and Age Category
H8c <- glm(data = BaseData, life_satisfaction ~ economy_satisfaction * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H8c)

# Model H8d: Interaction between Household Financial Wellbeing and Age Category
H8d <- glm(data = BaseData, life_satisfaction ~ Household_financial_wellbeing * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H8d)

# since significant, Calculate and extract marginal effects for H8d
marginal_effects_H8d <- margins(H8d, variables = "Household_financial_wellbeing", 
                                at = list(age_category = levels(BaseData$age_category)))
marginal_effects_H8d <- summary(marginal_effects_H8d)
marginal_effects_H8d <- as.data.frame(marginal_effects_H8d)
marginal_effects_H8d$age_category <- as.factor(marginal_effects_H8d$age_category)

# Plot marginal effects for H8d
ggplot(marginal_effects_H8d, aes(x = age_category, y = AME, ymin = lower, ymax = upper, color = age_category)) +
  geom_pointrange() +
  labs(title = "Marginal Effect of HFW and Age Interaction on LS",
       x = "Age Category",
       y = "AME of HFW on LS") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed")

# Model H8e: Interaction between Basic Need Affordability and Age Category
H8e <- glm(data = BaseData, life_satisfaction ~ basic_need_affordability * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H8e)

#  Exploring Environmental Interaction Effects

# Model H9a: Interaction between Housing Satisfaction and Age Category
H9a <- glm(data = BaseData, life_satisfaction ~ housing_satisfaction * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H9a)

# since significant, Calculate and extract marginal effects for H9a
marginal_effects_H9a <- margins(H9a, variables = "housing_satisfaction", 
                                at = list(age_category = levels(BaseData$age_category)))
marginal_effects_H9a <- summary(marginal_effects_H9a)
marginal_effects_H9a <- as.data.frame(marginal_effects_H9a)
marginal_effects_H9a$age_category <- as.factor(marginal_effects_H9a$age_category)

# Plot marginal effects for H9a
ggplot(marginal_effects_H9a, aes(x = age_category, y = AME, ymin = lower, ymax = upper, color = age_category)) +
  geom_pointrange() +
  labs(title = "Marginal Effect of Housing Satisfaction and Age Interaction on LS",
       x = "Age Category",
       y = "AME of Housing Satisfaction on LS") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed")

# Model H9b: Interaction between Neighbourhood Safety and Age Category
H9b <- glm(data = BaseData, life_satisfaction ~ Neigbourhood_safety * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H9b)

# Model H9c: Interaction between Access to Local Amenities and Age Category
H9c <- glm(data = BaseData, life_satisfaction ~ access_to_local_amenities * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H9c)

# since, significant, Calculate and extract marginal effects for H9c
marginal_effects_H9c <- margins(H9c, variables = "access_to_local_amenities", 
                                at = list(age_category = levels(BaseData$age_category)))
marginal_effects_H9c <- summary(marginal_effects_H9c)
marginal_effects_H9c <- as.data.frame(marginal_effects_H9c)
marginal_effects_H9c$age_category <- as.factor(marginal_effects_H9c$age_category)

# Plot marginal effects for H9c
ggplot(marginal_effects_H9c, aes(x = age_category, y = AME, ymin = lower, ymax = upper, color = age_category)) +
  geom_pointrange() +
  labs(title = "ME of Access to Local-Amenities & Age Interaction on LS",
       x = "Age Category",
       y = "AME of Access to Local Amenities on LS") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed")

# Analyzing Psychological Interaction Effects

# Model H10a: Interaction between Work-Life Balance and Age Category
H10a <- glm(data = BaseData, life_satisfaction ~ work_life_balance * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H10a)

# Model H10b: Interaction between Subjective Wellbeing and Age Category
H10b <- glm(data = BaseData, life_satisfaction ~ subjective_wellbeing * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H10b)

# Model H10c: Interaction between Work Hours Life Balance and Age Category
H10c <- glm(data = BaseData, life_satisfaction ~ work_hours_lifebalance * age_category, family = binomial(link = "logit"), x = TRUE)
summary(H10c)

# Step 11: Exploring Demographics 

# Model C11a: Interaction between Gender and Age Category
C11a <- glm(data = BaseData, life_satisfaction ~ gender * age_category, family = binomial(link = "logit"), x = TRUE)
summary(C11a)

# Model C11b: Interaction between Work Sector and Age Category
C11b <- glm(data = BaseData, life_satisfaction ~ work_sector * age_category, family = binomial(link = "logit"), x = TRUE)
summary(C11b)

# Model C11c: Interaction between Marital Status and Age Category
C11c <- glm(data = BaseData, life_satisfaction ~ marital_status * age_category, family = binomial(link = "logit"), x = TRUE)
summary(C11c)

# Model C11d: Interaction between Education Level and Age Category
C11d <- glm(data = BaseData, life_satisfaction ~ education_level * age_category, family = binomial(link = "logit"), x = TRUE)
summary(C11d)

# Model C11e: Interaction between Social Support and Age Category
C11e <- glm(data = BaseData, life_satisfaction ~ social_support * age_category, family = binomial(link = "logit"), x = TRUE)
summary(C11e)



# Visualization for Descriptive Analysis

# 1. Life Satisfaction by Chronic Health Issues
Health_issues_life <- TransformedData %>%
  group_by(chronic_health_issues, life_satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

fig1 <- ggplot(Health_issues_life, aes(x = chronic_health_issues, y = percentage, fill = life_satisfaction)) + 
  geom_bar(stat = "identity", position = "stack", colour = "black") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, 
            family = "sans", color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("#FF5722", "#4CAF50")) +
  theme(panel.border = element_blank(), axis.line = element_blank(), legend.position = "bottom", 
        legend.title = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  labs(title = "Life Satisfaction Distribution by Chronic Health Issues", 
       x = "Chronic Health Issues", y = "Percentage Distribution") +
  guides(fill = guide_legend(title = "Life Satisfaction"))

fig1

# 2. Life Satisfaction by Health Care Access
Healthcare_access_life <- TransformedData %>%
  group_by(healthcare_access, life_satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

fig2 <- ggplot(Healthcare_access_life, aes(x = healthcare_access, y = percentage, fill = life_satisfaction)) + 
  geom_bar(stat = "identity", position = "stack", colour = "black") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, 
            family = "sans", color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("#FF5722", "#4CAF50")) +
  theme(panel.border = element_blank(), axis.line = element_blank(), legend.position = "bottom", 
        legend.title = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  labs(title = "Life Satisfaction Distribution by Health Care Access", 
       x = "Access to Health Care Services", y = "Percentage Distribution") +
  guides(fill = guide_legend(title = "Life Satisfaction"))

fig2

# 3. Life Satisfaction by Time for Social Activities
Socialtime_life <- TransformedData %>%
  group_by(time_for_social_activities, life_satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

fig3 <- ggplot(Socialtime_life, aes(x = time_for_social_activities, y = percentage, fill = life_satisfaction)) + 
  geom_bar(stat = "identity", position = "stack", colour = "black") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, 
            family = "sans", color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("#FF5722", "#4CAF50")) +
  theme(panel.border = element_blank(), axis.line = element_blank(), legend.position = "bottom", 
        legend.title = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  labs(title = "Life Satisfaction Distribution by Time for Social Activities", 
       x = "Time for Social Activities", y = "Percentage Distribution") +
  guides(fill = guide_legend(title = "Life Satisfaction"))

fig3

# 4. Life Satisfaction by Neighbourhood Safety
Neighsafe_life <- TransformedData %>%
  group_by(Neigbourhood_safety, life_satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

fig4 <- ggplot(Neighsafe_life, aes(x = Neigbourhood_safety, y = percentage, fill = life_satisfaction)) + 
  geom_bar(stat = "identity", position = "stack", colour = "black") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, 
            family = "sans", color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("#FF5722", "#4CAF50")) +
  theme(panel.border = element_blank(), axis.line = element_blank(), legend.position = "bottom", 
        legend.title = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  labs(title = "Life Satisfaction Distribution by Neighbourhood Safety", 
       x = "Neighbourhood Safety", y = "Percentage Distribution") +
  guides(fill = guide_legend(title = "Life Satisfaction"))

fig4

# 5. Life Satisfaction by Access to Local Amenities
Localaccess_life <- TransformedData %>%
  group_by(access_to_local_amenities, life_satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

fig5 <- ggplot(Localaccess_life, aes(x = access_to_local_amenities, y = percentage, fill = life_satisfaction)) + 
  geom_bar(stat = "identity", position = "stack", colour = "black") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, 
            family = "sans", color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("#FF5722", "#4CAF50")) +
  theme(panel.border = element_blank(), axis.line = element_blank(), legend.position = "bottom", 
        legend.title = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  labs(title = "Life Satisfaction Distribution by Access to Local Amenities", 
       x = "Access to Local Amenities", y = "Percentage Distribution") +
  guides(fill = guide_legend(title = "Life Satisfaction"))

fig5

# 6. Life Satisfaction by Basic Need Affordability
Basicneed_life <- TransformedData %>%
  group_by(basic_need_affordability, life_satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

fig6 <- ggplot(Basicneed_life, aes(x = basic_need_affordability, y = percentage, fill = life_satisfaction)) + 
  geom_bar(stat = "identity", position = "stack", colour = "black") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, 
            family = "sans", color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("#FF5722", "#4CAF50")) +
  theme(panel.border = element_blank(), axis.line = element_blank(), legend.position = "bottom", 
        legend.title = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  labs(title = "Life Satisfaction Distribution by Basic Need Affordability", 
       x = "Basic Need Affordability", y = "Percentage Distribution") +
  guides(fill = guide_legend(title = "Life Satisfaction"))

fig6

# 7. Life Satisfaction by Subjective Well-Being
Subwellbeing_life <- TransformedData %>%
  group_by(subjective_wellbeing, life_satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

fig7 <- ggplot(Subwellbeing_life, aes(x = subjective_wellbeing, y = percentage, fill = life_satisfaction)) + 
  geom_bar(stat = "identity", position = "stack", colour = "black") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, 
            family = "sans", color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("#FF5722", "#4CAF50")) +
  theme(panel.border = element_blank(), axis.line = element_blank(), legend.position = "bottom", 
        legend.title = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  labs(title = "Life Satisfaction Distribution by Subjective Well-Being", 
       x = "Subjective Well-Being", y = "Percentage Distribution") +
  guides(fill = guide_legend(title = "Life Satisfaction"))

fig7

# 8. Life Satisfaction by Age Category
Age_life <- TransformedData %>%
  group_by(age_category, life_satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

fig8 <- ggplot(Age_life, aes(x = factor(age_category), y = percentage, fill = life_satisfaction)) + 
  geom_bar(stat = "identity", position = "stack", colour = "black") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, 
            family = "sans", color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("#FF5722", "#4CAF50")) +
  theme(panel.border = element_blank(), axis.line = element_blank(), legend.position = "bottom", 
        legend.title = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  labs(title = "Life Satisfaction Distribution by Age Category", 
       x = "Age Category", y = "Percentage Distribution") +
  guides(fill = guide_legend(title = "Life Satisfaction"))

fig8








