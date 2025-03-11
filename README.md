# Life-Satisfaction-Machine-Learning

# Project Portfolio: Predicting Life Satisfaction Across Europe

## Project Overview
This project investigates the determinants of life satisfaction across Europe by leveraging predictive modeling techniques. By analyzing various quality of life (QoL) factors—health, social, economic, psychological, and environmental—the study provides insights into how these factors influence life satisfaction across different age groups. The research employs advanced logistic regression analysis to assess these relationships and understand the moderating effects of age.

## Research Aim
To develop a predictive model that evaluates how various QoL factors impact overall life satisfaction in Europe, emphasizing the role of age as a moderating variable.

## Key Objectives
- Conduct a comprehensive literature review and empirical analysis to understand the impact of QoL factors on life satisfaction.  
- Utilize machine learning techniques and logistic regression models to quantify these relationships and analyze the moderation effects of age.  
- Provide targeted recommendations for policymakers to improve life satisfaction through age-specific interventions.

## Methodology
### 1. Data Collection & Preprocessing
- The study utilizes the European Quality of Life Survey (EQLS) dataset (2003-2016) and processes data using R.  
- Composite variables were created by aggregating multiple indicators of QoL, ensuring consistency with original scales and strong reliability metrics.

### 2. Technology Used
- **Programming Language:** R  
- **Data Manipulation & Transformation:** `dplyr`, `skimr`, `readr`  
- **Data Visualization:** `ggplot2`, `corrplot`, `gridExtra`, `RColorBrewer`  
- **Statistical Analysis:** `Hmisc`, `corrr`, `psych`, `broom`, `margins`, `pROC`  
- **Machine Learning & Model Building:** `caret`, `rpart`, `rattle`  
- **Data Importation:** `haven` (for reading .dta files)  

### 3. Statistical Analysis & Modeling
- Pearson correlation analysis to assess relationships between QoL factors.  
- Logistic regression modeling to predict life satisfaction outcomes.  
- Moderation analysis to examine the interaction effects between age and key QoL factors.

### 4. Model Performance Evaluation
- The predictive model achieved an accuracy of **79.64%** and an AUC (Area Under the ROC Curve) of **0.8507**, indicating excellent discriminative power.  
- Marginal effects analysis was performed to interpret significant moderation effects.

## Key Findings
- **Health Factors:** Healthcare access and quality significantly impact life satisfaction, with age moderating their effects.  
- **Social Factors:** Community engagement and family life satisfaction vary in importance across age groups.  
- **Economic Factors:** Job security, job satisfaction, and financial well-being play crucial roles in life satisfaction, with different age groups prioritizing these factors differently.  
- **Psychological Factors:** Work-life balance and subjective well-being consistently impact life satisfaction across all age groups.  
- **Environmental Factors:** Access to local amenities and housing satisfaction significantly influence well-being, with varying importance based on age.

## Visual Insights
- Data visualizations, including correlation matrices, bar charts, and ROC curves, illustrate key relationships and model performance.  
- Marginal effect plots highlight how specific QoL factors interact with age to influence life satisfaction.

## Conclusion & Recommendations
- Life satisfaction is a multifaceted construct influenced by a diverse range of QoL factors.  
- Policymakers should adopt **age-specific strategies** to enhance life satisfaction effectively.  
- Future research should explore additional machine learning approaches to refine predictive capabilities further.

This project offers valuable insights into life satisfaction determinants, providing a data-driven foundation for policy interventions aimed at improving well-being across different demographic groups in Europe.
