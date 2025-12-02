Breast Cancer Clinical Data Analysis

This repository contains exploratory data analysis and visualization of a breast cancer clinical dataset, focusing on staging patterns, treatment distributions, and data completeness. 

Project Objectives
- Summarize baseline clinical characteristics of the patient cohort.
- Explore AJCC staging distributions and chemotherapy regimen patterns.
- Analyze the relationship between BMI, tumor stage, and menopausal status.
- Assess the structure, extent, and mechanisms of missing data.
- Provide recommendations for data improvement and statistical handling of incomplete records.

Clinical Characteristics & Data Quality Overview

1. AJCC Stage Distribution
- Most patients were classified as Stage III, followed by Stage IV
- Very few cases were recorded as Stage I or II
- A substantial number of records were missing staging information

Interpretation:
- This distribution suggests a late-stage presentation pattern, commonly observed in resource-limited settings, with significant implications for treatment planning and outcomes analysis.

2. BMI Distribution
- The result shows a slightly right-skewed distribution with a significant subset of patients having BMI>3
- Postmenopausal women demonstrated a higher median BMI compared to premenopausal women.
- Statistical testing (Kruskal-Wallis) yielded a p-value of 0.058, suggesting a borderline significant relationship between BMI and tumor size.
- While not strictly statistically significant at the 0.05 level, it warrants further investigation with a more complete dataset.

3. Chemotherapy Regimen Frequency
- AC/EC-based regimens were most commonly administered among documented cases
- Taxane-based therapies were the second most frequent
- The largest proportion of entries was missing regimen data
- Hormonal and “Other” treatments were rarely recorded

Missing Data Overview
- Percentage of Missingness per Variable
- Missingness ranged from <5% to nearly 100%
- Several clinically relevant fields (e.g., imaging results, adjuvant therapy details) had >70% missingness
- Demographic variables were relatively complete

Missingness Distribution Across Observations
- Overall missingness was approximately 49.5%
- Missingness was not random, with blocks of consistently unreported variables

Implications:
- High missingness limits reliable treatment-effect analyses. Improving regimen documentation would be prioritized in future data collection.

Next Steps
- Improve data-collection protocols to reduce missing information
- Emphasize complete documentation of clinical and treatment variables
Avoid listwise deletion due to high data loss
Enable valid statistical modeling through imputation strategies, exclusion of high-missingness variables, and assessment of missingness mechanisms (MCAR/MAR/MNAR)
