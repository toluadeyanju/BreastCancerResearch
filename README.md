Breast Cancer Clinical Data Analysis

This repository contains exploratory data analysis and visualization of a breast cancer clinical dataset, focusing on staging patterns, treatment distributions, and data completeness. 

Project Objectives

- Summarize baseline clinical characteristics
- Explore staging and treatment patterns
- Assess missing data structure and completeness
- Identify limitations affecting downstream analysis
- Provide recommendations for data improvement

Clinical Characteristics & Data Quality Overview

AJCC Stage Distribution
- Most patients were classified as Stage III, followed by Stage IV
- Very few cases were recorded as Stage I or II
- A substantial number of records were missing staging information

Interpretation:
- This distribution suggests a late-stage presentation pattern, commonly observed in resource-limited settings, with significant implications for treatment planning and outcomes analysis.

Chemotherapy Regimen Frequency
- AC/EC-based regimens were most commonly administered among documented cases
- Taxane-based therapies were the second most frequent
- The largest proportion of entries were missing regimen data
- Hormonal and “Other” treatments were rarely recorded

Implications:
- High missingness limits reliable treatment-effect analyses. Improving regimen documentation should be prioritized in future data collection.


Missing Data Overview
- Percentage of Missingness per Variable
- Missingness ranged from <5% to nearly 100%
- Several clinically relevant fields (e.g., imaging results, adjuvant therapy details) had >70% missingness
- Demographic variables were relatively complete

Missingness Distribution Across Observations
• Overall missingness was approximately 49.5%
• Missingness was not random, with blocks of consistently unreported variables

Next Steps
- Improve data-collection protocols to reduce missing information
- Emphasize complete documentation of clinical and treatment variables
Avoid listwise deletion due to high data loss
Enable valid statistical modeling through imputation strategies, exclusion of high-missingness variables, and assessment of missingness mechanisms (MCAR/MAR/MNAR)
