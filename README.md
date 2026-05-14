# MIMIC-IV ICU Mortality Prediction Pipeline

End-to-end machine learning pipeline for 30-day ICU mortality prediction using the MIMIC-IV clinical database.

**Dataset:** MIMIC-IV — 94,000+ ICU stays extracted from 400M+ EHR records via SQL (BigQuery + DuckDB), stored as Parquet for efficient querying

**Methods:**
- Built scalable data pipeline with DuckDB and Parquet; queried structured EHR data using SQL on BigQuery
- Engineered time-aware features from pre-ICU lab results and earliest vital signs to prevent data leakage
- Trained and cross-validated Elastic Net, Random Forest, and XGBoost classifiers
- Analyzed missingness patterns and applied SHAP for feature importance
- Built interactive Shiny dashboard for model metric monitoring and result visualization

**Results:** Best model (XGBoost) achieved AUC = 0.65 on held-out test set

**Stack:** R, DuckDB, Parquet, BigQuery, XGBoost, SHAP, Shiny
