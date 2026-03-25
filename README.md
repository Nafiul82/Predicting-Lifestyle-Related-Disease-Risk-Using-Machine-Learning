# Heart Disease Risk Prediction System

# Overview

This project is a machine learning-based web application designed to predict the risk of heart disease based on user-provided health and lifestyle information. The system combines predictive modelling with an interactive interface to support early awareness and preventive healthcare.

This project was developed as part of a final-year dissertation.

# Features

* Predicts heart disease risk (Low, Moderate, High)
* Displays prediction probability using visual charts
* Provides lifestyle recommendations based on input factors
* Includes user authentication (login and registration)
* Web-based interface built using Shiny

# Machine Learning Models

The system implements and compares the following models:

* Logistic Regression
* Random Forest
* Gradient Boosting Machine (GBM)

Evaluation metrics include:

* Accuracy
* Precision
* Recall
* F1-Score
* ROC-AUC
* Confusion Matrix

## Project Structure

data/
train.csv
test.csv

models/
log_model.rds
rf_model.rds
gbm_model.rds

R/
prepare_data.R
split_data.R
train_model.R
evaluation_plots.R
chi_square_analysis.R
feature_importance.R

app.R
README.md


# Dataset

The dataset used in this project is publicly available on Kaggle:
https://www.kaggle.com/datasets/mahatiratusher/heart-disease-risk-prediction-dataset

The dataset includes:

* Demographic data (age, gender)
* Lifestyle factors (smoking, obesity, stress)
* Medical conditions (diabetes, blood pressure, cholesterol)
* Symptoms (chest pain, fatigue, palpitations)

# Disclaimer

This system is intended for educational and research purposes only. It is not a medical diagnostic tool and should not replace professional healthcare advice.

# Future Improvements

* Integration with real clinical datasets
* Explainable AI techniques (e.g., SHAP values)
* Improved personalization of recommendations
* Extension to a full health monitoring system

# Author

Md Nafiul Islam Khan (Final Year Bsc Computer Science (SWE))


This project is for academic use. For reuse or distribution, please contact the author.
