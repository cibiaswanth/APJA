# Customer Churn Prediction in the Banking Sector

This project focuses on predicting customer churn in the banking sector using machine learning techniques. The primary goal is to determine whether a customer will subscribe to a product, specifically the Term Deposit, based on various features captured during a telephonic-directed market campaign.

## Objective

The objective of this project is to build a robust prediction model for customer churn in the banking industry. With increasing competition, it's crucial for banks to retain customers and efficiently target potential subscribers, hence the deployment of machine learning models to predict subscription likelihood.

## Workflow Overview

### Data Processing
- Loading the dataset from a CSV file and examining its structure.
- Handling missing values using packages like Amelia and checking missing value patterns.
- Normalizing data using Min-Max scaling, Z-Score standardization, and SoftMax scaling to prepare features for modeling.

### Exploratory Data Analysis (EDA)
- Visualization of data distribution, checking normality of variables.
- Creating correlation maps to understand relationships among features.
- Histograms and boxplots to explore variable distributions and relationships.

### Model Building
- Employing Support Vector Machine (SVM) classification using 'e1071' and 'caret' libraries.
- Random Forest classification using 'randomForest' and 'caret' packages.
- Decision Tree modeling with 'rpart' and 'rpart.plot' libraries.

## File Structure

The project is organized into sections:
- Data loading and preprocessing.
- Exploratory data analysis.
- Model building and evaluation.

## Files Included
- CSV file (`Dis2.csv`) containing the dataset.
- R script with code for data processing, EDA, and model building.
- `readme.md` - This documentation providing an overview of the project.

## Instructions
1. Ensure the necessary R libraries are installed.
2. Load the provided CSV file (`Dis2.csv`) into your R environment.
3. Run the R script in sections to replicate data preprocessing, EDA, and model building steps.

## Conclusion

The project concludes with the implementation of various machine learning techniques for predicting customer churn in the banking sector. The models developed can assist in making informed decisions for targeting potential subscribers effectively.

Feel free to explore the code, data, and results included in this repository!

