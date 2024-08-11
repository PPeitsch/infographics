# Infographics Project

This project contains various data analyses on different datasets, showcasing a range of statistical and data visualization techniques. Below is a list of all analyses with links to their respective sections in this README.

## Table of Contents

1. [Insurance Analysis](#1-insurance-analysis)
2. [Titanic Dataset Analysis](#2-titanic-dataset-analysis)
3. [Flights Analysis](#3-flights-analysis)
4. [Titanic Dataset (Extended)](#4-titanic-dataset-extended)
5. [Tree Project](#5-tree-project)
6. [Infographics](#6-infographics)
7. [Linear Model](#7-linear-model)
8. [Relational Data](#8-relational-data)
9. [Confounders](#9-confounders)
10. [Bitcoin Analysis](#10-bitcoin-analysis)

## Analyses Overview

### 1. Insurance Analysis
This analysis explores a dataset of medical insurance charges in the USA. The dataset contains 1338 entries with 7 variables, including both categorical (sex, smoker, children, region) and continuous variables. The main focus is on how different factors affect insurance charges.

Key points:
- Analyzed the relationship between age and insurance costs
- Identified risk groups based on smoking status and BMI
- Visualized how these factors influence insurance charges

[View detailed analysis](./01_insurance/doc/01_insurance.pdf)

### 2. Titanic Dataset Analysis
This analysis examines the relationship between passenger class and survival rates in the Titanic disaster. It uses grouping functions to calculate passenger counts and mean ages for each passenger class and survival status.

Key findings:
- First-class passengers had a higher survival rate compared to second and third-class passengers
- The analysis suggests that economic factors may have influenced survival chances
- The findings raise questions about equity in emergency situations

[View detailed analysis](./02_titanic/doc/02_titanic.pdf)

### 3. Flights Analysis
This analysis focuses on the New York City flights dataset from 2013, available as an R package. The study aims to identify airlines with the most delays for flights departing from Los Angeles airport, considering only airlines with over 1000 flights that year.

Key points:
- Identified five airlines meeting the criteria: VX, UA, DL, B6, and AA
- Calculated mean, median, and standard deviation of delays for these airlines
- Conducted a more in-depth analysis of United Airlines (UA) delays by month

[View detailed analysis](./03_flights/doc/03_flights.pdf)

### 4. Titanic Dataset (Extended)
This extended analysis of the Titanic dataset explores the relationship between survival rates and passengers' sex and age, in addition to the previously examined passenger class factor.

Key findings:
- Female passengers had a higher survival rate than male passengers
- The majority of passengers were between 20 and 40 years old
- For male passengers, there were slight differences in survival rates based on age:
  - Higher survival rates for boys under 10
  - Lower survival rates for males between 10 and 20 years old

[View detailed analysis](./04_titanic/doc/04_titanic.pdf)

### 5. Tree Project
This project is a feasibility study for the location of Jacaranda trees based on geospatial analysis in Buenos Aires. The study aims to identify suitable locations for planting 20 new Jacaranda trees in the city.

Key points:
- Utilized data from the Buenos Aires Public Linear Tree Census
- Applied filters to identify streets with less than 15 trees and sidewalks wider than 3.5 meters
- Considered factors such as existing tree height and avenue locations for maximum visibility
- Identified 13 suitable blocks meeting all criteria for planting new Jacaranda trees

[View detailed analysis](./05_tree-project/doc/05_tree-project.pdf)

### 6. Infographics
This analysis focuses on flight delays in New York City for the year 2013, using the nycflights13 dataset. The study aims to determine the reasons for flight delays departing from New York City airports.

Key points:
- Analyzed the correlation between flight delays and annual mileage
- Identified airlines prone to more delays
- Found that delays are concentrated mainly in three destination airports: MSP, DTW, and ATL
- Suggested future analysis of other parameters like weather conditions

[View detailed analysis](./06_infografia/doc/06_infographics.pdf)

### 7. Linear Model
This analysis uses the US Health Insurance dataset to apply a Multiple Linear Regression (MLR) model. The study aims to predict insurance charges based on age and sex of the insured.

Key points:
- Used age and sex as explanatory variables
- Applied a quadratic transformation to the age variable
- The model explains approximately 96.19% of the variability in insurance charges
- Found that age has a significant positive correlation with insurance charges
- Identified that women have higher insurance costs than men

[View detailed analysis](./07_linear-model/doc/07_linear-model.pdf)

### 8. Relational Data
This study analyzes data from the top 5 European football leagues from 2014 to 2020. The analysis focuses on identifying the most influential players and those with the worst fair play record.

Key points:
- Calculated a score for player influence based on goals and assists
- Identified the top 5 most influential players, including Cristiano Ronaldo and Luis Suárez
- Evaluated fair play based on yellow and red cards received
- Visualized the scores of top players compared to the rest of the dataset

[View detailed analysis](./08_relational-data/doc/08_relational-data.pdf)

### 9. Confounders
This analysis compares two linear regression models for the relationship between sepal width and length in the Iris dataset, demonstrating the impact of confounding variables.

Key points:
- Compared a simple linear model (sepal width vs. length) with a model that includes species as a variable
- Found that the simple model poorly represents the data (low R-squared, high p-value)
- The model including species as a variable significantly improved the fit (higher R-squared, lower p-values)
- Demonstrated how ignoring the species variable can lead to erroneous conclusions
- Used ANOVA to compare the two models, reinforcing the importance of including the species variable

[View detailed analysis - 01](./09_confounders/doc/09_confounders-01.pdf)

[View detailed analysis - 02](./09_confounders/doc/09_confounders-02.pdf)

### 10. Bitcoin Analysis
This comprehensive analysis explores Bitcoin's characteristics as a potential form of money, comparing it with traditional financial assets and examining its relationship with macroeconomic variables.

Key points:
- Analyzed Bitcoin's volatility compared to S&P 500, NASDAQ, Gold, WTI, and Brent oil
- Investigated correlations between Bitcoin and traditional financial assets
- Examined Bitcoin's price and trading volume over time, particularly noting changes since 2020
- Explored Bitcoin's properties as a unit of account, medium of exchange, and store of value
- Developed a model to predict Bitcoin's price based on gold, Brent oil, and NASDAQ prices

Findings:
- Bitcoin shows higher volatility compared to traditional assets, questioning its role as a stable store of value
- Strong correlations observed between Bitcoin and indices like S&P 500 and NASDAQ
- Bitcoin's price and trading volume increased significantly since 2020
- While gaining acceptance in online transactions, Bitcoin's adoption in traditional establishments remains limited
- Bitcoin's high volatility and lack of intrinsic value raise questions about its function as a reliable store of value

The analysis concludes that Bitcoin is still in the process of establishing its role in the global financial landscape, and its volatility should be carefully considered in investment decisions.

[View detailed analysis](./11_btc-analysis/ppt/11_btc-analysis.pdf)

## Project Conclusion

This infographics project demonstrates a wide range of data analysis techniques and visualization methods applied to diverse datasets. Throughout the analyses, we've employed various statistical and data science approaches:

1. Exploratory Data Analysis (EDA): Used in multiple studies to understand data distributions, identify patterns, and detect anomalies.

2. Data Visualization: Implemented across all analyses to present findings clearly and effectively, using tools like ggplot2 in R.

3. Statistical Analysis: Applied in several studies, including hypothesis testing, correlation analysis, and ANOVA.

4. Regression Models: Utilized in the Insurance Analysis, Linear Model study, and Bitcoin Analysis to predict outcomes and understand relationships between variables.

5. Machine Learning Techniques: Employed in some analyses, such as the Tree Project for geospatial analysis.

6. Time Series Analysis: Applied in the Flights Analysis and Bitcoin Analysis to understand trends and patterns over time.

7. Comparative Analysis: Used in multiple studies to contrast different groups or conditions, such as in the Titanic survival analysis and the Confounders study.

8. Feature Engineering: Demonstrated in several analyses, including the creation of new variables to better understand underlying patterns.

9. Data Merging and Cleaning: Evidenced in the Bitcoin Analysis, where multiple datasets were combined and processed.

10. Domain-Specific Analysis: Each study incorporated relevant domain knowledge, from understanding insurance risk factors to analyzing cryptocurrency market dynamics.

These infographics not only present data-driven insights but also showcase the power of combining statistical analysis with effective data visualization. The project demonstrates how diverse datasets can be approached with a variety of analytical techniques to extract meaningful insights and support decision-making across different fields.