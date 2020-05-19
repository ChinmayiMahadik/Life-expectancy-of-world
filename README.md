# Life-expectancy-of-world
Exploring machine learning model using R programming

Since the data set we used had lots of missing data and outliers. 
For missing value of population and GDP iof countries over the period of year from 2000 and 2015. I handled it by imputing from “The world bank data” website.
Link: https://data.worldbank.org/indicator/SP.POP.TOTL

Linear regression models are very sensitvie to outliers, but our analysis need to include these outliers since they were important data for analysis. 
Hence I tried two approaches:
1) Transformation of independent variables
2) Robust regression.

Studying about robust regression, gave a bigger prospective to handle outliers in data (which you will generally find in real time data) 

