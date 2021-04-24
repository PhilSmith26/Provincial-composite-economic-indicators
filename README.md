# Provincial-composite-economic-indicators
Calculate Canadian provincial composite economic indicators

In Canada the national statistics organization, Statistics Canada, releases a variety of monthly economic indicators at intervals throughout any given month. Many of these indicators are available for each province and territory, as well as for Canada as a whole. Examples of these indicators include employment, housing starts and retail sales. Unfortunately though, Statistics Canada does not release any summary indicator, such as gross domestic product, either monthly or quarterly, on a province-by-province basis. It does release a national GDP indicator monthly and it also releases annual GDP estimates by province. The objective of this project is to calculate a single summary economic indicator for each of the ten provinces and, possibly, the three territories. 

The method used to calculate these monthly provincial composite economic indicators (PCEIs) is to normalize all the available monthly economic indicators for a given province and then calculate the principal components. The first principal component, accounting for more of the total variance than any of the other components, is the PCEI for that province. The calculations are done in R.

For more documentation see www.philipsmith.ca/PCEI.
