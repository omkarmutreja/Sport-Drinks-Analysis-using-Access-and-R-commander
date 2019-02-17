# Sport-Drinks-Analysis-using-Access-and-R-commander
This repository explores Dominick's store-level database, which was obtained during a span of seven years - 1989 to 1997. We have chosen to perform analysis on the product category - Sport Drinks.

## Data Selection:
To perform this analysis, we selected a subset consisting of three brands of sports drinks. To obtain a better understanding of various brands of sports drinks trends, we decided to choose two premium brands and one low price brand as our subset for data analysis.

We used the file: ‘Sports drinks high movement UPC’ to help us finalize on the three brands.

Chosen premium brands: All Sport Lemon Lime and All Sport Cherry Slam

Chosen low priced brand: Powerade Tidal Burst

## Data Preparation:
•	To create our subset file, we established relationships between three data sets ‘Weekly movement data’, ‘UPC and product description’ and ‘Store demographics’. These data sets were provided as a part of Dominick’s database of Sports Drinks.
•	We then used Access to combine information from these three datasets for the selected brand of sports drinks, and created a new subset.
•	We decided to create a new factor named “SEASON” in our subset. To do this, we considered the factor ‘REM’, and provided the below condition in Access to form seasons:

SEASON: IIF( [REM]>10 And [REM]<24, “WINTER”, IIF( [REM]>23 And [REM]<37, “SPRING”, IIF( [REM]>36 And [REM]<50, “SUMMER”, “FALL” )))

## Data Analysis
We used different statistical tools like R and excel to answer a few data questions, which then provided us with a better insight on our subset.

Our research data questions, and the corresponding insights obtained on them are as follows:
1) How does the demand for a brand depend on price? What is the price elasticity of demand of a brand?
2) How does demand depend on whether the product is on sale (Feat =1)?
3) Which demographic factors affect demand?
4) How does prices vary across brands?
5) How does the proportion of times a brand is on sale vary across brand?
6) How does the demands for the three brands vary over time?
