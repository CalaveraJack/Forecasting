# Forecasting with Fable and Prophet
Multiple time series forecasting with fable and tidyverse in R

It is my small work on the forecasting.
It is nice to show something - this specific situation deals with 3 extensive datasets on sales of consumer goods.
Unfortunately, I can not publish those datasets, but they looked as follows:
1) Binary_Date_Characteristics.csv: csv with of all dates in the year in the first column in the %Y-%m-%d format, then all the rest of the columns list their binary characteristics: for example, whether this day is a school holiday, work day, public holiday, etc.
2) Characteristics_And_Classification_Of_Products.csv - this is the csv file where the first column indicates the code of the product (720 distinct products), the other columns indicate the Category, Colour, Type and Size
3) Quantities_Of_Products_Sold_Per_Date.csv - this is the csv where every Product has a count of sales for the specific date for 1.5 years

The task here is to inspect the dataset and predict the first week after the next period.

It was a challenging task with the data from one of the major consulting companies, but we managed it!
