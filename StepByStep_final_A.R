#*******#
#Cleaning all default objects
#*******#
rm(list = ls())

#*******#
#Loading packages
#*******#
if (!require(zoo)) {
  install.packages("zoo")
  library(zoo)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(prophet)) {
  install.packages("prophet")
  library(prophet)
}
if (!require(fable.prophet)) {
  install.packages("fable.prophet")
  library(fable.prophet)
}
if (!require(feasts)) {
  install.packages("feasts")
  library(feasts)
}
if (!require(tibble)) {
  install.packages("tibble")
  library(tibble)
}
if (!require(fable)) {
  install.packages("fable")
  library(fable)
}
if (!require(fabletools)) {
  install.packages("fabletools")
  library(fabletools)
}
if (!require(hts)) {
  install.packages("hts")
  library(hts)
}
if (!require(data.table)) {
  install.packages("data.table")
  library(data.table)
}
if (!require(zoo)) {
  install.packages("zoo")
  library(zoo)
}
if (!require(xts)) {
  install.packages("xts")
  library(xts)
}
if (!require(tsibble)) {
  install.packages("tsibble")
  library(tsibble)
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}
#******#
#Reading dataset (make sure that you have selected correct worplace and copied original datasets there)
#******#
data_dates_ori<-read.csv("Binary_Date_Characteristics.csv")
data_chars_ori<-read.csv("Characteristics_And_Classification_Of_Products.csv")
data_quantities_ori<-read.csv("Quantities_Of_Products_Sold_Per_Date.csv")

#******#
#*Merging tables with data.table method
#*#******#
set.seed(1234)
n = 1e6
data_table_1 = data.table(data_quantities_ori, key="date")
data_table_2 = data.table(data_dates_ori, key="date")
data_1 = merge(data_table_1, data_table_2)
data_table_1 = data.table(data_chars_ori, key="Product")
data_table_2 = data.table(data_1, key="Product")
data_new = merge(data_chars_ori, data_1)
rm(data_1,data_table_1,data_table_2)

#*******#
#Building and plotting tsibble (Visualisation per category)
#*******#
datestart=as.Date("2018-01-01", '%Y-%m-%d')
dateend=as.Date("2019-06-30", '%Y-%m-%d')
step1=data.frame(cbind(data_new$Product, data_new$Type,data_new$Category, data_new$qty, data_new$date))
colnames(step1) <- c("Product","Type","Category", "qty","date")
step1$date=as.Date(step1$date)
step1$Product=as.character(step1$Product)
step1$Type=as.character(step1$Type)
step1$Category=as.character(step1$Category)
step1$qty=as.integer(step1$qty)
step1_tsibble <- as_tsibble(step1, index = date, key = Product)
step1_tsibble

aggregated_tsibble <- step1_tsibble |>
  aggregate_key(Category / Type / Product, qty = sum(qty))
aggregated_tsibble
#____________________________
#Plotting it
#____________________________
#Plotting the Categories
aggregated_tsibble |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(qty) +
  labs(y = "qty",
       title = "TimeSeries for all the Categories") +
  facet_wrap(vars(Category), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")
#____________________________


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~STEP 1~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~Data research and preparation~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#*******#
#Cleaning the tsibble
#*******#
#Firstly, we will need to clean the data. Two categories (Category1 and Category2) 
#were not sold during all the period are empty, and we suppose that some Product's were just not sold.
#those Product's which were not sold are easily forecastable:
#we will just say that the forecast for the needed period is 0
#Finding out the number of the Products that were never sold
dim(step1_tsibble %>%
      group_by(Product) %>%
      plotly::filter(all(sum(qty) == 0)) %>%
      distinct(Product))
#We have 224 Products that were never sold
#Let's remove them from the main tsibble and create a zerofc object keeping those Products outside
not_sold_Products <- step1_tsibble %>%
  group_by(Product) %>%
  plotly::filter(all(sum(qty) == 0)) %>%
  distinct(Product)
# Remove Products that were not sold ever from step1_tsibble
step1_tsibble <- step1_tsibble %>%
  plotly::filter(!(Product %in% not_sold_Products$Product))
# Create a table of empty forecasts for Products not sold ever outside of Category1 and Category2
zerofc <- not_sold_Products %>%
  distinct(Product) %>%
  mutate(qty = 0)
#Now let's aggregate the tsibble again
step1_tsibble_aggregated <- step1_tsibble |>
  aggregate_key(Category / Type / Product, qty = sum(qty))
step1_tsibble_aggregated
#____________________________
#Plotting it
#____________________________
#Plotting the Categories: New
step1_tsibble_aggregated |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(qty) +
  labs(y = "qty",
       title = "QTY aggregated per Category - non-zero Product - Overall") +
  facet_wrap(vars(Category), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")
#___________________________
#Success! Now all the categories include Product's that were sold during the period, thus, forecastable


#*******#
#Creating top-10 objects
#*******#
#For more proper understanding of the data, the next step is the research on top-10 Products's in each category
#Let's create a tsibble with only top-1o Products
top10_products <- step1_tsibble %>%
  group_by(Category, Type, Product) %>%
  summarize(total_qty = sum(qty)) %>%
  group_by(Category) %>%
  top_n(10, wt = total_qty) %>%
  ungroup()
# Filter the original tsibble to include only the top-10 Products
top10_tsibble <- step1_tsibble %>%
  filter(Product %in% top10_products$Product)
# Print the new tsibble
print(top10_tsibble)
#Aggregate the top-10 tsibble
top10_tsibble_aggregated <- top10_tsibble |>
  aggregate_key(Category / Type / Product, qty = sum(qty))
top10_tsibble_aggregated
#____________________________
#Plotting it
#____________________________
#Plotting the top-10 tsibble
top10_tsibble_aggregated |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(qty) +
  labs(y = "qty",
       title = "QTY aggregated per Category - non-zero Product - top-10") +
  facet_wrap(vars(Category), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")
#___________________________
#Now we have 4 tsibbles to process: "top10_tsibble" and "step1_tsibble_filtered"
#+ their aggregated versions



#*******#
#Decomposition on the daily level
#*******#
#Let's decompose those category-wide time-series using STL method
#____________________________
#Plotting it
#____________________________
step1_tsibble_aggregated %>%
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) %>%
  group_by(Category) %>%
  model(STL(qty)) %>%
  components() %>%
  autoplot()+
  labs(title = "STL Decomposition overall")

top10_tsibble_aggregated %>%
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) %>%
  group_by(Category) %>%
  model(STL(qty)) %>%
  components() %>%
  autoplot()+
  labs(title = "STL decomposition for top-10 Products in each category")
#________________________
#We can see that weekly seasonality, automatically selected by STL, is not scalable, trend is too complex
#Furthermore, even though top-10 Products are few, but they are quite obviously a driving factor of change
#You can also access the decomposition graphs per Category in additional.materials file
#The data has following features as well:
step1_tsibble |>
  features(qty, feat_stl)|>
  print(n = Inf)
top10_tsibble |>
  features(qty, feat_stl)|>
  print(n = Inf)
#____________________________
#Plotting it
#____________________________
#And use those features use these features in plots to identify what type of series are heavily trended and what are most seasonal.
#We also want to see whether some Categories or Types of Products are more prone to be seasonal or trendy
#Without type: but with different colors for categories
features_data <- step1_tsibble %>%
features(qty, feat_stl)
result_data <- step1_tsibble %>%
  left_join(features_data, by = "Product")
result_data |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_week, col = Category)) +
  geom_point()+ labs(title = "All Products: seasonal strength vs trend strength")
#With both categories and types
features_data <- step1_tsibble %>%
  features(qty, feat_stl)
result_data <- step1_tsibble %>%
  left_join(features_data, by = "Product")
result_data |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_week, col = Type)) +
  geom_point()+
  facet_wrap(vars(Category)) + labs(title = "All Products: seasonal strength vs trend strength")

rm(result_data,features_data)
#______________________________
#We can not pinpoint the specific Categories or Types being seasonal or trendy, but we can be sure that our data is sligtly more trendy than seasonal
#However, it is also important that the seasonality is present and outliers are there
#It means that:
#a)Final model will need individual approach per Product (fitting)
#b)Final model must include seasonality as well as trend



#*******#
#Autocorrelation data
#*******#
step1_tsibble |>
  features(qty, feat_acf)|>
  print(n = Inf)
top10_tsibble |>
  features(qty, feat_acf)|>
  print(n = Inf)
#We once again see that there are Products with some lag seasonality,
#but majority has a week autocorrelation measures,
#and they are distributed haphazardly
#the final approach must be tailored to each Product individually




#*******#
#Weekly data
#*******#
#Nevertheless, even thoug the amount of data filtered from zero,
#we still receive very slow results: building actual forecasts is almost impossible
#thus, we will start using weekly data
#We will use daily data once again for specific category - SPOILERS
#Weekly data is notoriously hard to use in classical models in packages
#ETS model refuses even to take seasonality with weekly data: we will keep that in mind for later
#For now, let's start 
# Round the dates to the nearest week
step1_tsibble_week<-step1_tsibble
step1_tsibble_week$date <- as.Date(cut(step1_tsibble_week$date, breaks = "week"))
step1_tsibble_week
# Aggregate the data per week
step1_tsibble_week <- aggregate(qty ~ date + Product + Type + Category, data = step1_tsibble_week, sum)
step1_tsibble_week
#Create tsibble
step1_tsibble_week <- step1_tsibble_week |>
  mutate(date = yearweek(date)) |>
  as_tsibble(index = date, key = c(Product, Category, Type))
step1_tsibble_week
#Create aggregated tsibble
step1_tsibble_week_aggregated <- step1_tsibble_week |>
  aggregate_key(Category / Type / Product, qty = sum(qty))
step1_tsibble_week_aggregated

#Create top-10 Product tsibble
# Filter the original tsibble to include only the top-10 Products
top10_tsibble_week <- step1_tsibble_week %>%
  filter(Product %in% top10_products$Product)
# Print the weekly Top-10 tsibble
print(top10_tsibble_week)
#Aggregate the new Top-10 tsibble
top10_tsibble_week_aggregated <- top10_tsibble_week |>
  aggregate_key(Category /Type/ Product, qty = sum(qty))
top10_tsibble_week_aggregated

#____________________________
#Plotting it
#____________________________
step1_tsibble_week_aggregated |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(qty) +
  labs(y = "qty",
       title = "TimeSeries for all the Categories") +
  facet_wrap(vars(Category), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

step1_tsibble_week_aggregated |>
  plotly::filter(is_aggregated(Product)) |>
  autoplot(qty) +
  labs(y = "qty",
       title = "TimeSeries for all the Categories and Types") +
  facet_wrap(vars(Category), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

#Plotting the Categories: top 10 Product of a category
top10_tsibble_week_aggregated |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(qty) +
  labs(y = "qty",
       title = "TimeSeries for all the Categories - top 10 Product in each") +
  facet_wrap(vars(Category), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

top10_tsibble_week_aggregated |>
  plotly::filter(is_aggregated(Product)) |>
  autoplot(qty) +
  labs(y = "qty",
       title = "TimeSeries for all the Categories and Types - top 10 Products in each Category") +
  facet_wrap(vars(Category), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")
#_________________________
#We see much more clearly how the graph looks like and made our dataset lightweight
#Hoever, we also see what we tried to ignore all of that time:
#Some categories and types started their sales not since the beginning of the period
#We need to solve this issue and build our baseline

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~STEP 2~~~~~~~~~~~~~~~~~~~~~~~~
#~~~Partitioning and de-grouping the data~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#*******#
#De-grouping the tsibble
#*******#
#We understand that if we want to even theoretically model the forecast, 
#both in-sample and out of sample accuracy is important
#While in-sample accuracy is easy to calculate,
#out-of sample accuracy requires test and training set to be partitioned
#And our categories did not "enter the market" at the same time:
#First sale for Group 1: CategoryA, CategoryB, CategoryC, CategoryD and CategoryE and CategoryF:
#around 2018-01-01
#First sale for Group 2: CategoryG CategoryH, CategoryI, CategoryJ and CategoryK
#is closer to the Christmas and New Year
#And Group 3: CategoryL started sales at the very end of the period
#***********#
#We decided to forecast those groups independently, 
#assuming the first day of sale within group as a start of the period
#**********#
#Let's create new tsibbles for those groups.
#SPOILERS: Group 1 and Group 2 will have weekly period. Group 3 will have daily period - for the lack of data

#*******#
#*Subseting groups and adjusting the periods
#*******#
step1_tsibble
step2_Group1<-subset(step1_tsibble,Category=="CategoryA"|Category=="CategoryB"|Category=="CategoryC"|Category=="CategoryD"|Category=="CategoryE"|Category=="CategoryF")
step2_Group2<-subset(step1_tsibble,Category=="CategoryG"|Category=="CategoryH"|Category=="CategoryI"|Category=="CategoryJ"|Category=="CategoryK")
step2_Group3<-subset(step1_tsibble,Category=="CategoryL")  

#Now let's find out the dates of the first sale to group 2 and group 3
first_non_zero_date_Group2 <- step2_Group2 %>%
  filter(qty != 0) %>%
  slice(1) %>%
  pull(date)
step2_Group2<-subset(step2_Group2,date>first_non_zero_date_Group2) #Adjusting the period
first_non_zero_date_Group3 <- step2_Group3 %>%
  filter(qty != 0) %>%
  slice(1) %>%
  pull(date)
step2_Group3<-subset(step2_Group3,date>first_non_zero_date_Group3) #Adjusting the period

#*******#
#*Making the groups data weekly
#*******#
#GROUP 1:
step2_Group1_week<-step2_Group1
step2_Group1_week$date <- as.Date(cut(step2_Group1_week$date, breaks = "week"))
step2_Group1_week
# Aggregate the data per week
step2_Group1_week <- aggregate(qty ~ date + Product + Type + Category, data = step2_Group1_week, sum)
step2_Group1_week
#Create tsibble
step2_Group1_week <- step2_Group1_week |>
  mutate(date = yearweek(date)) |>
  as_tsibble(index = date, key = c(Product, Category, Type))
#View tsibble
step2_Group1_week

#GROUP 2
step2_Group2_week<-step2_Group2
step2_Group2_week$date <- as.Date(cut(step2_Group2_week$date, breaks = "week"))
step2_Group2_week
# Aggregate the data per week
step2_Group2_week <- aggregate(qty ~ date + Product + Type + Category, data = step2_Group2_week, sum)
step2_Group2_week
#Create tsibble
step2_Group2_week <- step2_Group2_week |>
  mutate(date = yearweek(date)) |>
  as_tsibble(index = date, key = c(Product, Category, Type))
#View tsibble
step2_Group2_week

#GROUP 3
step2_Group3_week<-step2_Group3
step2_Group3_week$date <- as.Date(cut(step2_Group3_week$date, breaks = "week"))
step2_Group3_week
# Aggregate the data per week
step2_Group3_week <- aggregate(qty ~ date + Product + Type + Category, data = step2_Group3_week, sum)
step2_Group3_week
#Create tsibble
step2_Group3_week <- step2_Group3_week |>
  mutate(date = yearweek(date)) |>
  as_tsibble(index = date, key = c(Product, Category, Type))
#View tsibble
step2_Group3_week

#*******#
#*Aggregating the groups
#*******#
#Create aggregated tsibbles
#GROUP 1
step2_Group1_week_aggr <- step2_Group1_week |>
  aggregate_key(Category / Type / Product, qty = sum(qty))
step2_Group1_week_aggr
#______Plotting______
step2_Group1_week_aggr %>%
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) %>%
  group_by(Category) %>%
  model(STL(qty)) %>%
  components() %>%
  autoplot()+
  labs(title = "Group2: STL Decomposition overall")
#___________________
#GROUP 2
step2_Group2_week_aggr <- step2_Group2_week |>
  aggregate_key(Category / Type / Product, qty = sum(qty))
step2_Group2_week_aggr
#______Plotting______
step2_Group2_week_aggr %>%
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) %>%
  group_by(Category) %>%
  model(STL(qty)) %>%
  components() %>%
  autoplot()+
  labs(title = "Group2: STL Decomposition overall")
#___________________
#GROUP 3
step2_Group3_week_aggr <- step2_Group3_week |>
  aggregate_key(Category / Type / Product, qty = sum(qty))
step2_Group2_week_aggr
#______Plotting______
step2_Group3_week_aggr %>%
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) %>%
  group_by(Category) %>%
  model(STL(qty)) %>%
  components() %>%
  autoplot()+
  labs(title = "Group2: STL Decomposition overall") #Not looking OK
#___________________
step2_Group3_aggr <- step2_Group3 |>
  aggregate_key(Category / Type / Product, qty = sum(qty))
step2_Group3_aggr
#______Plotting______
step2_Group3_aggr %>%
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) %>%
  group_by(Category) %>%
  model(STL(qty)) %>%
  components() %>%
  autoplot()+
  labs(title = "Group2: STL Decomposition overall") #Way better, will be using daily data
#___________________
#Now we see why using daily data is beneficial for group 3
#Naturally, we see much more predictable seasonality and graphs on the group levels
#Now it is time to individually build a baseline for them


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~STEP 2~~~~~~~~~~~~~~~~~~~~~~~~
#~~~Building the baseline with ETS~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#For building the baseline we will be using ets
#We are skipping in-sample accuracy metrics: we are much more interested in the
#out-of sample accuracy

#We will partition the data 80 to 20 temporarily and method "Fit" to 
#fit the ets models to the individual Products
#Notice that the ets method of forecast package is not accepting seasonality for the 
#weekly data. We are ready to sacrifice seasonality: after all - it is a baseline
#Group 1:
fit <- step2_Group1_week_aggr |>
  filter(date(date) <= datestart+(dateend-datestart)*0.8) |>
  model(base = ETS(qty))
fc <- fit |> forecast(h = sprintf("%d days", as.numeric((dateend-datestart)*0.2)))
fc |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(
    step2_Group1_week_aggr |> plotly::filter(year(date) >= 2018),
    level = NULL
  ) +
  labs(y = "qty") +
  facet_wrap(vars(Category), scales = "free_y")
#Accuracy of ETS
#Bottom
accuracy_ets_group1<-fc |> accuracy(step2_Group1_week_aggr)
accuracy_ets_group1

#Group 2:
#_______________________________
fit <- step2_Group2_week_aggr |>
  filter(date(date) <= first_non_zero_date_Group2+floor((dateend-first_non_zero_date_Group2)*0.8)) |>
  model(base = ETS(qty))
fc <- fit |> forecast(h = sprintf("%d days", floor(as.numeric((dateend-first_non_zero_date_Group2)*0.2))))
fc |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(
    step2_Group2_week_aggr |> plotly::filter(year(date) >= 2018),
    level = NULL
  ) +
  labs(y = "qty") +
  facet_wrap(vars(Category), scales = "free_y")
#Accuracy of ETS
#Bottom
accuracy_ets_group2<-fc |> accuracy(step2_Group2_week_aggr)
accuracy_ets_group2

#Group 3:
#_______________________________
fit <- step2_Group3_aggr |>
  filter(date(date) <= first_non_zero_date_Group3+floor((dateend-first_non_zero_date_Group3)*0.8)) |>
  model(base = ETS(qty))
fc <- fit |> forecast(h = sprintf("%d days", floor(as.numeric((dateend-first_non_zero_date_Group3)*0.2))))
fc |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(
    step2_Group3_aggr |> plotly::filter(year(date) >= 2018),
    level = NULL
  ) +
  labs(y = "qty") +
  facet_wrap(vars(Category), scales = "free_y")
#Accuracy of ETS
#Bottom
accuracy_ets_group3<-fc |> accuracy(step2_Group3_aggr)
accuracy_ets_group3

#Binding the accuracy metrics to evaluate strong baseline as average of individual RMSE per forecasts
baseline_accuracy<-bind_rows(accuracy_ets_group1,accuracy_ets_group2,accuracy_ets_group3)
baseline_accuracy|>
  print(n = Inf)
mean(baseline_accuracy$RMSE)
#$$$$$$$$$$$
#$$$$$$$$$$$
#Mean RMSE is 14.90001
#Baseline is built:
#Out-of-Sample RMSE is 14.90001
#$$$$$$$$$$$
#$$$$$$$$$$$


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~STEP 3~~~~~~~~~~~~~~~~~~~~~~~~
#~~~Building a final forecast~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Now we know from the experience: we need to:
#1) Notice not only a trend, but seasonality even on weekly level
#2) Be able to generalize the model and do it within one object of mable/fable
#3) Be able to detect the reasons for the spikes = add holidays
#For that we will be using prophet

#*******#
#*Creating holidays object
#*******#
#Firstly, we need to take the look on the data_dates_ori
#We will be using only "Holidays" column - 
#the database doesn't contain promotions or Black Friday
#and celebrations like Oktoberfest
#So we will import all the present holidays column as single-intensity spike
#And then we will additionally introduce user-defined holidays,
holidays_united=data_dates_ori
holidays_hol=data.frame(cbind(holidays_united$date,holidays_united$holiday))
colnames(holidays_hol) <- c("ds","holiday")
holidays_hol$ds=as.Date(holidays_hol$ds)
holidays_hol$holiday=as.character(holidays_hol$holiday)
holidays_hol_tsibble=as_tsibble(holidays_hol,index = ds)
holidays_hol_tsibble$lower_window <- -1
holidays_hol_tsibble$upper_window <- 1
# Round the dates to the nearest week
holidays_hol_tsibble_week<-holidays_hol_tsibble
holidays_hol_tsibble_week$ds <- as.Date(cut(holidays_hol_tsibble_week$ds, breaks = "week"))
holidays_hol_tsibble_week
# Aggregate the data per week
holidays_hol_tsibble_week <- aggregate(holiday ~ ds, data = holidays_hol_tsibble_week, FUN = max)
#Add the windows
holidays_hol_tsibble_week$lower_window <- -1
holidays_hol_tsibble_week$upper_window <- 1
holidays_hol_tsibble_week
#Create tsibble
holidays_hol_tsibble_week <- holidays_hol_tsibble_week |>
  mutate(ds = yearweek(ds)) |>
  as_tsibble(index = ds)
#Create tsibble
holidays_hol_tsibble_week
#Mapping very important dates - by week, manually. But it can be done with the distionary
#For this task it is faster to input them manually
hol_hol_bf<-holidays_hol_tsibble_week
hol_hol_bf[c(44:46),]$holiday = "3 Weeks Before Black Friday"
hol_hol_bf[c(47,99),]$holiday = "Black Friday"
hol_hol_bf[c(48,100),]$holiday = "Week After Black Friday"
hol_hol_bf[c(52,105),]$holiday = "New Year"
hol_hol_bf[c(1,53),]$holiday = "Drei Heilige Könige"
hol_hol_bf[c(24,76),]$holiday = "Corpus Kristi"
hol_hol_bf[78,]$holiday = "!!!1-time Promotion!!!" #Here we add the spike - but we are not sure of its origin
hol_hol_bf[c(7,59),]$holiday = "St' Valentine's Day"
hol_hol_bf[c(14,66),]$holiday = "Easter"
hol_hol_bf[c(15,67),]$holiday = "Ostermontag"
hol_hol_bf[c(10,62),]$holiday = "Women's Day"
hol_hol_bf[c(9,61),]$holiday = "week before Women's Day"
hol_hol_bf[c(39,91),]$holiday = "Oktoberfest week"
hol_hol_bf[c(40,92),]$holiday = "Day of Unity"
hol_hol_bf[c(17,69),]$holiday = "Day of Labor"

holidays_hol_tsibble[486,]$holiday = "Day of Labor"
holidays_hol_tsibble[526,]$holiday = "Christ's body"
holidays_hol_tsibble[546,]$holiday = "!!!1-time Promotion!!!"
holidays_hol_tsibble[545,]$holiday = "day before 1-time Promotion!!!"
holidays_hol_tsibble[547,]$holiday = "day after 1-time Promotion!!!"
#*******#
#*Decomposing groups with Prophet
#*******#
#GROUP 1
step2_Group1_week_aggr %>%
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) %>%
  group_by(Category) %>% model(
    mdl = prophet(qty ~ growth("linear")+season("year",type="multiplicative") + holiday(holidays = hol_hol_bf,prior_scale=10)),
  ) %>%
  components() %>%
  autoplot()+
  labs(title = "Group1: Prophet Decomposition")
#We use the standard Fourrier Order
#GROUP 2
step2_Group2_week_aggr %>%
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) %>%
  group_by(Category) %>% model(
    mdl = prophet(qty ~ growth("linear")+season("year",type="additive") + holiday(holidays = hol_hol_bf,prior_scale=26)),
  ) %>%
  components() %>%
  autoplot()+
  labs(title = "Group2: Prophet Decomposition")
#We use the standard Fourrier Order
#GROUP 3
step2_Group3_aggr %>%
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) %>%
  group_by(Category) %>% model(
    mdl = prophet(qty ~ growth("linear")+season(90,type="additive",order=2) + holiday(holidays = holidays_hol_tsibble,prior_scale=10)),
  ) %>%
  components() %>%
  autoplot()+
  labs(title = "Group3: Prophet Decomposition")

#*******#
#*Forecasting and accuracy with Prophet
#*******#
#Group 1:
fit_gr1 <- step2_Group1_week_aggr |>
  filter(date(date) <= datestart+(dateend-datestart)*0.8) |>
  model(prophet = prophet(qty ~ growth("linear") + season("year", type = "additive")+holiday(holidays = hol_hol_bf,prior_scale=10)),
  )
fc_gr1 <- fit_gr1 |> forecast(h = sprintf("%d days", as.numeric((dateend-datestart)*0.2)))
fc_gr1 |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(
    step2_Group1_week_aggr |> plotly::filter(year(date) >= 2018),
    level = NULL
  ) +
  labs(y = "qty") +
  facet_wrap(vars(Category), scales = "free_y")
#Accuracy of Prophet
#Bottom
accuracy_prophet_group1<-fc_gr1 |> accuracy(step2_Group1_week_aggr)
accuracy_prophet_group1$RMSE
mean(accuracy_prophet_group1$RMSE)
#Mean(accuracy_prophet_group1$RMSE) [1] 16.22325

#Group 2:
#Will have n.changepoints error: we know why it happens, it does not result in overfitting
fit_gr2 <- step2_Group2_week_aggr |>
  filter(date(date) <= first_non_zero_date_Group2+floor((dateend-first_non_zero_date_Group2)*0.8)) |>
  model(prophet = prophet(qty ~ growth("linear") +holiday(holidays = hol_hol_bf,prior_scale=10)),
  )
fc_gr2 <- fit_gr2 |> forecast(h = sprintf("%d days", floor(as.numeric((dateend-first_non_zero_date_Group2)*0.2))))
fc_gr2 |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(
    step2_Group2_week_aggr |> plotly::filter(year(date) >= 2018),
    level = NULL
  ) +
  labs(y = "qty") +
  facet_wrap(vars(Category), scales = "free_y")
#We remove seasonality - in order to accomodate the small amounts of data, we leave only holidays and trend

#Accuracy of Prophet
#Bottom
accuracy_prophet_group2<-fc_gr2 |> accuracy(step2_Group2_week_aggr)
accuracy_prophet_group2$RMSE
mean(accuracy_prophet_group2$RMSE)
#Mean RMSE: [1] 5.813433

#Group 3:
fit_gr3 <- step2_Group3_aggr |>
  filter(date(date) <= first_non_zero_date_Group3+floor((dateend-first_non_zero_date_Group3)*0.8)) |>
  model(prophet = prophet(qty ~ growth("linear") + season(90,type="additive",order=2)+holiday(holidays = holidays_hol_tsibble,prior_scale=10)),
  )
fc_gr3 <- fit_gr3 |> forecast(h = sprintf("%d days", floor(as.numeric((dateend-first_non_zero_date_Group3)*0.2))))
fc_gr3 |>
  plotly::filter(is_aggregated(Product),is_aggregated(Type)) |>
  autoplot(
    step2_Group3_aggr |> plotly::filter(year(date) >= 2018),
    level = NULL
  ) +
  labs(y = "qty") +
  facet_wrap(vars(Category), scales = "free_y")
#Accuracy of Prophet
#Bottom
accuracy_prophet_group3<-fc_gr3 |> accuracy(step2_Group3_aggr)
accuracy_prophet_group3$RMSE
mean(accuracy_prophet_group3$RMSE)

prophet_accuracy<-bind_rows(accuracy_prophet_group1,accuracy_prophet_group2,accuracy_prophet_group3)
prophet_accuracy|>
  print(n = Inf)
mean(prophet_accuracy$RMSE)

#Now we have overall accuracy
#RMSE = 14.06443
#We beat the baseline: it is recommended that we will tune parameters 
#(such as seasonality further)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~STEP 4~~~~~~~~~~~~~~~~~~~~~~~~
#~~~Outputting the results~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(fit_gr1,fit_gr2,fit_gr3)
ffit_gr1 <- step2_Group1_week_aggr |>
  filter(date(date) <= dateend) |>
  model(prophet = prophet(qty ~ growth("linear") + season("year", type = "additive")+holiday(holidays = hol_hol_bf,prior_scale=10)),
  )
ffit_gr2 <- step2_Group2_week_aggr |>
  filter(date(date) <= first_non_zero_date_Group2+floor((dateend-first_non_zero_date_Group2)*0.8)) |>
  model(prophet = prophet(qty ~ growth("linear") +holiday(holidays = hol_hol_bf,prior_scale=10)),
  )
#Group 3:
ffit_gr3 <- step2_Group3_aggr |>
  filter(date(date) <= first_non_zero_date_Group3+floor((dateend-first_non_zero_date_Group3)*0.8)) |>
  model(prophet = prophet(qty ~ growth("linear") + season(90,type="additive",order=2)+holiday(holidays = holidays_hol_tsibble,prior_scale=10)),
  )

ffc_gr1 <- ffit_gr1 |> forecast(h ="1 week")

ffc_gr2 <- ffit_gr2 |> forecast(h = "1 week")

ffc_gr3 <- ffit_gr3 |> forecast(h = "1 week")

#Now we change negative values to 0's: this way the confusion of the results will be avoided
#In the subsequent research it might be rational to predict Counts

# Create forecast_gr1, forecast_gr2, forecast_gr3
forecast_gr1 <- data.frame(Product = as.character(ffc_gr1$Product), ForecastedSales = floor(ffc_gr1$.mean))
forecast_gr2 <- data.frame(Product = as.character(ffc_gr2$Product), ForecastedSales = floor(ffc_gr2$.mean))
forecast_gr3_interm<-ffc_gr3
forecast_gr3_interm<- aggregate(.mean ~ Product, data = ffc_gr3_interm, sum)
forecast_gr3 <- data.frame(Product = as.character(ffc_gr3_interm$Product), ForecastedSales = floor(ffc_gr3_interm$.mean))
rm(forecast_gr3_interm)
# Combine forecasts into one data frame
forecast <- rbind(forecast_gr1, forecast_gr2, forecast_gr3)
colnames(forecast) <- c("Product", "ForecastedSales")

# Filter out rows with Product equal to "<aggregated>"
forecast_filtered <- forecast %>% filter(SKU != "<aggregated>")

# Replace negative ForecastedSales with 0
forecast_filtered$ForecastedSales[forecast_filtered$ForecastedSales < 0] <- 0

print(forecast_filtered)
colnames(forecast_filtered) <- c("Product", "ForecastedSales")
colnames(zerofc) <- c("Product", "ForecastedSales")
# Combine forecast_filtered and zerofc
forecast_final <- rbind(forecast_filtered, zerofc)

write.csv(forecast_final,"forecast.csv",row.names=TRUE)