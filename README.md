# IS7012
---
title: "Data Wrangling Final Project"
author: "Saurav Nandi | Yash Karve | Simran Sudhir Chavan | Dhruval Chheda"
output: html_document
date: "8th October 2022"

---

### __Introduction__

The goal of our project is to use Data analytics to analyze and improve the sales and revenue generated from the poorly performing products sold at the Regork grocery chain. For this, we explored various datasets of the Completejourney package and attempt to find solutions for the above problems.



Our first business problem focuses on the analysis of the customer's brand preferences for various product categories. Here, we have chosen the top 15 product categories based on their total sales, as they cover a relatively high market share compared to other product categories. We further deep-dived and focused on the sales of the mentioned product categories and how they varied based on the coupon distribution from the respective product categories Private or National brands.



In our next Business problem, we focused on analyzing the "MEAT" department and how the sales trend over the year. Additionally, we focused more on the sales trend of five types of meat mainly: Meat, Pork, Chicken, Turkey and Smoked meats due to their striking trends during the holiday seasons.



We have used the transactions, products and coupons datasets to observe and come up with reasonable solutions for the above business problems which would be useful in helping Regork grow and improve its sales strategies.



**Our analysis can be used to answer questions like:**



1) What types of products are mostly purchased from private brands amongst the top 15 product categories?


2) What type of brand generally prefers to give out coupons to customers?


3) Which are the top 3 purchased meat products throughout the year?


4) In what month does the price of turkey rise and correspondingly the prices of other top-purchased meats drop? What could be the potential reason for it?

<hr>


### __Packages Required__

Following packages were used:


* __Knitr:__ Used to display an aligned table on the screen
* __Tidyverse:__ Used to tidy data
* __Lubridate:__ Used to manipulate date-time 
* __Dplyr:__ Used for data manipulation
* __Ggplot2:__ Used to plot charts
* __CompleteJourney:__ Used to analyze data

```{r, message=FALSE}

library(completejourney)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(knitr)

```

### __Data Preparation__

This sections contains all the procedures followed in getting the data analysis ready. Each step has been explained and the codes have been given.

#### {.tabset}

##### Data Import

We are using the Complete Journey package for this analysis. The dataset **transactions** represents grocery store shopping transactions over one year from a group of 2,469 households. The dataset **products** contains product related information like department, product category, product type and brand. Information about coupons given for products is in **coupons** dataset.
 
Data Import Code:
```{r}
library(completejourney)
transactions <- get_transactions()
transactions
products
coupons

```

__Details about the tables used:__
```{r}
colnames(transactions)
colnames(products)
colnames(coupons)

```

##### Data Preview

```{r}
glimpse(transactions)
glimpse(products)
glimpse(coupons)

```

##### Data Description
```{r, message=FALSE,  echo=FALSE}
data1 <- tribble(
  ~"Variable", ~"Description",
  "household_id", "Estimated age range",
  "store_id", "Uniquely identifies each store",
  "basket_id", "Uniquely identifies each purchase occasion",
  "product_id", "	Uniquely identifies each product",
  "quantity",  "Number of the product purchased during the trip",
  "retail_disc",  "Discount applied due to the retailer’s loyalty card program",
  "coupon_disc",  "Discount applied due to a manufacturer coupon",
  "coupon_match_disc",  "Discount applied due to retailer’s match of manufacturer coupon",
  "week", "Week of the transaction; Ranges 1-53",
  "transaction_timestamp",  "Date and time of day when the transaction occurred"
)
knitr::kable((data1), booktabs = TRUE,
caption = 'transactions')



data2 <- tribble(
  ~"Variable",                                ~"Description",
  "product_id",  "	Uniquely identifies each product",
  "manufacturer_id",  "Uniquely identifies each manufacturer",
  "department",  "Groups similar products together",
  "brand",  "Indicates private or national label brand",
  "product_category",  "Groups similar products together at lower level",
  "product_type", "Groups similar products together at lowest level",
  "package_size",  "Indicates package size (not available for all products)"
)
knitr::kable((data2), booktabs = TRUE,
caption = 'products')

data3 <- tribble(
  ~"Variable",                                ~"Description",
  "coupon_upc",  "Uniquely identifies each coupon (unique to household and campaign)",
  "product_id",  "Uniquely identifies each product",
  "campaign_id",  "Uniquely identifies each campaign",
)
knitr::kable((data3), booktabs = TRUE,
caption = 'coupons')


```

### __Exploratory Data Analysis__

#### {.tabset}

##### Analysis 1: Brand preference for most market share product categories 
Firstly we join transactions table with the products table in order to find top 15 product categories on the basis of their total sales. We have grouped by product_category and have found their total_sales by summing up their individual values. Then we have arranged the results in the descending order and thus, have founded top 15 product_categories with respect to their total sales

```{r, message =FALSE}
top15_bysale <- transactions %>%
  inner_join(products) %>%  
  group_by(product_category) %>%
  summarize(total_sales = sum(sales_value, na.rm =TRUE))%>%
  arrange(desc(total_sales))%>%
  top_n(15)

top15_bysale

```


We Converted the above table into a vector using the following code below so that we can use that vector for further data analysis

```{r, message=FALSE}
top15_bysale_1row <- top15_bysale[, -2]

top15_bysale_vector <- pull(top15_bysale_1row)
```

Then we Plotted the top 15 product_categories on the basis of their sales_values by Brand (Private or National)

```{r, message=FALSE, fig.width=22, fig.height=14}

txn_prod <- transactions %>% inner_join(products, by = "product_id")

txn_prod %>%
  filter(product_category %in% top15_bysale_vector) %>%
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(mapping  = aes(x = product_category, y = total_sales_value, fill = brand)
  ) +
  geom_col(position = "fill")+
  labs(title = "Sales as per brand",
       subtitle = "Plot displays the top 15 product categories total sales value by Brand",
       x = "Top 15 product categories",
       y = "Total sales") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "plain"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.background = element_rect(color = "black"),
    plot.background = element_rect(fill = "light blue"))

```

From analyzing the graphical representation above, we have understood that no private labels sell chicken and cigarettes and among the top 15 sold product categories and customers prefer buying dairy products like Fluid Mild Products and Cheese from Private Brands. Now lets analyze the coupons given out for these top 15 product categories by plotting the graph as shown below:-   

```{r, message=FALSE, fig.width=22, fig.height=14}

txn_prod %>%
  inner_join(coupons) %>%
  filter(product_category %in% top15_bysale_vector) %>%
  group_by(product_category, brand, coupon_upc) %>%
  summarize(total_coupons = n()) %>%
  ggplot(aes(y = product_category, x= total_coupons)) +
  geom_bar(aes(fill = brand),stat= "identity", position = "dodge") +
  scale_fill_brewer(palette = "Accent") +
  labs(y = "Top 15 product categories", x = "Coupons distributed") +
  ggtitle("Coupons distribution according to brands", subtitle = "Coupons distributed for top 15 product categories according to brand")

```

From the above graph, we have analyzed and understood that Coupons are sold mainly by Private Brands and most private coupons are given out for dairy products like Fluid Milk Products and Cheese which has significantly increased the Private label sales of those product categories. Similarly, most National coupons are given out for Frozen Meat and Meat Dinners which has significantly increased the National label’s sales of those product categories.


##### Analysis 2: Meat purchasing trend

The second analysis describes the meat department purchasing trend with respect to different product categories of meat for **Regork's** market. Given below is the graph that represents sales of different meat products under the meat department in the year 2017



```{r, message=FALSE, fig.width=18, fig.height=10}
txn_prod %>%
  mutate( month1  = month(transaction_timestamp)) %>%
  filter(department == "MEAT") %>%
  group_by(product_category, month1) %>%
  summarize(total_sales = sum(sales_value, na.rm =TRUE)) %>%
  ggplot(aes(x = month1, y = total_sales))  +
  geom_line() +
  scale_x_discrete(limits=month.abb) +
  facet_wrap(~ product_category, ncol = 5) +
  labs(title = "Meat purhcasing trend in the year 2017",
       subtitle = "Plot displays trend in purchase of meat products in a year",
       x = "2017",
       y = "Total sales") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "plain"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.background = element_rect(color = "black"),
    plot.background = element_rect(fill = "light blue"))

```

Based on the observation of the graphical representation analyzed above and below , we have observed the following analysis with respect to sales of Regork market :-

* Top 3 meats purchased are Beef, Chicken and Pork.
* Sale of rest of the meats is constant throughout the year.

For analyzing the meat categories with either increasing or decreasing sales, we have plotted them separately

```{r, message=FALSE, fig.width=18, fig.height=10}
transactions %>%
  inner_join(products) %>%
  mutate( month1  = month(transaction_timestamp)) %>%
  filter(department == "MEAT") %>%
  group_by(product_category, month1) %>%
  summarize(total_sales = sum(sales_value, na.rm =TRUE)) %>%
  filter(product_category %in% c("BEEF","CHICKEN","PORK","SMOKED MEATS","TURKEY")) %>%
  ggplot(aes(x = month1, y = total_sales))  +
  geom_line() +
  scale_x_discrete(limits=month.abb) +
  facet_wrap(~ product_category, ncol = 5) +
  labs(title = "Meat purhcasing trend in the year 2017",
       subtitle = "Plot displays trend in purchase of Beef, Chicken, Pork, Smoked Meats, Turkey",
       x = "2017",
       y = "Total sales") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "plain"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.background = element_rect(color = "black"),
    plot.background = element_rect(fill = "light blue"))

```

Based on the observation of the graphical representation analyzed above, we can say that:

* Meat products purchased in 2017 shows that purchase of turkey rises in the month of November(probably due to Thanksgiving) only and there is a decrease in beef, chicken, pork purchase in the same month.
* Smoked meat is purchased more in the month of April and December due to holiday season.

### __Summary__

The above exercise helped us understand the complete journey data set better. and the solution proposed to our business analysis 1 and 2 are as follows:-

1. In analysis 1, We suggest that in order to increase the total sales of the poorly performing product categories for Regork market, more coupons should be given out for those categories to increase its sales.

2. In analysis 2, we propose that in the month of November, Regork market should increase the stock and price of Turkey products to increase revenue of this category and decrease the price and stock of Beef, Chicken, Pork meat products to boost their sales.


**Limitations**

Since the dataset is limited, we had some limitations while analyzing the data :-

1. There is no coupon data for “Cigarettes” and negligible coupon data for “Beers/Ales” which affects the analysis.

2. While analyzing the sales of Smoked meats, we have not considered the temperature at that time. Irregular cold seasons might alter the sales of smoked meats as people won’t prefer going out and having a BBQ in cold temperatures.



# <center>Statistical Computing Final Project</center>

### <center>Vishal Singh | Rohit Bhoite | Junaid Shareef | Saurav Nandi</center>

<center><img src ="https://user-images.githubusercontent.com/112138890/205417637-65b5fe67-0afd-423a-89d0-62635abfc882.jpeg" width = "300" > </center>

## <ins>Introduction</ins>:

Our research aims to assess and increase the total sales and discounts of meat products like beef, Turkey, chicken, pork and frozen meat/ meat dinners as per age group by comparing the sales per month.

Our analysis can be used to answer questions like:

1)  *How can we optimize sales revenue spent on discounting for the selected meat products like beef, Turkey, chicken, pork and frozen meat/ meat dinners?* <br>
2)  *How should Regork better optimize their discounts based on the seasonal preference of Consumers?*

### <ins>Packages Required</ins>:

* *completejourney_py*: Used to prepare and analyze the data
* _Pandas:_ Used to form dataframes and perform analysis
* _Numpy:_ Used to form arrays, lists
* _Matplotlib:_ Used to plot graphs and provide in-depth analysis and visualizations on the business problem

### <ins>Data Preparation</ins>:

This sections contains all the procedures followed in getting the data analysis ready. Each step has been explained with the relevant code.

##### Data Import

We are using the Complete Journey package for performing the analysis. The *transactions* datasets represents grocery store shopping transactions over one year from a group of 2,469 households. The *products* dataset contains product related information like department, product category, product type and brand. The *demographics* dataset contains information related to different age groups belonging to different households buying these meat products which is present in the product_categories column of products table

**Packages Import Code:**

```{r, message=FALSE, fig.width=18, fig.height=10}
 import pandas as pd
 from completejourney_py import get_data
 import numpy as np
 import matplotlib.pyplot as plt
 import matplotlib.ticker as mtick
```
Below are the datasets used from CompleteJourney
```{r, message=FALSE, fig.width=18, fig.height=10}
 cj_data = get_data()
 demographics = cj_data['demographics']
 demographics.head(5)

 products = cj_data['products']
 products.head(5)

 transactions = cj_data['transactions']
 transactions.head(5)
```
**Data Description:**

### <ins>Transactions</ins>:

household_id -> Unique ID for each household<br>
store_id -> Uniquely identifies each store<br>
basket_id -> Uniquely identifies each purchase occasion<br>
product_id -> Uniquely identifies each product<br>
quantity -> Number of the product purchased during the visit<br>
retail_disc -> Discount applied due to the retailer’s loyalty card program<br>
coupon_disc -> Discount applied due to a manufacturer coupon<br>
coupon_match_disc -> Discount applied due to retailer’s match of manufacturer coupon<br>
week -> Week of the transaction; Ranges 1-53<br>
transaction_timestamp -> Date and time of day when the transaction occurred<br>

### <ins>Products</ins>:

product_id -> Uniquely identifies each product<br>
manufacturer_id -> Uniquely identifies each manufacturer<br>
department -> Groups similar products together<br>
brand -> Indicates private or national label brand<br>
product_category -> Groups similar products together at lower level<br>
product_type -> Groups similar products together at lowest level<br>
package_size -> Indicates package size (not available for all products)<br>

### <ins>Demographics</ins>:

household_id -> Unique ID for each household<br>
age -> Age Range of buyers<br>
income -> Income range of buyers<br>
home_ownership -> Rental/Owner of House<br>
marital_status -> Marital Status of buyers<br>
household_size -> Number of members in buyers house<br>
household_comp -> Demographic description of members of the house<br>
kids_count -> Number of kids in buyers house<br>

### <ins>Exploratory Data Analysis</ins>:

In this section, we have summed the retail, coupon and coupon match discount. We have also extracted month from the transaction timestamp as it will be useful for plotting graphs for  further analysis.
```{r, message=FALSE, fig.width=18, fig.height=10}
discount_amount =(
 transactions['retail_disc'] + transactions['coupon_disc']  + transactions['coupon_match_disc']
)
transactions['discount_amount'] = discount_amount
transactions['month'] =pd.DatetimeIndex(transactions['transaction_timestamp']).month
transactions.head(5)
```
In order to perform analysis for our business problem, we have created a sample data that comprises of product categories where we have mainly focused on meat department of Regork such as Beef, Frozen meat/meat dinners, Chicken, Pork and Turkey. We created this sample data by first merging the transactions dataframe with the products dataframe and then the demographics dataframe: 
```{r, message=FALSE, fig.width=18, fig.height=10}
df = transactions.merge(products, how = 'inner', on='product_id')

df1 = df.merge(demographics, how = 'inner', on='household_id')
df1.head(5)
```
Here we have the sample data to extract selected meat products to further analyze and explore the bottlenecks that hinder sales.
```{r, message=FALSE, fig.width=18, fig.height=10}
sample_data = (
(df1['product_category'] == 'BEEF') |
(df1['product_category'] == 'PORK') |
(df1['product_category'] == 'CHICKEN') |
(df1['product_category'] == 'FRZN MEAT/MEAT DINNERS') |
(df1['product_category'] == 'TURKEY')
)

chart1_data =  df1[sample_data]
chart1_data.head(5)
```
We have aggregated the sales value per meat product and represented it on a Pie-Chart to show the total sales percentages per product.
```{r, message=FALSE, fig.width=18, fig.height=10}
chart1 = ( 
chart1_data.groupby('product_category')
           .agg({'sales_value':'sum'})
           .sort_values(by = 'sales_value' , ascending = False)
)    
chart1

chart1.plot(kind = 'pie', subplots = True, ylabel = 'Percent Sales', autopct = '%1.0f%%',figsize = (10,12))
plt.title('Percent sales by select Meat Products')
plt.legend(loc = 'upper right')
plt.show()
```
**As we can infer from the pie chart, *Beef* has the most percentage of sales (45 0/0), followed by frzn meat/Frzn dinners(25%), and then Chicken(14%) , Pork (13%) and Turkey(3%) in the end.**

### <ins>ANALYSIS-1</ins>:

After finding out the total_sales of different selected meat categories, we have grouped the chart1_data by age and product_category in order to find its total sales value and discount_amount and store the result in chart2_data dataframe.
```{r, message=FALSE, fig.width=18, fig.height=10}
Chart2_data = (
chart1_data.groupby(['product_category','age'], as_index = False)
           .agg({'sales_value':'sum','discount_amount':'sum'})
           .sort_values(by = 'sales_value', ascending = False)
)    
Chart2_data.head(5)

sales_per_age = Chart2_data.drop('discount_amount' , axis='columns')

We pivot the data inorder to plot sales value of selected meat products against age groups.

sales_per_age.pivot(index='age', columns='product_category', values='sales_value')

tick_format = mtick.StrMethodFormatter('${x:,.0f}')

(
sales_per_age.pivot(index='age', columns='product_category', values='sales_value')
             .plot(kind = 'bar', figsize = (12,6) ) 
             .yaxis.set_major_formatter(tick_format)
)    
plt.xlabel('Age Group')
plt.ylabel('Total Sales')
plt.title('Sales by Age Group')
plt.legend(loc = 'upper right')
plt.xticks(rotation=0)
plt.show()
```
**As per the graph, we can observe that <ins>Beef</ins> is the most preffered meat across all age groups followed by frozen meat and chicken. The age group <ins>45-54</ins> contributes for the maximum sales for  the selected meat products.
Similarly, age group <ins>19-24</ins> are the lowest contributors to the overall sale. It can also be observed that all age groups have a similar trend in meat products purchasing with an acceptable error in margin.**
```{r, message=FALSE, fig.width=18, fig.height=10}
discount_per_age = Chart2_data.drop('sales_value' , axis='columns')

discount_per_age.pivot(index='age', columns='product_category', values='discount_amount')

(
discount_per_age.pivot(index='age', columns='product_category', values='discount_amount')
                .plot(kind = 'bar', figsize = (12,6) )
                .yaxis.set_major_formatter(tick_format)
)    
plt.xlabel('Age Group')
plt.ylabel('Discounts')
plt.title('Discounts by Age Group')
plt.legend(loc = 'upper right')
plt.xticks(rotation=0)
plt.show()
```
**Here we see the distribution of discounts for each of the selected meat products by age group. It is evident that there is a similar trend in availing discounts as it was in the total sales graph. The age group 45-54 availed the most discount on selected meat products.**

***Another observation is that total amount spent on chicken discounts does not translate to the total sales of chicken. The store is providing almost equivalent or even higher amount of discounts for chicken as for beef, but, the actual chicken sales is always observed to be between 25-30% of total beef sales.***

### <ins>ANALYSIS-2</ins>:

We further analyzed meat sales and discounts availed on selected meat products to identify any seasonal trends.<br>
For this analysis, we have grouped the chartl data by month and product_category in order to find its total sales value and discount amount and store the result in chart3 data dataframe.

```{r, message=FALSE, fig.width=18, fig.height=10}
Chart3_data = (
chart1_data.groupby(['product_category','month'], as_index = False)
           .agg({'sales_value':'sum','discount_amount':'sum'})
           .sort_values(by = 'month')
)    
Chart3_data.head(5)

sales_per_age_month = Chart3_data.drop('discount_amount' , axis='columns')

month = [1,2,3,4,5,6,7,8,9,10,11,12]
cal_month = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

sales_per_age_month.pivot(index='month', columns='product_category', values='sales_value')

(
sales_per_age_month.pivot(index='month', columns='product_category', values='sales_value')
                   .plot(kind = 'line', figsize  = (15,6), marker = 'o', markerfacecolor = 'grey')
                   .yaxis.set_major_formatter(tick_format)
)    
plt.xlabel('Month')
plt.ylabel('Sales')
plt.title('Sales by Month')
plt.legend(loc = 'upper right')
plt.grid()
plt.xticks(month,cal_month,rotation=0)
plt.show()
```
**From the above graph, we can justify that most of meat products follow a regular trend throughout the year with one exception, i.e., Turkey which has a spike in its sales during the month of November <ins>(mainly due to Thanksgiving)</ins> where all other meat products experience a dip in sales.**
```{r, message=FALSE, fig.width=18, fig.height=10}
discount_per_age_month = Chart3_data.drop('sales_value' , axis='columns')

discount_per_age_month.pivot(index='month', columns='product_category', values='discount_amount')

(
discount_per_age_month.pivot(index='month', columns='product_category', values='discount_amount')
                      .plot(kind = 'line', figsize  = (15,6), marker = 'o', markerfacecolor = 'grey')
                      .yaxis.set_major_formatter(tick_format)
)    
plt.xlabel('Month')
plt.ylabel('Discounts')
plt.title('Discount by Month')
plt.legend(loc = 'upper right')
plt.grid()
plt.xticks(month,cal_month,rotation=0)
plt.show()
```
**From this graph we observe that distributions availed on Beef and Chicken is the highest throughout the year. As we can see in the Thanksgiving (November) month, the spike in discounts is analogous to the sales trend and we can say that the store’s idea of promoting discounts on Turkey in the month of November seems to be working well with the masses.**

***When we compare the overall sales with the discounts availed throughout the year, it can be inferred that the store has tried to promote chicken sales by providing more discounts in the months of March, May and July but the sales trend were not much responsive to it.<br> 
The store should reduce the amount of discounts on chicken and redistribute the discount budget for Pork throughout the year and Turkey mainly in the month of November.***

## <ins>Summary</ins>:

The above data analysis has helped us in solving our business problem in an effective way and
thus, the solution proposed from our analysis is as follows:-
- ***The store should ensure good relations with vendors who provide Beef, Frozen meat as these are the highest selling selected meat products and a zero inventory for these products would result in a loss of sale.***
- ***The store is spending high amounts in discounts to promote chicken sales which is resulting in an unsuccessful attempt as Chicken sales is always found to be about 25-30% of Beef sales for any age group***
- ***The excess amounts being spent in terms of discounts on Chicken, can be utilized in a better way by promoting discounts on Pork throughout the year and on Turkey in the month of November, as they show great potential to contribute more towards net sales as per the trend and numbers.***

## <ins>Limitations</ins>:

***Since the data set has product category at a granular level rather than a generalized level we could only select a few products in the meat category as selecting all categories was causing cluttering in the visualization and there would have been anonymity in the data being produced.***




