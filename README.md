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




