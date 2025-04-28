product = read.csv("product_hierarchy.csv")
sale = read.csv("sales_ug.csv")
store = read.csv("store_cities.csv")

library(tidyverse)
library(conflicted)
library(dplyr)

## 1. Write the code to compute the total revenue of each store at the end of each day. Is there a noted difference between the days? Write also the code to calculate the total revenue over the seven day period. Plot the latter on a graph.

head(sale, 10)
str(sale)

#Total revenue of each store at the end of each day
revenue_each_day <- aggregate(revenue ~ store_id + date, #calculate revenue based on 
                              #store_id and date variables
                              data = sale, 
                              FUN = sum) #summation is abbreviated to sum
head(revenue_each_day, 10)

#Differences in revenues between the day?
tapply(revenue_each_day$revenue, 
       revenue_each_day$store_id, 
       diff) %>% #each array element represents the difference in revenue between 
  head(10) #the current day and the next day

class(tapply(revenue_each_day$revenue, revenue_each_day$store_id, diff))
#returns values in the form of arrays

# Total revenue generated from each store over seven days

seven <- aggregate(revenue ~ store_id, 
                   data = sale, 
                   FUN = sum) #use sum to calculate the total revenue
head(seven, 10)


#Plot
library(ggplot2)
ggplot(seven, aes(store_id, revenue)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(xlim = c(1, 30)) + #showing the revenues obtained by the first 30 stores 
  labs(title = "Total revenue obtained over seven days by each store",
       caption = "The plot shows only the first 30 stores' revenues due to overloading of data.
       Note: revenue - daily total sales revenue
             store_id - unique identifier of a store")


## 2. What’s the most popular product type (hierarchy 1) sold in all stores over a week? How much revenue did the stores receive for that product during the week? How does that compare with the second most popular product? Provide a table that shows the product type ranked from most to least popular. For each product type provide: how many subtypes (hierarchy 2) are there, how many products are in this product type, what’s the sales quantity, and the revenue generated.

head(product, 10)
str(product)


#The most popular product type (hierarchy 1) sold in all stores over a week
merged_sale_product_tab <- product %>% 
  select("product_id", "hierarchy1_id", "hierarchy2_id") %>%
  right_join(sale)

sort(table(merged_sale_product_tab$hierarchy1_id), decreasing = TRUE)


# How much revenue did the stores receive for that product during the week?
#revenue made
stores_rev_made1 <- 
  merged_sale_product_tab[which(merged_sale_product_tab$hierarchy1_id == "H00"),] #select rows
#where hiearchy1_id is "H00"

aggregate(revenue ~ store_id + date, 
          data = stores_rev_made1, 
          sum) %>%
  head(10)#shows the first 10 values of revenues made from products with hierarchy1_id of "H00"


# How does that compare with the second most popular product?
stores_rev_made2 <- merged_sale_product_tab[which(merged_sale_product_tab$hierarchy1_id == "H01"),] #select
#rows where hierarchy1_id is "H01"

#total revenue made in each store from the products with hierarchy1_id "H01" 
aggregate(revenue ~ store_id + date, 
          data = stores_rev_made2, 
          sum) %>%
  head(10)

#Comparison
stores_rev_made1 <- aggregate(revenue ~ store_id + date, data = stores_rev_made1, sum)
stores_rev_made2 <- aggregate(revenue ~ store_id + date, data = stores_rev_made2, sum)
nrow(stores_rev_made1) #showing the row numbers of stores_rev_made1
nrow(stores_rev_made2) #showing the row numbers of stores_rev_made2

#Merging 
store_rev_made_12binded <- stores_rev_made1 %>% #joins stores_rev_made1 to stores_rev_made2 
  full_join(stores_rev_made2, 
            by = c("store_id", "date")) # by "store_id" and "date"

#fix column names
colnames(store_rev_made_12binded) <-
  c("store_id", "date", "H00.revenue", "H01.revenue")

#assign 0 to NA values
store_rev_made_12binded[is.na(store_rev_made_12binded)] <- 0

#shows final result
head(store_rev_made_12binded, 10)

#----------------------------------
store_rev_made_12binded <- aggregate(cbind(store_rev_made_12binded$H00.revenue,
                                           store_rev_made_12binded$H01.revenue),
                                     by = list(store_rev_made_12binded$store_id),
                                     FUN = sum)

#changes colnames
colnames(store_rev_made_12binded) <- c("store_id","H00.revenue","H01.revenue")

#assign a new column with differences in revenues to store_rev_made_12binded
store_rev_made_12binded[,"revenue.differences"] <- 
  store_rev_made_12binded$H00.revenue - store_rev_made_12binded$H01.revenue

#shows final result
head(store_rev_made_12binded, 10)


#Plotting it showing the product type from most to least popular
plot(store_rev_made_12binded$revenue.differences)

# Provide a table showing the product type ranked from most to least popular
sort(table(merged_sale_product_tab$hierarchy1_id), decreasing = TRUE)


# For each product: how many subtypes products are there?
matx_1 <- table(product$hierarchy1_id, product$hierarchy2_id)
matx_1


# How many products are in this product type?
matx_1[1,1] #That is how much
colnames(matx_1)[1] # How much is in here
rownames(matx_1)[1] #Which is a subset of this


# Sales quantity
#hierarchy1_id:
aggregate(sales ~ hierarchy1_id, data = merged_sale_product_tab, sum)

#4 product types and each made a unique quantity of sales over the seven days
aggregate(sales ~ hierarchy1_id, data = merged_sale_product_tab, sum)[1,2]
aggregate(sales ~ hierarchy1_id, data = merged_sale_product_tab, sum)[2,2]
aggregate(sales ~ hierarchy1_id, data = merged_sale_product_tab, sum)[3,2]
aggregate(sales ~ hierarchy1_id, data = merged_sale_product_tab, sum)[4,2]

#hierarchy2_id:
sale_hier2 <- aggregate(sales ~ hierarchy1_id + hierarchy2_id, 
                        data = merged_sale_product_tab,
                        sum) %>%
  head(10)
sale_hier2

sale_hier2[order(sale_hier2$hierarchy1_id, - sale_hier2$sales),]

# Revenue generated from each product type:
#hierarchy1_id
aggregate(revenue ~ hierarchy1_id, 
          data = merged_sale_product_tab,
          sum)

#hierarchy2_id:
aggregate(revenue ~ hierarchy1_id + hierarchy2_id, 
          data = merged_sale_product_tab,
          sum) %>%
  head(10)



## 3. Compare the sales volumes between the two most common store types in the data set. How do they compare in terms of total revenue? Is there a relationship between a store’s size and its revenue?

head(store, 10)
str(store)


#Comparison 
sort(table(store$storetype_id), decreasing = TRUE)

merged_store_sale_tab <- store %>% 
  select("store_id", "storetype_id", "store_size") %>%
  right_join(sale)
head(merged_store_sale_tab, 10)

sale_ST <- aggregate(sales ~ storetype_id, 
                     data = merged_store_sale_tab,
                     sum)[c(3,4),] #[c(3,4),] is to display only the values of sales                                     #from ST03 and ST04.
sale_ST

#How do they compare?
#Total revenue of ST03 and ST04
rev_ST <- aggregate(revenue ~ storetype_id, 
                    data = merged_store_sale_tab,
                    sum)[c(3,4),] #shows only the values of revenue from ST03 and ST04
rev_ST

#Relationship??
rev_rel <- aggregate(revenue ~ store_id + store_size, data = merged_store_sale_tab, sum)
nrow(rev_rel) #nrow of observations
cor.test(rev_rel$store_size,rev_rel$revenue) #perform pearson correlation testing

#There seems to be a moderate positive correlation as the coefficient is 0.701293
#95% CI between 0.60 to 0.77 for correlation coefficient.
#the number of observations is large enough, with 128 rows.
#p-value is smaller than 0.05(default significance level).
#Therefore, null hypothesis is rejected and there is enough evidence to conclude that there is a significant linear relationship between store size and revenue.

#Hypothesis:
#H0: B = 0. There is no sufficient evidence of a linear relationship between `store_size` and `revenue`.
#HA: B != 0. There is sufficient evidence of a linear relationship between `store_size` and `revenue`.

summary(lm(revenue~store_size, data=rev_rel))
#p-value of `store_size` is smaller than 0.05
#R-squared refers to the 48% of the variance in the response variale `revenue`, is explained y the model, promoting moderate linear relationship.
#p-value is less than 0.05.
#Slope of the parameter is not equal to 0.
#RSE (residual standard error) is high, which explains why the scatterplot (in the next part) spread like a big fan-shaped.
#Therefore there is a linear relationship

#Visualisation
ggplot(data = rev_rel, aes(x = store_size, y = revenue)) +
  labs(title = "Relationship between store_size and revenue") + 
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE) # se = FALSE removes the confidence interval lines


## 4. Several different types of promotions were applied to the products during the period with various level of promotion rates. For each promotion type, display the different levels of promotion used during the period. Analyse the effectiveness of the promotion on the sales of the products.

# Display the different levels of promotion during the period
table(sale$promo_type_1, sale$promo_bin_1)

table(sale$promo_type_1, sale$promo_bin_1, sale$date) %>%
  head(2) #shows 2 days instead of 7 days to minimise the display of data.

# Analyse the effectiveness of the promotion on the sales of the products
x1 <- aggregate(cbind(sale$sales, sale$revenue),
                by = list(sale$promo_type_1, sale$date), #aggregated by these variables
                sum) #returns sales and revenue
colnames(x1) <- c("promo_type_1", "date", "sales", "revenue")
head(x1[order(x1$sales, decreasing = TRUE),], 10)

pl1 <- ggplot(data = x1,
              aes(x = date, y = sales, color = promo_type_1)) +
  geom_point() +
  geom_line(group = x1$promo_type_1) +
  coord_trans() 
pl1 +
  labs(title = "Sales trends over seven days on the types of promotion",
       caption = "*Note: scaling is not efficient, so subgraphs of sales trends
                will be provided to reinforce the visualisation on trends data")


pl1 + 
  coord_cartesian(ylim = c(5000,8000)) +
  labs(title = "Sales trend of PR14 over 7 days")

pl1 +
  coord_cartesian(ylim = c(0,600)) +
  labs(title = "Sales trends of other promotion types over 7 days")
