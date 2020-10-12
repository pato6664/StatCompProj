# File descriptions #

# sales_train.csv - the training set. Daily historical data from January 2013 to October 2015.
# test.csv - the test set. You need to forecast the sales for these shops and products for November 2015.
# sample_submission.csv - a sample submission file in the correct format.
# items.csv - supplemental information about the items/products.
# item_categories.csv  - supplemental information about the items categories.
# shops.csv- supplemental information about the shops.

# Fields #

# ID - an Id that represents a (Shop, Item) tuple within the test set
# shop_id - unique identifier of a shop
# item_id - unique identifier of a product
# item_category_id - unique identifier of item category
# item_cnt_day - number of products sold. You are predicting a monthly amount of this measure
# item_price - current price of an item
# date - date in format dd/mm/yyyy
# date_block_num - a consecutive month number, used for convenience. January 2013 is 0, February 2013 is 1,..., October 2015 is 33
# item_name - name of item
# shop_name - name of shop
# item_category_name - name of item category

# Read in data files


rm(list=ls())
library(tidyverse)
library(lubridate)
data_dir <- list.files('D:/Fall 2020/StatComputing/Project/1c_competition',full.names = T)

sales_train <- read_csv("D:/Fall 2020/StatComputing/Project/1c_competition/sales_train.csv", 
                        col_types = cols(date = col_datetime(format = "%d.%m.%Y")))
# Basic EDA and Data Cleaning 

sales_train %>% 
  summarise('DistinctShops'= n_distinct(shop_id),
            'DistinctItems'= n_distinct(item_id))

sales_train %>% 
  summarise('MinItemsSold'= min(item_cnt_day),
            'MaxItemsSold'= max(item_cnt_day))



# Plot time series for randomly selected shops and items 

sales_train %>% 
  distinct(shop_id) %>% 
  sample_n(6,replace = F) -> shop_id_df

sales_train %>% 
  distinct(item_id) %>% 
  sample_n(1) -> item_id_df 

sales_train %>% 
  group_by(shop_id) %>% 
  filter(shop_id %in% shop_id_df$shop_id & item_id == item_id_df$item_id) -> ts_plot_df 

ts_plot_df %>% 
  ggplot(aes(x=date,y=item_cnt_day,color=shop_id)) + 
  geom_line() + theme_minimal()+ facet_wrap(~shop_id,nrow = 3,ncol=2)

