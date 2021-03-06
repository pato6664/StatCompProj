rm(list=ls())
library(tidyverse)
library(lubridate)
data_dir <- list.files('D:/Fall 2020/StatComputing/Project/M5_Competition/Accuracy',full.names = T)

####################################################################################################################################
####################################################################################################################################

# File 1: “calendar.csv”
# Contains information about the dates the products are sold.
# • date: The date in a “y-m-d” format.
# • wm_yr_wk: The id of the week the date belongs to.
# • weekday: The type of the day (Saturday, Sunday, …, Friday).
# • wday: The id of the weekday, starting from Saturday.
# • month: The month of the date.
# • year: The year of the date.
# • event_name_1: If the date includes an event, the name of this event.
# • event_type_1: If the date includes an event, the type of this event.
# • event_name_2: If the date includes a second event, the name of this event.
# • event_type_2: If the date includes a second event, the type of this event.
# • snap_CA, snap_TX, and snap_WI: A binary variable (0 or 1) indicating whether the stores of CA, TX or WI allow SNAP3 purchases on the examined date. 1 indicates that SNAP purchases are allowed.

####################################################################################################################################
####################################################################################################################################


# File 2: “sell_prices.csv”
# Contains information about the price of the products sold per store and date.
# • store_id: The id of the store where the product is sold.
# • item_id: The id of the product.
# • wm_yr_wk: The id of the week.
# • sell_price: The price of the product for the given week/store. The price is provided per week (average across seven days). If not available, this means that the product was not sold during the examined week. Note that although prices are constant at weekly basis, they may change through time (both training and test set).

####################################################################################################################################
####################################################################################################################################

# File 3: “sales_train.csv”
# Contains the historical daily unit sales data per product and store.
# • item_id: The id of the product.
# • dept_id: The id of the department the product belongs to.
# • cat_id: The id of the category the product belongs to.
# • store_id: The id of the store where the product is sold.
# • state_id: The State where the store is located.
# • d_1, d_2, …, d_i, … d_1941: The number of units sold at day i, starting from 2011-01-29

####################################################################################################################################
####################################################################################################################################

sales_train <- data_dir[2]
