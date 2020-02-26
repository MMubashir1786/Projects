# Q- make features which are helpful in predicting customer behaviour, such that how much value it can 
#    generate in future based on these features oR "predictor lifetime value", 















# *************************** Libraries*****************************************************************************
library(tidyverse)
library(UsingR)
library(ggplot2)
library(plotly)

df_data <- read_csv("stordata.csv")
view(head(df_data))

colMeans(is.na(df_data))

df_data$CustomerID <- as.character(df_data$CustomerID)

df_data$InvoiceDate <- as.Date(df_data$InvoiceDate, format = "%m/%d/%Y")

df_data %>% 
  mutate(Subtotal = Quantity * UnitPrice) %>% 
  drop_na(CustomerID) %>% 
  group_by(InvoiceNo, InvoiceDate, CustomerID) %>% 
  summarise(Total = sum(Subtotal)) -> Data

View(Data)


Data %>% 
  group_by(CustomerID) %>% 
  arrange(InvoiceDate) %>% 
  summarise(LTV = sum(Total),
            firstOrderId = first(InvoiceNo),
            month_acquired = format(min(InvoiceDate),
                                    format = "%b-%y"),
            avgOrderValue = mean(Total),
            span_days = lubridate::days(round(max(InvoiceDate) -
                                                min(InvoiceDate)))$day,
            span_months = ceiling(lubridate::days(round(max(InvoiceDate) - 
                                                          min(InvoiceDate)))$day / 30),
            n = n()) %>% 
  mutate(avgDaysBtwRptPurchase = span_days %/% n,
         customerType = ifelse(n > 1, 
                               ifelse(n > 2, "above2orders", "2orders"),
                               "notRepeatCustomer"),
         repeatRate = round(n/span_months, 1)) -> df_customers

View(df_customers)

df_customers %>% 
  filter(n > 1) %>% 
  ggplot(aes(x = avgDaysBtwRptPurchase,
             color = customerType)) +
  geom_density()

ggplotly(ggplot(df_customers, aes(x = avgOrderValue, color = month_acquired)) +
           geom_density())
























#    Features: (end row should be a customer unlike a single product, as it is right now)
#             Life Time Value (LTV) of a customer (how much rev it has given you so far??)
#             order ID of first order (first Invoice ID)
#             month Acquired (month of first Invoice)
#             Avg Order value 
#             span Days (days b/w his first & last Purchase)
#             span Months (Months b/w ......)
#             Total no of invoices (unique):
#                                           if 1 invoice (not Repeat customer)
#                                           if 2 invoices (2 orders)
#                                           if more than 2 orders.. (more than 2 orders)


# we need to predict when the customer will visit again... and what he will buy next...
# in order to manage the future Inventory position periodically, to cater atleast regular regular customers..
# If over stock --- cashflow issues migh arises and danger of product expiration will increase the loss anticipation..
# if understock --- wouldnt be able to cater the customers...


# about the dataset -- Each row is 1 Item 
#                      Each row is 1 Invoice 





# it is important for someone incharge of the analysis, that he must understant the given problem throughly..
# lack of understanding leads to incorrect features which results in a loss of precious time, 
# which is the ultimate currency. 

