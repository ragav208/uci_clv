#Data prep for btyd:: 
#1) Select only required columns for btyd:: cust, date and sales 
ucl_log = ucl_data %>% ungroup() %>%  select(cust = CustomerID,date = InvoiceDate,sales = Value)

#Merge all transactions happening on the same day for each customer 
ucl_log_sum =  dc.MergeTransactionsOnSameDate(ucl_log)
print("ucl_log now has 1 transaction per customer for each day with sales = Total sales of the customer on that day")


#Seperate training period and holdout period 
#Split dataset (50-50)  
threshold_date = unique(ucl_data$InvoiceDate)[round(length(unique(ucl_data$InvoiceDate)) * 0.5)]
ucl.cal = ucl_log_sum %>% filter(date<=threshold_date)
#test.cal = ucl_log_sum %>% filter(date>threshold_date)

#Seperate repeat customers 
split.repeat = dc.SplitUpElogForRepeatTrans(ucl.cal)
clean.repeat = split.repeat$repeat.trans.elog
#clean.repeat has only customers who have transacted on more than 1 days

### Analysis for repeat customers on a day-day basis################ 
#See number of transactions made by a customer on each day
freq.cbt = dc.CreateFreqCBT(clean.repeat)
freq.cbt[1:3,1:5]

#See if a customer made any transaction on that day
reach.cbt = dc.CreateReachCBT(clean.repeat)
reach.cbt[1:3,1:5]

#See total money spent by customer on each day
spend.cbt = dc.CreateSpendCBT(clean.repeat)
spend.cbt[1:3,1:5]
##################################################

#Bring back customers who made no transactions in the given period 
tot.cbt.freq = dc.CreateFreqCBT(ucl.cal)
tot.cbt.reach = dc.CreateReachCBT(ucl.cal)
tot.cbt.sales = dc.CreateSpendCBT(ucl.cal)

ucl.cbt.freq = dc.MergeCustomers(tot.cbt.freq,freq.cbt)
ucl.cbt.reach = dc.MergeCustomers(tot.cbt.reach,reach.cbt)
ucl.cbt.spend = dc.MergeCustomers(tot.cbt.sales,spend.cbt)

## Building rf table 
birth.periods = split.repeat$cust.data$birth.per
last.dates = split.repeat$cust.data$last.date
cal.cbs.dates = data.frame(birth.periods,last.dates,threshold_date)
cal.cbs = dc.BuildCBSFromCBTAndDates(ucl.cbt.freq,cal.cbs.dates,per = "day")
# cal.cbs.spend = dc.BuildCBSFromCBTAndDates(ucl.cbt.spend,cal.cbs.dates,threshold_date,per = "day")

