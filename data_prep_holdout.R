## Data prep for data including holdout data
ucllog = dc.SplitUpElogForRepeatTrans(ucl_log_sum)$repeat.trans.elog
x.star = rep(0,nrow(cal.cbs))
cal.cbs2 = cbind(cal.cbs,x.star)
ucllog.cust = ucllog$cust
for(i in 1:nrow(cal.cbs2)){
  current.cust = rownames(cal.cbs2)[i]
  tot.cust.trans = length(which(ucllog.cust==current.cust))
  cal.trans = cal.cbs2[i,"x"]
  cal.cbs2[i,"x.star"] = tot.cust.trans-cal.trans
}

cal.cbs2[1:3,]


tot.cbt = dc.CreateFreqCBT(ucllog)
period = range(ucl_data$InvoiceDate)[2]-threshold_date+1

