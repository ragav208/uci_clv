

## Estimating model parameters:: Parameters are estimated by maximising log-likelihood
#start params = (1,1,1,1)
params_bgnbd = bgnbd.EstimateParameters(cal.cbs)
params_bgnbd
LL = bgnbd.cbs.LL(params_bgnbd,cal.cbs)
LL
#Run multiple iterations to get optimal parameters 
p.matrix = c(params_bgnbd,LL)
for(i in 1:10){
  params_bgnbd = bgnbd.EstimateParameters(cal.cbs,params_bgnbd)
  LL = bgnbd.cbs.LL(params_bgnbd,cal.cbs)
  p.matrix.row = c(params_bgnbd,LL)
  p.matrix = rbind(p.matrix,p.matrix.row)
}
colnames(p.matrix) <- c("r","alpha","a","beta","LL")
rownames(p.matrix) <- 1:nrow(p.matrix)

bgnbd.PlotTransactionRateHeterogeneity(params_bgnbd) ## gamma distribution
##  This plot shows that customers are more likely to have low values of individual poisson transaction process parameters. 
## This means that with time the number of transactions made by a customer reduces, which is expected

bgnbd.PlotDropoutRateHeterogeneity(params_bgnbd,lim = 0.05) ## gamma mixing distribution of Pareto dropout process



###Individual Level Estimate:: 
#1. Number of repeat transactions a new customer would make in given time period: t 
for(t in seq(40,100,by=10)){
  print(paste("Expected number of repeat transaction by a new customer in",t,"days =" ,bgnbd.Expectation(params_bgnbd,t = t)))
}
## This means that a new customer is most likely to transact atleast once more after 3 months

###For a specific customer::12662
cust.12662 <- cal.cbs["12662",]
x = cust.12662["x"]
t.x = cust.12662["t.x"]
T.cal = cust.12662["T.cal"]

for(t in seq(40,100,by=10)){
  print(paste("Expected number of repeat transaction by customer 12662 in",t,"days =" ,bgnbd.ConditionalExpectedTransactions(params_bgnbd,T.star = t,x,t.x,T.cal)))
}

## This shows that customer 12662 will most likely transact again atleast once within 2 months

## To re-iterate increasing frequence == reduction in expectation of transaction
for(x in seq(10,120,by=10)){
  print(paste("Expectation of transaction by customer 12662 in 90 days =" ,bgnbd.ConditionalExpectedTransactions(params_bgnbd,T.star = 360,x,t.x,T.cal)))
}



## Plotting model vs actual:: 
bgnbd.PlotFrequencyInCalibration(params_bgnbd,cal.cbs,5)
## The comparison of actual vs model/expected frequencies tells us that the model is not that bad. 


T.star =  as.numeric(max(ucl_log_sum$date)-threshold_date+1)
censor = 10
x.star = cal.cbs2[,"x.star"]
comp = bgnbd.PlotFreqVsConditionalExpectedFrequency(params_bgnbd,T.star = T.star,cal.cbs=cal.cbs2,x.star,censor)


T.cal =(cal.cbs2[,"T.cal"])
#str(T.cal)
T.tot = as.numeric(round((range(ucl_data$InvoiceDate)[2]-range(ucl_data$InvoiceDate)[1])))
# n.period.final = T.tot#length(w.track)


inc.tracking.weekly = bgnbd.PlotTrackingInc(params_bgnbd,T.cal=T.cal,T.tot/7,actual.inc.tracking.data = w.track)
inc.tracking.weekly[,1:10]


inc.tracking.daily = bgnbd.PlotTrackingInc(params_bgnbd,T.cal=T.cal,T.tot,actual.inc.tracking.data = d.track)
inc.tracking.daily[,20:30]


## Cummalative daily tracking 
cum.track = cumsum(d.track)
cum.tracking = bgnbd.PlotTrackingCum(params_bgnbd,T.cal,T.tot,cum.track)


## Cummalative weekly tracking 
cum.track.week = cumsum(w.track)
cum.tracking.week = bgnbd.PlotTrackingCum(params_bgnbd,T.cal,T.tot/7,cum.track.week)

