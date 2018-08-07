#Data first seperated from Excel sheet and load in R
store_view = read.csv(file.choose())
dc_view = read.csv(file.choose())
lead_time = read.csv(file.choose())

#Data has to be cleaned
summary(store_view)
#Columns of no use - purchase category,vendor, Uom
store_view = store_view[,c(-2,-6,-10)]
summary(lead_time)
summary(dc_view)
#Columns of no use - purchase category,vendor, Uom
dc_view = dc_view[,c(-2,-4,-7)]


#joining store_view with lead_time
store_view_new = merge(store_view,lead_time,by.x = c("Supply.Site","Store"), by.y = c("Source","Destination"),all.x = TRUE)


#Finding the difference in lead time, how late is the stock arriving at store from DC
Diff_lead_time = store_view_new$DC.to.Store.Lead.Time..in.Days. - store_view_new$Lead.Time..in.Days.
store_view_new = data.frame(store_view_new[,],Diff_lead_time)
unique(store_view_new$Diff_lead_time)
length(Diff_lead_time[Diff_lead_time==0])

#below plot shows how much is the Delay in lead time for each SKU.
ggplot(store_view_new, aes(y=Diff_lead_time, x = SKU))+geom_violin()

#below table shows which depot is the sending stock on time.
table(store_view_new$Store,store_view_new$Diff_lead_time)
table(store_view_new$SKU,store_view_new$Diff_lead_time)


#now daily sales for last month on rolling average is converted to factor levels to see for each SKU
store_view_new$daily_sales_last_month = cut(store_view_new$Daily.SALES.QTY...Rolling.Average.for.Last.Month, 
                                            breaks = c(75,105,135,165,195,225,255), labels = c("75-105","106-135","136-165","166-195","196-225","226-255"))
depots = unique(store_view_new$Store)
for (i in depots){
  depot = i
  print(table(store_view_new$SKU[store_view_new$Store == depot],store_view_new$daily_sales_last_month[store_view_new$Store == depot]))
  print(paste("Depot = ",i))  
  }

write.csv(store_view_new, "./store_view_new.csv")
