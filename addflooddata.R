flooddata = read.csv("FloodData.csv")
colnames(flooddata) = c("X", "99thPercentilePrecipDays1Y", "County", "99thPercentilePrecipDays3Y","99thPercentilePrecipDays5Y", "CountyFIPS")
flooddata = flooddata[, c(2:5)]


crispcleanfinal =  merge(flooddata, crispcleanfinal, by = "County", all.y = TRUE)
save(crispcleanfinal, file = "crispcleanfinal.RData")
