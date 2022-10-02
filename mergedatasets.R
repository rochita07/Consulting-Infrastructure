texascountyregions = read.csv("TexasCountyRegions.csv")

texascountyfips = read.csv("texascountyfips.csv", header = FALSE)
colnames(texascountyfips) = c("fips", "County", "State")

texasregionswithfips = merge(texascountyfips, texascountyregions, by = "County")
texasregionswithfips$fips[1] = "48001"
texasregionswithfips$fips = as.numeric(texasregionswithfips$fips)
crispwithregion = merge(crisp, texasregionswithfips,by.x = "DOV_county", by.y = "fips", x.all = TRUE, y.all = FALSE)

coastalcounties = c("Cameron", "Willacy", "Kenedy", "Kleberg", "Nueces", "San Patricio", "Refugio", "Aransas", "Matagorda", "Brazoria", "Galveston", "Chambers", "Jefferson", "Orange")

iscoastal = ifelse(crispwithregion$County %in% coastalcounties, "Coastal", "Not Coastal")
crispwithregion$coastal = iscoastal
crispwithregion$PPPSupportUse = rowMeans(cbind(as.numeric(crispwithregion$Q15_1),as.numeric(crispwithregion$Q15_2),as.numeric(crispwithregion$Q15_3),as.numeric(crispwithregion$Q15_4) ))
crispwithregion$PPPSupport = rowMeans(cbind(as.numeric(crispwithregion$Q17_1),as.numeric(crispwithregion$Q17_2),as.numeric(crispwithregion$Q17_3),as.numeric(crispwithregion$Q17_4) ))
crispwithregion$PPP_Policy = rowMeans(cbind(as.numeric(crispwithregion$Q22_1),as.numeric(crispwithregion$Q22_2),as.numeric(crispwithregion$Q22_3),as.numeric(crispwithregion$Q22_4),as.numeric(crispwithregion$Q22_5),as.numeric(crispwithregion$Q22_6),as.numeric(crispwithregion$Q22_7),as.numeric(crispwithregion$Q22_8),as.numeric(crispwithregion$Q22_9)  ))



save(crispwithregion, file = "crispwithregion.RData")
#rename weights to create weights for just harris county sample
crispnewweights = crispwithregion
crispnewweights$AllTexasWeight = crispnewweights$weight1_PID
crispnewweights$HarrisCountyWeight = rep(NA, nrow(crispnewweights))
crispnewweights$HarrisCountyWeight[crispnewweights$County == "Harris"] = crispnewweights$weight2_PID[crispnewweights$County == "Harris"]

save(crispnewweights, file = "crispnewweights.RData")
