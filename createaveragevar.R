library(psych)

#Q4
crispclean = crispnewweights


matq4 = matrix(as.numeric(unlist(as.data.frame(crispclean[,41:49]))), nrow = 810)

Q4avg = rowMeans(matq4)
crispclean$Q4avg = Q4avg


matq5 = matrix(as.numeric(unlist(as.data.frame(crispclean[,50:57]))), nrow = 810)

Q5avg = rowMeans(matq5)
crispclean$Q5avg = Q5avg


#Q5 is internally consistent and normal



#Q6, 57:72  

matq6 = matrix(as.numeric(unlist(as.data.frame(crispclean[,58:73]))), nrow = 810)

Q6avg = rowMeans(matq6)

crispclean$Q6avg = Q6avg






#normal, but not very consistent
#q19 120:126
matq19 = matrix(as.numeric(unlist(as.data.frame(crispclean[,121:127]))), nrow = 810)

Q19avg = rowMeans(matq19)
crispclean$Q19avg = Q19avg

#q20 127:133
matq20 = matrix(as.numeric(unlist(as.data.frame(crispclean[,128:134]))), nrow = 810)

Q20avg = rowMeans(matq20)
crispclean$Q20avg = Q20avg

#q21 134:139
matq21 = matrix(as.numeric(unlist(as.data.frame(crispclean[,135:141]))), nrow = 810)
Q21avg = rowMeans(matq21)
crispclean$Q21avg = Q21avg

irrelevant = c("S3", "Q3", "Q3_1", "Q3_2", "Q3_3", "Q3_4", 
               "Q3_5",  "Q3_6", "Q13_ord", "Q3_ord", "Q13", "Q3_13", "Q13_0","Q12", 
               "Q7_1_1_1", "Q7_2_1_1","Q7_3_1_1","Q7_4_1_1","Q7_5_1_1","Q7_6_1_1","Q7_7_1_1",
               "Q7_1_2_1","Q7_2_2_1","Q7_3_2_1","Q7_4_2_1","Q7_5_2_1","Q7_6_2_1","Q7_7_2_1",
               "Q11_1_1_1","Q11_2_1_1","Q11_3_1_1","Q11_4_1_1",
               "Q11_1_2_1","Q11_2_2_1","Q11_3_2_1","Q11_4_2_1", "Q12a")
irrelevantcol = which(colnames(crispclean) %in% irrelevant)
#remove variables relating to experiments or otherwise irrelevant
crispclean = crispclean[, -irrelevantcol]
crispclean = crispclean[, colSums(is.na(crispclean)) < 81]
colnames(crispclean)
#take out all the original data that has been avg
averaged = colnames(crispclean)[c(28:60, 72:75, 77:83, 84:106)]
averagedcol = which(colnames(crispclean) %in% averaged)
crispclean = crispclean[, -averagedcol]
#remove more irrelevant
crispclean = crispclean[, -c(1:8, 55:58, 69, 80)]
colnames(crispclean)
crispcleanfinal = crispclean
crispcleanfinal$HarrisCountyWeight =  rep(NA, nrow(crispnewweights))
crispcleanfinal$HarrisCountyWeight[crispcleanfinal$County == "Harris"] = crispcleanfinal$weight2_PID[crispcleanfinal$County == "Harris"]
library(dplyr)
crispcleanfinal$gender = factor(crispcleanfinal$ppgender, levels = c(1,2), labels = c("Male", "Female"))
crispcleanfinal$age = cut(crispcleanfinal$ppage, c(18, 30, 45, 60, 99), labels = c("18-29", "30-44", "45-59", "60+"))
crispcleanfinal$race = factor(crispcleanfinal$ppethm, levels = c(1,2,3,4), labels = c("White Non-Hispanic", "Black Non-Hispanic", "Other Non-Hispanic", "Hispanic"))
crispcleanfinal$education = factor(crispcleanfinal$ppeducat, levels = c(1,2,3,4), labels = c("<HS", "HS", "Some College", "College Grad"))
crispcleanfinal$income = factor(crispcleanfinal$ppinc7, levels = c(1,2,3,4,5,6), labels = c("<25k", "25-49k", "50-74k", "75-99k", "100-149k", ">150k"))
dropdem = colnames(crispcleanfinal[47:61])
dropdemcol = which(colnames(crispcleanfinal) %in% dropdem)
crispcleanfinal = crispcleanfinal[, -dropdemcol]
crispcleanfinal = crispcleanfinal[,  -c(23:26)]
set.seed(2021)
crispcleanfinal$fold = sample(1:4, size = nrow(crispcleanfinal), replace = TRUE )
crispcleanfinal$Harris = ifelse(crispcleanfinal$County == "Harris", "Harris", "Not Harris")
set.seed(684)
testtrain = rep(NA, nrow(crispclean))
testtrain[crispcleanfinal$Harris == "Harris"] = sample(c(rep("train", 225), rep("test", 56)))
testtrain[crispcleanfinal$Harris == "Not Harris"] = sample(c(rep("train", 423), rep("test", 106)))
table(testtrain)
crispcleanfinal$testtrain = testtrain
crispcleanfinal = crispcleanfinal[, -which(colnames(crispcleanfinal) == "fold")]

save(crispcleanfinal , file = "crispcleanfinal.RData")
