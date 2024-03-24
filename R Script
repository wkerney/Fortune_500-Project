fin<-read.csv("P3-Future-500-The-Dataset.csv",na.strings = "")
fin
head(fin)
tail(fin)
str(fin)
summary(fin)

#Changing from non-factor to factor

fin$ID <- factor(fin$ID)
str(fin)

fin$Inception <- factor(fin$Inception)
str(fin)

#FVT Conversion

fin$Expenses <- gsub("Dollars","", fin$Expenses)
fin$Expenses <- gsub(",","", fin$Expenses)
head(fin)

fin$Revenue <- gsub("\\$","",fin$Revenue)
fin$Revenue <- gsub(",","", fin$Revenue)

fin$Growth <- gsub("%", "", fin$Growth)

#Converting from characters

fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)
str(fin)

#Locating Missing Data

fin[!complete.cases(fin),] #true NA function

#Filtering using which()
#Which companies have 45 employees?
#fin[which(fin$Employees ==45),]

#Filtering rows with NA using is.na()

#fin[is.na(fin$Expenses),]
#fin[is.na(fin$Revenue),]

#Removing records with missing data

fin[!complete.cases(fin),]
fin[is.na(fin$Industry),]
fin[!is.na(fin$Industry),] #opposite
fin <- fin[!is.na(fin$Industry),]
fin

#Resetting the dataframe index

row.names(fin) <- 1:nrow(fin)
fin

#Replacing missing data: Factual Analysis

fin[!complete.cases(fin),]

fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City == "New York",]
fin[is.na(fin$State) & fin$City == "New York","State"] <-"NY"
#Validate
fin[c(10,377),]

fin[is.na(fin$State) & fin$City == "San Francisco","State"] <-"CA"
fin[c(82,265),]

#Replacing missing data: Median Imputation Method

fin[!complete.cases(fin),]

median(fin[fin$Industry == "Retail","Employees"], na.rm = TRUE)
median_empl_retail <- median(fin[fin$Industry == "Retail","Employees"], na.rm = TRUE)

fin[is.na(fin$Employees) & fin$Industry =="Retail",]
fin[is.na(fin$Employees) & fin$Industry =="Retail","Employees"] <- median_empl_retail
#confirm
fin[3,]

median_empl_fin_serv <- median(fin[fin$Industry == "Financial Services","Employees"], na.rm = TRUE)

fin[is.na(fin$Employees) & fin$Industry == "Financial Services","Employees"] <- median_empl_fin_serv

fin[330,]

fin[!complete.cases(fin),]

median(fin[fin$Industry == "Construction","Growth"], na.rm = TRUE)
median_empl_growth <- median(fin[fin$Industry == "Construction","Growth"], na.rm = TRUE)

fin[is.na(fin$Growth) & fin$Industry =="Construction",]
fin[is.na(fin$Growth) & fin$Industry =="Construction","Growth"] <- median_empl_growth

fin[8,]
#Revenue

fin[!complete.cases(fin),]

median_rev_const<- median(fin[fin$Industry == "Construction","Revenue"], na.rm = TRUE)
median_rev_const
fin[is.na(fin$Revenue) & fin$Industry =="Construction",]
fin[is.na(fin$Revenue) & fin$Industry =="Construction","Revenue"] <- median_rev_const

fin[!complete.cases(fin),]

#Expenses

fin[!complete.cases(fin),]

median_exp_const<- median(fin[fin$Industry == "Construction","Expenses"], na.rm = TRUE)
median_exp_const
fin[is.na(fin$Expenses) & fin$Industry =="Construction" & is.na(fin$Profit),]
fin[is.na(fin$Expenses) & fin$Industry =="Construction" & is.na(fin$Profit),"Expenses"] <- median_rev_const

fin[!complete.cases(fin),]
#IT Sevices (Row 15) must be left, because there's only one value and no median

#Replacing data
#Revenue-Expenses = Profit
#Expenses = Revenue-Profit

fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]
fin[c(8,42),]
fin[!complete.cases(fin),]

fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses),"Profit"]
fin[15,]
fin[!complete.cases(fin),]


#Viz
install.packages("ggplot2")
library(ggplot2)
#Scatterplot classified by industry showing revenue, expenses,profit
p <-ggplot(data=fin)
p
p + geom_point(aes(x=Revenue, y=Expenses, color =Industry, size=Profit))

#scatterplot that includes industry trends for expense-revenue relationship

d <-ggplot(data=fin,aes(x=Revenue, y=Expenses, color =Industry))
d + geom_point() +
  geom_smooth(fill=NA, size=1.2)

#Boxplots showing growth by industry

f<- ggplot(data=fin,aes(x=Industry, y=Growth, color=Industry))
f + geom_boxplot()

f + geom_jitter() +
  geom_boxplot(size=1, alpha= 0.5, outlier.color = NA)








