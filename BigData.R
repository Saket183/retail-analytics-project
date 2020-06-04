#packages used 
library(readxl)
library(tidyverse)
library(dplyr)
library(factoextra)
library(ggplot2)


write.csv(newdata,"E:/Assignment/Semester 2/Big Data Assignment/Assignment2/Resources/newdata.csv")



#checking the worksheets of the excel file
excel_sheets("E:/Assignment/Semester 2/Big Data Assignment/Assignment2/Resources/FinalData.xlsx")

#creating a df for each worksheet
posdata <- read_excel("E:/Assignment/Semester 2/Big Data Assignment/Assignment2/Resources/FinalData.xlsx",sheet ='POS DATA')
posdata$...7 <-NULL
posdata$...8 <-NULL
posdata$...9 <-NULL
posdata$...10 <-NULL
loyaltydata <- read_excel("E:/Assignment/Semester 2/Big Data Assignment/Assignment2/Resources/FinalData.xlsx",sheet ='LOYALTY')
barcodedata <- read_excel("E:/Assignment/Semester 2/Big Data Assignment/Assignment2/Resources/FinalData.xlsx",sheet ='barcodes')
Productdata <- read_excel("E:/Assignment/Semester 2/Big Data Assignment/Assignment2/Resources/FinalData.xlsx",sheet ='product taxonomy')

#data cleaning process
#remove all the barcodes with less than 12 digits from barcode sheet
barcodedata = barcodedata[(which(nchar(barcodedata$Barcode) == 12)),]
#we removed 200 records with false values

#remove all the barcodes with less than 12 digits from the posdata sheet
posdata <- posdata[(which(nchar(posdata$Barcode)==12)),]
#remove all the sum units that are negative in the posdata sheet
posdata <- posdata[(which(sign(posdata$Sum_Units)==1)),]

#remove all the sum values that are negative in the posdata sheet
posdata <- posdata[(which(sign(posdata$Sum_Value)==1)),]

#creating a new data frame 
newdata <- merge(posdata,barcodedata,by.x = 'Barcode',by.y = 'Barcode')
newdata <- merge(newdata,Productdata)
newdata <- merge(newdata,loyaltydata,by.x = 'Card_ID',by.y = 'CardholderID')

#Provide the day of the week 
newdata$Day <- weekdays(as.Date(newdata$Date))

#rearrange the columns of the datasets
newdata <- newdata[,c(6,7,15,5,8,9,10,1,11,2,12,3,13,4,14)]

#replace the N/a with Others in the column Card_Id
newdata$Card_ID[newdata$Card_ID=='N/A'] <- 'Others'

#assign a unique code to each item
newdata$ProductCode <- paste(newdata$CategoryA,newdata$CategoryB,newdata$CategoryC,newdata$CategoryD,sep = '-')

#Finding the total units and value of each item purchased
unitsProductdata <- newdata %>%
  group_by(ProductCode, CategoryDDescription) %>% 
  summarise(TotalUnit=sum(Sum_Units), 
            TotalValue = sum(Sum_Value)) %>%
  arrange(-TotalUnit)


#Finding the total value of each basket 
BasketValue <- newdata %>%
  group_by(Basket_ID) %>%
  summarise(TotalValue = sum(Sum_Value)) %>%
  arrange(-TotalValue)

#Finding the each categroy B demand
CategoryBdemand <- newdata %>%
  group_by(CategoryBDescription) %>% 
  summarise(TotalUnit=sum(Sum_Units), 
            TotalValue = sum(Sum_Value)) %>%
  arrange(-TotalUnit)

#Finding the each categroy C demand
CategoryCdemand <- newdata %>%
  group_by(CategoryCDescription) %>% 
  summarise(TotalUnit=sum(Sum_Units), 
            TotalValue = sum(Sum_Value)) %>%
  arrange(-TotalUnit)

#Finding the each categroy D demand
CategoryDdemand <- newdata %>%
  group_by(CategoryDDescription) %>% 
  summarise(TotalUnit=sum(Sum_Units), 
            TotalValue = sum(Sum_Value)) %>%
  arrange(-TotalUnit)



#converting all the items to lower
unitsProductdata$CategoryDDescription <- tolower(unitsProductdata$CategoryDDescription)


#use GGplot to plot the units of item sold 
ggplot(data = unitsProductdata[1:20,],aes(x=reorder(CategoryDDescription,-TotalUnit),y=TotalUnit))+
  geom_bar(stat = 'identity') +
  theme(
    axis.text.x = element_text(angle = 45,margin = margin(1,0,0,0, unit = 'cm')))

#finding the number of transactions per day of the week 
transactionperday <- newdata %>% group_by(Day) %>% summarise(TotalTransactions = n_distinct(Basket_ID))

#rearranging the values of the transaction
transactionperday$Day <- factor(transactionperday$Day,levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

#plot of the total transactions per day of the week 
ggplot(data = transactionperday,aes(x=Day,y=TotalTransactions))+
  geom_bar(stat = 'identity')+
  geom_label(aes(label=TotalTransactions))


str(newdata)

newdata_df <- as.data.frame(newdata)
str(newdata_df)

#converting the data frame into a tabular format
newdata_df_tabluar <- table(newdata_df$Basket_ID,newdata_df$ProductCode)
str(newdata_df_tabluar)


#convert table matrix into  a dataframe
newdata_df_tabluar_df <- as.data.frame.matrix(newdata_df_tabluar)
str(newdata_df_tabluar_df)


#converting the tabular dataframe into a matrix
newdata_matrix <- as.matrix(newdata_df_tabluar_df)


#lists the invoice with multiple items in the same invoice 
newdata_duplicate <- which(newdata_matrix[,]>1 | newdata_matrix[,]<0,arr.ind = TRUE)

#Transactions with count greater than 1
length(which((newdata_matrix[,]>1),arr.ind = TRUE))

#Transactions with count less than 0
length(which((newdata_matrix[,]<0),arr.ind = TRUE))

#force the above 1 values to change to 1 to indicate that it was purchased
newdata_matrix[which(newdata_matrix[,]>1 | newdata_matrix[,]<0)] <- 1


library(arules)
library(arulesViz)

newdata_trans <- as(newdata_matrix,'transactions')
summary(newdata_trans)


newdata_trans


rules_newdata <- apriori(newdata_matrix,parameter = list(supp=.003,conf = 0.40,target = "rules"))

rules_newdata_assoc <- inspect(head(sort(rules_newdata,by='confidence'),20))
rules_newdata_assoc



basketdata <- newdata %>% group_by(Basket_ID) %>% summarise(basektdetails = n_distinct(ProductCode))

trans <- as(split(newdata[,"Basket_ID"],newdata[,"ProductCode"]),"transactions")

remove(trans)

#association rules
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)

df <- newdata %>%
  select(Basket_ID, ProductCode) %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(ProductCode, value, fill = 0)

rownames(df) <- df$Basket_ID

itemMatrix <- as(as.matrix(df[, -1]), 'transactions')

inspect(itemMatrix)

gr_rules <- apriori(itemMatrix,parameter = list(supp =0.001,conf = 0.3,target = "rules"))
gr_rules  <- inspect(head(sort(gr_rules,by='confidence')))

plot(gr_rules, method = "graph",engine = 'interactive')

mean(BasketValue$TotalValue)
sum(BasketValue$TotalValue)

summary(basketdata$basektdetails)
sum(basketdata$basektdetails)
