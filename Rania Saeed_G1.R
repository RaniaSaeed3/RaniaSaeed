library(mice)
library(ggplot2)
library(tidyverse)

#read dataset
allometry<-read.csv('G1_Allometry.csv')
allometry

str(allometry)


###Data cleaning
#Dataset Sense
str(allometry)
nrow(allometry)
ncol(allometry)
colnames(allometry)<-c('species', 'diameter', 'height',   'leafarea','branchmass')
allometry



#Data Slicing
head(allometry,30)
tail(allometry,30)
df<-allometry[1:8,c(1,2,3)]
df
df2<-allometry[-(10:20),]
allometry[-(20:50),-c(1,3)]
allometry[-(20:50),1:3]

len<-head(allometry["height"],3)
len # data frame
class(len) # data frame

#filter dataset
filter1<-allometry[allometry$species == 'PSME' & allometry$diameter<50 , ]
filter1
filter2<-allometry[allometry$species == 'PIMO' & allometry$diameter>=50 , c(1,2)]
filter2


#Data Sorting
sort1<-allometry[order(allometry$diameter),]
sort1
sort2<-allometry[order(-allometry$diameter),1:2]
sort2


#Re-coding Column
#Re-code the species feature to A ,B and C

allometry$type[allometry$species=="PSME"]="A"
allometry$type[allometry$species=="PIPO"]="B"
allometry$type[allometry$species=="PIMO"]="C"
allometry

#Dealing with Missing Data
allometry
allometry<-read.csv('G1_Allometry.csv', na.strings=c(''))
allometry

complete.cases(allometry)
allometry[ ! complete.cases(allometry), ]
print(any(is.na(allometry$height)))        ###Contains NAs
allometry

# Remove (,)and convert into(.)from the height column to convert it to numeric to use in the analysis
allometry$height <- gsub(",", ".", allometry$height)
allometry$height<-as.numeric(allometry$height)
allometry



# Converting variables
allometry$species<-as.factor(allometry$species)
allometry$height <- as.numeric(allometry$height)
str(allometry)


#show all the rows that have NA
allometry[ ! complete.cases(allometry), ]

#using mice function (multipule imputation techinque)
pre.imputation <- mice(allometry , m = 6 , meth = c("","","pmm","","") , maxit =25 )

pre.imputation$imp
allometrynew<-complete(pre.imputation,4)
allometrynew


#is na funcion
allometry[is.na(allometry$height),]


#using median 
#Replace each NA in height according to the median of

allometry[is.na(allometry$height),]
med<-median(allometry[allometry$species == 'PSME','height'],na.rm = T)
allometry[is.na(allometry$height) & allometry$species == 'PSME', "height"] <- med

med2<-median(allometry[allometry$species == 'PIPO','height'],na.rm = T)
allometry[is.na(allometry$height) & allometry$species == 'PIPO', "height"] <- med2

med3<-median(allometry[allometry$species == 'PIMO','height'],na.rm = T)
allometry[is.na(allometry$height) & allometry$species == 'PIMO', "height"] <- med3

allometry

#data visualization

#histogram
Hist <- ggplot(allometry, aes(x = diameter)) +
  geom_histogram(binwidth = 5, color = "black", fill = "red", alpha = .9)+
  ggtitle("height Histogram")+
  labs(x = "diameter", y = "Number")
Hist
#barchart

Bar<- ggplot(allometry, aes(x = species)) +
  geom_bar() +
  labs(y = "species Category", title = "species Bar Chart") +
  facet_wrap(~species)

Bar

#scatter plot

Scat <- ggplot(allometry, aes(x = diameter, y = height)) +
  geom_point() +
  stat_smooth(se = FALSE) +
  labs(x = "diameter", y = "Height", title = "diameter and Height Scatter")

Scat
