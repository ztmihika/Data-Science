
data<- read.csv("D:/Data.csv",header = TRUE,sep = ",")
data

str(data)

summary(data)

mydata <-data
mydata

mydata$Age[mydata$Age == ""] <- 'NA'
mydata$Gender[mydata$Gender == ""] <- 'NA'
mydata$weight.kg.[mydata$weight.kg. == ""] <- 'NA'
mydata$Delivery_number[mydata$Delivery_number == ""] <- 'NA'
mydata$Delivery_time[mydata$Delivery_time == ""] <- 'NA'
mydata$Blood[mydata$Blood == ""] <- 'NA'
mydata$Caesarian[mydata$Caesarian == ""] <- 'NA'
mydata

mydata$Gender <- factor(mydata$Gender,
                        levels = c("Female","Male","female","male"),
                        labels = c(0,1,2,3))
mydata

mydata$Blood <- factor(mydata$Blood,
                        levels = c("high","normal","low"),
                        labels = c(1,2,3))
mydata

colSums(is.na(mydata))

removedata<- na.omit(mydata)
removedata

meanData <-mydata
meanData

meanData$Gender=as.numeric(meanData$Gender)
Gendermean=mean(meanData$Gender , na.rm = TRUE)
missingGenderMean= meanData$Gender[is.na(meanData$Gender)] <- Gendermean
meanData

meanData$Age=as.numeric(meanData$Age)
Agemean=mean(meanData$Age , na.rm = TRUE)
missingAgeMean= meanData$Age[is.na(meanData$Age)] <- Agemean
meanData

meanData$weight.kg.=as.numeric(meanData$weight.kg.)
Weightmean=mean(meanData$weight.kg. , na.rm = TRUE)
missingWeightMean= meanData$weight.kg.[is.na(meanData$weight.kg.)] <- Weightmean
meanData

meanData$Delivery_number=as.numeric(meanData$Delivery_number)
Deliverymean=mean(meanData$Delivery_number , na.rm = TRUE)
missingDeliveryMean= meanData$Delivery_number[is.na(meanData$Delivery_number)] <- Deliverymean
meanData

meanData$Delivery_time=as.numeric(meanData$Delivery_time)
DeliveryTimemean=mean(meanData$Delivery_time , na.rm = TRUE)
missingDeliveryTimeMean= meanData$Delivery_time[is.na(meanData$Delivery_time)] <- Deliverymean
meanData

meanData$Blood=as.numeric(meanData$Blood)
Bloodmean=mean(meanData$Blood , na.rm = TRUE)
missingBloodMean= meanData$Blood[is.na(meanData$Blood)] <- Bloodmean
meanData

meanData$Caesarian=as.numeric(meanData$Caesarian)
Caesarianmean=mean(meanData$Caesarian , na.rm = TRUE)
missingCaesarianMean= meanData$Caesarian[is.na(meanData$Caesarian)] <- Caesarianmean
meanData

medianData <-mydata
medianData

medianData$Gender=as.numeric(medianData$Gender)
Gendermedian=median(medianData$Gender , na.rm = TRUE)
missingGenderMedian= medianData$Gender[is.na(medianData$Gender)] <- Gendermedian
medianData

medianData$Age=as.numeric(medianData$Age)
Agemedian=median(medianData$Age , na.rm = TRUE)
missingAgeMedian= medianData$Age[is.na(medianData$Age)] <- Agemedian
medianData

medianData$weight.kg.=as.numeric(medianData$weight.kg.)
Weightmedian=median(medianData$weight.kg. , na.rm = TRUE)
missingWeightMedian= medianData$weight.kg.[is.na(medianData$weight.kg.)] <- Weightmedian
medianData

medianData$Delivery_number=as.numeric(medianData$Delivery_number)
Deliverymedian=median(medianData$Delivery_number , na.rm = TRUE)
missingDeliveryMedian= medianData$Delivery_number[is.na(medianData$Delivery_number)] <- Deliverymedian
medianData

medianData$Delivery_time=as.numeric(medianData$Delivery_time)
DeliveryTimemedian=median(medianData$Delivery_time , na.rm = TRUE)
missingDeliveryTimeMedian= medianData$Delivery_time[is.na(medianData$Delivery_time)] <- Deliverymedian
medianData

medianData$Blood=as.numeric(medianData$Blood)
Bloodmedian=median(medianData$Blood , na.rm = TRUE)
missingBloodMedian= medianData$Blood[is.na(medianData$Blood)] <- Bloodmedian
medianData

medianData$Caesarian=as.numeric(medianData$Caesarian)
Caesarianmedian=median(medianData$Caesarian , na.rm = TRUE)
missingCaesarianMedian= medianData$Caesarian[is.na(medianData$Caesarian)] <- Caesarianmedian
medianData

  
class(data$Age) 
breaks <- c(0, 18, 30, 50, Inf) 
labels <- c("Young", "Adult", "Middle-aged", "Senior") 
data$categorical_age <- cut(data$Age, breaks = breaks, labels = labels) 
head(data) 

age <- data$Age 
min_age <- min(age, na.rm = TRUE) 
max_age <- max(age, na.rm = TRUE) 
normalized_age <- (age - min_age) / (max_age - min_age) 
data$normalized_age <- normalized_age 
head(data) 


modeData <-mydata
modeData

mode <- function(x){
  unique_x <- unique(x)
  tabulate_x <-tabulate(match(x,unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

modeData$Gender=as.numeric(modeData$Gender)
genderMode = mode(modeData$Gender)
missingGenderMode= modeData$Gender[is.na(modeData$Gender)] <- genderMode
modeData

modeData$Age=as.numeric(modeData$Age)
ageMode = mode(modeData$Age)
missingAgeMode= modeData$Age[is.na(modeData$Age)] <- ageMode
modeData

modeData$weight.kg.=as.numeric(modeData$weight.kg.)
Weightmode=mode(modeData$weight.kg)
missingWeightMode= modeData$weight.kg.[is.na(modeData$weight.kg.)] <- Weightmode
modeData

modeData$Delivery_number=as.numeric(modeData$Delivery_number)
Deliverymode=mode(modeData$Delivery_number)
missingDeliveryMode= modeData$Delivery_number[is.na(modeData$Delivery_number)] <- Deliverymode
modeData

modeData$Delivery_time=as.numeric(modeData$Delivery_time)
DeliveryTimemode=mode(modeData$Delivery_time)
missingDeliveryTimeMode= modeData$Delivery_time[is.na(modeData$Delivery_time)] <- Deliverymode
modeData

modeData$Blood=as.numeric(modeData$Blood)
Bloodmode=mode(modeData$Blood)
missingBloodMode= modeData$Blood[is.na(modeData$Blood)] <- Bloodmode
modeData

modeData$Caesarian=as.numeric(modeData$Caesarian)
Caesarianmode=mode(modeData$Caesarian)
missingCaesarianMode= modeData$Caesarian[is.na(modeData$Caesarian)] <- Caesarianmode
modeData



data<- read.csv("D:/Data.csv",header = TRUE,sep = ",")
data

colSums(is.na(data))
data$Gender[data$Gender == ""] <- 'NA'
data
data$Gender <- factor(data$Gender,
                      levels = c("Female","Male","female","male"),
                      labels = c(0,1,2,3))
data

mode <- function(v) {
  uniquevalue <- na.omit(unique(v))
  uniquevalue[which.max(tabulate(match(v, uniquevalue)))] }
Gendermode = mode(data$Gender)
missingGenderMode= data$Gender[is.na(data$Gender)] <- Gendermode
data

data$Gender <- factor(data$Gender,
                      levels = c(0,1,2,3),
                      labels = c("Female","Male","female","male"))
data

options(max.print=10000)
data1<- read.csv("D:/Data.csv",header = TRUE,sep = ",")
data1
data1$Age[data1$Age == "160"] <- "60"
data1$Age[data1$Age == "165"] <- "65"
data1$Age[data1$Age == "150"] <- "50"
data1$weight.kg.[data1$weight.kg. == "75X"] <- "75"
data1

data2<- read.csv("D:/Data.csv",header = TRUE,sep = ",")
delete_row <-data2[c(1:6,8:9,12,14:15,17:30,32:35,38:42,44,45,47,49,50,52:55,57:60,62,65:71,74:77,79:81),]
delete_row
updateData <-delete_row
updateData


means <- colMeans(meanData)
print(means)


medians <- apply(medianData, 2, median)
print(medians)


modes <- apply(modeData, 2, mode)
print(modes)

newData <-medianData
newData


numeric_data <- newData[, sapply(newData, is.numeric)]
ranges <- sapply(numeric_data, range)
print(ranges)

variances <- sapply(numeric_data, var)
print(variances)

std_devs <- sapply(numeric_data, sd)
print(std_devs)

hist(modeData$Age, main = "Age Distribution", xlab = "Age", ylab = "Frequency")
hist(modeData$weight.kg., main = " Weight Distribution", xlab = "Weight", ylab = "Frequency")
hist(modeData$Delivery_number, main = "Delivery_number Distribution", xlab = "Delivery_number", ylab = "Frequency")
hist(modeData$Delivery_time, main = "Delivery_time Distribution", xlab = "Delivery_time", ylab = "Frequency")
hist(modeData$Heart, main = "Heart Distribution", xlab = "Heart", ylab = "Frequency")
hist(modeData$Caesarian, main = "Caesarian Distribution", xlab = "Caesarian", ylab = "Frequency")

barplot(table(medianData$Gender), main = "Gender Distribution", xlab = "Gender", ylab = "Frequency")
barplot(table(medianData$Blood), main = "Blood Distribution", xlab = "Blood", ylab = "Frequency")

plotData<-medianData
plotData

boxplot(plotData$Age, 
        main = "Age Distribution",
        ylab = "Age",
        col = "lightblue")

boxplot(plotData$weight.kg., 
        main = "Weight Distribution",
        ylab = "Weight",
        col = "lightblue")

boxplot(plotData$Delivery_number, 
        main = "Delivery_number Distribution",
        ylab = "Delivery_number",
        col = "lightblue")

boxplot(plotData$Delivery_time, 
        main = "Delivery_time Distribution",
        ylab = "Delivery_time",
        col = "lightblue")

boxplot(plotData$Heart, 
        main = "Heart Distribution",
        ylab = "Heart",
        col = "lightblue")

boxplot(plotData$Caesarian, 
        main = "Caesarian Distribution",
        ylab = "Caesarian",
        col = "lightblue")


data <- plotData$Age
length(data)
boxplot(data, plot = FALSE)$out
outliers <- boxplot(data, plot = FALSE)$out
data_no_outlier <- data[-which(data %in% outliers)] 
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)

data <- plotData$weight.kg.
length(data)
boxplot(data, plot = FALSE)$out
outliers <- boxplot(data, plot = FALSE)$out
data_no_outlier <- data[-which(data %in% outliers)] 
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)

data <- plotData$Delivery_number
length(data)
boxplot(data, plot = FALSE)$out
outliers <- boxplot(data, plot = FALSE)$out
data_no_outlier <- data[-which(data %in% outliers)] 
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)


data <- plotData$Delivery_time
length(data)
boxplot(data, plot = FALSE)$out
outliers <- boxplot(data, plot = FALSE)$out
data_no_outlier <- data[-which(data %in% outliers)] 
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)


data <- plotData$Heart
length(data)
boxplot(data, plot = FALSE)$out
outliers <- boxplot(data, plot = FALSE)$out
data_no_outlier <- data[-which(data %in% outliers)] 
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)


data <- plotData$Caesarian
length(data)
boxplot(data, plot = FALSE)$out
outliers <- boxplot(data, plot = FALSE)$out
data_no_outlier <- data[-which(data %in% outliers)] 
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)

duplicated(mydata)
which(duplicated(mydata))

















