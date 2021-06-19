

library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)



#setting working directory
getwd()
setwd("~/")

#read and import dataset
dta=read.csv("/healthcare-dataset-stroke-data.csv",header = TRUE,sep=",",as.is=F)

#display first 5 entries
head(dta,5)

#finding sized/dimension of dataset
dim(dta)

#viewing attributes and their data types
str(dta)


#showing summary of data attributes and their plots

summary(dta$age)
hist(dta$age,prob=T,col = "red")
x <- dta$age
h<-hist(x, breaks=15, col="red", xlab="age in yeras",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

help("hist")

summary(dta$avg_glucose_level)
class(dta$avg_glucose_level)
gl_sorted<- sort(dta$avg_glucose_level)
plot(dta$avg_glucose_level,ylab = "average glucose level")
boxplot(dta$avg_glucose_level)

summary(dta$gender)
plot(dta$gender)

summary(dta$hypertension)
hist(dta$hypertension)

summary(dta$heart_disease)
hist(dta$heart_disease)

summary(dta$ever_married)
plot(dta$ever_married,main=" Ever married")

summary(dta$work_type)
plot(dta$work_type,main="Work Type",horiz=TRUE)

summary(dta$bmi)
plot(dta$bmi)

plot(dta$stroke,horiz=TRUE,main="people with and without Strokes",xlab="no.of people",ylab="stroke status")


#viewing headers
names(dta)

#checking errors in data
colSums(is.na(dta))
colSums(dta == "N/A")
#checking for duplicate values
n_occur <- data.frame(table(dta$id))
n_occur[n_occur$Freq > 1,]

sum(dta$gender == "Other")

# removing rows with N/A values of bmi and "other" value of gender
# and converting attributes into suitable data type

dta <- dta %>%
  filter(
    bmi != "N/A",
    gender != "Other"
  ) %>%
  mutate(
    gender = factor(gender),
    hypertension = factor(hypertension),
    heart_disease = factor(heart_disease),
    ever_married = factor(ever_married),
    work_type = factor(work_type),
    Residence_type = factor(Residence_type),
    bmi = as.numeric(bmi),
    smoking_status = factor(smoking_status),
    stroke = factor(stroke),
  ) 

str(dta)
dim(dta)

#removing column dta$id
dta<-dta[ ,2:12]

summary(dta)

#visualizing stroke occurence for each attribute 

dta %>%
  ggplot(aes(x = gender, fill = stroke)) +
  geom_bar()

dta %>%
  ggplot(aes(x = hypertension, fill = stroke)) +
  geom_bar()

dta %>%
  ggplot(aes(x = heart_disease, fill = stroke)) +
  geom_bar()

dta %>%
  ggplot(aes(x = ever_married, fill = stroke)) +
  geom_bar()

dat_stroke %>%
  ggplot(aes(x = work_type, fill = stroke)) +
  geom_bar()

dta %>%
  ggplot(aes(x = Residence_type, fill = stroke)) +
  geom_bar()

dta %>%
  ggplot(aes(x = smoking_status, fill = stroke)) +
  geom_bar()


# now visualizing people with stroke with certain attributes

dat_prop <- dta %>%
  group_by(gender) %>%
  summarise(prop = sum(stroke == "1")/length(gender))

dat_prop

dat_prop %>%
  ggplot(aes(x = gender, y = prop)) +
  geom_col(fill = "#5C43AF")

dat_prop <- dta %>%
  group_by(hypertension) %>%
  summarise(prop = sum(stroke == "1")/length(hypertension))

dat_prop %>%
  ggplot(aes(x = hypertension, y = prop)) +
  geom_col(fill = "#023FC4")

dat_prop <- dta %>%
  group_by(heart_disease) %>%
  summarise(prop = sum(stroke == "1")/length(heart_disease))

 dat_prop %>%
  ggplot(aes(x = heart_disease, y = prop)) +
  geom_col(fill = "#00BFC4")

dat_prop <- dta %>%
  group_by(ever_married) %>%
  summarise(prop = sum(stroke == "1")/length(ever_married))

dat_prop %>%
  ggplot(aes(x = ever_married, y = prop)) +
  geom_col(fill = "#7FD0F9")

dat_prop <- dta %>%
  group_by(work_type) %>%
  summarise(prop = sum(stroke == "1")/length(work_type))

dat_prop %>%
  ggplot(aes(x = work_type, y = prop)) +
  geom_col(fill = "#00BF89")

dat_prop <- dta %>%
  group_by(Residence_type) %>%
  summarise(prop = sum(stroke == "1")/length(Residence_type))

dat_prop %>%
  ggplot(aes(x = Residence_type , y = prop)) +
  geom_col(fill = "#39DA24")

dat_prop <- dta %>%
  group_by(smoking_status) %>%
  summarise(prop = sum(stroke == "1")/length(smoking_status))

dat_prop %>%
  ggplot(aes(x = smoking_status, y = prop)) +
  geom_col(fill = "#EF5FC4")


# plotting boxplot for all numeric attributes

boxplot(dta$bmi)
summary((dta$bmi))

boxplot(dta$age)
summary(dta$age)

boxplot(dta$avg_glucose_level,dta$bmi)
summary(dta$avg_glucose_level)

#viewing age of persons according to their smoking status and stroke 

dta %>%
  ggplot(aes(x = smoking_status, y = age, color = stroke)) +
  geom_boxplot()

#ploting genome density curve of strokes related to numeric attributes

dta %>%
  ggplot(aes(x = age, fill = stroke)) +
  geom_density(alpha = 0.5) +
  theme(legend.position="none")

dta %>%
  ggplot(aes(x = avg_glucose_level, fill = stroke)) +
  geom_density(alpha = 0.5) +
  theme(legend.position="none")

dta %>%
  ggplot(aes(x = bmi, fill = stroke)) +
  geom_density(alpha = 0.5) +
  theme(legend.position="none")

