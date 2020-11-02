########################################################################
## These are example codes only
## You are encouraged to use your own codes,
## use other variables, and
## be creative in plotting
## There are many resources on-line and on canvas (home page) for R resources
##########################################################################


library(tidyverse)
library(ggplot2)

#You will need to set the working directory for you
#This ONLY works for L Boyle's computer
setwd("~/Documents/Classes/CET 521 INDE 546 Winter 2020/Survey2020")

#create a file called "survey" from the csv file.
survey <-read.csv("Class_Survey_W20.csv", header=TRUE)

colnames(survey) #names of each variable

#Create a list of the variable names

VarName<-colnames(survey)
VarName  #to see all the variable names
VarName[1]  #to view the first variable name

#the names of the variables are exactly how they are downloaded from Google Form.
#to confirm what variables are listed, you can run the table function as follows:

table(survey[,41])  #Are you a student,...
table(survey[,42])  #Are you a female,...
table(survey[,52])  #Are you a US citizen,...


#can rename using: rename(new variable name = existing variable name)

survey<-survey%>%rename(Tmode=VarName[1])  #change variable 1 to Tmode for Transportation mode
survey<-survey%>%rename(CommuteTime=VarName[2]) #change variable 2 to Commute time   
survey<-survey%>%rename(Age=VarName[43])  #change var 43 to Age   

head(survey,10L)  #to see the first 10 lines of data

#ggplot histogram of commute time
ggplot(survey, aes(x=CommuteTime)) + geom_histogram()

# Change color of histogram to blue
ggplot(survey, aes(x=CommuteTime)) + 
  geom_histogram(color="black", fill="blue")

#create a table to review var50: Do.you.have.a.US.driver.s.license
table(survey[,50])

USD<-survey[,50]  #creating a new variable called USD (for US driver license)

#ggplot boxplot of commute time by US Driver License (yes, no)
ggplot(survey, aes(x=USD, y=CommuteTime)) + 
  geom_boxplot(notch=TRUE)

# Change box plot line colors by groups
ggplot(survey, aes(x=USD, y=CommuteTime, color=USD)) +
  geom_boxplot()

###################################################
# How to create a new file with a subset of the data
###################################################

Example: Suppose I am interested in looking at Lyft/Uber users only

#Step 1: review the question on Lyft/Uber users
table(survey[,9])

#Step 2: create a new file of just those that use Uber/Lyft at least once a week on average
Uber<-survey%>%filter(survey[,9]>=1)  

#check number of rows of observations to ensure I filter correctly
nrow(Uber)  


#review Q12: 
# In.the.past.7.days..how.much.did.you.spend.on.Uber.Lyft.or.other.ride.hailing.apps..in.US.dollars.." 
summary(Uber[,12])

Uber<-Uber%>%rename(Spent=VarName[12]) 
head(Uber$Spent)  #check

ggplot(Uber, aes(x=Spent, y=CommuteTime)) + geom_point()

#create a new variable called "Spender" that separates those that 
# spend more than $25 on Uber/Lyft in the past 7 days

Uber%>%
  mutate(Spender=case_when(Uber[,12]>=25 ~ 1, Uber[,12]<25 ~ 0))  %>% 
  head(10L)  #just print the first 10 lines

#save variable, Spender in file Uber
Uber<-Uber%>%
  mutate(Spender=case_when(Uber[,12]>=25 ~ 1, Uber[,12]<25 ~ 0))

table(Uber$Spender)

ggplot(Uber, aes(x=factor(Spender), y=Age)) +
  geom_boxplot()
  

#You will notice that the above plot includes NA.
#To get rid of NA:
 
 Uber %>% 
 	drop_na(Spender) %>%
 	ggplot(aes(x = factor(Spender),y=Age))+
 	geom_boxplot()
  





