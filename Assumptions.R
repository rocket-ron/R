#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 5 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#----Set the working directory------
setwd("~/Dropbox/Zoe/R Book Chapter 7 Stuff")
setwd("~/Documents/Academic/Data/DSU_R/Chapter 05 (Exploring Data)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

setwd("~/Public/Academic/Data/DSU_R/Chapter 05 (Exploring Data)")
imageDirectory<-"~/Public/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"



names(dlf)

#----Install and Load Packages-----
install.packages("car")
install.packages("ggplot2")
install.packages("pastecs")
install.packages("psych")

library(car)
library(ggplot2)
library(pastecs)
library(psych)
library(Rcmdr)

#Read in the download data:

dlf <- read.delim("DownloadFestival.dat", header=TRUE)

#Remove the outlier from the day1 hygiene score
dlf$day1 <- ifelse(dlf$day1 > 20, NA, dlf$day1)



#Histograms for hygiene scores on day 1, day 2 and day 3.

#Histogram for day 1:

hist.day1 <- ggplot(dlf, aes(day1)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Hygiene score on day 1", y = "Density")
hist.day1

#Histogram for day 2:
hist.day2 <- ggplot(dlf, aes(day2)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Hygiene score on day 2", y = "Density")
hist.day2

#Histogram for day 3:

hist.day3 <- ggplot(dlf, aes(day3)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Hygiene score on day 3", y = "Density")
hist.day3

#Add the curves to the Histograms:

hist.day1 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day1, na.rm = TRUE), sd = sd(dlf$day1, na.rm = TRUE)), colour = "black", size = 1)

ggsave(file = paste(imageDirectory,"05 DLF Day 1 Hist.png",sep="/"))

hist.day2 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day2, na.rm = TRUE), sd = sd(dlf$day2, na.rm = TRUE)), colour = "black", size = 1)

ggsave(file = paste(imageDirectory,"05 DLF Day 2 Hist.png",sep="/"))


hist.day3 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day3, na.rm = TRUE), sd = sd(dlf$day3, na.rm = TRUE)), colour = "black", size = 1)

ggsave(file = paste(imageDirectory,"05 DLF Day 3 Hist.png",sep="/"))


#Q-Q plot for day 1:
qqplot.day1 <- qplot(sample = dlf$day1, stat="qq")
qqplot.day1

ggsave(file = paste(imageDirectory,"05 DLF Day 1 QQ.png",sep="/"))

#Q-Q plot for day 2:

qqplot.day2 <- qplot(sample = dlf$day2, stat="qq")
qqplot.day2
ggsave(file = paste(imageDirectory,"05 DLF Day 2 QQ.png",sep="/"))

#Q-Q plot of the hygiene scores on day 3:
qqplot.day3 <- qplot(sample = dlf$day3, stat="qq")
qqplot.day3
ggsave(file = paste(imageDirectory,"05 DLF Day 3 QQ.png",sep="/"))

#Quantifying normality with numbers
library(psych)		#load the psych library, if you haven't already, for the describe() function.

#Using the describe() function for a single variable.
describe(dlf$day1)

#Two alternative ways to describe multiple variables.
describe(cbind(dlf$day1, dlf$day2, dlf$day3))
describe(dlf[,c("day1", "day2", "day3")])

library(pastecs)
stat.desc(dlf$day1, basic = FALSE, norm = TRUE)

stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3), basic = FALSE, norm = TRUE)

round(stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE), digits = 3)



#Read in R exam data.
rexam <- read.delim("rexam.dat", header=TRUE)

#Set the variable uni to be a factor:
rexam$uni<-factor(rexam$uni, levels = c(0:1), labels = c("Duncetown University", "Sussex University"))

#Self test task:

round(stat.desc(rexam[, c("exam", "computer", "lectures", "numeracy")], basic = FALSE, norm = TRUE), digits = 3)

hexam <- ggplot(rexam, aes(exam)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x = "First Year Exam Score", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(rexam$exam, na.rm = TRUE), sd = sd(rexam$exam, na.rm = TRUE)), colour = "red", size = 1)
hexam
ggsave(file = paste(imageDirectory,"05 Rexam exam Hist.png",sep="/"))


hcomputer <- ggplot(rexam, aes(computer)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x = "Computer Literacy", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(rexam$computer, na.rm = TRUE), sd = sd(rexam$computer, na.rm = TRUE)), colour = "red", size = 1)
hcomputer
ggsave(file = paste(imageDirectory,"05 Rexam computer Hist.png",sep="/"))

hlectures <- ggplot(rexam, aes(lectures)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x = "Percentage of Lectures Attended", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(rexam$lectures, na.rm = TRUE), sd = sd(rexam$lectures, na.rm = TRUE)), colour = "red", size = 1)
hlectures
ggsave(file = paste(imageDirectory,"05 Rexam lectures Hist.png",sep="/"))

hnumeracy <- ggplot(rexam, aes(numeracy)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x = "Numeracy", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(rexam$numeracy, na.rm = TRUE), sd = sd(rexam$numeracy, na.rm = TRUE)), colour = "red", size = 1)
hnumeracy
ggsave(file = paste(imageDirectory,"05 Rexam numeracy Hist.png",sep="/"))



#Use by() to get descriptives for one variable, split by uni
by(data=rexam$exam, INDICES=rexam$uni, FUN=describe)
by(rexam$exam, rexam$uni, stat.desc, basic = FALSE, norm = TRUE)

#Use by() to get descriptives for two variables, split by uni
by(cbind(data=rexam$exam, data=rexam$numeracy), rexam$uni, describe)
by(rexam[, c("exam", "numeracy")], rexam$uni, stat.desc,basic = FALSE, norm = TRUE)


#Use describe for four variables in the rexam dataframe.
describe(cbind(rexam$exam, rexam$computer, rexam$lectures, rexam$numeracy))

#Use by() to get descriptives for four variables, split by uni
by(data=cbind(rexam$exam, rexam$computer, rexam$lectures, rexam$numeracy), rexam$uni, describe)


#Self test:
#Use by() to get descriptives for computer literacy and percentage of lectures attended, split by uni
by(cbind(data=rexam$computer, data=rexam$lectures), rexam$uni, describe)
by(rexam[, c("computer", "lectures")], rexam$uni, stat.desc, basic = FALSE, norm = TRUE)



#using subset to plot histograms for different groups:

dunceData<-subset(rexam, rexam$uni=="Duncetown University")
sussexData<-subset(rexam, rexam$uni=="Sussex University")

hist.numeracy.duncetown <- ggplot(dunceData, aes(numeracy)) + opts(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Numeracy Score", y = "Density") + stat_function(fun=dnorm, args=list(mean = mean(dunceData$numeracy, na.rm = TRUE), sd = sd(dunceData$numeracy, na.rm = TRUE)), colour = "red", size=1)
hist.numeracy.duncetown
ggsave(file = paste(imageDirectory,"05 dunce numeracy Hist.png",sep="/"))

hist.exam.duncetown <- ggplot(dunceData, aes(exam)) + opts(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + labs(x = "First Year Exam Score", y = "Density") + stat_function(fun=dnorm, args=list(mean = mean(dunceData$exam, na.rm = TRUE), sd = sd(dunceData$exam, na.rm = TRUE)), colour = "red", size=1)
hist.exam.duncetown
ggsave(file = paste(imageDirectory,"05 dunce exam Hist.png",sep="/"))


hist.numeracy.sussex <- ggplot(sussexData, aes(numeracy)) + opts(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Numeracy Score", y = "Density") + stat_function(fun=dnorm, args=list(mean = mean(sussexData $numeracy, na.rm = TRUE), sd = sd(sussexData $numeracy, na.rm = TRUE)), colour = "red", size=1)
hist.numeracy.sussex
ggsave(file = paste(imageDirectory,"05 sussex numeracy Hist.png",sep="/"))

hist.exam.sussex <- ggplot(sussexData, aes(exam)) + opts(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + labs(x = "First Year Exam Score", y = "Density") + stat_function(fun=dnorm, args=list(mean = mean(sussexData$exam, na.rm = TRUE), sd = sd(sussexData$exam, na.rm = TRUE)), colour = "red", size=1)
hist.exam.sussex
ggsave(file = paste(imageDirectory,"05 sussex exam Hist.png",sep="/"))

#self test:

dunceData<-subset(rexam, rexam$uni=="Duncetown University")
sussexData<-subset(rexam, rexam$uni=="Sussex University")


hist.computer.duncetown <- ggplot(dunceData, aes(computer)) + opts(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + labs(x = "Computer Literacy", y = "Density") + stat_function(fun=dnorm, args=list(mean = mean(dunceData$computer, na.rm = TRUE), sd = sd(dunceData$computer, na.rm = TRUE)), colour = "red", size=1)
hist.computer.duncetown
ggsave(file = paste(imageDirectory,"05 dunce computer Hist.png",sep="/"))

#To plot a histogram  for percentage of lectures attended for Duncetown University we would execute:

hist.lectures.duncetown <- ggplot(dunceData, aes(lectures)) + opts(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + labs(x = "Percentage of Lectures Attended", y = "Density") + stat_function(fun=dnorm, args=list(mean = mean(dunceData$lectures, na.rm = TRUE), sd = sd(dunceData$lectures, na.rm = TRUE)), colour = "red", size=1)
hist.lectures.duncetown
ggsave(file = paste(imageDirectory,"05 dunce lectures Hist.png",sep="/"))

#To plot a histogram  for computer literacy for Sussex University we would execute:

hist.computer.sussex <- ggplot(sussexData, aes(computer)) + opts(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + labs(x = "Computer Literacy", y = "Density") + stat_function(fun=dnorm, args=list(mean = mean(sussexData$computer, na.rm = TRUE), sd = sd(sussexData$computer, na.rm = TRUE)), colour = "red", size=1)
hist.computer.sussex
ggsave(file = paste(imageDirectory,"05 sussex computer Hist.png",sep="/"))

#To plot a histogram  for percentage of lectures attended for Sussex University we would execute:

hist.lectures.sussex <- ggplot(sussexData, aes(lectures)) + opts(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + labs(x = "Percentage of Lectures Attended", y = "Density") + stat_function(fun=dnorm, args=list(mean = mean(sussexData$lectures, na.rm = TRUE), sd = sd(sussexData$lectures, na.rm = TRUE)), colour = "red", size=1)
hist.lectures.sussex
ggsave(file = paste(imageDirectory,"05 sussex lectures Hist.png",sep="/"))




#Shapiro-Wilks test for exam and numeracy for whole sample
shapiro.test(rexam$exam)
shapiro.test(rexam$numeracy)



#Shapiro-Wilks test for exam and numeracy split by university
by(rexam$exam, rexam$uni, shapiro.test)
by(rexam$numeracy, rexam$uni, shapiro.test)


#qqplots for the two variables
qplot(sample = rexam$exam, stat="qq")
ggsave(file = paste(imageDirectory,"05 exam QQ.png",sep="/"))
qplot(sample = rexam$numeracy, stat="qq")
ggsave(file = paste(imageDirectory,"05 numeracy QQ.png",sep="/"))


#Levene's test for comparison of variances of exam scores in the two universities.
leveneTest(rexam$exam, rexam$uni)
leveneTest(rexam$exam, rexam$uni, center = mean)
leveneTest(rexam$numeracy, rexam$uni)


#########Log, square root, and reciprocal transformation:
dlf$day1LessThanOne <- dlf$day1 < 1
dlf$day1LessThanorEqualOne <- dlf$day1 <= 1
dlf$day1GreaterThanOne <- dlf$day1 > 1
dlf$day1GreaterThanorEqualOne <- dlf$day1 >= 1

#self-help tasks
dlf$logday1 <- log(dlf$day1 + 1)
dlf$logday2 <- log(dlf$day2 + 1)
dlf$logday3 <- log(dlf$day3 + 1)

#Histograms of the log transformed scores:

#Histogram for logday1:

hist.logday1 <- ggplot(dlf, aes(logday1)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Log Transformed Hygiene Score on Day 1", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(dlf$logday1, na.rm = TRUE), sd = sd(dlf$logday1, na.rm = TRUE)), colour = "red", size = 1)
hist.logday1
ggsave(file = paste(imageDirectory,"05 DLF Log Day 1 Hist.png",sep="/"))



#Histogram for logday2:

hist.logday2 <- ggplot(dlf, aes(logday2)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Log Transformed Hygiene Score on Day 2", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(dlf$logday2, na.rm = TRUE), sd = sd(dlf$logday2, na.rm = TRUE)), colour = "red", size = 1)
hist.logday2
ggsave(file = paste(imageDirectory,"05 DLF Log Day 2 Hist.png",sep="/"))


#Histogram for logday3:

hist.logday3 <- ggplot(dlf, aes(logday3)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Log Transformed Hygiene Score on Day 3", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(dlf$logday3, na.rm = TRUE), sd = sd(dlf$logday3, na.rm = TRUE)), colour = "red", size = 1)
hist.logday3
ggsave(file = paste(imageDirectory,"05 DLF Log Day 3 Hist.png",sep="/"))

#Create square root scores

dlf$sqrtday1 <- sqrt(dlf$day1)
dlf$sqrtday2 <- sqrt(dlf$day2)
dlf$sqrtday3 <- sqrt(dlf$day3)

#Histograms of the square root transformed scores:

#Histogram for sqrtday1:

hist.sqrtday1 <- ggplot(dlf, aes(sqrtday1)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Square Root of Hygiene Score on Day 1", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(dlf$sqrtday1, na.rm = TRUE), sd = sd(dlf$sqrtday1, na.rm = TRUE)), colour = "red", size = 1)
hist.sqrtday1
ggsave(file = paste(imageDirectory,"05 DLF sqrt Day 1 Hist.png",sep="/"))



#Histogram for sqrtday2:

hist.sqrtday2 <- ggplot(dlf, aes(sqrtday2)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Square Root of Hygiene Score on Day 2", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(dlf$sqrtday2, na.rm = TRUE), sd = sd(dlf$sqrtday2, na.rm = TRUE)), colour = "red", size = 1)
hist.sqrtday2
ggsave(file = paste(imageDirectory,"05 DLF sqrt Day 2 Hist.png",sep="/"))



#Histogram for sqrtday3:

hist.sqrtday3 <- ggplot(dlf, aes(sqrtday3)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Square Root of Hygiene Score on Day 2", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(dlf$sqrtday3, na.rm = TRUE), sd = sd(dlf$sqrtday3, na.rm = TRUE)), colour = "red", size = 1)
hist.sqrtday3
ggsave(file = paste(imageDirectory,"05 DLF sqrt Day 3 Hist.png",sep="/"))


#Create reciprocal scores

dlf$recday1 <- 1/(dlf$day1 + 1)
dlf$recday2 <- 1/(dlf$day2 + 1)
dlf$recday3 <- 1/(dlf$day3 + 1)

#Histograms of the reciprocal transformed scores:

#Histogram for recday1:

hist.recday1 <- ggplot(dlf, aes(recday1)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Reciprocal of of Hygiene Score on Day 1", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(dlf$recday1, na.rm = TRUE), sd = sd(dlf$recday1, na.rm = TRUE)), colour = "red", size = 1)
hist.recday1
ggsave(file = paste(imageDirectory,"05 DLF rec Day 1 Hist.png",sep="/"))



#Histogram for recday2:

hist.recday2 <- ggplot(dlf, aes(recday2)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Reciprocal of of Hygiene Score on Day 2", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(dlf$recday2, na.rm = TRUE), sd = sd(dlf$recday2, na.rm = TRUE)), colour = "red", size = 1)
hist.recday2
ggsave(file = paste(imageDirectory,"05 DLF rec Day 2 Hist.png",sep="/"))


#Histogram for recday3:


hist.recday3 <- ggplot(dlf, aes(recday3)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Reciprocal of of Hygiene Score on Day 3", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(dlf$recday3, na.rm = TRUE), sd = sd(dlf$recday3, na.rm = TRUE)), colour = "red", size = 1)
hist.recday3
ggsave(file = paste(imageDirectory,"05 DLF rec Day 3 Hist.png",sep="/"))


#Ifelse

dlf$day1NoOutlier <- ifelse(dlf$day1 > 5, NA, dlf$day1)

#----------Smart Alex task 1---------

chickFlick <- read.delim(file="ChickFlick.dat", header=TRUE)

#Use by() to get descriptives for arousal, split by film
by(data=chickFlick$arousal, INDICES=chickFlick$film, FUN=describe)


#Shapiro-Wilks test for arousal split by film
by(data=chickFlick$arousal, INDICES=chickFlick$film, FUN=shapiro.test)


#Levene's test for comparison of variances of arousal scores for the two films.
leveneTest(chickFlick$arousal, chickFlick$film, center=median)

#To plot a histogram  for arousal for Bridget Jones' Diary we would execute:
hist.arousal.Bridget <- ggplot(subset(chickFlick, chickFlick$film=="Bridget Jones' Diary"),aes(arousal)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), fill="white", colour="black", binwidth=1) + labs(x="Arousal", y = "Density") + stat_function(fun=dnorm, args=list(mean=mean(subset(chickFlick, chickFlick$film=="Bridget Jones' Diary")$arousal, na.rm=TRUE), sd=sd(subset(chickFlick, chickFlick$film=="Bridget Jones' Diary")$arousal, na.rm=TRUE) ), colour="black", size=1)
hist.arousal.Bridget


#To plot a histogram  for arousal for Memento we would execute:

hist.arousal.Memento <- ggplot(subset(chickFlick, chickFlick$film=="Memento"),aes(arousal)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), fill="white", colour="black", binwidth=1) + labs(x="Arousal", y = "Density") + stat_function(fun=dnorm, args=list(mean=mean(subset(chickFlick, chickFlick$film=="Memento")$arousal, na.rm=TRUE), sd=sd(subset(chickFlick, chickFlick$film=="Memento")$arousal, na.rm=TRUE) ), colour="black", size=1)
hist.arousal.Memento


#----------Smart Alex task 2---------


#Read in R exam data.
rexam <- read.table(file="rexam.dat", header=TRUE)

#Set the variable uni to be a factor:
rexam$uni<-factor(rexam$uni, levels = c(0:1), labels = c("Duncetown University", "Sussex University"))

rexam$lognumeracy <- log(rexam$numeracy)
rexam$sqrtnumeracy <- sqrt(rexam$numeracy)
rexam$recnumeracy <- 1/(rexam$numeracy)

#Histogram for numeracy:
hist.numeracy <- ggplot(rexam, aes(numeracy)) + 
             opts(legend.position = "none")+ 
              geom_histogram(aes(y=..density..), 
               colour="black", fill="white") + 
             labs(x="Numeracy scores", y = "Density") + 
             stat_function(fun=dnorm, 
               args=list (mean=mean (rexam$numeracy, na.rm=TRUE), 
                sd=sd(rexam$numeracy, na.rm=TRUE) ), 
                colour="black", size=1)
hist.numeracy

#Histogram for log numeracy:

hist.lognumeracy <- ggplot(rexam, aes(lognumeracy)) + 
             opts(legend.position = "none")+ 
              geom_histogram(aes(y=..density..), 
               colour="black", fill="white") + 
             labs(x="Log Transformed Numeracy scores", y = "Density") + 
             stat_function(fun=dnorm, 
               args=list (mean=mean (rexam$lognumeracy, na.rm=TRUE), 
                sd=sd(rexam$lognumeracy, na.rm=TRUE) ), 
                colour="black", size=1)
hist.lognumeracy

#Histogram for square root numeracy:

hist.sqrtnumeracy <- ggplot(rexam, aes(sqrtnumeracy)) + 
             opts(legend.position = "none")+ 
              geom_histogram(aes(y=..density..), 
               colour="black", fill="white") + 
             labs(x="Square Root of Numeracy scores", y = "Density") + 
             stat_function(fun=dnorm, 
               args=list (mean=mean (rexam$sqrtnumeracy, na.rm=TRUE), 
                sd=sd(rexam$sqrtnumeracy, na.rm=TRUE) ), 
                colour="black", size=1)
hist.sqrtnumeracy

#Histogram of the reciprocal transformed numeracy scores:
hist.recnumeracy <- ggplot(rexam, aes(recnumeracy)) + 
             opts(legend.position = "none")+ 
              geom_histogram(aes(y=..density..), 
               colour="black", fill="white") + 
             labs(x="Reciprocal of Numeracy Scores", y = "Density") + 
             stat_function(fun=dnorm, 
               args=list (mean=mean (rexam$recnumeracy, na.rm=TRUE), 
                sd=sd(rexam$recnumeracy, na.rm=TRUE) ), 
                colour="black", size=1)
hist.recnumeracy


#Shapiro-Wilks test for numeracy:
shapiro.test(rexam$numeracy)

#Shapiro-Wilks test for lognumeracy:
shapiro.test(rexam$lognumeracy)

#Shapiro-Wilks test for sqrtnumeracy:
shapiro.test(rexam$sqrtnumeracy)

#Shapiro-Wilks test for recnumeracy:
shapiro.test(rexam$recnumeracy)

