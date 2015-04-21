suppressPackageStartupMessages
setwd("~/OneDrive/MIDS/W203/Data/08")

library(boot)
library(ggm)
library(ggplot2)
library(Hmisc)
library(polycor)
library(pastecs)

examData <- read.delim("Exam Anxiety.dat", header = TRUE)

cor(examData[,c("Anxiety","Exam","Revise")], use="complete.obs", method = "kendall")

# create a matrix from the dataframe so that rcorr() can use it
# first remove non-numeric vectors in the data frame
exams <- subset(examData, select = c("Exam","Anxiety","Revise"))
# convert that dataset to a matrix
examMatrix <- as.matrix(exams)

# need to disambiguate rcorr from the version in ggm package
Hmisc::rcorr(as.matrix(examData[, c("Exam", "Anxiety", "Revise")]))

examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
cor(examData2)

cor.test(examData$Anxiety, examData$Exam)
cor.test(examData$Revise, examData$Exam)
cor.test(examData$Anxiety, examData$Revise)

cor(examData2)^2 * 100

liarData <- read.delim("The Biggest Liar.dat", header = TRUE)
liarData
liarHist.creativity <- ggplot(liarData, aes(Creativity), theme(legend.position = "none")) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Creativity Score", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(liarData$Creativity, na.rm = TRUE), sd = sd(liarData$Creativity, na.rm = TRUE)), colour = "black", size = 1)
liarHist.creativity

stat.desc(liarData$Creativity, basic = FALSE, norm = TRUE)

cor(liarData$Position, liarData$Creativity, method = "spearman")
Hmisc::rcorr(as.matrix(liarData[, c("Position", "Creativity")]))

cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman")

# partial correlation for Exam Anxiety while accounting for Revise
pc <- pcor(c("Exam", "Anxiety", "Revise"), var(examData2))
pc^2
pcor.test(pc, 1, 103)

cpData <- read.delim("Chamorro-Premuzic.dat", header = TRUE)
cor(cpData[,c("studentN", "studentE", "studentO", "studentA", "studentC", "lectureN", "lecturE", "lecturO", "lecturA", "lecturC")], use="pairwise.complete.obs", method = "pearson")

# extract just the data in which we're interested
cpMatrix <- as.matrix(cpData[, c("studentN", "studentE", "studentO", "studentA", "studentC", "lectureN", "lecturE", "lecturO", "lecturA", "lecturC")])
Hmisc::rcorr(cpMatrix)

# results -> studentN prefers lecturA r=.10, p < 0.05
# results -> studentE prefers lecturE r=.15, p < 0.05
# results -> studentO does not like lecureN r=-.10, p < 0.05
# results -> studentO prefers lecturO r=0.2, p < 0.01
# results -> studentO does not like lecturA, r=-.16, p < 0.05
