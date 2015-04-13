bplist00Ñ_WebMainResourceÕ	
_WebResourceTextEncodingName_WebResourceFrameName^WebResourceURL_WebResourceData_WebResourceMIMETypeUUTF-8P_]http://www.sagepub.com/dsur/study/DSUR%20R%20Script%20Files/Chapter%207%20DSUR%20Regression.ROOñ<html><head></head><body><pre style="word-wrap: break-word; white-space: pre-wrap;">#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 7 of:
#
#Field, A. P. &amp; Miles, J. N. V. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. London Sage
#
#(c) 2011 Andy P. Field &amp; Jeremy N. V. Miles
#-----------------------------------------------------------------------------------------------------------




#----Set the working directory------
setwd("~/Dropbox/Team Field/DSUR/DSUR_2/DSUR2 Data Files/Chapter 07 (Regression)")
setwd("~/Documents/Academic/Data/DSU_R/Chapter 07 (Regression)")
imageDirectory&lt;-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

setwd("~/Public/Academic/Data/DSU_R/Chapter 07 (Regression)")
imageDirectory&lt;-"~/Public/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"



#----Install Packages-----
install.packages("QuantPsyc")
install.packages("car")


#------And then load these packages, along with the boot package.-----
library(QuantPsyc)
library(car)
library(boot)
library(Rcmdr)


#Jane superbrain box
pubs&lt;-read.delim("pubs.dat", header = TRUE)
pubReg &lt;- lm(mortality ~ pubs, data = pubs)
summary(pubReg)
resid(pubReg)
rstandard(pubReg)
rstudent(pubReg)

PearsonResidual &lt;- (resid(pubReg)-mean(resid(pubReg)))/sd(resid(pubReg))

#----run the command to access the album1 data-----
album1&lt;-read.delim("Album Sales 1.dat", header = TRUE)

#----run the simple linear regression model---
albumSales.1 &lt;- lm(sales ~ adverts, data = album1)
summary(albumSales.1)
sqrt(0.3346)

#----access the album2 data----
album2&lt;-read.delim("Album Sales 2.dat", header = TRUE)


#---Run the multiple regression model----
albumSales.2&lt;-lm(sales ~ adverts, data = album2)
albumSales.3&lt;-lm(sales ~ adverts + airplay + attract, data = album2)
summary(albumSales.2)
summary(albumSales.3)

#---We can obtain standardized parameter estimates with the lm.beta() function---
lm.beta(albumSales.3)
#---Confidence intervals are obtained with the confint() function----
confint(albumSales.3)

#----To compare the R2 in two models, use the ANOVA command---
anova(albumSales.2, albumSales.3)


#----Obtain casewise diagnostics and add them to the original data file.---

album2$residuals&lt;-resid(albumSales.3)
album2$standardized.residuals &lt;- rstandard(albumSales.3)
album2$studentized.residuals &lt;- rstudent(albumSales.3)
album2$cooks.distance&lt;-cooks.distance(albumSales.3)
album2$dfbeta &lt;- dfbeta(albumSales.3)
album2$dffit &lt;- dffits(albumSales.3)
album2$leverage &lt;- hatvalues(albumSales.3)
album2$covariance.ratios &lt;- covratio(albumSales.3)

#Save file
write.table(album2, "Album Sales With Diagnostics.dat", sep = "\t", row.names = FALSE)
#look at the data (and round the values)
round(album2, digits = 3)


#----List of standardized residuals greater than 2--------------
album2$standardized.residuals&gt;2| album2$standardized.residuals &lt; -2

#---Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2.----------
album2$large.residual &lt;- album2$standardized.residuals &gt; 2 | album2$standardized.residuals &lt; -2

#---Count the number of large residuals-------------
sum(album2$large.residual)


#---Display the value of sales, airplay, attract, adverts, and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------
album2[album2$large.residual,c("sales", "airplay", "attract", "adverts", "standardized.residuals")]

#-----Cook's distance, leverage and covariance ratio for cases with large residuals.---------
album2[album2$large.residual , c("cooks.distance", "leverage", "covariance.ratios")]


#----The Durbin-Watson test is obtained with either dwt() or durbinWatsonTest()---
durbinWatsonTest(albumSales.3)
dwt(albumSales.3)

#----Obtaining the VIF---
vif(albumSales.3)

#----The tolerance is 1/VIF---
1/vif(albumSales.3)

#----The mean VIF---
mean(vif(albumSales.3))


#---Histogram of studentized residuals---

hist(album2$studentized.residuals)
hist(rstudent(albumSales.3))

#--Plot of residuals against fitted (predicted) values, with a flat line at the mean--
plot(albumSales.3$fitted.values,rstandard(albumSales.3))
abline(0, 0)

#same as above
plot(albumSales.3)

#Publication quality graphs

album2$fitted &lt;- albumSales.3$fitted.values

histogram&lt;-ggplot(album2, aes(studentized.residuals)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x = "Studentized Residual", y = "Density")
histogram + stat_function(fun = dnorm, args = list(mean = mean(album2$studentized.residuals, na.rm = TRUE), sd = sd(album2$studentized.residuals, na.rm = TRUE)), colour = "red", size = 1)
ggsave(file = paste(imageDirectory,"07 album sales ggplot Hist.png",sep="/"))

scatter &lt;- ggplot(album2, aes(fitted, studentized.residuals))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")+ labs(x = "Fitted Values", y = "Studentized Residual") 
ggsave(file=paste(imageDirectory,"07 Album sales ggplot scatter.png",sep="/"))

qqplot.resid &lt;- qplot(sample = album2$studentized.residuals, stat="qq") + labs(x = "Theoretical Values", y = "Observed Values") 
qqplot.resid
ggsave(file=paste(imageDirectory,"07 Album sales ggplot QQ.png",sep="/"))


#---R tends to give values to too many decimal places, you can usefully round these values to 2 decimals.
round(rstandard(albumSales.3), 2)



##------Bootstrapping------
#---Write a bootstrap function.
object&lt;-boot(data,function,replications)


bootReg&lt;-function(formula, data, i)
{
	d &lt;- data[i,]
	fit &lt;- lm(formula, data = d)
	return(coef(fit))
	}

#----bootstrapping our regression model, with 2000 replications---
bootResults&lt;-boot(statistic = bootReg, formula = sales ~ adverts + airplay + attract, data = album2, R = 2000,)

#---We can then obtaine the bootstrap confidence intervals for the intercept:---
boot.ci(bootResults, type = "bca", index = 1)

#---And the three slope estimates---
boot.ci(bootResults, type = "bca", index = 2)
boot.ci(bootResults, type = "bca", index = 3)
boot.ci(bootResults, type = "bca", index = 4)

#-----Read in data for Glastonbury Festival Regression----
gfr&lt;-read.delim(file="GlastonburyFestivalRegression.dat", header = TRUE)

#Print the first 10 cases of the dataframe
head(gfr, n = 10)
#set contrasts quickly
contrasts(gfr$music)&lt;-contr.treatment(4, base = 4)
#set contrasts with helpful names

crusty_v_NMA&lt;-c(1, 0, 0, 0)
indie_v_NMA&lt;-c(0, 1, 0, 0)
metal_v_NMA&lt;-c(0, 0, 1, 0)
contrasts(gfr$music)&lt;-cbind(crusty_v_NMA, indie_v_NMA, metal_v_NMA)



#----Exactly the same results can be obtained with------
glastonburyModel&lt;-lm(change ~ music, data = gfr) 
summary(glastonburyModel)


#---To produce group means of each of the four groups-----
round(tapply(gfr$change, gfr$music, mean, na.rm=TRUE), 3)


#********************Labcoat Leni*******************************

#Load data &amp; set gender to be a factor

PersonalityData&lt;-read.delim("Chamorro-Premuzic.dat", header = TRUE)
PersonalityData$Gender&lt;-factor(PersonalityData$Gender, levels = c(0:1), labels = c("Female", "Male"))

#Create dataframes containing variables for each analysis (need to do this because of missing values). Drop variables not in analysis
dropVars&lt;-names(PersonalityData) %in% c("lecturerE","lecturerO", "lecturerA", "lecturerC")
neuroticLecturer&lt;-PersonalityData[!dropVars]

dropVars&lt;-names(PersonalityData) %in% c("lecturerN","lecturerO", "lecturerA", "lecturerC")
extroLecturer&lt;-PersonalityData[!dropVars]

dropVars&lt;-names(PersonalityData) %in% c("lecturerE","lecturerN", "lecturerA", "lecturerC")
openLecturer&lt;-PersonalityData[!dropVars]

dropVars&lt;-names(PersonalityData) %in% c("lecturerE","lecturerO", "lecturerN", "lecturerC")
agreeLecturer&lt;-PersonalityData[!dropVars]

dropVars&lt;-names(PersonalityData) %in% c("lecturerE","lecturerO", "lecturerA", "lecturerN")
concLecturer&lt;-PersonalityData[!dropVars]

#Delete cases with any missing values on any variable
neuroticLecturer &lt;-neuroticLecturer[complete.cases(neuroticLecturer),]
extroLecturer &lt;-extroLecturer[complete.cases(extroLecturer),]
openLecturer &lt;-openLecturer[complete.cases(openLecturer),]
agreeLecturer &lt;-agreeLecturer[complete.cases(agreeLecturer),]
concLecturer &lt;-concLecturer[complete.cases(concLecturer),]

#-----Neurotic Lecturer-----------
#-----Create two models-------
LecturerN.1&lt;- lm(lecturerN ~ Age + Gender, data= neuroticLecturer)
LecturerN.2 &lt;- lm(lecturerN ~ Age + Gender + studentN + studentE + studentO + studentA + studentC, data= neuroticLecturer)
#-----Run an anova to compare the two models------
anova(LecturerN.1, LecturerN.2)
#-----To obtain output----
summary(LecturerN.1)
summary(LecturerN.2)
#----Statistics------
vif(LecturerN.2)
dwt(LecturerN.2)

#---Histogram-----
hist(rstudent(LecturerN.2))

#-----Confidence intervals-----
confint(LecturerN.2)

##-----obtain the standardized beta estimates:------
install.packages("QuantPsyc")
Library(QuantPsyc)
lm.beta(LecturerN.1)
lm.beta(LecturerN.2)
#-----Extroverted Lecturer-----------
#----Create two models-------
LecturerE.1 &lt;- lm(lecturerE ~ Age + Gender, data=extroLecturer)
LecturerE.2 &lt;- lm(lecturerE ~ Age + Gender + studentN + studentE + studentO + studentA + studentC, data= extroLecturer)
#-----Run an anova to compare the two models------
anova(LecturerE.1, LecturerE.2)
#-----To obtain output----
summary(LecturerE.1)
summary(LecturerE.2)
#----Statistics------
vif(LecturerE.2)
dwt(LecturerE.2)

#---Histogram-----
hist(rstudent(LecturerE.2))

#-----Confidence intervals-----
confint(LecturerE.2)

##-----obtain the standardized beta estimates:------
install.packages("QuantPsyc")
Library(QuantPsyc)
lm.beta(LecturerE.1)
lm.beta(LecturerE.2)
#-----Openness to Experience Lecturer-----------
#----Create two models-------
LecturerO.1 &lt;- lm(lecturerO ~ Age + Gender, data=openLecturer)
LecturerO.2 &lt;- lm(lecturerO ~ Age + Gender + studentN + studentE + studentO + studentA + studentC, data=openLecturer)
#-----Run an anova to compare the two models------
anova(LecturerO.1, LecturerO.2)
#-----To obtain output----
summary(LecturerO.1)
summary(LecturerO.2)
#----Statistics------
vif(LecturerO.2)
dwt(LecturerO.2)

#---Histogram-----
hist(rstudent(LecturerO.2))

#-----Confidence intervals-----
confint(LecturerO.2)

##-----obtain the standardized beta estimates:------

lm.beta(LecturerO.1)
lm.beta(LecturerO.2)
#-----Agreeableness Lecturer-----------
#----Create two models-------
LecturerA.1 &lt;- lm(lecturerA ~ Age + Gender, data=agreeLecturer)
LecturerA.2 &lt;- lm(lecturerA ~ Age + Gender + studentN + studentE + studentO + studentA + studentC,data=agreeLecturer)
#-----Run an anova to compare the two models------
anova(LecturerA.1, LecturerA.2)
#-----To obtain output----
summary(LecturerA.1)
summary(LecturerA.2)
#----Statistics------
vif(LecturerA.2)
dwt(LecturerA.2)

#---Histogram-----
hist(rstudent(LecturerA.2))

#-----Confidence intervals-----
confint(LecturerA.2)

##-----obtain the standardized beta estimates:------

lm.beta(LecturerA.1)
lm.beta(LecturerA.2)
#-----Concientious Lecturer-----------

#----Create two models-------
LecturerC.1 &lt;- lm(lecturerC ~ Age + Gender, data=concLecturer)
LecturerC.2 &lt;- lm(lecturerC ~ Age + Gender + studentN + studentE + studentO + studentA + studentC,data=concLecturer)
#-----Run an anova to compare the two models------
anova(LecturerC.1, LecturerC.2)
#-----To obtain output----
summary(LecturerC.1)
summary(LecturerC.2)
#----Statistics------
vif(LecturerC.2)
dwt(LecturerC.2)

#---Histogram-----
hist(rstudent(LecturerC.2))

#-----Confidence intervals-----
confint(LecturerC.2)

##-----obtain the standardized beta estimates:------

lm.beta(LecturerC.1)
lm.beta(LecturerC.2)

#*********************Smart Alex********************

#---Task 1------
#load in the pubs.dat data:

pubs&lt;-read.delim("pubs.dat", header = TRUE)

#create a regression model to predict mortality from number of pubs:

pubsReg &lt;-lm(mortality ~ pubs, data = pubs)

#obtain output of the regression:

summary(pubsReg)

#--Bootstrap the regression parameters:
#first execute the bootreg() function from the book chapter.

#We can then use the function to obtain the bootstrap samples:
bootResults&lt;-boot(statistic = bootReg, formula = mortality ~ pubs, data = pubs, R = 2000)

#Obtain the bootstrap confidence intervals for the intercept and slope:
boot.ci(bootResults, type = "bca", index = 1)

boot.ci(bootResults, type = "bca", index = 2)

#---Task 2------

#load in the Supermodel.dat data--

Supermodel&lt;-read.delim("Supermodel.dat", header = TRUE)

#----create a regression model to predict salery from Age, number of years being a supermodel and beauty-----
Supermodel.1 &lt;- lm(salary~age + beauty + years, data= Supermodel)

#--obtain output of the regression---

summary(Supermodel.1)

##-----obtain the standardized beta estimates:------

lm.beta(Supermodel.1)

##---is the model valid?----
vif(Supermodel.1)
1/vif(Supermodel.1)
dwt(Supermodel.1)
resid(Supermodel.1)
rstandard(Supermodel.1)

#----Histogram-----
hist(rstandard(Supermodel.1))

##---Plot of the standardized residuals----- 
plot(Supermodel.1$fitted.values,rstandard(Supermodel.1))

#---It also helps to add a horizontal line at the mean--
abline(0,0)

#To obtain some other plots, we can use the plot() function:

plot(Supermodel.1)
#----Obtain casewise diagnostics and add them to the original data 
Supermodel$cooks.distance&lt;-cooks.distance(Supermodel.1)
Supermodel$residuals&lt;-resid(Supermodel.1)
Supermodel$standardized.residuals &lt;- rstandard(Supermodel.1)
Supermodel$studentized.residuals &lt;- rstudent(Supermodel.1)
Supermodel$dfbeta &lt;- dfbeta(Supermodel.1)
Supermodel$dffit &lt;- dffits(Supermodel.1)
Supermodel$leverage &lt;- hatvalues(Supermodel.1)
Supermodel$covariance.ratios &lt;- covratio(Supermodel.1)

#----List of standardized residuals greater than 2--------------
Supermodel$standardized.residuals&gt;2| Supermodel$standardized.residuals &lt; -2

#---Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2.----------
Supermodel$large.residual &lt;- Supermodel$standardized.residuals &gt; 2| Supermodel$standardized.residuals &lt; -2

#---Count the number of large residuals-------------
sum(Supermodel$large.residual)

#-----If we want to display only some of the variables we can use:----
Supermodel[,c("salary", "age", "beauty", "years", "standardized.residuals")]

#---Display the value of salary, age, beauty, years, and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------

Supermodel[Supermodel$large.residual,c("salary", "age", "beauty", "years", "standardized.residuals")]

#------Task 3-------------------------

#-----Read in data for Glastonbury Festival Regression----

gfr&lt;-read.delim("GlastonburyFestivalRegression.dat", header=TRUE)

#---Create three dummy variables. Make sure you don't do this if there are missing data.---
gfr$crusty&lt;-gfr$music=="Crusty"
gfr$metaller&lt;-gfr$music=="Metaller"
gfr$indie.kid&lt;-gfr$music=="Indie Kid"

#---Create a regression model---------

gfr.1 &lt;- lm(gfr$change ~ gfr$crusty + gfr$metaller + gfr$indie.kid, data=gfr)
summary(gfr.1)

##---is the model valid?----
vif(gfr.1)
1/vif(gfr.1)

# The DurbinÃ¢â‚¬â€œWatson statistic: 

dwt(gfr.1)

#----Histogram-----
hist(rstandard(gfr.1))

##---Plot of the standardized residuals----- 
plot(gfr.1$fitted.values,rstandard(gfr.1))

#---It also helps to add a horizontal line at the mean--
abline(0,0)

#To obtain some other plots, we can use the plot() function:
plot(gfr.1)

#----Obtain casewise diagnostics and add them to the original data 
gfr$cooks.distance&lt;-cooks.distance(gfr.1)
gfr$residuals&lt;-resid(gfr.1)
gfr$standardized.residuals&lt;-rstandard(gfr.1)
gfr$studentized.residuals&lt;-rstudent(gfr.1)
gfr$dfbeta&lt;-dfbeta(gfr.1)
gfr$dffit&lt;-dffits(gfr.1)
gfr$leverage&lt;-hatvalues(gfr.1)
gfr$covariance.ratios&lt;-covratio(gfr.1)

#----List of standardized residuals greater than 2--------------
gfr$standardized.residuals&gt;2| gfr$standardized.residuals &lt; -2

#---Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2.----------
gfr$large.residual &lt;- gfr$standardized.residuals &gt; 2| gfr$standardized.residuals &lt; -2

#---Count the number of large residuals-------------
sum(gfr$large.residual)

#-----If we want to display only some of the variables we can use:----
gfr[,c("change", "crusty", "metaller", "indie.kid", "standardized.residuals")]

#---Display the value of change, crusty, metaller, indie.kid, and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------

gfr[gfr$large.residual,c("change", "crusty", "metaller", "indie.kid", "standardized.residuals")]

#------Task 4----------

#-----Read in data for Child Aggression----

ChildAggression&lt;-read.delim("ChildAggression.dat", header = TRUE)

#---Conduct the analysis hierarhically entering parenting style and sibling aggression in the first step-------

ChildAggression.1&lt;-lm(Aggression ~ Sibling_Aggression + Parenting_Style, data = ChildAggression)

#------And the remaining variables in a second step-----

ChildAggression.2&lt;-lm(Aggression ~ Sibling_Aggression+Parenting_Style+ Diet + Computer_Games + Television, data=ChildAggression)

#----View the output of the two regressions---

summary(ChildAggression.1)
summary(ChildAggression.2)

#----To compare the R2 in two models, use the ANOVA command---

anova(ChildAggression.1, ChildAggression.2)

#---VIF------

vif(ChildAggression.1)
1/vif(ChildAggression.1)

vif(ChildAggression.2)
1/vif(ChildAggression.2)


#----The Durbin-Watson test is obtained with either dwt() or durbinWatsonTest()---

durbinWatsonTest(ChildAggression.1)
dwt(ChildAggression.2)

#---Histogram of standardized residuals---

hist(rstandard(ChildAggression.2))

#--Plot of residuals against fitted (predicted) values, with a flat line at the mean--
plot(ChildAggression.2$fitted.values,rstandard(ChildAggression.2))
abline(0, 0)

#---We can obtain standardized parameter estimates with the lm.beta() function---

lm.beta(ChildAggression.1)
lm.beta(ChildAggression.2)

#---Confidence intervals are obtained with the confint() function----
confint(ChildAggression.2)

#----You can round them to make life easier----
round(confint(ChildAggression.2), 2)

#To obtain some other plots, we can use the plot() function:

plot(ChildAggression.2)

#----Obtain casewise diagnostics and add them to the original data 
ChildAggression$cooks.distance&lt;-cooks.distance(ChildAggression.2)
ChildAggression$residuals&lt;-resid(ChildAggression.2)
ChildAggression$standardized.residuals &lt;- rstandard(ChildAggression.2)
ChildAggression$studentized.residuals &lt;- rstudent(ChildAggression.2)
ChildAggression$dfbeta &lt;- dfbeta(ChildAggression.2)
ChildAggression$dffit &lt;- dffits(ChildAggression.2)
ChildAggression$leverage &lt;- hatvalues(ChildAggression.2)
ChildAggression$covariance.ratios &lt;- covratio(ChildAggression.2)

#----List of standardized residuals greater than 2--------------
ChildAggression$standardized.residuals&gt;2| ChildAggression$standardized.residuals &lt; -2

#---Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2.----------
ChildAggression$large.residual &lt;- ChildAggression$standardized.residuals &gt; 2| ChildAggression$standardized.residuals &lt; -2

#---Count the number of large residuals-------------
sum(ChildAggression$large.residual)

#-----If we want to display only some of the variables we can use:----
ChildAggression[,c("Aggression", "Sibling_Aggression","Parenting_Style","Diet","Computer_Games", "Television", "standardized.residuals")]

#---Display the value of Aggression, Parenting_Style, Diet, Computer_Games and Television and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------

ChildAggression[ChildAggression$large.residual,c("Aggression", "Sibling_Aggression","Parenting_Style","Diet","Computer_Games", "Television", "standardized.residuals")]

</pre></body></html>Ztext/plain    ( F ] l ~ ” š › ûPð                           Pû