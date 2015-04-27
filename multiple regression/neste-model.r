# Today's Agenda
# Using the 1994 GSS you will examine the dependent variable “partyid”, which is “political party identification” 
# (ie., strong democrat to strong republican on a 7-point scale). You will treat this as a metric variable for the purposes of this investigation.
# Your task: 
# 0) load the GSS1994_daughters dataset.
# 1) Recode “sex of the first child” (kdsex1) into a dummy variable (daughter1) where female is coded as 1 or ‘true’.  
# Examine partyid and turn it into a numeric variable (the as.numeric function should be helpful here).
# 2) Conduct a linear regression with “daughter1” as your independent variable.  
# 3) Conduct another linear regression with “daughter1” and one or more additional independent variables.  You may use the following function to view the description
# of each variable:
# desc = function(var, df) {
# 	attr(df,"var.labels")[match(var, colnames(df))]
# }
# For example, typing desc(“partyid”, GSS) will return a description of partyid.  You may also use the following command to pull up a table of all variables and descriptions:
# data.frame(names = colnames(GSS), labels = attr(GSS, "var.labels"))
# 4) Assess the improvement, if any, from the first model to the second model.
# 5) Did the coefficient for daughter1 change from the first model to the second model? If so, how do you explain this change?


# X (first child) -> Y (Party ID)

# Z = control, want it to be exogenous to X and Y.

# Post-treatment bias 

# X -> Z -> Y

# X -> Y will be downwardly biased.  


setwd('C:/Users/thomasa/Downloads/MIDS/EAD/Week13')
load('daughters.rdata')

#1)
#dummy code the daughter variable
GSS$daughter1 = as.numeric(GSS$kdsex1 == 'female')

#create numeric party variable
GSS$party_num = as.numeric(GSS$partyid)

#Other party is coded as 8 so recode to NA
GSS$party_num[GSS$party_num==8] = NA

#check distribution
table(GSS$party_num)
hist(GSS$party_num)

#2)
mod = lm(party_num ~ daughter1,data=GSS)
mod
summary(mod)
plot(mod)

#3)
#not sure what this does
desc = function(var, df) {
 	attr(df,"var.labels")[match(var, colnames(df))]
}
desc('age',GSS)

#Try Age
mod1 = lm(party_num ~ daughter1 + age,data=GSS)
mod1
summary(mod1)
plot(mod1)
#not statistically significant - don't include

#Try Race
#recode to remove iap
GSS$race = factor(as.character(GSS$race),levels=c('black','white','other'))

#contrasts
contrasts(GSS$race)

#model
mod2 = lm(party_num ~ daughter1 + race,data=GSS)
mod2
summary(mod2)
plot(mod2)



# 4)
#model comparison base with Age
AIC(mod)
AIC(mod1)

#anova
anova(mod,mod1)
#no improvement

#try race
AIC(mod)
AIC(mod2)
anova(mod,mod2)

# 5) Does the effect of daughter1 change from base model (daughter1) to model2 (daughter1 + race)
# highly overlapping confidence levels
confint(mod)
confint(mod2)


t.test(GSS$party_num[GSS$race=='black'],GSS$party_num[GSS$race=='other'])



#income
cbind(as.character(GSS$income),as.numeric(gsub('.*\\$| .*','',as.character(GSS$income))))


mod3 = lm(party_num ~ daughter1 + race + polviews,data=GSS)
mod3
summary(mod3)
plot(mod3)




