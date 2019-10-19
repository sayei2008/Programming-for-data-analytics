# Reading and Preparing Data as given in the assignment
library(readxl)
library(ggplot2)
# http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets
orig_list <- data.frame(readxl::read_excel("datasets/Titanic/titanic3_assignment.xls"))
plist <- orig_list
dim(plist)
summary(plist)
# Converting survived to a logical value using as.logical
plist$survived<-as.logical(plist$survived)
summary(plist)
# Changing class to string using as.character in pclass
plist$pclass<-as.character(plist$pclass)
plist$pclass[plist$pclass=="1"]<-"First" #changing 1 to first
plist$pclass[plist$pclass=="2"]<-"Second"#changing 2 to second
plist$pclass[plist$pclass=="3"]<-"Third"#changing 3 to third
summary(plist)
unique(plist$pclass)#Cross checking as given in assignment question
# Simple imputation of age (mean of all ages) using is.na and replacing them with mean of age
plist$age[is.na(plist$age)] = mean(plist$age, na.rm=TRUE)
summary(plist)
# Simple imputation of fare  (mean of all fares) using is.na and replacing them with mean of fare
plist$fare[is.na(plist$fare)] = mean(plist$fare, na.rm=TRUE)
summary(plist)
# Simple imputation of place of embarking (randomly generated) with seed of 99
set.seed(99)#replacing all na with either S or C or Q randomly
plist$embarked[is.na(plist$embarked)] <- sample(c("S","C","Q"), size=sum(is.na(plist$embarked)), replace = T)
summary(plist)
unique(plist$embarked)
# Create new category (age cohort)
plist$age_cohort <- NA#creating new column age cohort
plist$age_cohort[plist$age>=16 && plist$age<60]<-"Adult" #providing conditions as given in question
plist$age_cohort[plist$age<16]<-"Child"
plist$age_cohort[plist$age>=60]<-"Elderly"
summary(plist)
#Put in full town origin (Queenstown (Q) replaced by Cobh)
plist$embarked[plist$embarked=="S"]<-"Southampton"#replacing S with Southampton 
plist$embarked[plist$embarked=="C"]<-"Cherbourg"#replacing C with Cherbourg
plist$embarked[plist$embarked=="Q"]<-"Cobh"# replacing Q with Cobh
summary(plist)
unique(plist$embarked)
# Double checking dataset with the given conditions in assignment
head(plist)
dim(plist)
table(plist$survived, plist$sex)
table(plist$survived)
table(plist$survived, plist$pclass)
table(plist$survived, plist$age_cohort)
table(plist$survived, plist$embarked)
#ggplot() is the main function for performing the plot
#ggtitle() is used to provide the title of the given plot
#theme() is used to set the position of the legend properly and removing extra junk wordings
#geom_bar() is used to generate bar plot
#geom_point() is used to generate scatter plot
#xlab() is used to give x axis label and ylab() is used to give y axis label 
#Plot1
ggplot(data=plist) +ggtitle("Survival Numbers by Travel Class") +
#providing the data and giving the title name for the plot
theme(legend.position="top", legend.title = element_blank()) +
#giving the legend position as top and removing extra values to match the output given in the assignment
geom_bar(mapping = aes(x = survived, fill=pclass)) +
#mapping the bar graph and filling it with pclass values to differentiate with colours and providing stat="count" to get the number of people and providing width value to match the output as given in the assignment
ylab("Number") +  xlab("Survived") # assigning the label names for x and y axis respectively
#plotting bar graph with x axis as survived and y axis as the number of people who survived based on the class differentiated by colour in y axis
#Plot2
ggplot(data=plist) +ggtitle("Survival Numbers by Gender") +
theme(legend.position="top", legend.title = element_blank()) +
geom_bar(mapping = aes(x = survived, fill=sex)) +
ylab("Number") + xlab("Survived")
#plotting bar graph with survived as x axis and y axis as number of people survived based on gender differentiated in colour in y axis
#Plot3
ggplot(data=plist) +ggtitle("Survival Numbers by Age Cohort") +
theme(legend.position="top", legend.title = element_blank()) +
geom_bar(mapping = aes(x = survived, fill=age_cohort)) +
ylab("Number") +xlab("Survived")
#Plotting bar graph with survived as x axis and number of people survived based on age cohort differentiated in colour in y axis
#Plot4
ggplot(data=plist) + ggtitle("Survival Numbers by Embarkation Location") +
theme(legend.position="top", legend.title = element_blank()) +
geom_bar(mapping = aes(x = survived, fill=embarked)) +
ylab("Number") +xlab("Survived")
#Plotting bar graph with survived as X axis and number of people who survived based on embarkation location differentiated in colour in y axos
#Plot5
ggplot(data=plist, aes(survived)) +ggtitle("Survival Proportion by Class") +
theme(legend.position="top", legend.title = element_blank()) +
geom_bar(mapping = aes(fill=pclass), position = "fill") +
ylab("Proportion") +xlab("Survived")
##A bar plot is generated with survived in x axis with the proportion of survived differentiated by colour based on class in y axis
#proportion is achieved through the help of position ="fill"
#Plot6
ggplot(data=plist, aes(survived)) +ggtitle("Survival Proportion by Gender") +
theme(legend.position="top", legend.title = element_blank()) +
geom_bar(mapping = aes(fill=sex), position = "fill") +
ylab("Proportion") +xlab("Survived")
#A bar plot is generated with survived in x axis with the proportion of survived differentiated by colour based on gender in y axis
#Plot7
ggplot(data=plist, aes(survived)) +ggtitle("Survival Proportion by Age Cohort") +
theme(legend.position="top", legend.title = element_blank()) +
geom_bar(mapping = aes(fill=age_cohort), position = "fill") +
ylab("Proportion") +xlab("Survived")
#A bar plot is generated with survived in x axis with the proportion of survived differentiated by colour based on age cohort in y axis
#Plot8
ggplot(data=plist, aes(survived)) + ggtitle("Survival Proportion by Embarkation") +
theme(legend.position="top", legend.title = element_blank()) +
geom_bar(mapping = aes(fill=embarked), position = "fill") +
ylab("Proportion") +xlab("Survived")
#A bar plot is generated with survived in x axis with the proportion of survived differentiated by colour based on place of embarkation in y axis
#Plot9 
ggplot(data=plist, aes(survived))+ ggtitle("Survival Numbers by Cohort and Travel Class") +
theme(legend.position="top", legend.title = element_blank()) +
geom_bar(mapping = aes(x = survived, fill=age_cohort)) +
ylab("Number") +xlab("Survived") +facet_grid(~pclass)
#3 bar plots are generated by having survived in x axis and no of people based on cohort in y axis 
#The split is done based on class with the help of the facet_grid() function
#Plot10
ggplot(data=plist, aes(survived))+ggtitle("Survival Numbers by Gender and Travel Class") +
theme(legend.position="top", legend.title = element_blank()) +
geom_bar(mapping = aes(x = survived, fill=sex)) +
ylab("Number") +  xlab("Survived") + facet_grid(~pclass)
#3 bar plots are generated based on class by having survived in x axis and no of people based on gender in y axis 
#Plot11
ggplot(data=plist, aes(survived))+ggtitle("Age V Fare by place of Embarkation")+
theme(legend.position="top", legend.title = element_blank()) +
geom_point(mapping = aes(x = age, y=fare, colour = embarked )) +
ylab("Fare") +  xlab("Age")
# Scatter plot is created with age in x axis and fare in y axis with each scatter point differentiated by 3 colour based on their place of embarkation
#Plot12 
ggplot(data=plist, aes(x = age, y=fare))+ggtitle("Age V Fare with Linear Model") +
geom_point() +geom_smooth(method = "lm") +
ylab("Fare") +xlab("Age") 
#Generating a scatter plot with age in x axis and fare in y axis with a linear model applied onto them using the geom_smooth()function
#Plot13
ggplot(data=plist)+ggtitle("Age V Fare with Survival info") +
theme(legend.position="top", legend.title = element_blank()) +
geom_point(aes(x = age, y=fare, colour = survived)) +
ylab("Fare") +  xlab("Age")
#Scatter plot is made with age in x axis and fare in y axis with additional colour differentiation in the scatter points denoting which class it is respectively
#Plot14
ggplot(data=plist)+ggtitle("Age V Fare By Travel Class and Point of Departure") +
theme(legend.position="top", legend.title = element_blank()) +
geom_point(aes(x = age, y=fare, colour = embarked)) +
ylab("Fare") +  xlab("Age") + facet_wrap(~pclass)
#Creating a scatter plot with age in x axis and fare in y axis with the splits based on class using facet_wrap()
#and the points as the embarked values denoting point of departure