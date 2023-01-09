getwd()
setwd("D:/OneDrive - Corvinus University of Budapest/AK/Econometrics/Assignment")

###### Stage1: Organize our raw _data #####

rm(list = ls()) #Removing all previous data variables

pres_16 <- read.csv("2016-precinct-president.csv") #Dataframe of election results
counties_15 <- read.csv("acs2015_county_data.csv") #Dataframe of county demographics

#Let's take a look at the structure of our data
str(pres_16) 
str(counties_15)

#Pres_16 Database has characters where it could have been factor. 
#This takes up lots of memory, so our first thing to do is to change these to factors.
pres_16$year <- as.factor(pres_16$year)
pres_16$stage <- as.factor(pres_16$stage)
pres_16$special <- as.factor(pres_16$special)
pres_16$state <- as.factor(pres_16$state)
pres_16$mode <- as.factor(pres_16$mode)
pres_16$precinct <- as.factor(pres_16$precinct)
str(pres_16)

#We keep fip as the ID, and precint in case in future we make a precinct level analysis
#and latitude and longitude too so that we can include it to our demographics data. 
#We sum up our election results data with grouping the votes on each party. 
cols_to_keep1 <- c("county_fips","county_lat","county_long","votes","party","precinct")
pres_16 = pres_16[,(names(pres_16) %in% cols_to_keep1)] #Dropping all other columns
str(pres_16)
#Let's see how unique our data is: 
#We keep state name and county name too to avoid duplicates. 
#See: https://www.quora.com/Are-county-names-in-the-United-States-unique
pres_election_counties <- pres_16[c("state","county_name")]
census_counties <- counties_15[c("State","County")]


pres_county_names <- unique(pres_election_counties)
census_county_names <- unique(census_counties)

print("presidential elections database unique counties:")
print(nrow(pres_county_names))
print("our external county database unique counties: ")
print(nrow(census_counties))
#It seems like our data is okay.  

str(pres_16)
str(counties_15)

#From now on let's leave the pres_16 database and let's do the transformation on a df called data 
data <- pres_16

#We change all non republican and non democratic column values to small party values. 
data$party<-ifelse(data$party%in%c("democratic","republican"),data$party,"small party")
str(data)

#We group by all county and add together the votes of each party
#We call this "agg_df" because here the data will be aggreagated, and 
#we lose the precinct level data at this transformation
agg_df <- aggregate(data$votes, by=list(data$county_fips,data$party), FUN=sum)
str(agg_df)

#We have to convert county_fip data to CensusId to prepare for the leftjoin.
#Why we are here let's rename Party and Votes too as they lost their 
#column name during the groupby aggregation
library(dplyr)
names(agg_df)[1] <- "CensusId"
names(agg_df)[2] <- "Party"
names(agg_df)[3] <- "Votes"
str(agg_df)

#In this next section we are using some "not so smart" tricks to merge rows with 
#the same CensusID together. Right now votes for democrats and votes for republicans
#are in 2 different rows, but we could just join this together - so we shall! 
names(pres_16)[1] <- "CensusId"

dem_df <- subset(agg_df, Party=="democratic")
names(dem_df)[3] <- "DemVotes" 
dem_df <- dem_df[c(1,3)]
str(dem_df)

rep_df <- subset(agg_df, Party=="republican")
names(rep_df)[3] <- "RepVotes"
rep_df <- rep_df[c(1,3)]
str(rep_df)

prot_df <- subset(agg_df, Party=="small party")
names(prot_df)[3] <- "ProtVotes"
prot_df <- prot_df[c(1,3)]
str(prot_df)

#let's merge the votes together
all_votes <- left_join(dem_df,rep_df)
all_votes <- left_join(all_votes,prot_df)
str(all_votes)
#let's keep ID and the spatial variables of the database, the rest we can drop as we
#have already stored results for each party (democract, republican, or small party)
#in the all_votes df
cols_to_keep2 <- c("CensusId","county_lat","county_long")
pres_16 = pres_16[,(names(pres_16) %in% cols_to_keep2)]
pres_16 <- pres_16[!duplicated(pres_16), ]

#Time to join the pres_16 data with Census ID and the coordinates and also the demography data
str(pres_16)
counties_15 <- left_join(pres_16, counties_15)

#let's make the raw data with the input of voting data - our raw_dataset is done! 
raw_data <- left_join(all_votes, counties_15)
str(raw_data)
raw_data$State <- as.factor(raw_data$State)
raw_data$County <- as.factor(raw_data$County)

#We are having quite many rows, let's drop data that we do not need. 
library(tidyverse)
raw_data <- subset(raw_data, !is.na(TotalPop))
str(raw_data)

#We have cleaned out: (2709 - 2705) = 4 rows where population was not given for us.   
#Time to save our database! 

write.csv(raw_data, "raw_data.csv", row.names=TRUE)

###### Stage2: Structuring our raw_data to data fitting to our analysis #####
library(tidyverse)
raw_data <- read.csv("raw_data.csv")
raw_data$State <- as.factor(raw_data$State)
raw_data$County <- as.factor(raw_data$County)
raw_data = subset(raw_data, select = -c(X) )

#Let's see how precise our data is! 
#How many votes dems and reps got in each state?  
checkup <- raw_data %>% group_by(State) %>% 
  summarise(across(c(RepVotes, DemVotes), sum))
#What are the battleground states? Let's make a variable for them
battleground_cutoff <- 0.05 #Battleground shall be all states that has lower margin of victory
checkup$StateRepMargin <- (checkup$RepVotes - checkup$DemVotes) / (checkup$RepVotes + checkup$DemVotes)

#Let's define strongholds and battleground states
checkup$StateType <- ifelse (checkup$StateRepMargin < battleground_cutoff 
                             & checkup$StateRepMargin > -1*battleground_cutoff,
                             "battleground","stronghold")
#Of course let's also store for each county whether it was in a state won by which party
checkup$StateWon <- ifelse (checkup$StateRepMargin > 0, "republican","democrat")

str(checkup)
checkup = subset(checkup, select = -c(RepVotes,DemVotes,StateRepMargin) )
#We add the frehly created variables to our raw_data with left_join
raw_data <- left_join(raw_data,checkup)
str(raw_data)
print("demvotes")
print(sum(raw_data$DemVotes))
print("repvotes")
print(sum(raw_data$RepVotes))
#Our data is precise, in the output we can see that in red states RepVotes are higher, and in blue states DemVotes are higher. Good! 
#We also see that the data is somewhat realistic that the popular vote was decided for the democrats, which is also realistic for 2016. 

#Time to add some of our own data: 
#Women Ratio is probably better than number of women and number of citizens together. 
#We might lose some R value by dropping the absolute data and scaling them down, but 
#we may reduce heteroskedasticity and earn a degree of freedom too
raw_data$WomenRatio <- raw_data$Women / raw_data$TotalPop 
#We have unemployment but it is different from the ratio of ppl who actively works
raw_data$EmploymentRatio <- raw_data$Employed / raw_data$Citizen
#TotalVotes = how many ballots were cast in the county 
raw_data$TotalVotes <- raw_data$DemVotes + raw_data$RepVotes + raw_data$ProtVotes
#Turnout in county: 
raw_data$Vote_per_Citizens <- raw_data$TotalVotes / raw_data$Citizen
#ratio compared to all people living in county
raw_data$Vote_per_Population <- raw_data$TotalVotes / raw_data$TotalPop
raw_data$Rep_per_Dem <- raw_data$RepVotes / raw_data$DemVotes #Advantage of republicans
raw_data$RepRatio <- raw_data$RepVotes / raw_data$TotalVotes
raw_data$DemRatio <- raw_data$DemVotes / raw_data$TotalVotes
raw_data$ProtRatio <- raw_data$ProtVotes / raw_data$TotalVotes

#Time to define our target varibale: scaled party advantage
raw_data$RepMargin <- (raw_data$RepVotes - raw_data$DemVotes) / (raw_data$RepVotes + raw_data$DemVotes)
raw_data$AbsoluteRepAdv <- raw_data$RepVotes - raw_data$DemVotes #unscaled party advantage
#Did the republican candidate win in the end? 
raw_data$RepWon <- ifelse(raw_data$RepVotes > raw_data$DemVotes, TRUE, FALSE)

#These variables are not needed anymore
cols_to_drop1 <- c("Men","Women","IncomeErr","IncomePerCapErr","Employed","CensusId")
raw_data = raw_data[,!(names(raw_data) %in% cols_to_drop1)]

str(raw_data)
clean_data <- na.omit(raw_data)
write.csv(clean_data, "clean_data.csv", row.names=TRUE)


###### Stage 3: Basic analysis of dataframe #####
#setwd("D:/OneDrive - Corvinus University of Budapest/AK/Econometrics/Assignment")

### 3.1 Basic models ###
#setwd("D:/OneDrive - Corvinus University of Budapest/Documents/AK/Assignment")
clean_data <- read.csv("clean_data.csv")
clean_data$StateType <- as.factor(clean_data$StateType)
clean_data$StateWon <- as.factor(clean_data$StateWon)
clean_data = subset(clean_data, select = -c(X,State,County) )
str(clean_data)

#Let's delete the variables that are to analyze the results from different angles
#But data generated AFTEr the elections so not relevant for regression prediction
data = subset(clean_data, select = -c(DemVotes, RepVotes, ProtVotes, 
                                      Vote_per_Population, Rep_per_Dem, RepRatio,DemRatio,ProtRatio,AbsoluteRepAdv,RepWon))
str(data)


model_1 <- lm(RepMargin ~ . , data = data) #We put all the data we have into model_1
summary(model_1) #The model is quite precise, especially the ethnic ratios has a strong effect

#Let's drop all the data that is insignificant 
model_2 <- lm(RepMargin ~ . 
              - Native - Professional - Service - Office - Construction -Production
              - Drive - Carpool - Transit - Walk - OtherTransp - WorkAtHome 
              - PrivateWork - PublicWork - SelfEmployed - FamilyWork - EmploymentRatio 
              - TotalVotes ,data = data) 
summary(model_2)

AIC(model_1,model_2) #AIC of model_1 is lower: seems like model_1 is better
BIC(model_1,model_2) #BIC says the same, maybe we don't even need Walsh test to be smarter about it
broom::glance(model_1)
broom::glance(model_2)
#Wald F-test
waldtest(model_1,model_2,test="F")

#Wald khi-test
waldtest(model_1,model_2,test="Chi")

#Let's take the most signiificant values: size, color, economy: does anyone care?
model_3 <- lm(RepMargin ~ Vote_per_Citizens + White + Income + Unemployment, data=data)
summary(model_3)

waldtest(model_1,model_3,test="F")

#This specific model_4 were proposed by my beloved cousin while I was working on this
#assignment at my Grandma's 80th Birthday Party. Thank you for the idea, Vince! 
model_4 <- lm(RepMargin ~ Income + Hispanic + Income*Hispanic + Income*White, White, data=data)
summary(model_4)

AIC(model_1,model_3) 
BIC(model_1,model_3) 

#https://www.youtube.com/watch?v=SiUEJWC4zok
library(lmtest)
resettest(model_1,power=2:4,type="regressor",data)
resettest(model_2,power=2:4,type="regressor",data)
resettest(model_3,power=2:4,type="regressor",data)
resettest(model_4,power=2:4,type="regressor",data)

min(data$county_lat)
max(data$county_lat)
2.539e-02*
2.539e-02* (max(data$county_lat) - min(data$county_lat))

library(ggplot2)
library(tidyr)
data <- clean_data
#Analysis of Total Population & Citizens 
# Basic histogram
library(ggplot2)

##### 3.2. Basic plots 
d
hist_total_pop <-ggplot(data, aes(TotalPop)) + geom_histogram()
hist_total_pop

hist_log_totalpop <- ggplot(data, aes(log(TotalPop),fill=RepWon)) + geom_histogram() + scale_color_brewer(palette = "RdBu",direction=-1)
hist_log_totalpop 
color_brewer()
hist_log_citizen <- ggplot(data, aes(x=log(Citizen), color=RepWon, fill=RepWon)) +
  geom_histogram(alpha=0.5, position="identity")

hist_log_citizen
ggplot(data, aes(x=log(TotalVotes), y= log(Citizen),color=RepWon)) + geom_point() 
mean(log(data$TotalPop))
sd(log(data$TotalPop))


ggplot(data, aes(x=TotalPop, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=log(TotalPop), y= RepMargin, color = RepWon)) + geom_point() + geom_smooth(method=lm)

ggplot(data, aes(x=TotalPop, y= TotalVotes)) + geom_point() 

ggplot(data, aes(x=RepWon, y=TotalVotes)) +geom_boxplot() 
ggplot(data, aes(x=RepWon, y=log(TotalVotes))) +geom_boxplot()
ggplot(data, aes(x=county_lat, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=county_long, y= RepMargin)) + geom_point() + geom_smooth(method=lm)

#Analysis of Hispanic, White, Black, Native, Asian
ggplot(data, aes(Hispanic)) + geom_histogram()
ggplot(data, aes(White)) + geom_histogram()
ggplot(data, aes(Black)) + geom_histogram()
ggplot(data, aes(Native)) + geom_histogram()
ggplot(data, aes(Asian)) + geom_histogram()
ggplot(data, aes(WomenRatio)) + geom_histogram()

ggplot(data, aes(x=Hispanic, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=White, y= RepMargin,colour=StateWon)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Black, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Native, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Asian, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
scatter_womenratio <- ggplot(data, aes(x=WomenRatio, y= RepMargin)) + geom_point()
scatter_womenratio + 
  geom_hline(yintercept=0.5, linetype="dashed", color = "red") +
  geom_vline(xintercept = median(data$WomenRatio), linetype="dotted", color = "red", size=1.5)

ggplot(data, aes(y=Black, x= Hispanic, color=RepWon)) + geom_point() 

#Analysis of Income, IncomePerCap, Poverty, ChildPoverty
ggplot(data, aes(Income)) + geom_histogram()
ggplot(data, aes(IncomePerCap)) + geom_histogram()
ggplot(data, aes(Poverty)) + geom_histogram()
ggplot(data, aes(ChildPoverty)) + geom_histogram()
str(data)
scatter_income <- ggplot(data, aes(x=Income, y= RepMargin)) + geom_point() + geom_smooth(method=lm) +
  ggtitle("Effect of Income on the Republican Margin")
scatter_income + 
  geom_vline(xintercept = median(data$Income), linetype="dashed", color = "red", size=1.5, show.legend = TRUE)

ggplot(data, aes(x=IncomePerCap, y= RepMargin,colour=StateWon)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Poverty, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=ChildPoverty, y= RepMargin)) + geom_point() + geom_smooth(method=lm)

#Analysis of Professional, Service, Office, Construction, Production, EmploymentRatio, Unemployment
ggplot(data, aes(Professional)) + geom_histogram()
ggplot(data, aes(Service, color=RepWon, fill=RepWon)) + geom_histogram()
ggplot(data, aes(Office)) + geom_histogram()
ggplot(data, aes(Construction, color=RepWon, fill=RepWon)) + geom_histogram()
ggplot(data, aes(Production, color=RepWon, fill=RepWon)) + geom_histogram()
ggplot(data, aes(EmploymentRatio, color=RepWon, fill=RepWon)) + geom_histogram()


ggplot(data, aes(x=Professional, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Service, y= RepMargin, color=RepWon, fill=RepWon)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Office, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Construction, y= RepMargin, color=RepWon, fill=RepWon)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Production, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=EmploymentRatio, y= RepMargin)) + geom_point() + geom_smooth(method=lm)


#Analysis of PrivateWork, PublicWork, SelfEmployed, FamilyWork, 
ggplot(data, aes(PrivateWork, color=RepWon, fill=RepWon)) + geom_histogram()
ggplot(data, aes(PublicWork)) + geom_histogram()
ggplot(data, aes(log(PublicWork), color=RepWon, fill=RepWon)) + geom_histogram()
ggplot(data, aes(SelfEmployed)) + geom_histogram()
ggplot(data, aes(FamilyWork)) + geom_histogram()

ggplot(data, aes(x=PrivateWork, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=PublicWork, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=log(PublicWork), y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=SelfEmployed, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=FamilyWork, y= RepMargin)) + geom_point() + geom_smooth(method=lm)

#Analysis of Drive, Carpool, Transit, Walk, OtherTransp, WorkAtHome, MeanCommute
ggplot(data, aes(Drive, color=RepWon, fill=RepWon)) + geom_histogram()
ggplot(data, aes(Carpool)) + geom_histogram()
ggplot(data, aes(Transit)) + geom_histogram()
ggplot(data, aes(Walk)) + geom_histogram()
ggplot(data, aes(log(Walk))) + geom_histogram()
ggplot(data, aes(OtherTransp)) + geom_histogram()
ggplot(data, aes(log(OtherTransp))) + geom_histogram()
ggplot(data, aes(WorkAtHome)) + geom_histogram()
ggplot(data, aes(log(WorkAtHome))) + geom_histogram()
ggplot(data, aes(MeanCommute, color=RepWon, fill=RepWon)) + geom_histogram()

ggplot(data, aes(x=Drive, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Carpool, y= RepMargin, color=RepWon)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Transit, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Walk, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=log(Walk), y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=OtherTransp, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=log(OtherTransp), y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=WorkAtHome, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=log(WorkAtHome), y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=MeanCommute, y= RepMargin)) + geom_point() + geom_smooth(method=lm)

#Analysis of DemVotes, RepVotes, ProtVotes, Rep_per_Dem, RepRatio, DemRatio, ProtRatio, RepMargin, AbsoluteRepAdv, RepWon
ggplot(data, aes(DemVotes)) + geom_histogram()
ggplot(data, aes(log(DemVotes))) + geom_histogram()
ggplot(data, aes(RepVotes)) + geom_histogram()
ggplot(data, aes(log(RepVotes))) + geom_histogram()
ggplot(data, aes(ProtVotes)) + geom_histogram()
ggplot(data, aes(log(ProtVotes))) + geom_histogram()
ggplot(data, aes(Rep_per_Dem)) + geom_histogram()
ggplot(data, aes(log(Rep_per_Dem),color=RepWon)) + geom_histogram()
ggplot(data, aes(RepRatio)) + geom_histogram()
ggplot(data, aes(DemRatio)) + geom_histogram()
ggplot(data, aes(ProtRatio)) + geom_histogram()
ggplot(data, aes(log(ProtRatio))) + geom_histogram()
repmargin_histogram <- ggplot(data, aes(RepMargin,fill=StateWon,)) + 
  geom_histogram() +
  ggtitle("Republican margin's histogram with \ncomulative representation of state's winning party")
repmargin_histogram + theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

boxplot(data$RepMargin, main="Boxplot of Repmargin variable")


ggplot(data, aes(AbsoluteRepAdv)) + geom_histogram()
ggplot(data, aes(log(AbsoluteRepAdv))) + geom_histogram()
table(data$RepWon)
table(data$StateType)
table(data$StateWon)
ggplot(data, aes(Vote_per_Citizens)) + geom_histogram()

ggplot(data, aes(x=log(DemVotes), y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=log(RepVotes), y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=log(ProtVotes), y= RepMargin)) + geom_point() + geom_smooth(method=lm)
show_rep_strongholds <- ggplot(data, aes(x=Rep_per_Dem, y= log(AbsoluteRepAdv),colour=StateType)) + geom_point() + geom_smooth()
show_rep_strongholds
ggplot(data, aes(y=Rep_per_Dem, x= log(AbsoluteRepAdv))) + geom_point() + geom_smooth()
ggplot(data, aes(x=RepRatio, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=DemRatio, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=log(ProtRatio), y= RepMargin)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=log(ProtVotes), y= log(AbsoluteRepAdv))) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=ProtVotes, y= AbsoluteRepAdv)) + geom_point() + geom_smooth(method=lm)
ggplot(data, aes(x=Vote_per_Citizens, y= RepMargin)) + geom_point() + geom_smooth(method=lm)
Citizens_RepMargin <- ggplot(data, aes(x=log(Citizen), y= RepMargin, colour=StateWon)) + 
  geom_point() + geom_smooth() +
  ggtitle("How size of the county affects the election outcome")
Citizens_RepMargin

### Part 3.3: Getting descriptive statistics ### 


summary(data)


############# 4. Advanced model analysis ###########
#### 4.1 Principle Component Analysis

#Correlation
library(corrplot)
corr_data <- data
corr_data[] <- lapply(corr_data[], as.numeric)
str(corr_data)

cormatrix <- cor(corr_data[])
cormatrix
corrplot::corrplot(cormatrix, method="shade", diag = F )
#We see that there is strong correlation between mileage and age
#Seems like mileage, age and power strongly correlate with the price
#Let's make a picture of this
pdf("quickplot.pdf",         # File name
    width = 8, height = 7, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")          # Paper size


str(data)
#This was ugly as hell, so let's try another way! 
corr_data = subset(data, select = c(Hispanic, White, Black, Native, Asian, Pacific,WomenRatio, 
                                    Income, IncomePerCap,Poverty,ChildPoverty,RepMargin))
corr_data[] <- lapply(corr_data[], as.numeric)

c1 <- cormatrix <- cor(corr_data[])
c1
corrplot::corrplot(c1, method="shade", diag = F )
str(data)
#Let's try again
corr_data = subset(data, select = c(Professional,Service,Office,Construction,Production,
                                    Drive,Carpool,Transit,Walk,OtherTransp,
                                    WorkAtHome,MeanCommute,RepMargin))
corr_data[] <- lapply(corr_data[], as.numeric)

c2 <- cormatrix <- cor(corr_data[])
c2
corrplot::corrplot(c2, method="shade", diag = F )

#Let's check the remaining: 
corr_data = subset(data, select = c(PrivateWork,PublicWork,SelfEmployed,FamilyWork,
                                    Unemployment,EmploymentRatio,Vote_per_Citizens,
                                    RepMargin))
corr_data[] <- lapply(corr_data[], as.numeric)

c3 <- cormatrix <- cor(corr_data[])
c3
corrplot::corrplot(c3, method="shade", diag = F )



data = subset(clean_data, select = -c(DemVotes, RepVotes, ProtVotes, 
                                      Vote_per_Population, Rep_per_Dem, RepRatio,DemRatio,ProtRatio,AbsoluteRepAdv,RepWon))
pca_data_1 = subset(data, select = -c(TotalPop,Citizen,StateWon,StateType)) 

model_5 <- lm(RepMargin ~ . , data = pca_data_1) 
summary(model_5)
pca_model_1 <- prcomp(pca_data_1[], center = TRUE, scale = TRUE)
pca_model_1
summary(pca_model_1)
plot(pca_model_1, type = "l")

pca_plot_data_1 <- cbind(pca_data_1, pca_model_1$x[,1:2])
library(ggplot2)
ggplot(pca_plot_data_1, aes(PC1, PC2)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, col = "black")

cor(pca_data_1,pca_plot_data_1)
str(pca_data)


str(data)
summary(model_1)

model_6 <- lm(RepMargin ~ . 
              - TotalPop - Citizen - TotalVotes - Income 
              - Unemployment - ChildPoverty - StateWon -StateType
              - county_lat - county_long
              + MeanCommute*IncomePerCap - MeanCommute
              + Drive*IncomePerCap - Drive
              + Carpool*IncomePerCap - Carpool
              + Transit*IncomePerCap - Transit 
              + Walk*IncomePerCap - Walk
              + OtherTransp*IncomePerCap - OtherTransp
              ,data = data) 
summary(model_6)

model_7 <- lm(RepMargin ~ . 
              - TotalPop - Citizen - TotalVotes - Income 
              - Unemployment - ChildPoverty - StateWon -StateType
              - county_lat - county_long
              + MeanCommute*IncomePerCap - MeanCommute
              + Drive*IncomePerCap - Drive
              + Carpool*IncomePerCap - Carpool
              + Transit*IncomePerCap - Transit 
              + Walk*IncomePerCap - Walk
              + OtherTransp*IncomePerCap - OtherTransp
              + EmploymentRatio*PrivateWork - PrivateWork
              + EmploymentRatio*PublicWork - PublicWork
              + EmploymentRatio*SelfEmployed - SelfEmployed
              + EmploymentRatio*FamilyWork - FamilyWork
              ,data = data) 
summary(model_7)

pca_data_2 = subset(data, select = -c(TotalPop, Citizen, TotalVotes, Income,
                                      Unemployment, ChildPoverty, StateWon, StateType,
                                      county_lat, county_long, White, Pacific,
                                      Service, PublicWork, SelfEmployed, FamilyWork))
model_8 <- lm(RepMargin ~ . , data=pca_data_2)
summary(model_8)
#Calculate VIF variables
library(car)
round(vif(model_8),5)

#Let's check for Black value: 
Tolweight <- 1/vif(model_8)[2]
Tolweight

#correlation matrix between the explanatory variables
corr_matrix <- cor(pca_data_2[,1:21])
inverse_martix <- solve(corr_matrix)
round(diag(inverse_martix),5)
round(vif(model_8),5)

#Manual PCA
normalised_data <- scale(pca_data_2[,1:21])
#covariance and correlation
cov_matrix_normalised <- cov(normalised_data)
corr_matrix_original <- cor(pca_data_2[,1:21])

round(cov_matrix_normalised-corr_matrix_original,5)
#Eigenvalues and Eigenvectors
result <- eigen(cov_matrix_normalised)
eigenvalue <- result$values
eigenvectors <- result$vectors
eigenvalue
#We will keep the first 8 primary components, because here the variance is larger than 1.
#(Original variable hat 1 unit of variance)

#principle component: normalised data and eigenvector's linear combination
principle_components <- normalised_data %*% eigenvectors
principle_components[,1:8]
pca_output_result <- principle_components[,1:8]
print(pca_output_result)


pca_model_2 <- prcomp(pca_data_2[,1:21], center = TRUE, scale = TRUE)
str(pca_model_2)

summary(pca_model_2)
plot(pca_model_2, type = "l")



#Adding the 8 principle component to the original database
pca_data_2 <- cbind(pca_data_2,pca_model_2$x[,1:8])
round(cor(pca_model_2$x[,1:8]),5)

pca_data_2
#Which variable went into which principle component?
korrel <- cor(pca_data_2[,-22])
library(corrplot)
corrplot(korrel,method="color")

#Age ?s height v?ltoz?knak nem kell fokomponens
#megtartjuk oket k?l?n magyar?z? v?ltoz?k?nt

#Dobjuk a fokomponenseket
pca_data_2[,c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")] <- NULL

pca_data_3 <- pca_data_2
pca_data_3
str(pca_data_3)

#Now it's time to leave out Hispanic, Black, and IncomePerCap! 
pca_model_3 <- prcomp(pca_data_3[,c(3,4,6:21)], center = TRUE, scale= TRUE)

summary(pca_model_3)
plot(pca_model_3, type = "l")

#First 4 components keep 55% of the variance. 
PC3 <- pca_model_3$x[,1:4]
pca_data_3 <- cbind(pca_data_3,PC3)

#Regression model
combined_model_with_pca_3 <- lm(RepMargin ~ Hispanic + Black + IncomePerCap
                       + PC1 + PC2 + PC3 + PC4, data=pca_data_3)
summary(combined_model_with_pca_3)


broom::glance(model_5,combined_model_with_pca_3)

################ Non-linear Regressions + Model Fine-tuning #######
str(data)
model_9 <- lm(RepMargin ~ .
              - TotalPop - Citizen - Income - TotalVotes
              +StateType*StateWon - StateType - StateWon
              - county_lat - county_long - White 
              - Transit - Walk - OtherTransp - WorkAtHome - MeanCommute
              + Construction*Unemployment - Construction - EmploymentRatio
              - FamilyWork - SelfEmployed
              ,data = data) 
summary(model_9)

model_9 <- lm(RepMargin ~ .
              - TotalPop - Citizen - Income - ChildPoverty - EmploymentRatio - TotalVotes  
              - StateType - StateWon - county_lat - county_long - White - Pacific 
              - Transit - Walk - OtherTransp - WorkAtHome - MeanCommute
              + Construction*Unemployment - Construction - Unemployment
              - PublicWork - FamilyWork - SelfEmployed
              
              - Poverty - Carpool
              ,data = data) 
summary(model_9)

AIC(model_1, model_9)
BIC(model_1, model_9)


model_10 <- lm(RepMargin ~ 
              + White + Hispanic + Black + Unemployment + IncomePerCap + Service +  Drive + PrivateWork
              + StateType*White + StateType*IncomePerCap
              ,data = data) 
summary(model_10)
AIC(model_1, model_10)
BIC(model_1, model_10)


########### Heteroskedasticity ##########
clean_data <- read.csv("clean_data.csv")
clean_data$StateType <- as.factor(clean_data$StateType)
clean_data$StateWon <- as.factor(clean_data$StateWon)
clean_data = subset(clean_data, select = -c(X,State,County) )
data = subset(clean_data, select = -c(DemVotes, RepVotes, ProtVotes, 
                                      Vote_per_Population, Rep_per_Dem, RepRatio,DemRatio,ProtRatio,AbsoluteRepAdv,RepWon))
str(data)


library(skedastic)
#we save the estimated y suqare and the error square
estimated_y <- model_1$fitted.values
err2 <- model_1$residuals^2
plot(estimated_y,err2)
#?bra alapj?n nem tunik konstansnak a hibatag varianci?ja
#heteroszkedasztikusnak tunik a hibatag


#White test only with the squared values
white_1 <- white(model_1, interactions = FALSE)
white_1$statistic
white_1$p.value
#p-value is almost 0
#H0 can be dropped
#H0: homoskedastic error
#There is heteroskedasticity in the model
white_2 <- white(model_2, interactions = FALSE)
white_2$statistic
white_2$p.value

white_4 <- white(model_4, interactions = FALSE)
white_4$statistic
white_4$p.value

white_6 <- white(model_6, interactions = TRUE)
white_6$statistic
white_6$p.value

white_8 <- white(model_8, interactions = FALSE)
white_8$statistic
white_8$p.value

#White with interactions and squared values
white_9 <- white(model_9, interactions = TRUE)
white_9$p.value

white_10 <- white(model_10, interactions = FALSE)
white_10$statistic
white_10$p.value
#p-value is almost 0, H0 is not accepted, heteroscedastic error


#Breusch-Pagan test
library(lmtest)
bptest(model_1,studentize = FALSE)
#p-value almost 0, we drop H0, heteroscedastic error
shapiro.test(model_1$residuals)
ks.test(model_1$residuals, 'pnorm')
#H1: two-sided heteroskedasticity exist

#hogyan lehet ezt megoldani?
coeftest(model_1)

#Robust standard errors
#https://www.youtube.com/watch?v=8L28qPNUcTM
#http://rstudio-pubs-static.s3.amazonaws.com/300060_d2f81f64f48443748969d7c1f6cc7249.html
#https://rpubs.com/cyobero/187387
#Because standard errors are inconsistent and also "incoherent

library(sandwich)
coeftest(model_1, vcov. =vcovHC(model_1,type="HC1"))
summary(model_1)


coeftest(model_10, vcov. =vcovHC(model_10,type="HC1"))
summary(model_10)
stargazer::stargazer(u,model_10,type="text",digits=4)

#I am terribly ashamed that I have no time left for a WLS solution for heteroscedasticity. 
#I might do it during a TDK! 
  
###### Comparing models #######

summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
summary(model_5)
summary(pca_model_1)
summary(model_6)
summary(model_7)
summary(model_8)
summary(pca_model_2)
summary(pca_model_3)
summary(combined_model_with_pca_3)
summary(model_9)
summary(model_10)

model_10_eq <- function(White=median(data$White), Hispanic=median(data$Hispanic), Black=median(data$Black), 
                        Unemployment = median(data$Unemployment), IncomePerCap=median(data$IncomePerCap),
                        Service=median(data$Service),Drive=median(data$Drive),
                        PrivateWork=median(data$PrivateWork),Stronghold=1) {
  RepMargin <-  7.717e-01 + 8.947e-03*White + 5.471e-03*Hispanic -1.617e-03*Black -
    1.518e-02*Unemployment -2.849e-05*IncomePerCap -1.997e-02*Service + 
    7.159e-03*Drive - 8.425e-03*PrivateWork -2.429e-01*Stronghold +
    2.308e-03*White*Stronghold + 6.209e-06*IncomePerCap*Stronghold
  return(RepMargin)
}
model_10_eq(70,10,15,5,50000,20,80,1)
6.209e-04
median(data$Service)
mean(data$Drive)
library(stargazer)
library(tidyverse)
library(lmtest)

AIC(model_3,model_4,model_5)
BIC(model_3,model_4,model_5)


anova(model_2, model_3)
anova(model_4, model_5)

#Wald F-test
waldtest(model_2,model_3,test="F")

#Wald khi-test
waldtest(model_7,model_8,test="Chi")

waldtest(model_4,model_5,test="F")
waldtest(model_4,model_5,test="Chi")
#Wow, without Age and Overdrove the model became significantly worse. 
#P is insignificant, therefore the restricted model it is not a better model. 
#H0 is dropped, which means the variance of the residuals is significantly more. 

stargazer(model_3, model_4, model_6, model_8, model_9, model_10, type = "text")
#Based on F the model_5 has a high F but very unprecise though. 

AIC(model_3, model_4, model_10)
BIC(model_3, model_4, model_10)


################ PREDICTION 

data$Unemployment = data$Black + 0.05
prediction <- predict(model_1, newdata = data, interval = "predict")
summary(prediction)
summary(model_1)
######### LOGIT REGRESSIONS ######## 
clean_data <- read.csv("clean_data.csv")
clean_data$StateType <- as.factor(clean_data$StateType)
clean_data$StateWon <- as.factor(clean_data$StateWon)
clean_data = subset(clean_data, select = -c(X,State,County) )
data = subset(clean_data, select = -c(DemVotes, RepVotes, ProtVotes, 
                                      Vote_per_Population, Rep_per_Dem, RepRatio,DemRatio,ProtRatio,AbsoluteRepAdv))
data$CloseRace <- ifelse(data$RepMargin < 0.20 & data$RepMargin > -0.20, TRUE,FALSE)
table(data$CloseRace)
RepWon_logit_data = subset(data, select = -c(CloseRace, RepMargin))
CloseRace_logit_data = subset(data, select = -c(RepWon, RepMargin))

RepWon_logit_model <- glm(RepWon ~ ., data=RepWon_logit_data, family = binomial(link = "logit"))
summary(RepWon_logit_model)

CloseRace_logit_model <- glm(CloseRace ~ ., data=CloseRace_logit_data, family = binomial(link = "logit"))
summary(CloseRace_logit_model)

