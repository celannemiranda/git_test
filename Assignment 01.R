Assignment 01
Celine-Anne L. Miranda

1. WHO dataset
Load WHO dataset  
WHO<-read.csv("WHO.csv")

b. Country with the biggest Population
PopulationMaxID <- which.max(WHO$Population)
WHO$Country[PopulationMaxID]
[1] China
194 Levels: Afghanistan Albania Algeria ... Zimbabwe

c. Population of Malaysia
Use subsetting function containing Malaysia from Who csv file
malaysia<-subset(WHO,Country=='Malaysia')
See the population from the Malaysia subset
malaysia$Population
[1] 29240

d. Country with lowest literacy rate
LiteracyRateMinID<-which.min(WHO$LiteracyRate)
WHO$Country[LiteracyRateMinID]  
[1] Mali
194 Levels: Afghanistan Albania ... Zimbabwe

e. Richest Country in Europe based on GNI
WHO.Europe=subset(WHO, Region=='Europe')
GNIMaxID<-which.max(WHO.Europe$GNI)
WHO.Europe$Country[GNIMaxID]
[1] Luxembourg
194 Levels: Afghanistan Albania ... Zimbabwe

f. Mean Life Expectancy of Countries in Africa
Africa=subset(WHO, Region=='Africa')
mean_life_expectancy_africa=mean(Africa$LifeExpectancy,na.rm = TRUE)
mean_life_expectancy_africa
[1] 57.95652

g. Number of countries with population over 10M
countries_pop_over_10m=subset(WHO,Population>10000)
dim(countries_pop_over_10m)[1]
[1] 86

h. Top 5 Names of Countries in Americas with highest Child Mortality Rate
americas=subset(WHO,Region=='Americas')
index_order_childmortality_for_mortality=order(americas$ChildMortality, decreasing=TRUE)
americas_ordered_by_childmortality=americas[index_order_childmortality_for_mortality,]
top_5_childmortality_americas=head(americas_ordered_by_childmortality,5)
top_5_childmortality_americas$Country
[1] Haiti                           
[2] Bolivia (Plurinational State of)
[3] Guyana                          
[4] Guatemala                       
[5] Dominican Republic              
194 Levels: Afghanistan Albania Algeria ... Zimbabwe

2. The NBA Historical Performance
Data Loading 
library('readxl')
Historical_NBA_Performance <- read_excel("Historical NBA Performance.xlsx")
View(Historical_NBA_Performance)

a. Year Chicago Bulls has highest winning percentage
bulls=subset(Historical_NBA_Performance,Team=='Bulls')
WinningPercentage_bulls=max(bulls$'WinningPercentage')
WinningPercentageMaxID_bulls=which.max(bulls$`Winning Percentage`)
bulls$Year[WinningPercentageMaxID_bulls]
[1] "1995-96"

b. Teams with an even win-loss record (ie team whose recorded win % for the year are 0.500)
teams_winloss=subset(Historical_NBA_Performance,'Winning Percentage'==0.5)
teams_winloss

3. The seasons stats dataset
Data Loading
season_stats<-read.csv('Seasons_Stats.csv')

a. Player with highest 3-point attempt rate
x = aggregate(season_stats$X3PAr~season_stats$Player+season_stats$Year, FUN=sum, na.rm=TRUE) 
x$`season_stats$Player`[max(x$`season_stats$X3PAr`)]
[1] Adrian Dantley*
3922 Levels:  A.C. Green A.J. Bramlett A.J. English A.J. Guyton A.J. Hammons ... Zydrunas Ilgauskas

b. Players with highest free throw rate in season
y=aggregate(season_stats$FT.~season_stats$Player+season_stats$Year,FUN = sum,na.rm=TRUE)
y$`season_stats$Player`[max(y$`season_stats$FT.`)]
[1] Al Miksis
3922 Levels:  A.C. Green A.J. Bramlett A.J. English A.J. Guyton ... Zydrunas Ilgauskas

c. Year "The King" recorded his highest number of points
lbj=subset(season_stats,Player=='LeBron James')
lbj_record_maxpoints=max(lbj$PTS,na.rm = TRUE)
subset(lbj, PTS==lbj_record_maxpoints)$Year
[1] 2006

d. Year "The Goat" scored his most number of points
mj=subset(season_stats,Player=='Michael Jackson')
subset(mj,PTS==max(mj$PTS))$Year
[1] 1988

e. Get Kobe's PER when his MP is lowest
kobe=subset(season_stats,Player=='Kobe Bryant')
subset(kobe,MP==min(kobe$MP))$PER
[1] 10.7

4. The National Universities Ranking
Load dataframe
universities=read.csv("National Universities Rankings.csv")

a. Most # of undergrad
universities$UE = as.numeric(gsub(",", "", universities$Undergrad.Enrollment))
universities$Name[which.max(universities$UE)]
[1] University of Central Florida
231 Levels: Adelphi University American University Andrews University ... Yeshiva University

b. Average Tuition of TOP 10 Universities
top_10<-universities[order(universities$Rank),][1:10,]
top_10$tuition_no_dollar=gsub(pattern="\\$|\\,",replacement="",top_10$Tuition.and.fees)
mean(as.numeric(top_10$tuition_no_dollar))
[1] 49895.2
