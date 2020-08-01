library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)

# Parameters

sample = 10000
scenarios = 1
educNum = 1:5
educLevels = c("No certificate, diploma or degree","High School","Apprenticeship","College","University")
share = c(0.175,0.216,0.176,0.183,0.250) # shares of educ

meanInc = c(26094,33819,40311,44437,69022) # mean income per person in 2016

# correcting income to 2018 levels by multiplying by 1.06978, assuming all groups growing with the same pace
# 1.06978 consists of 4% growth in 2017 and 2.8% assumed growth in 2018, which is average growth in 2010-2016)
meanInc = meanInc*1.06978 # correction to get 2018 income

# sdInc = c(28872,39834,34512,43925,82998) # Original, too high, because of strange income of 652888CAD which is repeating several times.
sdInc = c(21747,30542,29153,36764,55175) # REAL standard deviation without income of 652888CAD, still produces long tails, but quite realistic income distribution for Quebec.

set.seed(2000)
sim2=matrix(sample(educNum,sample*scenarios,prob=share,replace=TRUE),nrow=sample)
educLev = NULL
for (i in sim2) {
e = educLevels[i] 
educLev = c(educLev,e)
}

head(educLev)

prop.table(table(sim2))

set.seed(2005)
income2=NULL
for(i in sim2){
  location <- log(meanInc[i]^2 / sqrt(sdInc[i]^2 + meanInc[i]^2))
  shape <- sqrt(log(1 + (sdInc[i]^2 / meanInc[i]^2)))
  person <- rlnorm(n=1, location, shape)
  income2=c(income2,person)
}
income_cat2 =ifelse(income2<=50000,1,ifelse(income2<=100000,2,3))
table(income_cat2)
prop.table(table(income_cat2)) 
median(income2)
prop.table(table(income_cat2,sim2),2)

options(digits=2,scipen=3)
data2=as.data.frame(cbind(sim2,income2, income_cat2))
# head(data2)

# data2 %>%
# group_by(educLev) %>%
# summarize(N=n())

data2 %>%
filter(income2<=300000) %>%
  ggplot(aes(x=income2,col=factor(V1)))+geom_density()+
  ggtitle("Income density by education level")+
  xlab("Annual income")+
  ylab("Density")+theme(legend.title = element_blank())
  # +labs(fill = educLev)

data2 %>% 
  # group_by(V1)%>%
  ggplot(aes(x=factor(V1),fill=factor(income_cat2)))+geom_bar(position = "fill")

data2 %>% 
  group_by(V1) %>%
  summarize(mean_Inc = mean(income2))

#### 3. Simulating cars###

carType=1:5
carTypeLevel = c("None","Passenger car (gas)","Light truck (gas)","Passenger car (hybrid)",
"Passenger car (electric)")
carType
carTypeLevel

# Car type distribution by education profile. No such data found in statistics.
# Made up based on assumptions that include Angus Reid data on electric/hybrid car ownership by educ groups 
# and a general assumption that more expensive cars will be overrepresented in higher-income groups. 
# NOT REAL. Can be changed if our assumptions change.

NoHighSchool=c(0.2000,0.4777,0.3175,0.0024,0.0024)
HighSchool=c(0.1659,0.4777,0.3496,0.0034,0.0034)
Apprenticeship=c(0.1300,0.4777,0.3849,0.0037,0.0037)
College=c(0.1300,0.4264,0.4350,0.0043,0.0043)
University=c(0.0500,0.3099,0.6265,0.0068,0.0068)

# Creating a list that contains car structure for all education groups.
carDist = list(NoHighSchool,HighSchool,Apprenticeship,College,University)

# Check that everything adds up to 1
for (i in 1:length(carDist)) {
  print(sum(carDist[[i]]))
}

carDist[[2]][2]

# Simulating cars - just one run

set.seed(40)
car=NULL
sample = 10000
scenarios = 1

for(i in sim2){
  carInd <- sample(carType,1,prob=carDist[[i]],replace=TRUE)
  car=c(car,carInd)
}

options(digits=4) 
table(car)
prop.table(table(car))
prop.table(table(car,sim2),2)

#### 4. Simulating distances###

# Distance to work travelled by car in QC according to 2016 census by StatCan
# Using binned data, finding their midpoints and probabilities for those midpoint distances.
# Much better approximation to StatCan data than using lognorm function.

# distLabel is not used in simulations, just to show bins present in StatCan Data
distLabel = c("Less than 1 km",
"1 to 2.9 km",
"3 to 4.9 km",
"5 to 6.9 km",
"7 to 9.9 km",
"10 to 14.9 km",
"15 to 19.9 km",
"20 to 24.9 Km",
"25 to 29.9 km",
"30 to 34.9 km",
"35 km or more")

distMidpoint = c(0.5,
2.0,
4.0,
6.0,
8.5,
12.5,
17.5,
22.5,
27.5,
32.5,
37.5,
42.5,
47.5,
52.5,
57.5,
62.5,
67.5,
72.5,
77.5,
82.5,
87.5,
92.5,
97.5,
102.5)

distProb = c(0.0459,
	 0.1462,
	 0.1206,
	 0.0979,
	 0.1246,
	 0.1537,
	 0.0992,
	 0.0663,
	 0.0418,
	 0.0273,
	 0.0268,
	 0.0175,
	 0.0114,
	 0.0074,
	 0.0048,
	 0.0031,
	 0.0020,
	 0.0013,
	 0.0009,
	 0.0006,
	 0.0004,
	 0.0002,
	 0.0002,
	 0.0001)

set.seed(568932)
# to use this one for multiple scenarios
# km=matrix(sample(distMidpoint,sample*scenarios,prob=distProb,replace=TRUE),nrow=sample) 
km=sample(distMidpoint,10000,prob=distProb,replace=TRUE)
hist(km, main="Generated commuting distance distribution, km",xlab="")

data3 = cbind(data2,car,same_car = ifelse(runif(sample)<0.9,0,1),finalkm=km*2*365)
data3 = data3 %>% mutate(same_car=ifelse(car==5 | car==4,ifelse(runif(sample)<=0.25,0,1),same_car))

# Amount of CO2 emissions by car type

data3 = data3 %>%
  mutate(CO2 = ifelse(car==1 | car ==5,0,ifelse(car==2,181,ifelse(car==3,303,36))))%>%
  mutate(CO2 = CO2 * finalkm/1000000000) # should get thousand tons of CO2 emitted

# TAKING OUTLIERS
# data3 = data3 %>%
#   filter(finalkm<=50000)

# DONT DELETE YET!

# Matrix of random distances
set.seed(1)
newkm = matrix(nrow=nrow(data3))

for (i in 1:500) {
# to use this one for multiple scenarios
# km=matrix(sample(distMidpoint,sample*scenarios,prob=distProb,replace=TRUE),nrow=sample) 
  gkm=sample(distMidpoint,10000,prob=distProb,replace=TRUE)
  newkm=cbind(newkm,gkm)
    }
newkm=newkm[,-1]
dim(newkm)
head(newkm)
newkm=newkm*2*365
head(newkm)

##### Simulating scenarios. Looking 3 years ahead (fixed distance scenarios)


# Current baseline + several scenarios ran for 3 years ahead: assume 21% change their cars given changes in price or willingness to buy. 
# For the current baseline, an initial 10000*500 matrix with cars created which is then used to randomly replace 21% of cars in each column.
# ###

# Simulating initial matrix of 10000*500 current cars that will be used in function carMatrix2

set.seed(2005)
carMatInit=matrix(nrow=10000,ncol=500)

# sim=matrix(sample(educ,sample*scenarios,prob=prob,replace=TRUE),nrow=sample)

for (j in 1:500) {
for(i in 1:10000){
  carMatInit[i,j] <- sample(carType,1,prob=carDist[[data3$V1[i]]],replace=TRUE)
}
}

ncol(carMatInit)
prop.table(table(carMatInit))

base.hy = apply(carMatInit==4,2,sum)/10000*100
base.elec = apply(carMatInit==5,2,sum)/10000*100
base.truck = apply(carMatInit==3,2,sum)/10000*100
base.car = apply(carMatInit==2,2,sum)/10000*100
par(mfrow=c(2,2))
boxplot(base.hy, main="Share of hybrid cars in the car fleet, %")
boxplot(base.elec, main="Share of electric cars in the car fleet, %")
boxplot(base.truck, main="Share of light trucks in the car fleet, %")
boxplot(base.car, main="Share of regular cars in the car fleet, %")
par(mfrow=c(1,1))

# int.elec = c(0.05,0.05,0.065,0.065,0.12) # intention to buy elec from survey /2
int.elec = c(0.043,0.043,0.056,0.056,0.104) # intention to buy elec from survey /2, corrected for Quebec

# int.hy = c(0.06,0.06,0.09,0.09,0.15) # intention to buy hybrid from survey /2
int.hy = c(0.038,0.038,0.057,0.057,0.095) # intention to buy hybrid from survey /2, corrected for Quebec

# int.truck = c(0.8,0.8,0.8,0.8,0.8)
int.truck = c(0.400,0.440,0.485,0.548,0.789) # 63% of new cars bought now are light trucks*0.87 car penetration = 54.8% of changed cars


price.elec = 43750
price.hy = 42000
price.truck = 46575 # with tax

# Function carMatrix2: produces 10000*500 matrix with new cars based on !!!10000*500!!! matrix of old cars
# based on given educ,income,intention to buy etc.
# many default values in a function

carMatrix2 =  function(price.hy = price.hy,price.elec=price.elec,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.07,
int.hy=int.hy,int.elec=int.elec,int.truck=int.truck) {

will.hy=NULL
for(i in educ){
  will_i <- int.hy[i]*0.4*income2[i]/price.hy
  will.hy=c(will.hy,will_i)
}

will.elec=NULL
for(i in educ){
  will_i <- int.elec[i]*0.4*income2[i]/price.elec
  will.elec=c(will.elec,will_i)
}

# will.truck=NULL
# for(i in educ){
#   will_i <- int.truck[i]*0.3*income2[i]/price.truck
#   will.truck=c(will.truck,will_i)
# }

will.truck=NULL
for(i in educ){
  will_i <- int.truck[i]
  will.truck=c(will.truck,will_i)
}

mcar = old_car_matrix

for (k in 1:n_runs) {
  mySample = sample(1:n_rows,n_rows*perc_change_car)
    for (i in mySample) {
    mcar[i,k] = sample(carType,1,prob=c(carDist[[educ[i]]][1], # keep the same share of people without a car
    (1-carDist[[educ[i]]][1]-will.truck[i]-will.hy[i]-will.elec[i]),#allow cars on gas to vary
    will.truck[i], will.hy[i], will.elec[i]),replace=TRUE) 
        }
  }
  return(mcar)
}

# Function to get 500 values of CO2 emissions (thousand tons of CO2) with fixed distance travelled (1 vector)
CO2.Emis = function(carMatrix,annual_km=data3$finalkm){
co2 = carMatrix
for (i in 1:ncol(carMatrix)) {
  for (j in 1:nrow(carMatrix)) {
    co2[j,i] = ifelse(carMatrix[j,i]==1 | carMatrix[j,i] ==5,0,ifelse(carMatrix[j,i]==2,181,ifelse(carMatrix[j,i]==3,303,36)))
  }
}
co2fin = t(matrix(annual_km)) %*% co2
co2fin=co2fin/1000000000
}

# Calculating current emissions for 10000*500 matrix of current cars and 10000*1 matrix of distances
baseline = CO2.Emis(carMatInit)
baseline

# A code chunk for experiments
# int.elec = c(0.05,0.05,0.065,0.065,0.12) # intention to buy elec from survey /2
int.elec = c(0.043,0.043,0.056,0.056,0.104) # intention to buy elec from survey /2, corrected for Quebec

# int.hy = c(0.06,0.06,0.09,0.09,0.15) # intention to buy hybrid from survey /2
int.hy = c(0.038,0.038,0.057,0.057,0.095) # intention to buy hybrid from survey /2, corrected for Quebec
# int.truck = c(1,1,1,1,1)
int.truck = c(0.400,0.440,0.485,0.548,0.789) # 63% of new cars bought now are light trucks*0.87 car penetration = 54.8% of changed cars


price.elec = 43750
price.hy = 42000
price.truck = 46575 # with tax

will.hy=NULL
for(i in sim2){
  will_i <- int.hy[i]*0.4*income2[i]/price.hy
  will.hy=c(will.hy,will_i)
}

will.elec=NULL
for(i in sim2){
  will_i <- int.elec[i]*0.4*income2[i]/price.elec
  will.elec=c(will.elec,will_i)
}

# will.truck=NULL
# for(i in sim2){
#   will_i <- 0.4*income2[i]/price.truck
#   will.truck=c(will.truck,will_i)
# }

will.truck=NULL
for(i in sim2){
  will_i <- int.truck[i]
  will.truck=c(will.truck,will_i)
}

tot.will = will.hy+will.elec+will.truck
sum(tot.will>1)
par(mfrow=c(2,2))
hist(will.hy)
hist(will.elec)
hist(will.truck)
hist(tot.will)
par(mfrow=c(1,1))

# Scenario 1: emissions in 3 years when 21% changed cars under current policy
set.seed(184326)
mcar2.1 = carMatrix2(price.hy = price.hy,price.elec=price.elec,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.21,
int.hy=int.hy,int.elec=int.elec,int.truck=int.truck)
head(mcar2.1)
co2new2.1 = CO2.Emis(mcar2.1)
co2new2.1

prop.table(table(mcar2.1))

hy.1 = apply(mcar2.1==4,2,sum)/10000*100
elec.1 = apply(mcar2.1==5,2,sum)/10000*100
truck.1 = apply(mcar2.1==3,2,sum)/10000*100
car.1 = apply(mcar2.1==2,2,sum)/10000*100

# Scenario 2: emissions in 3 years when 21% changed cars with -10% price for hy and elec
set.seed(184326)
mcar2.2 = carMatrix2(price.hy = price.hy*0.9,price.elec=price.elec*0.9,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.21,
int.hy=int.hy,int.elec=int.elec,int.truck=int.truck)
co2new2.2 = CO2.Emis(mcar2.2)
co2new2.2
prop.table(table(mcar2.2))

prop.table(table(mcar2.2))

hy.2 = apply(mcar2.2==4,2,sum)/10000*100
elec.2 = apply(mcar2.2==5,2,sum)/10000*100
truck.2 = apply(mcar2.2==3,2,sum)/10000*100
car.2 = apply(mcar2.2==2,2,sum)/10000*100

# Scenario 3: emissions in 3 years when 21% changed cars with -25% price for hy and elec
set.seed(184326)
mcar2.3 = carMatrix2(price.hy = price.hy*0.75,price.elec=price.elec*0.75,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.21,
int.hy=int.hy,int.elec=int.elec,int.truck=int.truck)
co2new2.3 = CO2.Emis(mcar2.3)
co2new2.3
prop.table(table(mcar2.3))

prop.table(table(mcar2.3))

hy.3 = apply(mcar2.3==4,2,sum)/10000*100
elec.3 = apply(mcar2.3==5,2,sum)/10000*100
truck.3 = apply(mcar2.3==3,2,sum)/10000*100
car.3 = apply(mcar2.3==2,2,sum)/10000*100

# Scenario 3a: emissions in 3a years when 21% changed cars with -40% price for hy and elec
set.seed(184326)
mcar2.3a = carMatrix2(price.hy = price.hy*0.6,price.elec=price.elec*0.6,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.21,
int.hy=int.hy,int.elec=int.elec,int.truck=int.truck)
co2new2.3a = CO2.Emis(mcar2.3a)
co2new2.3a
prop.table(table(mcar2.3a))

hy.3a = apply(mcar2.3a==4,2,sum)/10000*100
elec.3a = apply(mcar2.3a==5,2,sum)/10000*100
truck.3a = apply(mcar2.3a==3,2,sum)/10000*100
car.3a = apply(mcar2.3a==2,2,sum)/10000*100

# Scenario 4: emissions in 3 years when 21% changed cars with 20% increase in int to buy hy and elec due to CAMPAIGN
set.seed(184326)
mcar2.4 = carMatrix2(price.hy = price.hy,price.elec=price.elec,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.21,
int.hy=int.hy*1.2,int.elec=int.elec*1.2,int.truck=int.truck)
co2new2.4 = CO2.Emis(mcar2.4)
co2new2.4

prop.table(table(mcar2.4))

hy.4 = apply(mcar2.4==4,2,sum)/10000*100
elec.4 = apply(mcar2.4==5,2,sum)/10000*100
truck.4 = apply(mcar2.4==3,2,sum)/10000*100
car.4 = apply(mcar2.4==2,2,sum)/10000*100

# Scenario 5: emissions in 3 years when 21% changed cars with 40% increase in int to buy hy and elec due to CAMPAIGN
set.seed(185326)
mcar2.5 = carMatrix2(price.hy = price.hy,price.elec=price.elec,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.21,
int.hy=int.hy*1.4,int.elec=int.elec*1.4,int.truck=int.truck)
co2new2.5 = CO2.Emis(mcar2.5)
co2new2.5
prop.table(table(mcar2.5))

hy.5 = apply(mcar2.5==4,2,sum)/10000*100
elec.5 = apply(mcar2.5==5,2,sum)/10000*100
truck.5 = apply(mcar2.5==3,2,sum)/10000*100
car.5 = apply(mcar2.5==2,2,sum)/10000*100

# Scenario 6: emissions in 3 years when 21% changed cars with 10% DECLINED intention to buy trucks
set.seed(186326)
mcar2.6 = carMatrix2(price.hy = price.hy,price.elec=price.elec,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.21,
int.hy=int.hy,int.elec=int.elec,int.truck=int.truck*0.9)
co2new2.6 = CO2.Emis(mcar2.6)
co2new2.6
prop.table(table(mcar2.6))

hy.6 = apply(mcar2.6==4,2,sum)/10000*100
elec.6 = apply(mcar2.6==5,2,sum)/10000*100
truck.6 = apply(mcar2.6==3,2,sum)/10000*100
car.6 = apply(mcar2.6==2,2,sum)/10000*100

# Scenario 7: emissions in 3 years when 21% changed cars with 20% DECLINED intention to buy trucks
set.seed(187327)
mcar2.7 = carMatrix2(price.hy = price.hy,price.elec=price.elec,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.21,
int.hy=int.hy,int.elec=int.elec,int.truck=int.truck*0.8)
co2new2.7 = CO2.Emis(mcar2.7)
co2new2.7
prop.table(table(mcar2.7))

hy.7 = apply(mcar2.7==4,2,sum)/10000*100
elec.7 = apply(mcar2.7==5,2,sum)/10000*100
truck.7 = apply(mcar2.7==3,2,sum)/10000*100
car.7 = apply(mcar2.7==2,2,sum)/10000*100

# Scenario 8: emissions in 3 years when 21% changed cars with 10% price down and 20% increase in intention to buy hy and elec
set.seed(187327)
mcar2.8 = carMatrix2(price.hy = price.hy*0.9,price.elec=price.elec*0.9,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.21,
int.hy=int.hy*1.2,int.elec=int.elec*1.2,int.truck=int.truck)
co2new2.8 = CO2.Emis(mcar2.8)
co2new2.8
prop.table(table(mcar2.8))

hy.8 = apply(mcar2.8==4,2,sum)/10000*100
elec.8 = apply(mcar2.8==5,2,sum)/10000*100
truck.8 = apply(mcar2.8==3,2,sum)/10000*100
car.8 = apply(mcar2.8==2,2,sum)/10000*100

# Scenario 9: emissions in 3 years when 21% changed cars with double intention to buy hy and elec (original from Angues Reid survey)
set.seed(187327)
mcar2.9 = carMatrix2(price.hy = price.hy,price.elec=price.elec,price.truck=price.truck,income=income2,
old_car_matrix=carMatInit,educ=data3$V1,n_rows=nrow(data3),n_runs=500,perc_change_car=0.21,
int.hy=int.hy*2,int.elec=int.elec*2,int.truck=int.truck)
co2new2.9 = CO2.Emis(mcar2.9)
co2new2.9
prop.table(table(mcar2.9))

hy.9 = apply(mcar2.9==4,2,sum)/10000*100
elec.9 = apply(mcar2.9==5,2,sum)/10000*100
truck.9 = apply(mcar2.9==3,2,sum)/10000*100
car.9 = apply(mcar2.9==2,2,sum)/10000*100

mean(co2new2.1)/mean(baseline)
mean(co2new2.2)/mean(baseline)
mean(co2new2.3)/mean(baseline)
mean(co2new2.3a)/mean(baseline)
mean(co2new2.4)/mean(baseline)
mean(co2new2.5)/mean(baseline)
mean(co2new2.6)/mean(baseline)
mean(co2new2.7)/mean(baseline)
mean(co2new2.8)/mean(baseline)
mean(co2new2.9)/mean(baseline)

co2new.df = data.frame(t(rbind(baseline,co2new2.1, co2new2.2, co2new2.3,co2new2.3a,co2new2.4,co2new2.5,co2new2.6,co2new2.7,co2new2.8,co2new2.9)))
# co2new.df = cbind(co2new.df, co2new.1a,co2new.2a,co2new.3a)
co2new.df = rownames_to_column(co2new.df , var = "ID")
names(co2new.df) = c("ID","Baseline","S1","S2","S3","S3a","S4","S5","S6","S7","S8","S9")
head(co2new.df)
co2new.df_long = gather(co2new.df, key="Scenario", value = "CO2_emis",-ID)#,X1,X2,X3)
head(co2new.df_long)

boxplot(co2new.df_long$CO2_emis~co2new.df_long$Scenario, main="Total CO2 emissions for the analyzed scenarios", 
xlab="Scenario", ylab="Annual CO2 emissions, '000 tones", cex.axis=0.85)

hy.df = data.frame(t(rbind(base.hy,hy.1, hy.2, hy.3,hy.3a,hy.4,hy.5,hy.6,hy.7,hy.8,hy.9)))
hy.df = rownames_to_column(hy.df , var = "ID")
names(hy.df) = c("ID","Baseline","S1","S2","S3","S3a","S4","S5","S6","S7","S8","S9")
head(hy.df)
hy.df_long = gather(hy.df, key="Scenario", value = "hy.share",-ID)#,X1,X2,X3)
head(hy.df_long)

boxplot(hy.df_long$ hy.share~hy.df_long$Scenario, main="Share of hybrid cars in private fleet, %", 
xlab="Scenario", ylab="Share of hybrid cars in private fleet, %", cex.axis=0.85)

elec.df = data.frame(t(rbind(base.elec,elec.1, elec.2, elec.3,elec.3a,elec.4,elec.5,elec.6,elec.7,elec.8,elec.9)))
elec.df = rownames_to_column(elec.df , var = "ID")
names(elec.df) = c("ID","Baseline","S1","S2","S3","S3a","S4","S5","S6","S7","S8","S9")
head(elec.df)
elec.df_long = gather(elec.df, key="Scenario", value = "elec.share",-ID)#,X1,X2,X3)
head(elec.df_long)

boxplot(elec.df_long$ elec.share~elec.df_long$Scenario, main="Share of electric cars in private fleet, %", 
xlab="Scenario", ylab="Share of electric cars in private fleet, %", cex.axis=0.85)

truck.df = data.frame(t(rbind(base.truck,truck.1, truck.2, truck.3,truck.3a,truck.4,truck.5,truck.6,truck.7,truck.8,truck.9)))
truck.df = rownames_to_column(truck.df , var = "ID")
names(truck.df) = c("ID","Baseline","S1","S2","S3","S3a","S4","S5","S6","S7","S8","S9")
head(truck.df)
truck.df_long = gather(truck.df, key="Scenario", value = "truck.share",-ID)#,X1,X2,X3)
head(truck.df_long)

boxplot(truck.df_long$ truck.share~truck.df_long$Scenario, main="Share of light trucks in private fleet, %", 
xlab="Scenario", ylab="Share of light trucks in private fleet, %", cex.axis=0.85)

car.df = data.frame(t(rbind(base.car,car.1, car.2, car.3,car.3a,car.4,car.5,car.6,car.7,car.8,car.9)))
car.df = rownames_to_column(car.df , var = "ID")
names(car.df) = c("ID","Baseline","S1","S2","S3","S3a","S4","S5","S6","S7","S8","S9")
head(car.df)
car.df_long = gather(car.df, key="Scenario", value = "car.share",-ID)#,X1,X2,X3)
head(car.df_long)

boxplot(car.df_long$ car.share~car.df_long$Scenario, main="Share of regular cars in private fleet, %", 
xlab="Scenario", ylab="Share of regular cars in private fleet, %", cex.axis=0.85)

# Results comparison 4A
co2new.df4A = data.frame(t(rbind(baseline,co2new2.1,co2new2.2, co2new2.3,co2new2.3a)))
co2new.df4A = rownames_to_column(co2new.df4A , var = "ID")
names(co2new.df4A) = c("ID","Baseline","S1","S2","S3","S3a")
head(co2new.df4A)
co2new.df_long_4A = gather(co2new.df4A, key="Scenario_with_price_change", value = "CO2_emis",-ID)#,X1,X2,X3)
head(co2new.df_long_4A)

boxplot(co2new.df_long_4A$CO2_emis~co2new.df_long_4A$Scenario_with_price_change, main="CO2 emissions for scenarios with price decline of elctric/hybrid cars", 
xlab="Scenario", ylab="Annual CO2 emissions, '000 tones", cex.axis=0.8,cex.main=0.9)

# Results comparison 4B
co2new.df4B = data.frame(t(rbind(baseline,co2new2.4, co2new2.5,co2new2.6,co2new2.7,co2new2.8)))
co2new.df4B = rownames_to_column(co2new.df4B , var = "ID")
names(co2new.df4B) = c("ID","Baseline","S4","S5","S6","S7","S8")
head(co2new.df4B)
co2new.df_long_4B = gather(co2new.df4B, key="Scenario_with_willingness_change", value = "CO2_emis",-ID)
head(co2new.df_long_4B)

boxplot(co2new.df_long_4B$CO2_emis~co2new.df_long_4B$Scenario_with_willingness_change, main="CO2 emissions for scenarios with willingness change", 
xlab="Scenario", ylab="Annual CO2 emissions, '000 tones", cex.axis=0.6,cex.main=0.9)

# Results comparison 4C
co2new.df4C = data.frame(t(rbind(baseline,co2new2.9)))
co2new.df4C = rownames_to_column(co2new.df4C , var = "ID")
names(co2new.df4C) = c("ID","Baseline","S9")
head(co2new.df4C)
co2new.df_long_4C = gather(co2new.df4C, key="Scenario_with_exponential", value = "CO2_emis",-ID)
head(co2new.df_long_4C)

boxplot(co2new.df_long_4C$CO2_emis~co2new.df_long_4C$Scenario_with_exponential, main="CO2 emissions for scenarios with exponential curve", 
xlab="Scenario", ylab="Annual CO2 emissions, '000 tones", cex.axis=0.9,cex.main=0.9)

# Creating csv files with generated data frames.
write.csv(co2new.df_long, "co2.csv")
write.csv(hy.df_long, "hy.csv")
write.csv(elec.df_long, "elec.csv")
write.csv(truck.df_long, "truck.csv")
write.csv(car.df_long, "car.csv")
write.csv(km, "km.csv")