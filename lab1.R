library(mosaic)
library(MASS)

help(TenMileRace)

race.times <- TenMileRace$net

race.times < 4000
## number of fast runners
sum(race.times < 4000)

## get list of fast runners
subset(TenMileRace, net < 4000)
fast.runners = filter(TenMileRace, net < 4000) 

## filter by sex
female.runners <- filter(TenMileRace, sex == "F") 

## filter by nonAmericans
library(stringi)
nonAmericans <- filter(TenMileRace, stri_length(state) > 2) 

## get table
table(TenMileRace$sex)

## breakdown of runners by state excluding non americans
american.runners <- filter(TenMileRace, stri_length(state) == 2) 
table(droplevels(american.runners)$state) 

## where the greatest amount of runners come from
droplevel.american.runners <- droplevels(american.runners)$state 
max(table(droplevel.american.runners))
american.runners.table <- table(droplevel.american.runners) 
names(american.runners.table[max(american.runners.table) == american.runners.table]) 

## number of male runners who finished the race in less
## time than the fastest female runner
filter(TenMileRace, net < min(filter(TenMileRace, sex == "F" )$net)) 
nrow(filter(TenMileRace, net < min(filter(TenMileRace, sex == "F" )$net))) 

## typical runners are runners not within fastest and slowest 5%
fastest <- quantile(TenMileRace$net, 0.05)
slowest <- quantile(TenMileRace$net, 0.95)
typical.runners <- race.times[race.times > fastest & race.times < slowest] 
range(typical.runners, na.rm = TRUE)
