# GoMMAPPS Unreconciled Double Observer Code
# Survey: July 2018
# Script matches and codes unreconciled double observer records
# need AmbiguousGrpsFunction.R and Matching-Function.R


#Libraries
library(dplyr)
library(tidyr)
library(reshape)
library(stringr)

#Load data and clean up 
setwd()
#Survey data
DataJ18 <- read.csv(file = "./gommapps_aerialSurvey_July2018.csv", header = TRUE, stringsAsFactors = F)

# (Step 1 = create ordered data frame
# Order data
OrderJ18 <- DataJ18[
  with(DataJ18, order(year, month, day, secs)),
  ]

# coding to ensure any bird observations with same time stamp as Beg or End are within Beg/End rows:
OrderJ18$alpha.order <- OrderJ18$species
OrderJ18$alpha.order[OrderJ18$alpha.order == "BEGCOUNT"] <- "aaa"
OrderJ18$alpha.order[OrderJ18$alpha.order == "ENDCOUNT"] <- "zzz"

OrderJ18 <- arrange(OrderJ18, year, month, day, hexagon, transect, str_sub(seat,1,1), secs, alpha.order, count) %>% select(-GPSerror, -alpha.order)

# Step 2 Code each transect BEG/END section uniquely:
# create transect-side code (tranID) and determine number of observers on side for each tranID:
data2obs <- OrderJ18 %>% group_by(year, month, day, hexagon, transect, side = str_sub(seat,1,1)) %>%
  mutate(tranID = group_indices(), obs = n_distinct(initials)) %>%
  ungroup()

# Step 3 (two observers must be counting!)
data2obs <- data2obs %>% filter(obs == 2) %>% select(-obs, -voice) # drop transect-sides with only one observer

# Step 4 (create a cumulative index of beg/end records for each transect)
data2obs <- data2obs %>% mutate(begend = ifelse(species %in% c("BEGCOUNT","ENDCOUNT"), 1, 0)) %>%
  group_by(tranID) %>% mutate(begend = cumsum(begend))

# Drop observations when only one person is counting (both need to say BEG, and one cannot have said END)
rows2keep <- max(data2obs$begend)-4*c(1:(max(data2obs$begend)/4))+2 # keep all rows btwn 2nd BEG and 1st END pairs

# Step 5
options(digits = 7)
data2obs$secs <- as.numeric(data2obs$secs)
#data2obs$secs

# interim step to group observations within 10 secs .... drop BEGCOUNT and other non-bird records 
#and assign first obs deltaTime = 10, so it is coded group = 1
data2obs <- data2obs %>% filter((begend %in% rows2keep) & (species != "BEGCOUNT")) %>% 
  mutate(deltaTime = c(10,diff(secs)), grp = ifelse(deltaTime > 10, 1, 0))

# Step 6
# cumulate index to create "within 10 sec" grouping variable for transect sections:
data2obs <- data2obs %>% group_by(tranID, begend) %>% mutate(grp = cumsum(grp))


# Step 7 Assign all observations in groups with data for only one observer as "noMatch"
data2obs <- data2obs %>% group_by(tranID, begend, grp) %>% 
  mutate(num.obs = length(unique(initials)), reconcile = ifelse(num.obs == 1, "noMatch", "TBD")) %>%
  ungroup()

# Add binned counts
data2obs$bin <- NA
for (i in 1:length(data2obs$count)){
  if (data2obs$count[i] == 0) 
    data2obs$bin[i] <- 1
  if (data2obs$count[i] > 0 & data2obs$count[i] < 11)
    data2obs$bin[i] <- 2
  if (data2obs$count[i] > 10 & data2obs$count[i] < 101)
    data2obs$bin[i] <- 3
  if (data2obs$count[i] > 100 & data2obs$count[i] < 1001)
    data2obs$bin[i] <- 4
  if (data2obs$count[i] > 1000)
    data2obs$bin[i] <- 5
}

# Step 8
# process species data
#species lists
speciesD <- read.csv(file = "./specieslists.csv", header = TRUE, stringsAsFactors = F)
spcode <- speciesD$MASTERLIST
sptype <- speciesD[,1:27]


#first create generic species codes for all species codes
spgroup <- matrix(data = "BIRD", nrow = 151, ncol = 1)

for(i in 1:length(spgroup)){
  if(spcode[i] %in% speciesD$TERNS)
    spgroup[i] <- "LARID" 
  if(spcode[i] %in% speciesD$GULLS)
    spgroup[i] <- "LARID"
  if(spcode[i] %in% speciesD$CORMORANTS)
    spgroup[i] <- "CORM"
  if(spcode[i] %in% speciesD$MERGANSERS)
    spgroup[i] <- "MERG"
  if(spcode[i] %in% speciesD$LOONS)
    spgroup[i] <- "LOON"
  if(spcode[i] %in% speciesD$SCAUP)
    spgroup[i] <- "SCAU"
  if(spcode[i] %in% speciesD$SCOTERS)
    spgroup[i] <- "SCOT"
  if(spcode[i] %in% speciesD$PELICANS)
    spgroup[i] <- spcode[i]
  if(spcode[i] %in% speciesD$HERONS.EGRETS)
    spgroup[i] <- "HERO"
  if(spcode[i] %in% speciesD$PHALAROPES)
    spgroup[i] <- "PHAL"
  if(spcode[i] %in% speciesD$TROPICBIRDS)
    spgroup[i] <- "TROP"
  if(spcode[i] %in% speciesD$BOOBYS)
    spgroup[i] <- spcode[i]
  if(spcode[i] %in% speciesD$GREBES)
    spgroup[i] <- "GREB"
  if(spcode[i] %in% speciesD$IBIS)
    spgroup[i] <- "IBIS"
  if(spcode[i] %in% speciesD$NODDYS)
    spgroup[i] <- spcode[i]
  if(spcode[i] %in% speciesD$PETRELS)
    spgroup[i] <- spcode[i]
  if(spcode[i] %in% speciesD$JAEGERS)
    spgroup[i] <- "JAEG"
  if(spcode[i] %in% speciesD$SHEARWATERS)
    spgroup[i] <- spcode[i]
  if(spcode[i] %in% speciesD$TEALS)
    spgroup[i] <- "TEAL"
  if(spcode[i] %in% speciesD$GEESE)
    spgroup[i] <- spcode[i]
  if(spcode[i] %in% speciesD$DUCKS)
    spgroup[i] <- "DUCK"
  if(spcode[i] == "NOGA")
    spgroup[i] <- spcode[i]
}

spcode


# now add generic species code:
genericSp <- data.frame(species = spcode,
                        genericSp = spgroup,
                        stringsAsFactors = F)

# add a row for plain BIRD records
bird <- c("BIRD", "BIRD")
genericSp <- rbind(genericSp, bird)

# add generic and calculate total birds counted, also create an index so new reconcile value can be assigned to correct record:
data2obs <- left_join(data2obs, genericSp)


data2obs <- data2obs %>% mutate(index = row.names(data2obs)) # create unique index needed for matching code

# subset data to groups with birds for both observers, create front/rear seat code
x <- data2obs %>% filter(num.obs == 2) %>% mutate(pos = str_sub(seat,2,2))

# flag groups with multiple species in same generic group and generic code
# Seems like there are no such cases
x <- x %>% group_by(tranID, begend, grp) %>% mutate(ambigGrp = tibble(species, genericSp) %>% ambiguousGrps.fn) %>% ungroup()

# mutate will not take data frame with group_by so create data frame within mutate and then it will be group_by values only .... apply function to that
y <- x %>% group_by(tranID, begend, grp) %>% mutate(reconcile = tibble(pos, species, genericSp, count, bin, ambigGrp, index) %>% BinMatching.fn)
y <- ungroup(y)

# Step 9 .. merge reconciled grps to full list
data2obs <- left_join(data2obs, y %>% select(index, reconcile2 = reconcile, ambigGrp)) %>%
  mutate(reconcile = ifelse(reconcile == "TBD", reconcile2, reconcile), ambigGrp = replace_na(ambigGrp, "no")) %>%
  select(-reconcile2)

# omit non-bird records
data2obs <- data2obs[!is.na(data2obs$genericSp),]

# tally up matching column
resultsJ18 <- data2obs %>% group_by(reconcile) %>% tally()
resultsJ18


#PerfectMatch = perfectMatch + countMatchSp
432+26
#PerfectGenericMatch = countMatchGeneric + perfectBinMatch
71+0
#noMatch = noMatch + noMatchgrp
557+57


#----------------------------------------------------------------------#
# detection checks
#crew member detection
crewJ18 <-data2obs %>% group_by(initials, hexagon, reconcile) %>% tally()
crewJ18
length(unique((crewJ18$initials)))
# split by crew initials to match up double observers
split <- split(crewJ18, crewJ18$initials)
# make each one a df
# to a data.frame
als <- as.data.frame(split[[1]]) #als
jsw <- as.data.frame(split[[2]])
rmw <- as.data.frame(split[[3]])
rrw <- as.data.frame(split[[4]])
sde <- as.data.frame(split[[5]])
wch <- as.data.frame(split[[6]])

#match up double observers to compare (from plane 708)
als.wch <- merge(als, wch, by = "hexagon")
als.sde <- merge(als, sde, by = "hexagon")
sde.wch <- merge(sde, wch, by = "hexagon")

#obs 1
als1 <-als.wch %>% group_by(reconcile.x) %>% tally()
als1

als2 <- als.sde %>% group_by(reconcile.x) %>% tally()
als2

#obs1 totals (als)
obs1J18 <- (47+45)/(261+280) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)

#obs2
sde1 <- als.sde %>% group_by(reconcile.y) %>% tally()
sde1

sde2 <- sde.wch %>% group_by(reconcile.x) %>% tally()
sde2

#obs2 total** pilot
#obs2 totals sde
obs2J18 <- (42+13)/(261+72) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)

#obs3 
wch1 <- als.wch %>% group_by(reconcile.y) %>% tally()
wch1

wch2 <- sde.wch %>% group_by(reconcile.y) %>% tally()
wch2

#totals wch
obs3J18 <- (54+17)/(280+72) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)


#match up double observers to compare (from plane 736)
rrw.rmw <- merge(rrw, rmw, by = "hexagon")
rrw.jsw <- merge(rrw, jsw, by = "hexagon")
rmw.jsw <- merge(rmw, jsw, by = "hexagon")

#obs 4
rrw1 <- rrw.rmw %>% group_by(reconcile.x) %>% tally()
rrw1

rrw2 <- rrw.jsw %>% group_by(reconcile.x) %>% tally()
rrw2

#obs4 totals 
obs4J18 <- (31+18)/(68+58) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)


#obs5
rmw1 <- rrw.rmw %>% group_by(reconcile.y) %>% tally()
rmw1

rmw2 <- rmw.jsw %>% group_by(reconcile.x) %>% tally()
rmw2

#obs5 total

obs5J18 <- (27+6)/(58+6) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)

#obs6** pilot
jsw1 <- rmw.jsw %>% group_by(reconcile.y) %>% tally()
jsw1

jsw2 <-  rrw.jsw %>% group_by(reconcile.y) %>% tally()
jsw2

#totals obs6
obs6J18 <- (6+23)/(6+68) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)

#by plane detection
planeJ18 <- data2obs %>% group_by(tailNo, reconcile) %>% tally()
planeJ18




#----------------------------------------------------------------------#
#counting 
#counting checks
countJ18 <- filter(data2obs, count ==1)
countJ18 %>% group_by(reconcile) %>% tally()

countJ18 <- filter(data2obs, count < 6)
countJ18 %>% group_by(reconcile) %>% tally()

countJ18 <- filter(data2obs, count > 5 & count < 31)
countJ18 %>% group_by(reconcile) %>% tally()

countJ18 <- filter(data2obs, count > 30)
countJ18 %>% group_by(reconcile) %>% tally()
