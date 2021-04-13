# GoMMAPPS Unreconciled Double Observer Code
# Survey: Feb 2019
# Script matches and codes unreconciled double observer records
# need AmbiguousGrpsFunction.R and Matching-Function.R


#Libraries
library(dplyr)
library(tidyr)
library(reshape)
library(stringr)

#Load data and clean up 
#Survey data
DataF19 <- read.csv(file = "./gommapps_aerialSurvey_Feb2019.csv", header = TRUE, stringsAsFactors = F)

# (Step 1 = create ordered data frame
# Order data
OrderF19 <- DataF19[
  with(DataF19, order(year, month, day, secs)),
  ]

# coding to ensure any bird observations with same time stamp as Beg or End are within Beg/End rows:
OrderF19$alpha.order <- OrderF19$species
OrderF19$alpha.order[OrderF19$alpha.order == "BEGCOUNT"] <- "aaa"
OrderF19$alpha.order[OrderF19$alpha.order == "ENDCOUNT"] <- "zzz"

OrderF19 <- arrange(OrderF19, year, month, day, hexagon, transect, str_sub(seat,1,1), secs, alpha.order, count) %>% select(-GPSerror, -alpha.order)

# Step 2 Code each transect BEG/END section uniquely:
# create transect-side code (tranID) and determine number of observers on side for each tranID:
data2obs <- OrderF19 %>% group_by(year, month, day, hexagon, transect, side = str_sub(seat,1,1)) %>%
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
y <- x %>% group_by(tranID, begend, grp) %>% mutate(reconcile = tibble(pos, species, genericSp, count, ambigGrp, index) %>% Matching.fn)
y <- ungroup(y)

# Step 9 .. merge reconciled grps to full list
data2obs <- left_join(data2obs, y %>% select(index, reconcile2 = reconcile, ambigGrp)) %>%
  mutate(reconcile = ifelse(reconcile == "TBD", reconcile2, reconcile), ambigGrp = replace_na(ambigGrp, "no")) %>%
  select(-reconcile2)

# omit non-bird records
data2obs <- data2obs[!is.na(data2obs$genericSp),]

# tally up matching column
resultsF19 <- data2obs %>% group_by(reconcile) %>% tally()
resultsF19



#-----------------------------------------------------------------------------#

# detection checks
#crew member detection
crewF19 <-data2obs %>% group_by(initials, hexagon, reconcile) %>% tally()
crewF19
length(unique((crewF19$initials)))

# split by crew initials to match up double observers
split <- split(crewF19, crewF19$initials)

# make each one a df
# to a data.frame
dwd <- as.data.frame(split[[1]]) 
jsw <- as.data.frame(split[[2]])
nlw <- as.data.frame(split[[3]])
phs <- as.data.frame(split[[4]])
rrw <- as.data.frame(split[[5]])
sde <- as.data.frame(split[[6]])

#match up double observers to compare (from plane 723)
phs.rrw <- merge(phs, rrw, by = "hexagon")
phs.sde <- merge(phs, sde, by = "hexagon")
rrw.sde <- merge(rrw, sde, by = "hexagon")

#obs 2 (sde)
sde1 <-phs.sde %>% group_by(reconcile.y) %>% tally()
sde1

sde2 <- rrw.sde %>% group_by(reconcile.y) %>% tally()
sde2

#obs2 totals (sde)
obs2F19 <- (27+14)/(347+53) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)


#obs4
rrw1 <-rrw.sde %>% group_by(reconcile.x) %>% tally()
rrw1

rrw2 <-phs.rrw %>% group_by(reconcile.y) %>% tally()
rrw2

#obs4 total
#obs4 totals rrw
obs4F19 <- (10+69)/(53+347) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)

#obs7 
phs1 <- phs.rrw %>% group_by(reconcile.x) %>% tally()
phs1

phs2 <- phs.sde %>% group_by(reconcile.x) %>% tally()
phs2

#totals phs
obs7F19 <- (71+30)/(347+181) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)


#match up double observers to compare (from plane 736)
dwd.jsw <- merge(dwd, jsw, by = "hexagon")
dwd.nlw <- merge(dwd, nlw, by = "hexagon")
nlw.jsw <- merge(nlw, jsw, by = "hexagon")

#obs 6 (jsw)
jsw1 <- dwd.jsw %>% group_by(reconcile.y) %>% tally()
jsw1

jsw2 <- nlw.jsw %>% group_by(reconcile.y) %>% tally()
jsw2

#obs6 totals 
obs6F19 <- (28+10)/(111+17) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)


#obs8 (dwd)
dwd1 <- dwd.jsw %>% group_by(reconcile.x) %>% tally()
dwd1

dwd2 <- dwd.nlw %>% group_by(reconcile.x) %>% tally()
dwd2

#obs8 total

obs8F19 <- (26+58)/(111+203) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)

#obs9 (nlw)
nlw1 <- nlw.jsw %>% group_by(reconcile.x) %>% tally()
nlw1

nlw2 <-  dwd.nlw %>% group_by(reconcile.y) %>% tally()
nlw2

#totals obs9
obs9F19 <- (11+61)/(17+203) #no match for both paired situations/total observations for paired observers (this is the length of both merged dfs above)

#by plane detection
planeF19 <- data2obs %>% group_by(tailNo, reconcile) %>% tally()
planeF19


#----------------------------------------------------------------------------#
#counting checks
#1 individual
countF19 <- filter(data2obs, count == 1)
countF19 %>% group_by(reconcile) %>% tally()

#less than 6
countF19 <- filter(data2obs, count < 6)
countF19 %>% group_by(reconcile) %>% tally()

#between 6-30
countF19 <- filter(data2obs, count > 5 & count < 31)
countF19 %>% group_by(reconcile) %>% tally()

#>30
countF19 <- filter(data2obs, count > 30)
countF19 %>% group_by(reconcile) %>% tally()


