# This script contains code to reproduce figure 3 from the manuscript
########################################################################################
rm(list = ls())

#Load necessary packages
library(tidyverse)
library(ggplot2)
library(robustbase)
library(cowplot)

#Functions
se <- function(x){sd(x)/sqrt(length(x))}


setwd()

########################################################################################
# Create Panel A first
# Load data from winter 18 survey
DataF18 <- read.csv(file = "./gommapps_aerialSurvey_Feb2018_birds.csv", header = TRUE)

#species lists so non-bird records can be excluded from data
speciesD <- read.csv(file = "./specieslists.csv", header = TRUE)
spcode <- speciesD$MASTERLIST
sptype <- speciesD[,1:19]

#Filter out non-bird records
BirdF18 <- DataF18 %>% filter(species %in% spcode) %>% droplevels()


#split seat into side and position so double observer records can be separated
BirdF18 <- BirdF18 %>% separate(seat, into = c("side", "position"), sep = 1, remove = FALSE) %>% arrange(hexagon)

#Format data 
#Hexagon information gives number of transects and hexagons surveyed 
#not conditional on bird presence
#Hexagon info for Feb 2018
TransectF18 <- DataF18 %>% filter(species == "BEGCOUNT") %>% 
  arrange(hexagon) %>%
  separate(seat, into = c("side", "position"), sep = 1, remove = FALSE) %>%
  mutate(side = as.numeric(as.factor(side))) %>%
  mutate(position = as.numeric(as.factor(position)))
TransectF18$hex.tran <- with(TransectF18, interaction(transect, hexagon))

#number of hexagon units sampled
nhexF18 <- unique(TransectF18$hexagon)

#Feb 2018 hexagon/transect data
DOdataF18 <- BirdF18 %>% 
  group_by(hexagon, transect, side, position) %>%
  summarize(count = sum(count), records = n()) %>% 
  arrange(hexagon)
DOdataF18$side <- as.numeric(as.factor(DOdataF18$side))
DOdataF18$position <- as.numeric(as.factor(DOdataF18$position))
nobsF18 <- length(DOdataF18$count)
hexF18 <- DOdataF18$hexagon


#Determine which side was double observer side
# if there was a person in position 2 == double observer 
dosideF18 <- TransectF18 %>% 
  filter(position == 2) %>%
  dplyr::group_by(hexagon, side) %>%
  dplyr::summarize(unique(hexagon))
dosideF18 <- cbind(as.numeric(dosideF18$side), as.numeric(dosideF18$hexagon))

# Summarize counts and records by observer on double observer side
DoObsF18 <- array(NA, dim = c(max(nhexF18), 6))
DoObsF18[nhexF18,1:6] <- 0
for(i in 1:nobsF18){
  tryCatch({if(dosideF18[which(dosideF18[,2]==hexF18[i]),1] == DOdataF18$side[i])
    if(DOdataF18$position[i]==1){
      DoObsF18[hexF18[i],1] <- DOdataF18$records[i] + DoObsF18[hexF18[i],1]
      DoObsF18[hexF18[i],2] <- DOdataF18$count[i] + DoObsF18[hexF18[i],2]
    }else{
      DoObsF18[hexF18[i],3] <- DOdataF18$records[i] + DoObsF18[hexF18[i],3]
      DoObsF18[hexF18[i],4] <- DOdataF18$count[i] + DoObsF18[hexF18[i],4]
    }else{
      DoObsF18[hexF18[i],5] <- DOdataF18$records[i] + DoObsF18[hexF18[i],5]
      DoObsF18[hexF18[i],6] <- DOdataF18$count[i] + DoObsF18[hexF18[i],6]
    }
  }, error = function(e){})
}

#Name the columns
DoObsF18 <- na.omit(DoObsF18)
colnames(DoObsF18) <- c("recordsF", "countF", "recordsR", "countR","records3","count3")
DoObsF18 <- tbl_df(DoObsF18)



#Create the plot to compare front and rear observer counts
totalF18.count<-ggplot(data = DoObsF18, aes(y = countR, x = countF)) + 
  geom_abline(slope = 1, intercept = 0, color = "dodgerblue3", size = 1.2) +
  geom_point(alpha = 0.3, size = 3) +
  theme_classic(base_size = 12) +
  labs(y = "Rear", x = "Front") +
  xlim(0,600)+
  ylim(0,600)

totalF18.count.log<-ggplot(data = DoObsF18, aes(y = log(countR), x = log(countF))) + 
  geom_abline(slope = 1, intercept = 0, color = "dodgerblue3", size = 1.1) +
  geom_point(alpha = 0.3, size = 1.5) +
  theme_bw(base_size = 8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "log10(Rear)", x = "log10(Front)") +
  xlim(0,2)+
  ylim(0,2)

#plot together
totalF18.count + annotation_custom(ggplotGrob(totalF18.count.log), xmin = 375, xmax = 638, 
                                   ymin = -5, ymax = 350)



########################################################################################
# Create Panel B
#load data
d <- read.csv("./FlockQuizResults.csv", header = T, as.is = T)

# Data Cleaning
#separate out data background questions on experience and quiz questions
d2 <- subset(d[,7:28])

#pull out the key found in row one
key <- d2[3,]
key <- as.numeric(key)

#remove key from data 
d2 <- d2[-3,]

#remove test responses (all responses before 4/15/2019)
d2 <- d2[-(1:11),]

#get rid of na's
d2[d2==""]  <- NA 
d2[!complete.cases(d2),] 
d2 <- drop_na(d2)

#change to numeric
d3 <- lapply(d2[,1:22], as.numeric)
d3 <- as.data.frame(d3)


#do percent diff
perc.diff <- d3[,1:22]
perc.diff <- sweep(perc.diff, 2, key)
perc.diff <- sweep(perc.diff, 2, FUN = "/", key)

#find col means and SE of absolute differences
perc <- colMeans(perc.diff)
perc.se <- apply(FUN = se, perc.diff, MARGIN = 2)
perc.upr <- perc + (perc.se * 1.96)
perc.lwr <- perc - (perc.se * 1.96)

#combine with key into data frame
perc.df <- data.frame(cbind(perc, perc.lwr, perc.upr, key))

#order by flock size
perc.df <- perc.df[order(key),]

#same but for absolute value of percent difference
#because over and under estimates cancel when we average
abs.perc <- abs(perc.diff)

#find col means and SE of absolute differences
abp <- colMeans(abs.perc)
abp.se <- apply(FUN = se, abs.perc, MARGIN = 2)
abp.upr <- abp + (abp.se * 1.96)
abp.lwr <- abp - (abp.se * 1.96)

#combine with key into data frame
abp.df <- data.frame(cbind(abp, abp.lwr, abp.upr, key))

#order by flock size
abp.df <- abp.df[order(key),]

#plot mean absolute difference and CIs for different flock sizes
errors <- ggplot(data = abp.df, aes(x = key, y = abp * 100)) +
  geom_errorbar(aes(x = key, ymax = abp.upr * 100, ymin = abp.lwr * 100), position = "dodge") +
  geom_point(aes(x = key, y = abp * 100, position="dodge"), color = "dodgerblue3", size = 3) +
  theme_classic(base_size = 12)+
  xlab("True Flock Size")+
  ylab("Observer Error")


#visualize quiz response data to show under- and overestimated flock sizes

# Make count data long format
data.long <- gather(d3, key = "q", value = "response")

# Add key back to long form data
data.long$key <- rep(key, each = 79)

med <- d3[,1:22]
med.mat <- data.matrix(med)

resp.med <- sort(colMedians(med.mat))
plot.dat <- as.data.frame(cbind(resp.med, sort(key)))
colnames(plot.dat) <- c('resp.med', "key")

#plot mean absolute difference and CIs for different flock sizes
responses <- ggplot() +
  geom_point(aes(x = data.long$key, y = data.long$response), alpha = 0.2, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "dodgerblue3", size = 1.2) +
  geom_point(aes(x = plot.dat$key, y = plot.dat$resp.med), color = "darkorange1", size = 2) +
  theme_bw(base_size = 8)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlim(0,900)+
  ylim(0,900) +
  xlab("True Flock Size")+
  ylab("Quiz Responses")


#plot together panel A
A <- totalF18.count + annotation_custom(ggplotGrob(totalF18.count.log), xmin = 375, xmax = 638, 
                                   ymin = -5, ymax = 350)
#plot together panel B
B <- errors + annotation_custom(ggplotGrob(responses), xmin = 400, xmax = 890, 
                                 ymin = -2, ymax = 32)


#save plot
units1 <- "Fig3"
fname <- paste0(units1[1],".pdf")
pdf(fname, height=7, width=5)

# plot panels together
plot_grid(A, B, labels = "AUTO", ncol = 1, align = "v")


dev.off()


