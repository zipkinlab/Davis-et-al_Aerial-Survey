# This script contains code to reproduce pieces of figure 3 from the paper
# Individual figures from this script and Appendix 2 script
# were exported to Inkscape to be combined during post-processing.

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
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  #geom_smooth(color = "red", method = "lm") +
  #geom_text(data = totalF18.df.count, aes(x = x, y = y, label = label), parse = TRUE) +
  theme_bw(base_size = 12) +
  labs(y = "Rear", x = "Front") +
  xlim(0,600)+
  ylim(0,600)

totalF18.count.log<-ggplot(data = DoObsF18, aes(y = log(countR), x = log(countF))) + 
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  #geom_smooth(color = "red", method = "lm") +
  #geom_text(data = totalF18.df.count, aes(x = x, y = y, label = label), parse = TRUE) +
  theme_bw(base_size = 8) +
  labs(y = "log10(Rear)", x = "log10(Front)") +
  xlim(0,2)+
  ylim(0,2)

#plot together
totalF18.count + annotation_custom(ggplotGrob(totalF18.count.log), xmin = 375, xmax = 638, 
                                   ymin = -5, ymax = 350)