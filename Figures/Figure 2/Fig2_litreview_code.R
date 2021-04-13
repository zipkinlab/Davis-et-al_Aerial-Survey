### Literature Review figure ###

# This script creates Fig 2 from the paper with general and methods-focused
# literature.

## Set-up
#Load libraries
library(tidyverse)
library(ggplot2)
library(gg.gap)



#load data
d <- read.csv("./aerial-papers.csv", header = T, as.is = T)

#convert publication year to a numeric value
d$Bin <- as.numeric(as.character(d$Bin))

#omit the NA value from Excel formatting
d <- na.omit(d)


#convert to long format for easier processing
d.long <- gather(d, type, count, Methods:General, factor_key=TRUE)
d.long

#change names so bar chart stacks correctly (alphabetically)
levels(d.long$type)[levels(d.long$type)=="Methods"] <- "Aerial Survey Methods"
levels(d.long$type)[levels(d.long$type)=="General"] <- "General Aerial Survey"

# Create the stacked bar chart
p <- ggplot(d.long, aes(fill = forcats::fct_rev(type), y=count, x=Bin)) + 
  geom_bar(position = "stack", stat="identity", width = 0.5)+
  scale_fill_manual(values = c("darkorange1", "dodgerblue3"))+
  ylab("Number of Papers")+
  xlab("Year")+
  theme_classic(base_size = 12)+
  theme(legend.title = element_blank())+
  geom_smooth(method = "lm", se = F, color = "black") #add regression lines


# change ggplot theme and add legend
# need to do this extra step because gg.gap does not like formatting above
p <- p +
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_blank(),
    axis.line = element_blank(),
    legend.position = "right"
  )
  
# Create a break in the y-axis 
gg.gap(plot=p,
       segments=c(27,35),
       tick_width = 5, c(1, 0, 0.2),
       ylim=c(0,40))
add.legend(plot = p, margin = c(top=1,right=90,bottom=150,left=1))

# Save the figure
ggsave(filename="./fig2.png", 
       device="png",
       path=path, height=2.5, width=3.5, units="in", dpi=300)