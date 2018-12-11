setwd("C:/Users/Tyler/Dropbox/stats/Macy_Boem_stuff") #Tyler for home, bds4513 for laptop
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(fitdistrplus)
library(logspline)
library(ggpubr)
library(lubridate)
library(plyr)
?read.csv
field_data = read.csv("Field_data_arranged.csv", header = T, na.strings = ".")
field_data1 = read.csv("Field_data_Corrected.csv", header = T, na.strings = ".")
boem_data = read.csv("BOEM_CSV_MACY.csv", header = T)

#install.packages("varhandle")
#library(varhandle)
#varhandle has the unfactor function
#field_data1 = unfactor(field_data)

levels(field_data$Date)


field_data$Date = as.character(field_data$Date)
field_data$Time = as.character(field_data$Time)
field_data$Murie_Site = as.character(field_data$Murie_Site)
field_data$Station = as.character(field_data$Station)

#I guess this conversion didnt become necessary
#field_data$TEMP_INT = as.numeric(as.character(field_data$TEMP_INT))
#field_data$TEMP_BOT = as.numeric(as.character(field_data$TEMP_BOT))
#field_data$DON_INT = as.numeric(as.character(field_data$DON_INT))
#field_data$DON_BOT = as.numeric(as.character(field_data$DON_BOT))
#field_data$PH_INT = as.numeric(as.character(field_data$PH_INT))
#field_data$PH_BOT = as.numeric(as.character(field_data$PH_BOT))
#field_data$TURB_BOT = as.numeric(as.character(field_data$TURB_BOT))
#field_data$SECCHI = as.numeric(as.character(field_data$SECCHI))
#field_data$Depth = as.numeric(as.character(field_data$Depth))
#field_data$CHL_INT = as.numeric(as.character(field_data$CHL_INT))
#field_data$CHL_BOT = as.numeric(as.character(field_data$CHL_BOT))
#field_data$TP_BOT = as.numeric(as.character(field_data$TP_BOT))
#field_data$TN_INT = as.numeric(as.character(field_data$TN_INT))
#field_data$TN_BOT = as.numeric(as.character(field_data$TN_BOT))
#
##This can be done more elegantly like so if the 
#field_data %>%
#  mutate_if(is.factor, as.character) -> field_data

#field_data %>%
#  mutate_if(is.character, as.numeric) -> field_data




#Had to uninstall tidyverse and install lubridate package separately to get this function to work
#Be aware
library(lubridate)

#this takes character data doesnt work with factors
#dont change format year because we are gonna arrnage the dates by year, then month then day
field_data[,19] = mdy(field_data$Date)
colnames(field_data)[19] = "date_year"

field_data1[,26] = mdy(field_data1$Date)
colnames(field_data1)[26] = "date_year"

?mdy
field_data[field_data$Date == "7/30/2014",]
#Arrange the whole dataframe based on date_year column
#Can't do this with the factor year or characters, need it in date format
field_data$date_year
#if u arrange dates in the format m/d/y they will arrange by month and day
#so wait to change formatting until after they have been arranged
field_data = field_data %>% arrange(date_year)
field_data$date_year

#Now change for the format to what u like
field_data1$date_year = format(field_data1$date_year, "%m/%d/%Y")
field_data$date_year =  format(field_data$date_year, "%m/%d/%Y")



#look at the order of the Date column
#Convert date back to factor with levels
?as.factor
field_data$Date = as.factor(field_data$Date)
levels(field_data$Date)

#This converts the levels of the factor Date into the correct order
#BUT IT CHANGES THE DATA SO DO NOT DO THIS~!!!!!!!!!!!!!!!!!!!!!
#that the dataframe is now arranged in earliest to latest

#levels(field_data$Date) = unique(field_data$Date)
#field_data[field_data$Date == "7/30/2014",]

#Change the levels manually
#We want these factor levels to be in the correct order for the graphs
unique(field_data$Date)
#You cannot 
field_data$Date = factor(field_data$Date, levels = c("10/17/2013", "2/3/2014", "2/11/2014",  "2/17/2014",  "5/22/2014",  
                                                     "7/30/2014",  "9/10/2014",  "11/13/2014", "3/17/2015", "3/22/2015", 
                                                     "5/12/2015",  "7/20/2015",  "7/22/2015", "11/2/2015","2/1/2016",   
                                                     "4/27/2016",  "8/17/2016",  "11/11/2016", "2/12/2017", "2/17/2017",
                                                     "6/1/2017",   "7/16/2017",  "7/18/2017" ))

#check levels and make sure 7/30 Date matches 7/30 date_year
levels(field_data$Date)
field_data[field_data$Date == "7/30/2014",]


#convert the integer labels to strings
field_data$topo = ifelse(field_data$topo==1,
                         paste("Ridge"),
                         paste("Swale")) 

field_data$Region = ifelse(field_data$Region==1,
                           paste("Bull"),
                           ifelse(field_data$Region==2,
                                  paste("Chester"),
                                  ifelse(field_data$Region==3,
                                         paste("N. Shoal"),
                                         paste("S. Shoal")))) 


#filter out the dates that don't correspond to year 1 and 2 for boem_data using levels of boem_data$Date
#This is the other dataset with the biomass
level = levels(as.factor(boem_data$Date))
level
unique(field_data$Date)

#convert field_data to these dates
field_data = field_data %>%
  filter(Date %in% level)

field_data1 = field_data1 %>%
  filter(Date %in% level)

levels(field_data$Date)
unique(field_data$Date)

#to drop the factor levels that dont occur re-call the factor function to get rid of the extra levels not in "level"
#and the boem_data
field_data$Date = factor(field_data$Date)
levels(field_data$Date) #YAY pretty cool trick

#make sure that everything lines up all nice like
field_data[field_data$Date == "7/30/2014",]



library(plyr)
#Revalue Integrated into Surface as part of the plyr package
field_data$INT_BOT = revalue(field_data$INT_BOT, c("Integrated" = "Surface"))


detach("package:plyr", unload=TRUE) 
field_summary = group_by(field_data, date_year, Region,INT_BOT) %>%
  summarise(
    count = n(),
    Temp_M = mean(TEMP, na.rm = T),
    sd_Temp = sd (TEMP,na.rm = T),
    Sal_M = mean(SAL, na.rm = T),
    DON_M = mean(DON, na.rm = T),
    PH_M = mean(PH, na.rm = T),
    TURB_M = mean(TURB, na.rm = T),
    SECCHI = mean(SECCHI, na.rm = T),
    Depth_mean = mean(Depth, na.rm = T),
    CHL_M = mean(CHL, na.rm = T),
    sd_CHL = sd(CHL, na.rm = T),
    TP_M = mean(TP, na.rm = T),
    TN_M = mean(TN, na.rm = T),
    sd_P = sd(TP, na.rm = T),
    sd_N = sd(TN, na.rm = T)
  )

field_summary_bar = group_by(field_data, Date, Region,INT_BOT) %>%
  summarise(
    count = n(),
    Temp_M = mean(TEMP, na.rm = T),
    sd_Temp = sd (TEMP,na.rm = T),
    Sal_M = mean(SAL, na.rm = T),
    DON_M = mean(DON, na.rm = T),
    PH_M = mean(PH, na.rm = T),
    TURB_M = mean(TURB, na.rm = T),
    SECCHI = mean(SECCHI, na.rm = T),
    Depth_mean = mean(Depth, na.rm = T),
    CHL_M = mean(CHL, na.rm = T),
    sd_CHL = sd(CHL, na.rm = T),
    TP_M = mean(TP, na.rm = T),
    TN_M = mean(TN, na.rm = T),
    sd_P = sd(TP, na.rm = T),
    sd_N = sd(TN, na.rm = T)
  )
field_summary1 = group_by(field_data1, date_year, Region) %>%
  summarise(
    count = n(),
    Temp_I = mean(TEMP_INT, na.rm = T),
    Temp_B = mean(TEMP_BOT, na.rm = T),
    Sal_I = mean(SAL_INT, na.rm = T),
    Sal_B = mean(SAL_BOT, na.rm = T),
    DON_I = mean(DON_INT, na.rm = T),
    DON_B = mean(DON_BOT, na.rm = T),
    PH_I = mean(PH_INT, na.rm = T),
    PH_B = mean(PH_BOT, na.rm = T),
    TURB_I = mean(TURB_INT, na.rm = T),
    TURB_B = mean(TURB_BOT, na.rm = T),
    SECCHI = mean(SECCHI, na.rm = T),
    Depth_mean = mean(Depth, na.rm = T),
    CHL_I = mean(CHL_INT, na.rm = T),
    CHL_B = mean(CHL_BOT, na.rm = T),
    TP_I = mean(TP_INT, na.rm = T),
    TP_B = mean(TP_BOT, na.rm = T),
    TN_I = mean(TN_INT, na.rm = T),
    TN_B = mean(TN_BOT, na.rm = T)
    
  )

field_summary2 = group_by(field_data, date_year, Region) %>%
  summarise(
    count = n(),
    Temp_M = mean(TEMP, na.rm = T),
    Sal_M = mean(SAL, na.rm = T),
    DON_M = mean(DON, na.rm = T),
    PH_M = mean(PH, na.rm = T),
    TURB_M = mean(TURB, na.rm = T),
    SECCHI = mean(SECCHI, na.rm = T),
    Depth_mean = mean(Depth, na.rm = T),
    CHL_M = mean(CHL, na.rm = T),
    TP_M = mean(TP, na.rm = T),
    TN_M = mean(TN, na.rm = T),
    sd_P = sd(TP, na.rm = T),
    sd_N = sd(TN, na.rm = T)
  )



#---------------------Chlorophyll BOXPLOTS----------------------------
ggplot(field_data, aes(x=Date, y=CHL)) + 
  geom_boxplot() + #and to make the dates not weird
  facet_wrap(~INT_BOT) +
  ggtitle("Chlorophyll Concentration from Differing Depths") +
  ylab(expression("Chlorophyll Concentration,"~mu*g~m^3)) +
  coord_flip() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank()
        )
ggsave(filename = paste("CHL_box.png", sep = ""), plot = last_plot())

#------------------------------BOXPLOT TN--------------------------
ggplot(field_data, aes(x=Date, y=TN)) + 
  geom_boxplot() + #and to make the dates not weird
  facet_wrap(~INT_BOT) +
  ggtitle("Total Nitrogen from Differing Depths") +
  ylab(expression("Total Nitrogen,"~mg~L^-1)) +
  coord_flip() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank()
  )
ggsave(filename = paste("TN_box.png", sep = ""), plot = last_plot())

#------------------------BOXPLOT TP------------------------------
ggplot(field_data, aes(x=Date, y=TP)) + 
  geom_boxplot() + #and to make the dates not weird
  facet_wrap(~INT_BOT) +
  ggtitle("Total Phosphorous from Differing Depths") +
  ylab(expression("Total Phosphorous,"~mg~L^-1)) +
  coord_flip() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank()
  )
ggsave(filename = paste("TP_box.png", sep = ""), plot = last_plot())



#Also, experiment with the melt function to put multiple variables into one vector so they can 
#be placed on the same graph
#Make sure to use the group variable to connect the lines
pd <- position_dodge(width = 0.1)
position_
#---------------------------------PHOSPHOROUS LINE---------------------
ggplot(data=field_summary, aes(x=date_year, y=TP_M, colour=INT_BOT)) +
  geom_point(position = pd) + geom_line(position = pd) +
  #geom_errorbar(aes(ymin = TP_M - (sd_P), 
                   # ymax = TP_M + (sd_P),
                    #width = 0.1),
               # position = pd) + #make sure position is outside of aes()
  geom_hline(yintercept = c(0.02,0.04,0.06), color = "grey") +
  facet_wrap(~Region) +
  xlab("Date") +
  ylab(expression("Average Phosphorous, "~mg~L^-1)) +
  #ggtitle("Total Phoshorous Variation Based on Depth") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.line = element_line(colour = "black"),
        #panel.grid.major = element_line("grey"),
        #panel.grid.minor = element_line("grey"), if u want minor grids too
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
        ) +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "6 months",
               limits = as.Date(c("2013-04-01", "2017-03-01"))
               )

ggsave(filename = paste("TP_line.png", sep = ""), plot = last_plot())

max(field_summary$date_year) 
class(field_summary$date_year)

?geom_hline


#-----------------------------PHOSPHOROUS BAR PLOT--------------------------------------------------
ggplot(data=field_summary_bar, aes(x=Date, y=TP_M, group = INT_BOT)) +
  geom_bar(stat = "identity",
           aes(fill = INT_BOT),
           position = position_dodge(0.9)) +
  #geom_errorbar(aes(ymin = TP_M - (sd_P), 
  # ymax = TP_M + (sd_P),
  #width = 0.1),
  # position = pd) + #make sure position is outside of aes()
  #geom_hline(yintercept = c(0.02,0.04,0.06), color = "grey") +
  geom_text(aes(label = ifelse(TP_M == 0, "n.c.",""),
                hjust = -0.1,
                vjust = 0.2,
                angle = 90)) +
  facet_wrap(~Region) +
  xlab("Date") +
  ylab(expression("Average Phosphorous, "~mg~L^-1)) +
  #ggtitle("Total Phoshorous Variation Based on Depth") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.major.x = element_blank(),
        #panel.grid.minor = element_line("grey"), if u want minor grids too
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
        )




ggsave(filename = paste("TP_bar.png", sep = ""), plot = last_plot())

#--------------------------------NITROGEN LINE PLOTS--------------------------------------------
ggplot(data=field_summary, aes(x=date_year, y=TN_M, colour=INT_BOT, group = INT_BOT)) +
  geom_point(position = pd) +
  geom_line(position = pd) +
  geom_hline(yintercept = c(0.1,0.2,0.3, 0.4, 0.5), color = "grey") +
  #geom_errorbar(aes(ymin = TN_M - (sd_N), 
                    #ymax = TN_M + (sd_N),
                    #width = 0.1),
                #position = pd) +
  facet_wrap(~Region) +
  ylab(expression("Average Nitrogen,"~mg~L^-1)) +
  xlab("Date") +
  #ggtitle("Total Nitrogen Variation Based on Depth") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.line = element_line(colour = "black"),
        #panel.grid.major = element_line("grey"),
        #panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
        ) +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "6 months",
               limits = as.Date(c("2013-04-01", "2017-03-01"))
               )
ggsave(filename = paste("TN_line.png", sep = ""), plot = last_plot())



#------------------------------------------------------------TN BARPLOT-------------------------------------------

ggplot(data=field_summary_bar, aes(x=Date, y=TN_M, group = INT_BOT)) +
  geom_bar(stat = "identity",
           aes(fill = INT_BOT),
           position = position_dodge(0.9)) +
  #geom_errorbar(aes(ymin = TP_M - (sd_P), 
  # ymax = TP_M + (sd_P),
  #width = 0.1),
  # position = pd) + #make sure position is outside of aes()
  #geom_hline(yintercept = c(0.02,0.04,0.06), color = "grey") +
  geom_text(aes(label = ifelse(TN_M == 0, "n.c.",""),
                hjust = -0.1,
                vjust = 0.2,
                angle = 90)) +
  facet_wrap(~Region) +
  xlab("Date") +
  ylab(expression("Average Nitrogen, "~mg~L^-1)) +
  #ggtitle("Total Phoshorous Variation Based on Depth") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.major.x = element_blank(),
        #panel.grid.minor = element_line("grey"), if u want minor grids too
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
        )

ggsave(filename = paste("TN_bar.png", sep = ""), plot = last_plot())

#-----------------------------CHLOROPHYLL LINE PLOTS--------------------------------------
ggplot(data=field_summary, aes(x=date_year, y=CHL_M, colour=INT_BOT, group = INT_BOT)) +
  geom_point(position = pd) +
  geom_line(position = pd) +
  #geom_errorbar(aes(ymin = CHL_M - (sd_CHL), 
                    #ymax = CHL_M + (sd_CHL),
                    #width = 0.1),
                #position = pd) +
  facet_wrap(~Region) +
  ylab(expression("Average Chlorophyll,"~mu*g~m^3)) +
  xlab("Date") +
  ggtitle("Chlorophyll Variation Based on Depth") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        #panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
       ) +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "6 months",
               limits = as.Date(c("2013-07-01", "2017-03-01"))
              )

ggsave(filename = paste("CHL_line.png", sep = ""), plot = last_plot())


#---------------------------------------TEMP PLOT---------------------------------------------
ggplot(data=field_summary, aes(x=date_year, y=Temp_M, colour=INT_BOT, group = INT_BOT)) +
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = c(15,20,25,30), color = "grey") +
  #geom_errorbar(aes(ymin = Temp_M - (sd_Temp), 
                    #ymax = Temp_M + (sd_Temp),
                    #width = 0.1),
                #position = pd) +
  facet_wrap(~Region) +
  ylab(expression("Temperature"~(degree*C))) +
  scale_y_continuous(limits = c(15,30))+
  xlab("Date") +
  #ggtitle("Temperature Variation Based on Depth") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.line = element_line(colour = "black"),
        #panel.grid.major = element_line("grey"),
        #panel.grid.minor = element_line("grey"), if u want minor grids too
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
        ) +
  scale_x_date(date_labels = "%b-%Y",
             date_breaks = "6 months",
             limits = as.Date(c("2013-04-01", "2017-03-01"))
            )

ggsave(filename = paste("line_temp.png", sep = ""), plot = last_plot())



#Before the temp bar plot
field_summary_bar
field_summary_bar[field_summary_bar$Date =="7/20/2015" & field_summary_bar$Temp_M==0,]
#assign values in Temp that are NA to 0 so it fits the conditional in the plot
field_summary_bar$Temp_M[is.na(field_summary_bar$Temp_M)] <- 0

#-----------------------------------TEMP BAR PLOTS----------------------------------------------------

ggplot(data=field_summary_bar, aes(x=Date, y=Temp_M, group = INT_BOT)) +
  geom_bar(stat = "identity",
           aes(fill = INT_BOT),
           position = position_dodge(0.9)) +
  #geom_errorbar(aes(ymin = TP_M - (sd_P), 
  # ymax = TP_M + (sd_P),
  #width = 0.1),
  # position = pd) + #make sure position is outside of aes()
  #geom_hline(yintercept = c(0.02,0.04,0.06), color = "grey") +
  geom_text(aes(label = ifelse(Temp_M == 0, "n.c.",""), #could also use is.na()
                hjust = -0.1,
                vjust = 0.2,
                angle = 90)) +
  facet_wrap(~Region) +
  xlab("Date") +
  ylab(expression("Temperature"~(degree*C))) +
  #ggtitle("Total Phoshorous Variation Based on Depth") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.major.x = element_blank(),
        #panel.grid.minor = element_line("grey"), if u want minor grids too
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
  )

ggsave(filename = paste("Temp_bar.png", sep = ""), plot = last_plot())

#-------------------------------THESE TWO PLOTS NOT USED BUT USEFUL FOR FURTHER PLOTTAGE--------
#------------------------------PLOT WITH TN AND TP-------------------------------------------------#
install.packages("reshape2")
library(reshape2)
melt_summary = melt( data = field_summary2, 
                     id.vars = c("Date", "Region"), 
                     measure.vars =c("TN_M","TP_M" )
                    )
melt_sd = melt( data = field_summary2, 
                     id.vars = c("Date", "Region"), 
                     measure.vars = c("sd_N","sd_P" )
                    )


ggplot(data=melt_summary, aes(x=Date, y=value, colour=variable, group = variable)) +
  geom_point(position = pd) + 
  geom_errorbar(aes(ymin = value - (melt_sd$value), 
                    ymax = value + (melt_sd$value),
                    width = 0.1)) +
  geom_line(position = pd) +
  
  #geom_point(data=field_summary2, aes(x=Date, y=TP_M, colour="blue")) + 
 # geom_errorbar(data=field_summary2,   aes(x = TP_M, ymin = (TP_M - 2*sd_P), ymax = (TP_M + 2*sd_P))) +
  #geom_line(data=field_summary2, aes(x=Date, y=TP_M, colour="blue", group = 2)) +
  
  facet_wrap(~Region) +
  ylab(expression("Average Concentration,"~mg~L^-1)) +
  ggtitle("Variation of Total Phosphorous and Nitrogen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
  )
ggsave(filename = paste("TN_TP_line.png", sep = ""), plot = last_plot())







ggplot(data=field_summary, aes(x=Date, y=DON_M, colour=INT_BOT, group = INT_BOT)) +
  geom_point() + geom_line() +
  facet_wrap(~Region) +
  ylab(expression("Dissolved Organic Nitrogen,"~mg~L^-1)) +
  ggtitle("Dissolved Organic Nitrogen Variation Based on Depth") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
        )
ggsave(filename = paste("line_DON.png", sep = ""), plot = last_plot())


#--------------------------------------------END NOT USED--------------------------






#TP plots
#THIS SHIT IS HANDY BUT BE CAREFUL
for (depth in unique(field_summary$INT_BOT)) {
  #new plot window for each plot
  dev.new() 
  #The print function below, be careful how you subset it
  print(ggplot(field_summary[field_summary$INT_BOT==depth,], aes(x=Date, y=TP_M)) + 
          ggtitle("Average Total Phosphorous Per Region",subtitle = depth) +
          xlab("Date") + ylab(expression("Average Total Phosphorous,"~mg~L^-1)) +
          facet_wrap(~Region) +
          geom_bar(stat = "identity") + #<----dont forget the plus 
          coord_flip() +
          theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_line("grey"),
                panel.grid.minor = element_line("grey"),
                panel.border = element_blank(),
                panel.background = element_blank(),
                legend.title=element_blank())
        
  )
  ggsave(filename = paste("TP_barplot_",depth,".png", sep = ""), plot = last_plot())
}



for (depth in unique(field_summary$INT_BOT)) {
  #new plot window for each plot
  dev.new() 
  #The print function below, be careful how you subset it
  print(ggplot(field_summary[field_summary$INT_BOT==depth,],aes(x=Date, y=TN_M)) + 
          ggtitle("Average Total Nitrogen Per Region",subtitle = depth) +
          xlab("Date") + ylab(expression("Average Total Nitrogen,"~mg~L^-1)) +
          facet_wrap(~Region) +
          geom_bar(stat = "identity") + #<----dont forget the plus 
          #scale_fill_brewer(palette="Set1")
          #scale_fill_manual(values=c("#4daf4a", "#377eb8", "#e41a1c","#9b42f4")) +
          #and to make the dates not weird
          coord_flip() +
          theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_line("grey"),
                panel.grid.minor = element_line("grey"),
                panel.border = element_blank(),
                panel.background = element_blank())
        
  )
  ggsave(filename = paste("TN_barplot_",depth,".png", sep = ""), plot = last_plot())
}


for (depth in unique(field_summary$INT_BOT)) {
  #new plot window for each plot
  dev.new() 
  #The print function below, be careful how you subset it
  print(ggplot(field_summary[field_summary$INT_BOT==depth,],
               aes(x=Date, y=TN_M)) + 
          ggtitle("Average Total Nitrogen Per Region",subtitle = depth) +
          xlab("Date") + ylab(expression("Average Total Nitrogen,"~mg~L^-1)) +
          facet_wrap(~Region) +
          geom_bar(stat = "identity") + #<----dont forget the plus 
          #scale_fill_brewer(palette="Set1")
          #scale_fill_manual(values=c("#4daf4a", "#377eb8", "#e41a1c","#9b42f4")) +
          #and to make the dates not weird
          coord_flip() +
          theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_line("grey"),
                panel.grid.minor = element_line("grey"),
                panel.border = element_blank(),
                panel.background = element_blank())
  )
  
  ggsave(filename = paste("TN_barplot_",depth,".png", sep = ""), plot = last_plot())
}
?geom_bar

ggplot(field_summary, aes(x=Date, y=TP_M, fill = INT_BOT, group = INT_BOT)) + 
  ggtitle("Average Total Phosphorous by Date") +
  xlab("Date") + ylab(expression("Average Total Phosphorous,"~mg~L^-1)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") + #<----dont forget the plus also dodge = side by side
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
        )
ggsave(filename = paste("TP_barplot_depth.png", sep = ""), plot = last_plot())


ggplot(field_summary, aes(x=Date, y=TN_M, fill = INT_BOT, group = INT_BOT)) + 
  ggtitle("Average Total Nitrogen by Date") +
  xlab("Date") + ylab(expression("Average Total Nitrogen,"~mg~L^-1)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") + #<----dont forget the plus also dodge = side by side
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
  )
ggsave(filename = paste("TN_barplot_depth.png", sep = ""), plot = last_plot())

ggplot(field_summary, aes(x=Date, y=CHL_M, fill = INT_BOT, group = INT_BOT)) + 
  ggtitle("Average Chlorophyll by Date") +
  xlab("Date") + ylab(expression("Average Chlorophyll Concentration,"~mu*g~m^3)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") + #<----dont forget the plus also dodge = side by side
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
  )
ggsave(filename = paste("CHL_barplot_depth.png", sep = ""), plot = last_plot())

write.csv(data.frame(summary(mod)$coefficients), file="table1.csv")


#But it won’t work on unbalanced outputs, like the complete summary, e.g.:
  
#write.csv(data.frame(summary(mod)), file="table1.csv")