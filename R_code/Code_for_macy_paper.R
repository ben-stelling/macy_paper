
#1. Anderson DR, Burnham KP, Thompson WL (2000) Null hypothesis testing: Problems, prevalence, and an alternative. Journal of Wildlife Management 64: 912-923. 
#2. Gigerenzer G (2004) Mindless statistics. Journal of Socio-Economics 33: 587-606. 
#3. Johnson DH (1999) The Insignificance of Statistical Significance Testing. The Journal of Wildlife Management 63: 763-772.
#install.packages("installr")
#library(installr)
#updateR()
install.packages("tidyverse", dependencies = T)
install.packages("ggthemes")
install.packages("ggrepel")
install.packages("plyr")
install.packages("fitdistrplus")
install.packages("logspline")
install.packages("ggpubr")
install.packages("cowplot")
install.packages("gridExtra")
install.packages("lubridate")
library(plyr)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(fitdistrplus)
library(logspline)
library(ggpubr)
library(lubridate)
library(MASS)
#Dont forget to change user
setwd("C:/Users/Tyler/Dropbox/stats/Macy_Boem_stuff")
#remove.packages("tidyverse")
#insert data 
#make sure to put the .csv
boem_data = read.csv("BOEM_CSV_MACY.csv", header = T)
#Order the dataframe by date
boem_data
class(boem_data$Date)
levels(boem_data$Date)
#dont think changing boem_data$Date to a character is necessary
#boem_data$Date = as.character.Date(boem_data$Date)
#Had to uninstall tidyverse and install lubridate package separately to get this function to work
#Be aware
library(lubridate)
boem_data[,15] = mdy(boem_data$Date)
colnames(boem_data)[15] = "date_year"
#dplyr
boem_data = boem_data %>% arrange(date_year)


#the unique is in order but the levels arent which will ruin our plots later if we use Date as factor
unique(boem_data$Date)
boem_data[boem_data$Date=="7/30/2014",]

levels(boem_data$Date)
#This converts the levels of the factor Date into the correct order
#that the dataframe is now arranged in
#levels(boem_data$Date) = unique(boem_data$Date)<---don't do this is messes up the order
#do this instead
boem_data$Date = factor(boem_data$Date, levels = c("10/17/2013", "2/3/2014", "2/11/2014",  "2/17/2014",  "5/22/2014",  
                                                     "7/30/2014",  "9/10/2014",  "11/13/2014", "3/17/2015", "3/22/2015", 
                                                     "5/12/2015",  "7/20/2015",  "7/22/2015"))
str(boem_data$Date)
levels(boem_data$Date)
boem_data$Date
boem_data[boem_data$Date=="7/30/2014",]
#not sure if this is necessary
#boem_data$Date = as.factor(boem_data$Date)

#boem_data$Date = boem_data$Date[order(as.Date(boem_data$Date, format="%m/%d/%Y")),]
#daily$DoW <- ordered(daily$DoW, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
#"Friday", "Saturday", "Sunday"))
str(boem_data)
?order
#Carbon conversion


#Use this instead
#does the conversion in per each entry
#this takes um^3/mL and gives ug/mL
boem_data[,16] = ifelse(boem_data$Classification == "Diatom",(boem_data$Total.BV/1000000)*0.065,
                        ifelse(boem_data$Classification == "Dinoflagellate", (boem_data$Total.BV/1000000)*0.16,
                        (boem_data$Total.BV/1000000)*0.22)
                       )

#USING IFELSE AND GREPL BOTH USEFUL
#grepl lets us give a value ridge/swale to sites with R in them or not
#NOT SURE WHAT FIXED = TRUE DOES ifelse (condition, if true do this, else do this)
boem_data[,17] = ifelse(grepl("R", boem_data$Murie.Site, fixed = TRUE),
                         paste("Ridge"),
                         paste("Swale")
                        ) 
              

#name the columns
colnames(boem_data)[16] = "carbon_biomass" 
colnames(boem_data)[17] = "topo" 

summary(boem_data$carbon_biomass)
glimpse(boem_data)

#Checking the data for outliers
#Rhizosolenia imbricta = questionable
boem_data[boem_data$carbon_biomass >0.5,]
#made fake data points so I could address blanks in the biomass stacked bar graphs
boem_data[boem_data$carbon_biomass == 0.0,]
boem_o = boem_data[boem_data$Season=="",]
boem_o

#removing rows and columns that don't have data or are outliers/questionable
#remember only to do this once
boem_data = boem_data[-c(6997,18945),]


# using plyr revalue function
levels(boem_data$Site.Category)
#Using revalue to rename CC shoals to just shoals have to assign
#to the original to make permanent
library(plyr)
?revalue
#recode is the dplyr version--may work
boem_data$Site.Category = revalue(boem_data$Site.Category, c("Cape Canaveral Shoals" = "S. Shoal",
                                                             "Dredge" = "N. Shoal"))
boem_data$Classification = revalue(boem_data$Classification, c("PPP" = "PPP & Other Cyano"
                                                             ))
boem_data$Depth = revalue(boem_data$Depth, c("Integrated" = "Surface"))




levels(boem_data$Depth)
levels(boem_data$Classification)
#to rearrange the site category to match the look of field data graphs
boem_data$Site.Category = factor(boem_data$Site.Category,levels(boem_data$Site.Category)[c(1,3, 4, 2)])
levels(boem_data$Site.Category)


#dplyr version of str()
glimpse(boem_data)

#Checking the names and classes
class(boem_data$Classification)
names(table(boem_data$Classification))
names(table(boem_data$Date)) 
unique(boem_data$Date)
names(table(boem_data$Season))
unique(boem_data$Date)
levels(boem_data$Date)
glimpse(boem_biomass_sum)
unique(boem_data)




#---------------------------------------------------Summarizing the Dataframes-------------------------------------

#Calculating the sum of each site
#CREATING A DATAFRAME to group the data by Date->Site_region-> Depth -> Classification-->murie site
#do this for both day and night
?arrange
detach("package:plyr", unload=TRUE) 
#may have to do this if error: function should not be called 
#directly comes up
boem_biomass_sum = boem_data %>%
  group_by(Date, Site.Category, Depth, topo, Time, Season, Classification, Murie.Site) %>%
  summarise(
    count = n(),
    mean = mean(carbon_biomass, na.rm = T),
    sd = sd(carbon_biomass, na.rm = T),
    sum = sum(carbon_biomass)
  )


range(boem_biomass_sum$sum)
#Only way I could figure out how to do this
#Take the dataframe I just made and make a new one with it
#Mean carbon_biomass/site based on the ridge/swale, depth etc
# x1000 because g/L not mg/L

#both day and night values
biomass_site_mean = group_by(boem_biomass_sum, Date, Site.Category, Season, Time, Depth, Classification) %>%
    summarise(
      mean_site = mean(sum, na.rm = T)*1000
    )
class(biomass_site_mean$Date)






levels(biomass_site_mean$Date)

range(biomass_site_mean$mean_site)

#how to count how many unique values are in a column
length(unique(boem_data$topo))

boem_biomass_sum$sum
str(boem_biomass_sum)
#----------------------------------REORDERING FACTORS--------------------------------------------------
#ggplot(gap_asia_2007, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) 
#------------------------------------PLOTTING THE DATA--------------------------------------------------------

#Boxplot by region
#bull and chester together dredge then canaveral or could just number
#one word 
ggplot(biomass_site_mean, aes(x=Site.Category, y=mean_site)) + 
  geom_boxplot() + 
  coord_flip() +#and to make the dates not weird
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank()
        )
  

#Boxplots of Dates and Classification
ggplot(biomass_site_mean, aes(x=Date, y=mean_site)) + 
  geom_boxplot() +
  facet_wrap(~Classification) +
  ylab(expression("Mean Biomass Per Site,"~mu*g~C~mL^-1)) +
  coord_flip()+ #to make the dates not weird
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(filename = paste("biomass_boxplot.png", sep = ""), plot = last_plot())


ggplot(biomass_site_mean, aes(x=Date, y=mean_site*1000, fill = Depth, group = Depth)) + 
  ggtitle("Average Biomass by Date") +
  xlab("Date") + ylab(expression("Average Biomass,"~mu*g~C~L^-1)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") + #<----dont forget the plus also dodge = side by side
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank()
  )
ggsave(filename = paste("biomass_barplot_depth.png", sep = ""), plot = last_plot())

#Boxplots of regions
ggplot(biomass_site_mean, aes(x=Date, y=mean_site)) + 
  geom_boxplot() + 
  facet_wrap(~Site.Category) +
  coord_flip() #and to make the dates not weird
  

?scale_color_brewer
#Boxplots of depth
ggplot(biomass_site_mean, aes(x=Date, y=mean_site)) + 
  geom_boxplot() + 
  facet_wrap(~Depth) +
  coord_flip() #and to make the dates not weird

##Boxplots of depth
ggplot(biomass_site_mean, aes(x=Date, y=mean_site)) + 
  geom_boxplot() + 
  facet_grid(Classification~Depth) +
  coord_flip() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("grey"),
        panel.grid.minor = element_line("grey"),
        panel.border = element_blank(),
        panel.background = element_blank())
  #and to make the dates not weird

unique(biomass_site_mean$Date)
data$Treatment <- factor(data$Treatment, levels=unique(data$Treatment))




#
#biomass_site_mean$mask = 0
#biomass_site_mean$mask[biomass_site_mean$date_year == "2013-10-17"] = 1
#max.value <- max(biomass_site_mean$mean_site)
#max.value.other <- max(biomass_site_mean$mean_site[biomass_site_mean$date_year != "2013-10-17"])
#min.value.oct <- min(biomass_site_mean$mean_site[biomass_site_mean$date_year == "2013-10-17"])
#scale <- floor(min.value.oct / max.value.other) - 1
#biomass_site_mean$mean_site[biomass_site_mean$mask == 1] = biomass_site_mean$mean_site[biomass_site_mean$mask == 1] / scale
#step <- 50
#low.end <- max(biomass_site_mean$mean_site[biomass_site_mean$date_year != "2013-10-17"])
#up.start <- ceiling(max(biomass_site_mean$mean_site[biomass_site_mean$date_year != "2013-10-17"]))
#breaks <- seq(0, max(biomass_site_mean$mean_site), step)
#labels <- seq(0, low.end+step, step)
#labels <- append(labels, scale * seq(from=ceiling((up.start + step) / step) * step, length.out=length(breaks) - length(labels), by=step))


#------------------------------------Main Biomass BarPlots-----------------------------------
#Barplots of category and classification
#THIS SHIT IS HANDY BUT BE CAREFUL
for (depth in unique(biomass_site_mean$Depth)) {
  #new plot window for each plot
  dev.new() 
  #The print function below, be careful how you subset it
         print(ggplot(biomass_site_mean[biomass_site_mean$Depth==depth,],
               aes(x= Date, 
                   y=mean_site,
                   fill = Classification)) + 
            ggtitle("", subtitle = depth) +
            xlab("Date") + 
            ylab(expression("Mean Biomass Per Site,"~mu*g~C~L^-1)) +
            ylim(0,600) +
            facet_wrap(~Site.Category) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = ifelse(mean_site == 0, "n.c.",""), hjust = -0.1)) + #<----dont forget the plus 
              #scale_fill_brewer(palette="Set1")
            #geom_text hard to figure out
          #geom_text(aes(Date, label=sum(mean_site*1000), y=500*mean_site), size=5, colour="black") +
            scale_fill_manual(values=c("#377eb8", "#FFCC00", "#e41a1c","#008000")) +
              #and to make the dates not weird
            coord_flip() +
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_line("grey"),
                panel.grid.minor = element_line("grey"),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text = element_text(color = "black", size = 11),
                axis.title = element_text(color = "black", size = 12),
                plot.subtitle= element_text(size=12, face="bold", color="black"))
              )
 ggsave(filename = paste("biomass_barplot_final_",depth,".png", sep = ""), plot = last_plot())
} #end the loop

#-------------------------------------------DAY BIOMASS BAR PLOTS----------------------------------------
#Barplots of category and classification
#adding a row in after taking it out

geom_point()
colnames(day_site_mean)
#Make a row for the empty slot on the bar graph---> so that we can place the n.c. in the right place
df = data.frame(Date = as.factor("10/17/2013"), Site.Category = "Chester", Season = "Fall", Time = "Day",
           Depth = "Bottom", Classification = "Diatom", mean_site = 0, stringsAsFactors = T)
str(df)
str(day_site_mean)
#Dont use bind_rows -> will coerce factors into cahracters
day_site_mean = rbind.data.frame(day_site_mean, df)
warnings()
#THIS SHIT IS HANDY BUT BE CAREFUL
for (depth in unique(day_site_mean$Depth)) {
  #new plot window for each plot
  dev.new() 
  #The print function below, be careful how you subset it
  print(ggplot(day_site_mean[day_site_mean$Depth==depth,],
               aes(x= Date, 
                   y=mean_site,
                   fill = Classification)) + 
          ggtitle("", subtitle = depth) +
          xlab("Date") + 
          ylab(expression("Mean Biomass Per Site,"~mu*g~C~L^-1)) +
          ylim(0,600) +
          facet_wrap(~Site.Category) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = ifelse(mean_site== 0, "n.c.",""), hjust = -0.1)) + #<----dont forget the plus 
          #scale_fill_brewer(palette="Set1")
          #geom_text hard to figure out
          #geom_text(aes(Date, label=sum(mean_site*1000), y=500*mean_site), size=5, colour="black") +
          scale_fill_manual(values=c("#377eb8", "#FFCC00", "#e41a1c","#008000")) +
          #and to make the dates not weird
          coord_flip() +
          theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_line("grey"),
                panel.grid.minor = element_line("grey"),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text = element_text(color = "black", size = 11),
                axis.title = element_text(color = "black", size = 12),
                plot.subtitle= element_text(size=12, face="bold", color="black"))
  )
  ggsave(filename = paste("day_biomass_barplot_final_",depth,".png", sep = ""), plot = last_plot())
} #end the loop



#Extra plot to look at the data
ggplot(biomass_site_mean, aes(x=Date, y=mean_site*1000, fill = Classification)) + 
  facet_grid(Site.Category~Depth) +
  geom_bar(stat = "identity") + #<----dont forget the plus 
  #and to make the dates not weird
  coord_flip()


levels(day_site_mean$Date)


#-------------------------------------STATS ANALYSIS--------------------------------

unique(boem_data$Date)

#Added data with 0 values to make plots with n.c. in it....remove these for the stats
biomass_site_mean = biomass_site_mean[!biomass_site_mean$mean_site==0,]



#Checking the type of distribution
library(fitdistrplus)
#Distributions "beta", "cauchy", "chi-squared", "exponential", 
#"gamma", "geometric", "log-normal", "lognormal", "logistic",
#"negative binomial", "normal", "Poisson", "t" and "weibull" 

#Check what the distribution of biomass sums look like
ggplot(data = biomass_site_mean,mapping = aes(mean_site))+
geom_histogram()

#Try a log10 transformation to see how this looks
#Add a column to the boem_biomass_site_mean dataframe
biomass_site_mean[,8] = log10(biomass_site_mean$mean_site)

#add a category that includes the depth and class in one column
biomass_site_mean[,9] =  as.factor(paste(biomass_site_mean$Depth, biomass_site_mean$Classification, sep = "-" ))
biomass_site_mean[,10] = as.factor(paste(biomass_site_mean$Site.Category,biomass_site_mean$Classification, sep = "-" ))
biomass_site_mean[,11] = as.factor(paste(biomass_site_mean$Depth,biomass_site_mean$Season, sep = "-" ))
biomass_site_mean[,12] = as.factor(paste(biomass_site_mean$Site.Category,biomass_site_mean$Depth, sep = "-" ))
biomass_site_mean[,13] = ifelse(biomass_site_mean$Date %in% c("10/17/2013", "2/3/2014", "2/11/2014",
                                                              "2/17/2014", "5/22/2014", "7/30/2014"),
                                                              "Year 1", 
                                                              "Year 2")
biomass_site_mean$V13 = as.factor(biomass_site_mean$V13)
biomass_site_mean[,14] = as.factor(paste(biomass_site_mean$Classification, biomass_site_mean$year, sep = "-" ))


colnames(biomass_site_mean)[8] = "log_biomass"
colnames(biomass_site_mean)[9] = "depth_class"
colnames(biomass_site_mean)[10] = "region_class"
colnames(biomass_site_mean)[11] = "depth_season"
colnames(biomass_site_mean)[12] = "region_depth"
colnames(biomass_site_mean)[13] = "year"
colnames(biomass_site_mean)[14] = "class_year"


#For seasonal Analysis remove the Nights
day_site_mean = biomass_site_mean[!biomass_site_mean$Time =="Night",]

#For depth comparisons -> split the data into surface_site_mean and bottom site mean
surface_site_mean = biomass_site_mean[biomass_site_mean$Depth == "Surface",]
bottom_site_mean = biomass_site_mean[biomass_site_mean$Depth == "Bottom",]
str(day_site_mean)

unique(biomass_site_mean$Site.Category)


#assign this column to just log_biomass for easier call up
log_biomass = biomass_site_mean$log_biomass




#Make summaries based on depth and classification
#Make sure to detach plyr or the summarise gets weird..need it to be dplyr

#log_biomass_means = group_by(biomass_site_mean, depth_class) %>%
  #summarise(
    #mean = mean(log_biomass, na.rm = T),
    #sd = sd(log_biomass, na.rm = T)
 #)


summary(boem_biomass_sum$sum)
summary(biomass_site_mean$mean_site)
#Look at the histogram
ggplot(data = biomass_site_mean,mapping = aes(log_biomass)) +
  geom_histogram(binwidth = 0.1)

?fitdistr

install.packages("lawstat")
library(lawstat)
#null hypothesis there are no differences in variance among the groups 
#in this case sites, dates, and classification
#The results show differences in variance < 0.05
?levene.test
levene.test(log_biomass, biomass_site_mean$depth_class) #unequal variances
levene.test(log_biomass, biomass_site_mean$region_class) #unequal variance
levene.test(log_biomass, biomass_site_mean$Classification) #unequal variance
levene.test(log_biomass, biomass_site_mean$Site.Category)
levene.test(log_biomass, biomass_site_mean$region_depth)
levene.test(log_biomass, biomass_site_mean$Depth)
levene.test(day_site_mean$log_biomass, day_site_mean$Season)


#one-way ANOVA with welch's correction
aov_depth_class = oneway.test(log_biomass~depth_class, data = biomass_site_mean, na.action = na.omit, var.equal = F)
aov_class = oneway.test(log_biomass~Classification, data = biomass_site_mean, na.action = na.omit, var.equal = F)
aov_region_class = oneway.test(log_biomass~region_class, data = biomass_site_mean, na.action = na.omit, var.equal = F)
aov_depth_class
aov_class
aov_region_class
#Contains duncan.test
install.packages("userfriendlyscience")
#if u need to detach
detach("package:userfriendlyscience", unload = T)
library(userfriendlyscience)



#Duncans test tells us which groups are different
posthocTGH(biomass_site_mean$log_biomass, biomass_site_mean$depth_class)
?posthocTGH



#fitdistr(boem_data$carbon_biomass, "weibull")
#another test dont worry about
#?ks.test
#ks.test(boem_data$carbon_biomass, "pweibull", scale=0.003047173, shape=.5974489)

#use the fitdistr to looks at the distribution of newly transformed data
fitdistr(log_biomass, "normal")

#Cullen and Frey graph shows the observations are close to normal
par(mfrow=c(1,1))
descdist(na.exclude(log_biomass), discrete = F)
?descdist


#Test to see if normal is a good estimate of the distribution
#pnorm is just a normal distribution
#not relevant
ks.test(log_biomass, "pnorm", mean=mean(log_biomass), sd=sd(log_biomass))


#--------------------------LINEAR MODELS---------------------------------
#dont think this is a thing
#Not sure
lmodel_log = lm(log_biomass~biomass_site_mean$Classification)
summary(lmodel_log)
lmodel_log1 = lm(log_biomass~biomass_site_mean$depth_class)
summary(lmodel_log1)
lmodel_log2 = lm(log_biomass~biomass_site_mean$region_class)
summary(lmodel_log2)

#-----------------------------ANOVA & t-test----------------------------------------------------------
#depth_class ANOVA
log_aov_depth_class = aov(log_biomass~depth_class, data = biomass_site_mean)
summary(log_aov_depth_class)
#
#classification ANOVA
log_aov_class = aov(log_biomass~Classification, data = biomass_site_mean)
summary(log_aov_class)

#T-test- depth
t_depth = t.test(log_biomass~biomass_site_mean$Depth)
t_depth

#Region based AOV
log_aov_region = aov(log_biomass~Site.Category, data = biomass_site_mean)
summary(log_aov_region)

#region+class aov
log_aov_region_class = aov(log_biomass~region_class, data = biomass_site_mean)
summary(log_aov_region_class)

#Regional & depth
log_aov_region_depth = aov(log_biomass~region_depth, data = biomass_site_mean)
summary(log_aov_region_depth)

#Seasonal aov===> using the day only data
log_aov_season = aov(log_biomass~Season, data = day_site_mean)
summary(log_aov_season)

#depth and seasonal aov using day data
log_aov_depth_season = aov(log_biomass~depth_season, data = day_site_mean)
summary(log_aov_depth_season)


#t-test of the year 1 vs year 2
t_year_surf = t.test(surface_site_mean$log_biomass~surface_site_mean$year)
t_year_surf
t_year_bot = t.test(bottom_site_mean$log_biomass~bottom_site_mean$year)
t_year_bot

#aov of the class and year group for the different depth groups --surf and bot
aov_class_year_surf = aov(log_biomass~class_year, data = surface_site_mean)
summary(aov_class_year_surf)
aov_class_year_bot = aov(log_biomass~class_year, data = bottom_site_mean)
summary(aov_class_year_bot)

unique(boem_data$Depth)
unique(boem_data$Site.Category)

#Look at the pairwise t-test for some interesting stuff
pairwise.t.test(log_biomass, boem_biomass_sum$Date,
                p.adjust.method = "BH", pool.sd = FALSE)
pairwise.t.test(log_biomass, boem_biomass_sum$Classification,
                p.adjust.method = "BH", pool.sd = FALSE)


#Dont know if this model is valid or not still
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lmodel_log)

#Get this residuals for each ANOVA
aov_resid <- residuals(object = log_aov_depth_class)
aov_resid1 = residuals(object = log_aov_class)
aov_resid3 = residuals(object = log_aov_region)
aov_resid4 = residuals(object = log_aov_region_class)
aov_resid5 = residuals(object = log_aov_season)
aov_resid6 = residuals(object = log_aov_depth_season)
aov_resid7 = residuals(object = log_aov_region_depth)
aov_resid8 = residuals(object = aov_class_year_surf)
aov_resid9 = residuals(object = aov_class_year_bot)
par(mfrow=c(2,2))
plot(aov_resid)
plot(aov_resid1)
plot(aov_resid3)
plot(aov_resid4)
plot(aov_resid5)
plot(aov_resid6)
plot(aov_resid7)
plot(aov_resid8)
plot(aov_resid9)


## Run Shapiro-Wilk test to test for normality
#want > 0.05  
shapiro.test(x = aov_resid)
shapiro.test(x = aov_resid1)
shapiro.test(x = aov_resid3) # 0.03 regions
shapiro.test(x = aov_resid4)
shapiro.test(x = aov_resid5) #0.02 seasons
shapiro.test(x = aov_resid6) #0.02 seasons
shapiro.test(x = aov_resid7) #0.15
shapiro.test(x = aov_resid8) #0.92
shapiro.test(x = aov_resid9) #0.32
#Contains duncan.test
install.packages("agricolae")
library(agricolae)

#Duncans test tells us which groups are different
#p = 0.05
?duncan.test
dunc_class = (duncan.test(log_aov_class,"Classification", alpha = 0.05, console= T))
dunc_region = (duncan.test(log_aov_region,"Site.Category", alpha = 0.05, console= T))
dunc_depth_class = (duncan.test(log_aov_depth_class,"depth_class", alpha = 0.05, console= T))
dunc_region_class = (duncan.test(log_aov_region_class,"region_class", alpha = 0.05, console= T))
dunc_season = (duncan.test(log_aov_season,"Season", alpha = 0.05, console= T))
dunc_depth_season = (duncan.test(log_aov_depth_season,"depth_season", alpha = 0.05, console= T))
dunc_region_depth = (duncan.test(log_aov_region_depth,"region_depth", alpha = 0.05, console= T))
dunc_class_year_surf= (duncan.test(aov_class_year_surf,"class_year", alpha = 0.05, console= T))
dunc_class_year_bot= (duncan.test(aov_class_year_bot,"class_year", alpha = 0.05, console= T))

#Make the results from the duncan test into dataframes
class_df = as.data.frame(dunc_class$groups)
region_df = as.data.frame(dunc_region$groups)
depth_class_df = as.data.frame(dunc_depth_class$groups)
region_class_df = as.data.frame(dunc_region_class$groups)
season_df = as.data.frame(dunc_season$groups)

#change the heading labels for the dataframes
colnames(class_df) <- c( "Log Biomass", "Groups")
colnames(region_df) <- c("Log Biomass", "Groups")
colnames(depth_class_df) <- c( "Log Biomass", "Groups")
colnames(region_class_df) <- c( "Log Biomass", "Groups")
colnames(season_df) <- c("Log Biomass", "Groups")
?grid.table

#check out this package its CRAY
library(gridExtra)
#initiate the making of pdf giving it the right dimensions
pdf("duncan_tables1.pdf", heigh = 11, width = 8.5)
#grid arrange doesnt with with grid.table
grid.arrange(
  tableGrob(class_df, theme = ttheme_minimal()),
  tableGrob(region_df, theme = ttheme_minimal()),
  tableGrob(depth_class_df, theme = ttheme_minimal()),
  tableGrob(region_class_df, theme = ttheme_minimal()),
  tableGrob(season_df, theme = ttheme_minimal()),
  nrow = 3
)
dev.off()
?grid.table
#write.csv(rbind(date_df, class_df, region_df), "duncan.csv")

#install.packages("xlsx")
#library(xlsx)

#wb = createWorkbook()

#sheet = createSheet(wb, "Sheet 1")

#addDataFrame(dataframe1, sheet=sheet, startColumn=1, row.names=FALSE)
#addDataFrame(dataframe2, sheet=sheet, startColumn=10, row.names=FALSE)

#saveWorkbook(wb, "My_File.xlsx")

#Measures this effect size
install.packages("lsr")
library(lsr)
etaSquared(log_aov) #Date
etaSquared(log_aov1) #Class
etaSquared(log_aov3) #Site

install.packages("effsize")
library(lsr)

####---------------------END------------------------------------- ###
#----------------------------------------------Rest is just extra junk code------------------------------------------------------------------
## Rest is just extra information not relevant to paper

#install.packages("effsize")
#library(effsize)



#theme(axis.text.x = element_text(angle = 90))

#Ordering factors variables


library(dplyr)

#boem_data = 
  #mutate(boem_data,
         #Date = factor(Date, levels=unique(Date)))
#boem_data
#Use ddply to make summarize
#?ddply
#This shit is crazay
#mean_table = ddply(boem_data,.(Date), summarize,
              #means = mean(log_carbon), sd = sd(log_carbon))
 
#mean_table
#ggplot(data=mean_table,mapping = aes(x=Date, y = means))+
   #facet_wrap(~Classification) +
   #geom_bar(stat = "identity")


#Use dplyr to get means and shit different than the plyr ddply seen above
#group_by(boem_data, Date) %>%
  #summarise(
    #count = n(),
    #mean = mean(log_carbon, na.rm = T),
    #sd = sd(log_carbon, na.rm = T)
    #)

#making a box plot
ggplot(boem_data, aes(x=Date, y=carbon_biomass)) + 
  geom_boxplot() + #and to make the dates not weird
  coord_flip()
#theme(axis.text.x = element_text(angle = 90))

install.packages("lattice")
library(lattice)

histogram(~ carbon_biomass | Date, 
          data=boem_data,
          layout=c(1,3)) 
histogram(~ carbon_biomass | Classification, 
          data=boem_data,
          layout=c(1,4)) 

?histogram
boem_aov = aov(carbon_biomass~Date, data = boem_data)
summary(boem_aov)

boem_aov_class = aov(carbon_biomass~Classification, data = boem_data)
summary(boem_aov_class)

pairwise.t.test(boem_data$carbon_biomass, boem_data$Date,
                p.adjust.method = "BH", pool.sd = FALSE)
pairwise.t.test(boem_data$carbon_biomass, boem_data$Classification,
                p.adjust.method = "BH", pool.sd = FALSE)

boem_oneway_date = oneway.test(carbon_biomass~Date, data = boem_data)
summary(boem_oneway_date)

boem_oneway_class = oneway.test(carbon_biomass~Classification, data = boem_data)
summary(boem_oneway_class)

plot(boem_aov,1)
plot(boem_aov_class,1)

plot(boem_aov,2)
plot(boem_aov_class,2)

# Extract the residuals
 

ks.test(x=aov_residuals, y = "pnorm")
ks.test(x=rnorm(10^4),y='pnorm',alternative='two.sided')
?ks.test

#used when ANOVA assumptions are not met
kruskal.test(carbon_biomass ~ Date, data = boem_data)

#Dunn test
install.packages( "FSA")
library(FSA)

PT = dunnTest(carbon_biomass ~ Date,
              data=boem_data,
              method="bh") 
PT


install.packages("rcompanion")
library(rcompanion)
cldList(comparison = PT$Comparison,
        p.value    = PT$P.adj,
        threshold  = 0.05)


install.packages("car")
library(car)
leveneTest(carbon_biomass ~ Date, data = boem_data)
leveneTest(carbon_biomass ~ Classification, data = boem_data)



library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

