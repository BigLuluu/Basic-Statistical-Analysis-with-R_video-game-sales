#-------------------------------------------------------------------#
# Module 6 Final Project - Video Game Sales Analysis: Christina Jin #
#-------------------------------------------------------------------#

# Load libraries
install.packages("plyr")
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
library("plyr")
library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("plotrix")
library("ggplot2")
library("moments")
library(colorspace)
library(RColorBrewer)
library(vcd)



# Loading data
vgfull <- read.csv("C:/Users/Lu_ki/OneDrive - Northeastern University/NU Master Courseworks/ALY6000/Course Materials/Module 6/3_Project/Datasets/vgsales.csv")
summary(vgfull)



# Data cleaning: filter records with Year and Publisher = N/A
vgsales <- vgfull[vgfull$Year != "N/A" & vgfull$Publisher !="N/A", ] 
vgsales$Rank <- NULL
summary(vgsales)



# plot overall global gaming sales analysis
def_par <- par (mar=c(5,5,4,0))
hist(vgsales$Global_Sales,
     breaks = 400,
     xlab = "Sales (in Million)",
     ylab = "Frequency",
     main = "Global Game Sales",
     col = c("cyan"),
     cex.axis = 2, cex.lab = 2,cex.main = 3)
# plot overall global gaming sales analysis with less frequency limit
hist(vgsales$Global_Sales,
     breaks = 200,
     ylim = c(0,100),
     xlab = "Sales (in Million)",
     ylab = "Frequency",
     main = "Global Game Sales",
     col = c("cyan"),
     cex.axis = 2, cex.lab = 2,cex.main = 3)
# plot overall global gaming sales analysis with less sales limit
hist(vgsales$Global_Sales,
     breaks = 3200,
     xlim = c(0, 5),
     xlab = "Sales (in Million)",
     ylab = "Frequency",
     main = "Global Game Sales",
     col = c("cyan"),
     cex.axis = 2, cex.lab = 2,cex.main = 3)



# Descriptive Data: Central Tendency Measurements
NorthA <-  vgsales$NA_Sales
EU <- vgsales$EU_Sales
JP <- vgsales$JP_Sales
Other <- vgsales$Other_Sales
Global <- vgsales$Global_Sales

mode <- function(x)
{ux <- unique(x)
tab <- tabulate(match(x,ux))
ux[tab == max(tab)]}

# finding Min, Mode, Q1, Median, Mean, Q3, and Max
df_NA <- data.frame("NA" = c(min(NorthA), 
                            mode(NorthA),
                            quantile(NorthA,probs = c(0.25)),
                            median(NorthA),
                            mean(NorthA),
                            quantile(NorthA,probs = c(0.75)),
                            max(NorthA)))
df_EU <- data.frame("EU" = c(min(EU), 
                            mode(EU),
                            quantile(EU,probs = c(0.25)),
                            median(EU),
                            mean(EU),
                            quantile(EU,probs = c(0.75)),
                            max(EU)))
df_JP <- data.frame("JP" = c(min(JP), 
                             mode(JP),
                             quantile(JP,probs = c(0.25)),
                             median(JP),
                             mean(JP),
                             quantile(JP,probs = c(0.75)),
                             max(JP)))
df_Other <- data.frame("Other" = c(min(Other), 
                                  mode(Other),
                                  quantile(Other,probs = c(0.25)),
                                  median(Other),
                                  mean(Other),
                                  quantile(Other,probs = c(0.75)),
                                  max(Other)))
df_Global <- data.frame("Global" = c(min(Global), 
                                   mode(Global),
                                   quantile(Global,probs = c(0.25)),
                                   median(Global),
                                   mean(Global),
                                   quantile(Global,probs = c(0.75)),
                                   max(Global)))
df_stat <- data.frame(c(1:7),round(df_NA,2), round(df_EU,2), round(df_JP,2), 
                      round(df_Other,2), round(df_Global,2))
df_stat
# flip row and columns for easy plotting
df_stat = data.frame(t(df_stat[-1]))
df_stat
row.names(df_stat)<-c("North America", "Europe", "Japan", "Other Countries", "Global")
colnames(df_stat) <- c("Min", "Mode","Q1","Median","Mean","Q3","Max")
df_stat

# setting color ramps
color.function1 <- colorRampPalette(c(rgb(136,43,229,max=255),rgb(124,239,216,max=255)))
color.ramp1 <- color.function1(n=nrow(x))
color.function2 <- colorRampPalette(c(rgb(102,0,102,max=255),rgb(255,153,51,max=255)))
color.ramp2 <- color.function2(n=nrow(x))
color.function3 <- colorRampPalette(c(rgb(124,239,216,max=255),rgb(136,43,229,max=255)))
color.ramp3 <- color.function3(n=4)
color.function4 <- colorRampPalette(c(rgb(255,153,51,max=255), rgb(102,0,102,max=255)))
color.ramp4 <- color.function4(n=10)

# plot all descriptive statistics above for each region
par(mar=c(6,6,4,4))
rp = barplot(height=df_stat$Min, width=1,space=0.15,
          ylab = "Sales (in Million)",las = 1,
          ylim=c(0,0.6),
          cex.lab = 1.5, xlab=NA,
          col="white", border = NA)
box(lty=1, col = "black")
text(seq(0.65,6, by=1.15),y=-0.01, adj=1, 
     xpd=TRUE, label = c(rownames(df_stat)), 
     cex=1.25, srt=40)
lines(rp, df_stat$Min, type = "b", cex=1.25, pch=19, lwd=3, col="Salmon")
lines(rp, df_stat$Q1, type = "b", cex=1.25, pch=19, lwd=3, col="Orange")
lines(rp, df_stat$Median, type = "b", cex=1.25, pch=19, lwd=3, col="lightgreen")
  text(seq(0.65,6, by=1.15), y=df_stat$Median,label=df_stat$Median, pos=2, col="lightgreen")
lines(rp, df_stat$Q3, type = "b", cex=1.25, pch=19, lwd=3, col="lightblue")
  text(seq(0.65,6, by=1.15), y=df_stat$Q3,label=df_stat$Q3, pos=4, col="lightblue")
lines(rp, df_stat$Mean, type = "b", cex=1.25, pch=19, lwd=3, col="pink")
  text(seq(0.65,6, by=1.15), y=df_stat$Mean,label=df_stat$Mean, pos=3, col="pink")
title("Regional Sales Statistics", line=1.5, cex.main=2)
legend("topleft", inset=0.005, c("Mean","Q3","Median", "Q1","Min"),
       pch=19, cex=1, col=c("pink","lightblue","lightgreen","Orange","Salmon"), bty="n", 
       bg="transparent", pt.cex=1.75,y.intersp = 0.25)


# sum of each regional market
df_sum_tmp <- data.frame(sum=c(sum(NorthA), sum(EU), sum(JP), sum(Other),sum(Global)))
row.names(df_sum_tmp)<-c("North America", "Europe", "Japan", "Other Countries", "Global")
colnames(df_sum_tmp) <- c("Total Sales")
df_sum_tmp
Percent <- (df_sum_tmp$`Total Sales`/sum(Global))*100
df_sum <- data.frame(cbind(df_sum_tmp,Percent=round(Percent,1)))
df_sum
df_sum_region <- df_sum[1:4,]
df_sum_region

# plot pipe chart
pie(df_sum_region$Percent, label = "",
    col = color.ramp3,edges = 200, radius =1, clockwise = TRUE, border = NA)
text(locator(4),c("North America 49.1%","Europe 27.3%","Japan 14.6%", "Other Countries 9.0%"),col=color.ramp3)
title("Regional Sales Percentage", line=0.5, cex.main=2)
  


# find the top 10 games with THE most global sales 
topgames <- head(vgsales[,c(1:5,10)], n=10)
topgames
#  plot bar chart
par(mar=c(8,5,4,0)+1)
barplot(height=topgames$Global_Sales,
        width = 1,space = 0.15,
        border = NA,
        ylab = "Global Sales (in Million)",
        ylim = c(0,90),
        main = "Top 10 Global Sales By Game",
        cex.axis = 1.25, cex.lab = 1.25,cex.main = 2,
        col=color.ramp1)
text(seq(0.65,11, by=1.15),y=-2, adj=1, 
     xpd=TRUE, label = topgames$Name, 
     cex=1.25, srt=35)
text(seq(0.65,11, by=1.15), y=topgames$Global_Sales, label=topgames$Global_Sales, pos=3)



# Best-selling games - Top 10 (continuous, and modified)
# count distinct games
count(distinct(vgsales,vgsales$Name))
# combine records of the same game (with the same name)
# that are published at different years or platforms
# and list the top 10
game <- aggregate(vgsales, list(Game_Name=vgsales$Name), function(x)
  if(is.numeric(x))sum(x) else paste0(x, collapse = "/"))
game$Name <- NULL
game <- arrange(game, desc(game$Global_Sales))
head(game, n=10)
topgames_rev <- head(game[,c(1:5,10)], n=10)
topgames_rev

#  plot new bar chart for the updated data set
par(mar=c(9,5,4,0)+1)
barplot(height=topgames_rev$Global_Sales,
        width = 1,space = 0.15,
        border = NA,
        ylab = "Global Sales (in Million)",
        ylim = c(0,90),
        main = "Top 10 Global Sales By Game (Updated)",
        cex.axis = 1.25, cex.lab = 1.25,cex.main = 2,
        col=color.ramp1)
text(seq(0.65,11, by=1.15),y=-2, adj=1, 
     xpd=TRUE, label = topgames_rev$Game_Name, 
     cex=1.15, srt=35, col=color.ramp1)
text(seq(0.65,11, by=1.15), y=topgames_rev$Global_Sales,
     label=topgames_rev$Global_Sales, pos=3, col=color.ramp1)



# find the popularity (frequency) of each genre of games around the world
unique(vgsales$Genre)
count(vgsales,Genre)
Count <- table(vgsales$Genre)
Percent <- (Count/sum(Count))*100
popular_genre <- data.frame(cbind(Count, Percent=round(Percent,2)))
popular_genre
popular_genre <- arrange(popular_genre,desc(Percent))
popular_genre
popular_genre <- cbind(popular_genre,
           CumCount=cumsum(popular_genre$Count),
           CumPercent=paste(cumsum(popular_genre$Percent),"%",sep = ""))
popular_genre


# plot bar chart of Genre Popularity Distribution
par(mar=c(6,7,4,5))
pg = barplot(height=popular_genre$Count,
        width=1, space=0.15, las=2,
        ylim=c(0,5.02*max(popular_genre$Count, na.rm = TRUE)),
        axes = F,border = NA,
        ylab = "Cummulative Counts", line=4,
        cex.lab = 1.75,
        col=color.ramp2)
title("Gerne Popularity Distribution", line=1.5, cex.main=2)
text(seq(0.65,14, by=1.15),y=-300, adj=1, 
     xpd=TRUE, label = c(rownames(popular_genre)), 
     cex=1.25, srt=45, col=color.ramp2)
text(seq(0.65,14, by=1.15), y=popular_genre$Count, 
     label=popular_genre$Count, pos=3, col = color.ramp2)
lines(pg, popular_genre$CumCount, type = "b", cex=0.7, pch=19, col=color.ramp2)
axis(2, at=popular_genre$CumCount, 
     labels=popular_genre$CumCount, 
     tick=TRUE, cex.axis=0.95, las=1)
axis(4, at=popular_genre$CumCount, 
     labels=popular_genre$CumPercent, 
     tick=TRUE, cex.axis=0.95, las=1)



# find some popular gaming platforms that had the most games released
# and most game been played by the public
unique(vgsales$Platform)
count(vgsales,Platform)
Count <- table(vgsales$Platform)
Percent <- (Count/sum(Count))*100
popular_Platform <- data.frame(cbind(Count, Percent=round(Percent,2)))
popular_Platform
popular_Platform <- arrange(popular_Platform,desc(Percent))
popular_Platform
popular_Platform <- cbind(popular_Platform,
                       CumCount=cumsum(popular_Platform$Count),
                       CumPercent=paste(cumsum(popular_Platform$Percent),"%",sep = ""))
popular_Platform <- head(popular_Platform, n=10)
popular_Platform


# plot bar chart of Platform Popularity Distribution
par(mar=c(4,7,3,5))
pg = barplot(height=popular_Platform$Count,
             width=1, space=0.15, las=2,
             ylim=c(0,6.5*max(popular_Platform$Count, na.rm = TRUE)),
             axes = F,border = NA,
             ylab = "Cummulative Counts", line=4,
             cex.lab = 1.75,
             col=color.ramp4)
title("Plaform Popularity Distribution", line=1, cex.main=2)
text(seq(0.65,12, by=1.15),y=-300, adj=1, 
     xpd=TRUE, label = c(rownames(popular_Platform)), 
     cex=1.25, srt=45, col=color.ramp4)
text(seq(0.65,12, by=1.15), y=popular_Platform$Count, 
     label=popular_Platform$Count, pos=3, col = color.ramp4)
lines(pg, popular_Platform$CumCount, type = "b", cex=0.7, pch=19, col=color.ramp4)
axis(2, at=popular_Platform$CumCount, 
     labels=popular_Platform$CumCount, 
     tick=TRUE, cex.axis=0.95, las=1)
axis(4, at=popular_Platform$CumCount, 
     labels=popular_Platform$CumPercent, 
     tick=TRUE, cex.axis=0.95, las=1)



# find some popular gaming publisher that had the most games released
# and most game been played by the public
unique(vgsales$Publisher)
count(vgsales,Publisher)
Count <- table(vgsales$Publisher)
Percent <- (Count/sum(Count))*100
popular_Publisher <- data.frame(cbind(Count, Percent=round(Percent,2)))
popular_Publisher
popular_Publisher <- arrange(popular_Publisher,desc(Percent))
popular_Publisher
popular_Publisher <- cbind(popular_Publisher,
                          CumCount=cumsum(popular_Publisher$Count),
                          CumPercent=paste(cumsum(popular_Publisher$Percent),"%",sep = ""))
popular_Publisher <- head(popular_Publisher, n=10)
popular_Publisher


# plot bar chart of Publisher Popularity Distribution
par(mar=c(9.5,8,2,5)+1)
pg = barplot(height=popular_Publisher$Count,
             width=1, space=0.15, las=2,
             ylim=c(0,8*max(popular_Publisher$Count, na.rm = TRUE)),
             axes = F,border = NA,
             ylab = "Cummulative Counts", line=4,
             cex.lab = 1.75,
             col=color.ramp2)
title("Publisher Popularity Distribution", line=0.5, cex.main=2)
text(seq(0.65,12, by=1.15),y=-300, adj=1, 
     xpd=TRUE, label = c(rownames(popular_Publisher)), 
     cex=1.25, srt=40, col=color.ramp2)
text(seq(0.65,12, by=1.15), y=popular_Publisher$Count, 
     label=popular_Publisher$Count, pos=3, col = color.ramp2)
lines(pg, popular_Publisher$CumCount, type = "b", cex=0.7, pch=19, col=color.ramp2)
axis(2, at=popular_Publisher$CumCount, 
     labels=popular_Publisher$CumCount, 
     tick=TRUE, cex.axis=0.95, las=1)
axis(4, at=popular_Publisher$CumCount, 
     labels=popular_Publisher$CumPercent, 
     tick=TRUE, cex.axis=0.95, las=1)





