##install.packages('scales')
##install.packages('ggthemes')
##install.packages('lubridate')

getwd()
setwd("..../Desktop/datasets")


library(scales)
library(ggplot2)
library(ggthemes)
library(lubridate)

TSPData <- read.csv('shareprices.csv')
str(TSPData)


TSPData$date <- as.Date(TSPData$date,'%m/%d/%Y')

ggplot() +
  geom_line(aes(x = TSPData$date,y = TSPData$G,color = 'G Fund'),linetype = 'solid',size = 1) +
  geom_line(aes(x = TSPData$date,y = TSPData$F,color = 'F Fund'),linetype = 'solid',size = 1) +
  geom_line(aes(x = TSPData$date,y = TSPData$C,color = 'C Fund'),linetype = 'longdash',size = 1) +
  geom_line(aes(x = TSPData$date,y = TSPData$S,color = 'S Fund'),linetype = 'longdash',size = 1) +
  geom_line(aes(x = TSPData$date,y = TSPData$I,color = 'I Fund'),linetype = 'dashed',size = 1) +
  theme(legend.position = c(0.1,.89)) +
  labs(x = '',y = 'Price per Share ($)',
       title = 'TSP Gains (Jan 1, 2008 - Dec 31,2019)',col = ' Funds')
#############################-------------------------------------------------------------%%%%%%%  

ggplot()+
  geom_boxplot(aes(x = as.factor(colnames(TSPData[2])), y = as.integer(TSPData$G)),size = 1) +
  geom_boxplot(aes(x = as.factor(colnames(TSPData[3])), y = as.integer(TSPData$F)),size = 1) +
  geom_boxplot(aes(x = as.factor(colnames(TSPData[4])), y = as.integer(TSPData$C)),size = 1) +
  geom_boxplot(aes(x = as.factor(colnames(TSPData[5])), y = as.integer(TSPData$S)),size = 1) +
  geom_boxplot(aes(x = as.factor(colnames(TSPData[6])), y = as.integer(TSPData$I)),size = 1) +
  theme_economist()+
  geom_jitter() + 
  labs(x = 'Funds',y = 'Dollars per Share ($)',
       title = 'TSP Boxplots(Jan 1, 2008 - Dec 31,2019)')

#--------Boxplot Stats--------------
GBoxStats <- boxplot.stats(TSPData$G)
FBoxStats <- boxplot.stats(TSPData$F)
CBoxStats <- boxplot.stats(TSPData$C)
IBoxStats <- boxplot.stats(TSPData$I)
SBoxStats <- boxplot.stats(TSPData$S)
                             
FundStats <- matrix(c(GBoxStats$stats[3],GBoxStats$stats[5],GBoxStats$stats[1],GBoxStats$stats[2],GBoxStats$stats[4],
                      FBoxStats$stats[3],FBoxStats$stats[5],FBoxStats$stats[1],FBoxStats$stats[2],FBoxStats$stats[4],
                      CBoxStats$stats[3],CBoxStats$stats[5],CBoxStats$stats[1],CBoxStats$stats[2],CBoxStats$stats[4],
                      IBoxStats$stats[3],IBoxStats$stats[5],IBoxStats$stats[1],IBoxStats$stats[2],IBoxStats$stats[4],
                      SBoxStats$stats[3],SBoxStats$stats[5],SBoxStats$stats[1],SBoxStats$stats[2],SBoxStats$stats[4]),
                    nrow  = 5, byrow = T)
colnames(FundStats) <- c('Median','High','Low','1st Quartile','3rd Quartile')
rownames(FundStats) <- c('G Fund','F Fund','C Fund','I Fund','S Fund')
FundStats <- as.table(FundStats)
print(FundStats)
#---------------------------------------------------------------------------------------------------------------------

TSPData$month <- month(TSPData$date,label = T)
TSPData$year <- year(TSPData$date)

#C Bar----------------------------------------------------------------------------------------
CStd <- sd(TSPData$C)
CAvg <- aggregate(TSPData$C ~ TSPData$month,TSPData,mean)

ggplot(CAvg,aes(CAvg$`TSPData$month`,CAvg$`TSPData$C`)) + 
  geom_bar(stat = 'identity',fill = 'steelblue',width = 0.9) +
  geom_errorbar(aes(ymin = CAvg$`TSPData$C` - CStd, ymax = CAvg$`TSPData$C` + CStd),width=0.5,
                position = position_dodge(0.9)) + theme_economist() +
  labs(x = '',y = 'AVG Price ($)',title='C Fund Averages (2008 - 2019)' ) + 
  geom_text(aes(label = round(CAvg$`TSPData$C`,3)),
            position=position_dodge(width=0.9), vjust=-0.25,
            size = 5,color = 'magenta')
                                                    
#---------------------------------------------------------------------------------------------------
SAvg <- aggregate(TSPData$S ~ TSPData$month,TSPData,mean)
SStd <- sd(TSPData$S)
SAvg <- aggregate(TSPData$S ~ TSPData$month,TSPData,mean)

ggplot(SAvg,aes(CAvg$`TSPData$month`,SAvg$`TSPData$S`)) + 
  geom_bar(stat = 'identity',fill = 'steelblue',width = 0.9) +
  geom_errorbar(aes(ymin = SAvg$`TSPData$S` - SStd, ymax = SAvg$`TSPData$S` + SStd),width=0.5,
                position = position_dodge(0.9)) + theme_economist() +
  labs(x = '',y = 'AVG Price ($)',title='S Fund Averages (2008-2019)' ) + 
  geom_text(aes(label = round(SAvg$`TSPData$S`,3)),
              position=position_dodge(width=0.9), vjust=-0.25,
              size = 5,color = 'magenta')
#----------------------------------------------------------------------------------------------------
IAvg <- aggregate(TSPData$I ~ TSPData$month,TSPData,mean)
IStd <- sd(TSPData$I)
IAvg <- aggregate(TSPData$I ~ TSPData$month,TSPData,mean)

ggplot(CAvg,aes(IAvg$`TSPData$month`,IAvg$`TSPData$I`)) + 
  geom_bar(stat = 'identity',fill = 'steelblue',width = 0.9) +
  geom_errorbar(aes(ymin = IAvg$`TSPData$I` - IStd, ymax = IAvg$`TSPData$I` + IStd),width=0.5,
                position = position_dodge(0.9)) + theme_economist() +
  labs(x = '',y = 'AVG Price ($)',title = 'I Fund Averages (2008 - 2019)' ) + 
  geom_text(aes(label = round(IAvg$`TSPData$I`,3)),
              position=position_dodge(width=0.9), vjust=-0.25,
             size = 5,color = 'magenta')
#--------------------------------------------------------------------------------------------------
FAvg <- aggregate(TSPData$F ~ TSPData$month,TSPData,mean)
FStd <- sd(TSPData$F)
IAvg <- aggregate(TSPData$I ~ TSPData$month,TSPData,mean)

ggplot(IAvg,aes(IAvg$`TSPData$month`,IAvg$`TSPData$I`)) + 
  geom_bar(stat = 'identity',fill = 'steelblue',width = 0.9) +
  geom_errorbar(aes(ymin = IAvg$`TSPData$I` - IStd, ymax = IAvg$`TSPData$I` + IStd),width=0.5,
                position = position_dodge(0.9)) + theme_economist() +
  labs(x = '',y = 'AVG Price ($)', title = 'F Fund Averages (2008-2019)' ) + 
  geom_text(aes(label = round(IAvg$`TSPData$I`,3)),
            size = 5,color = 'magenta') 
#---------------------------------------------------------------------------------------------------





