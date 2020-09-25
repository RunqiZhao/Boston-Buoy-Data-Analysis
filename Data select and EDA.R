# MA615 Boston-Buoy-Data-Analysis
# Data select and EDA
library(tidyverse)
library(corrplot)
library(lubridate)
library(GGally)

load("BuoyData.Rdata")
tmpdata  <-  data_87_19[ , !names(data_87_19) %in% c("WD","WDIR","MWD","VIS","TIDE","DEWP")]



planes <-  group_by(tmpdata, DATE)
delay  <-  summarise(planes, WSPD = mean(WSPD, na.rm = TRUE),
                     GST = mean(GST, na.rm = TRUE), 
                     WVHT = mean(WVHT, na.rm = TRUE), 
                     DPD = mean(DPD, na.rm = TRUE),
                     APD = mean(APD, na.rm = TRUE), 
                     PRES = mean(PRES, na.rm = TRUE),
                     ATMP = mean(ATMP, na.rm = TRUE), 
                     WTMP = mean(WTMP, na.rm = TRUE))
# delay$DATE[2]-delay$DATE[1] == 1


planes2 <-  group_by(tmpdata, MONTH)
delay2  <-  summarise(planes2, WSPD = mean(WSPD, na.rm = TRUE),
                      GST = mean(GST, na.rm = TRUE), 
                      WVHT = mean(WVHT, na.rm = TRUE), 
                      DPD = mean(DPD, na.rm = TRUE),
                      APD = mean(APD, na.rm = TRUE), 
                      PRES = mean(PRES, na.rm = TRUE),
                      ATMP = mean(ATMP, na.rm = TRUE), 
                      WTMP = mean(WTMP, na.rm = TRUE))

tmpdata  <-  na.omit(tmpdata)
planes3 <-  group_by(tmpdata, DATE)
delay3  <-  summarise(planes3, WSPD = mean(WSPD, na.rm = TRUE),
                      GST = mean(GST, na.rm = TRUE), 
                      WVHT = mean(WVHT, na.rm = TRUE), 
                      DPD = mean(DPD, na.rm = TRUE),
                      APD = mean(APD, na.rm = TRUE), 
                      PRES = mean(PRES, na.rm = TRUE),
                      ATMP = mean(ATMP, na.rm = TRUE), 
                      WTMP = mean(WTMP, na.rm = TRUE))

cor_de <- cor(delay3[,2:9])

res_cor <- cor(cor_de)
corrplot(corr=res_cor)
# correlation
corrplot(corr = res_cor,order = "AOE",type="upper",method="pie",tl.pos = "d",tl.cex = 0.75,tl.col = "black")
corrplot(corr = res_cor,add=TRUE, type="lower", method="number",order="AOE",diag=FALSE,tl.pos="n", cl.pos="n")

#plot(delay$DATE,delay$ATMP,type = "l")
ggplot(data = delay2, aes(x = MONTH, y = ATMP))+
  geom_line()

#plot(delay$GST,delay$ATMP)
#plot(delay2$MONTH,delay2$WTMP,type = "l")


year <- year(delay2$MONTH)
month <- month(delay2$MONTH)
aveTEM <- delay2$ATMP
Tdata <- data.frame(year, month, aveTEM)
names(Tdata) <- c("year","month","MeanTemp")

# heat map
ggplot(data=Tdata, aes(x=year,y=month)) + 
  theme_bw(base_family = "STKaiti") +
  geom_tile(aes(fill = MeanTemp),colour = "white") + 
 # geom_text(aes(label = round(MeanTemp,1))) +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(10,'Spectral'))) + 
  theme(legend.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position="top") + 
  ggtitle("Mean Temp") +
  labs(x="Year",y = "Month") +
  theme(plot.title = element_text(hjust = 0.5))

## matrix plot #day
smdata <- data.frame(delay3[2:9])
ggscatmat(smdata) + theme_bw(base_family = "STKaiti") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "",y = "")
# by month
smdata <- data.frame(delay2[2:9])
ggscatmat(smdata) + theme_bw(base_family = "STKaiti") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "",y = "")


