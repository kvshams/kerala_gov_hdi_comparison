#Download raw data
#https://globaldatalab.org/shdi/shdi/IND/?levels=1%2B4&interpolation=0&extrapolation=0&nearest_real=0
# Download data teh data and save as a csv file eg: GDL_Sub_national_HDI_data_05262020.csv 
require(data.table)
require(ggplot2)

hdi_data <- read.table("GDL_Sub_national_HDI_data_05262020.csv", h=T, sep=",", stringsAsFactors=F)
hdi_data <- hdi_data[hdi_data$Region=="Kerala",]
hdi_data <- hdi_data[,colnames(hdi_data) %like% "X"]
hdi_data <- as.data.frame(t(hdi_data))
#hdi_data <- t(hdi_data)
colnames(hdi_data) <- c("HDI")
hdi_data$Years <- rownames(hdi_data)
#hdi_data <- hdi_data[hdi_data$Years %like% "X",]
hdi_data$Years <- as.numeric(gsub("X","",hdi_data$Years))
hdi_data$HDI <- as.numeric(as.character(hdi_data$HDI))


hdi_data$HDI_PreviousYear <- NA

for (year in seq(1991,2018)) {
	hdi_data[hdi_data$Years==year,]$HDI_PreviousYear <-  hdi_data[hdi_data$Years==(year-1),]$HDI
}
hdi_data$Increase <-  hdi_data$HDI - hdi_data$HDI_PreviousYear
# https://kerala.gov.in/chief-ministers-since-1957

hdi_data$Gov_inPreviousYear_5YearMean <- 	ifelse(hdi_data$Years>=1992 & hdi_data$Years<=1996 , "UDF_1992-1996",
								ifelse(hdi_data$Years>=1997 & hdi_data$Years<=2001,"LDF_1997-2001",
								ifelse(hdi_data$Years>=2002 & hdi_data$Years<=2006,"UDF_2002-2006",
								ifelse(hdi_data$Years>=2007 & hdi_data$Years<=2011,"LDF_2007-2011",
								ifelse(hdi_data$Years>=2012 & hdi_data$Years<=2016,"UDF_2012-2016","LDF_remaining")))))
hdi_data$Gov_inPreviousYear <- 	ifelse(hdi_data$Years>=1992 & hdi_data$Years<=1996 , "UDF",
								ifelse(hdi_data$Years>=1997 & hdi_data$Years<=2001,"LDF",
								ifelse(hdi_data$Years>=2002 & hdi_data$Years<=2006,"UDF",
								ifelse(hdi_data$Years>=2007 & hdi_data$Years<=2011,"LDF",
								ifelse(hdi_data$Years>=2012 & hdi_data$Years<=2016,"UDF","LDF")))))
hdi_data$Years <- as.factor(hdi_data$Years)



#### Visualize the Plots
ggplot(hdi_data, aes(x=Years, y=Increase, color=Gov_inPreviousYear)) + geom_point()+
                geom_smooth(method=lm)+
                theme_classic()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                scale_x_continuous(breaks=seq(1990, 2018))


ggplot(hdi_data, aes(x=Years, y=HDI, color=Gov_inPreviousYear)) +
                 geom_point()+ geom_smooth(method=lm)+
                 theme_classic()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                 scale_x_continuous(breaks=seq(1990, 2018))

##### Caluctae aggregate mean increase
Mean_increase <- aggregate(x = hdi_data$Increase,
          by = list(hdi_data$Gov_inPreviousYear_5YearMean),
          FUN = mean) 


colnames(Mean_increase) <- c("Gov","Mean_HDI_Increase")
Mean_increase$Alliance <- ifelse(Mean_increase$Gov %like% "UDF", "UDF", "LDF")

#### Visualize the Plot
ggplot(Mean_increase, aes(x=Alliance, y=Mean_HDI_Increase)) + geom_boxplot() +
                   geom_jitter(shape=16, position=position_jitter(0.2)) +
                   theme_classic()




