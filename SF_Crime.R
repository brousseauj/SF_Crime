library("RSocrata")
library(lubridate)
library(chron)
setwd("~/Desktop/Fun Data for Fun/SF_Crime/")

rd=read.csv("Map__Crime_Incidents_-_from_1_Jan_2003.csv")
pop = 837442


#SF finance estimates 
total_pop=read.csv('tot_pop.csv')
colnames(total_pop)=c('Year','Population')
total_pop$Population=as.numeric(gsub('\\,','',total_pop$Population))

# complete cases only
rd= rd[complete.cases(rd),]
df = rd[c(2,4,5,6,7,10,11)]
df$Date=as.POSIXlt(df$Date,format='%m/%d/%Y')
df$Time=as.POSIXlt(df$Time,format='%H:%M')

irrelevant_crime=c('NON-CRIMINAL','OTHER OFFENSES',
                   'DRUNKENNESS','FORGERY/COUNTERFEITING',
                   'LOITERING','EMBEZZLEMENT','FAMILY OFFENSES',
                   'BAD CHECKS','BRIBERY','TREA','WARRANTS','MISSING PERSON',
                   'FRAUD','SECONDARY CODES','SUSPICIOUS OCC','RECOVERED VEHICLE',
                   'RUNAWAY','SUICIDE','GAMBLING','PORNOGRAPHY/OBSCENE MAT',
                   'SEX OFFENSES, NON FORCIBLE','LIQUOR LAWS')


df= df[(!df$Category %in% irrelevant_crime),]
library(maps)
library(ggmap)
sf <- get_map(location = 'san francisco',zoom=13,color = 'bw')
sf_map = ggmap(sf,extent = 'device')
df$DayOfWeek = factor(df$DayOfWeek,levels(df$DayOfWeek)[c(4,2,6,7,5,1,3)])

## Overall Crime Density by year

df$Year = year(df$Date)
allyears = unique(df$Year)
for (i in 1:length(allyears)){
crime_density_year = sf_map + stat_density2d(data=df[df$Year==allyears[i],],
                                        aes(x=X, y=Y,fill=..level..,alpha=..level..),
                                        geom="polygon",contour=TRUE) +
  scale_fill_gradient(low="orange", high="red",guide='none') +
  scale_alpha(guide="none")+ggtitle(allyears[i])+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0))

ggsave(paste0('crime_density_year_',allyears[i],'.png'),crime_density_year,dpi=500)
}

df_2016 = df[df$Year==2016,]
AllCrime_type = sf_map + geom_point(data=df_2016, aes(x=X,y=Y),size=.2,alpha=.75,color='red')+
  facet_wrap(~Category,labeller = label_wrap_gen(width = 10, multi_line = TRUE),ncol = 6)+
  theme(strip.text.x = element_text(size = 8, face='bold',family="Trebuchet MS"))
ggsave('All_Crime_2016_type.png',AllCrime_type,dpi=500)

## how crime rate really gone down?
df = merge(df,total_pop,by='Year')
unique_years = unique(df$Year)
for (i in 1:length(unique_years)){
  crime_rate[i] = (nrow(df[df$Year==unique_years[i],])/total_pop$Population[i])*100000
}
crime_rate = as.data.frame((crime_rate))
crime_rate$Year = total_pop$Year


violent_crime_line=ggplot(crime_rate, aes(x=Year,y=`(crime_rate)`))+
  geom_line(size=2,color='aquamarine3')+
  ylab('')+xlab('')+theme_dark()+ggtitle('Crime Rate, per 100,000 people')+
  scale_x_continuous(breaks = seq(2004,2016)) +
  annotate("text", label = "Last Updated: Oct 10, 2016",
           x = 2015, y = 300, size = 5, colour = "white",family="Trebuchet MS")+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#666666", face="bold", size=32, hjust=0))
        

#Yes it has. 

## Violent crime only. 

violent = c('CHILD ABUSE','DOMESTIC VIOLENCE','FELONY ASSAULT',
            'FORCIBLE RAPE','HOMICIDE','MISDEMEANOR ASSAULT',
            'ROBBERY','THREATS','WEAPONS')

df_2016$violent = ifelse(df_2016$Category %in% violent,1,0)

df_count=as.data.frame(table(df_2016$Category))


violent_crime_bar=ggplot(crime_rate, aes(x=Year,y=V1))+
  geom_line()+
  coord_flip()+
  ylab('Incidents per 100,000 People')+xlab('')+theme_light()

## time of days 

library(lubridate)
df$timeframe = ifelse(hour(df$datetime)>8 & hour(df$datetime)<20,'Day','Night')

crime_density_days = sf_map + stat_density2d(aes(x = X, y = Y, 
                                                      fill = ..level.. , alpha = ..level..),size = 2, bins = 4, 
                                                  data = df, geom = 'polygon') +
  scale_alpha(range = c(.3,1), guide = FALSE) +
  scale_fill_continuous(guide = guide_legend(title = "Crime Density"))+
  theme(legend.position='top')+
  facet_wrap(~weekday,ncol = 7)

ggsave('crime_density_day.png',crime_density_days,dpi=300)


##all crime by days

Crime_type_day = ggplot(data=df,aes(x=crimetype))+
  geom_bar(fill='white')+
  facet_grid(~weekday,scales = "free_y")+theme(legend.position="none")+coord_flip()+xlab('')+
  ylab('Number of Incidences')+theme_dark()

ggsave('day_crime_bar.png',violentCrime_type_day,dpi=300,height = 8,width=11)

## all crimes by time 
library(lubridate)
df_2016$hour = hour(df_2016$Time)

crimetype_hour = ggplot(df_2016,aes(x=hour,fill=Category))+geom_bar()+
  xlab('Hour')+ylab('')+theme_dark()+ggtitle('Crime Incidences By Hour')+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_vline(xintercept = c(8.5,17.5))+
  scale_y_continuous(limits = c(0,5000), expand = c(0, 0)) +
  annotate("text", label = "Work Day, 9AM - 5PM", x = 13, y = 4600, size = 8, colour = "white",family="Trebuchet MS")
ggsave('crimetype_hour.png',crimetype_hour,dpi=300)

crimetype_hour_day = ggplot(df_2016,aes(x=hour,fill=Category))+geom_bar()+
  xlab('Hour')+ylab('')+theme_dark()+ggtitle('Crime Incidences By Hour')+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),legend.position="bottom",
        legend.title=element_blank())+
  geom_vline(xintercept = c(8.5,17.5))+
  annotate("text", label = "Work Day", x = 13, y = 700, size = 4, colour = "white",family="Trebuchet MS")+
  facet_wrap(~DayOfWeek,ncol=2)+
  scale_y_continuous(expand = c(0, 0))
ggsave('crimetype_hour_day.png',crimetype_hour_day,dpi=300)

## closer look at theft and robbery
theft=c('LARCENY/THEFT','ROBBERY','BURGLARY','VEHICLE THEFT')
df_theft = df_2016[df_2016$Category %in% theft,] 

theft_hour_day = ggplot(df_theft,aes(x=hour,fill=Category))+geom_bar()+
  xlab('Hour')+ylab('')+theme_dark()+ggtitle('Theft Incidences By Hour')+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),legend.position="bottom",
        legend.title=element_blank())+
  geom_vline(xintercept = c(8.5,17.5))+
  annotate("text", label = "Work Day", x = 13, y = 630, size = 4, colour = "white",family="Trebuchet MS")+
  facet_wrap(~DayOfWeek,ncol=2)+
  scale_y_continuous(limits=c(0,700),expand = c(0, 0))
ggsave('theft_hour_day.png',theft_hour_day,dpi=300)




