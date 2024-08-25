

setwd("C:\Users\Code Vatika\OneDrive - London Metropolitan University\Islington\Sem 3\R Programming")
library(dplyr)
library(ggplot2)
library(treemap)
library(corrplot)
library(plotly)
library(leaflet)
library(tidyverse)
library(caret)
library(e1071)
library(tm)
library(tidytext)
library(wordcloud)


world_terrorism_df <- read.csv("globalterrorismdb_0718dist.csv")
head(world_terrorism_df)
#changing countryname to small case
world_terrorism_df$country_txt <- tolower(world_terrorism_df$country_txt)
#filtering data belongs to nepal
nepalTerror_df <- world_terrorism_df[world_terrorism_df$country_txt %in% 'nepal',]
tail(nepalTerror_df)
#filtering columns
col_selections <- c('iyear', 'imonth', 'iday', 'provstate', 'city', 'latitude', 'longitude', 'success',
                    'attacktype1_txt', 'targtype1_txt', 'targsubtype1_txt', 'natlty1_txt', 'gname',
                    'weaptype1_txt', 'nkill', 'nwound', 'motive')
terror_df <- nepalTerror_df[,col_selections]
colnames(terror_df) <- c('year', 'month', 'day', 'dev_region', 'city', 'latitude', 'longitude', 'attack_state',
                         'attacktype', 'target_sector', 'target', 'victims_nationality', 'attacker',
                         'weapon', 'no_of_kills', 'no_of_injured', 'motive')
terror_df$no_of_kills[is.na(terror_df$no_of_kills)] <- 0
terror_df$no_of_injured[is.na(terror_df$no_of_injured)] <- 0
terror_df$motive[is.na(terror_df$motive)] <- "Unknown"

sapply(terror_df, table)
summary(terror_df)
#summary(terror_df)

colSums(is.na(terror_df))


#Univaraiate analysis
#boxplot 
boxplot(terror_df$year)
filter(terror_df, year<1990)

#histogram
hist(terror_df$year, main="Histogram of year distribution", xlab="Years")
ggplot(terror_df, aes(year))+geom_histogram(bins="100")
hist(log(terror_df$no_of_kills))
table(terror_df$year)

#barplot
barplot(table(terror_df$year))

#multivariate analysis
#Correlation
cor(nepalTerror_df$attacktype1, nepalTerror_df$weaptype1)

terrorCor <- nepalTerror_df[,c("iyear","imonth","iday", 'latitude', 'longitude', 'success',
                               'attacktype1', 'targtype1', 'targsubtype1', 'natlty1',
                               'weaptype1', 'nkill', 'nwound')]
terrorCor <- na.omit(terrorCor)
correlations <- cor(terrorCor) #correlation
corrplot::corrplot(correlations, method="circle") #heatmap

#scatter plot
#Kills per year
plot(terror_df$year, log(terror_df$no_of_kills), main="Years vs kills", xlab = "Year", ylab = "Number of kills")
ggplot(terror_df, aes(year, log(no_of_kills), col=as.factor(month), shape=as.factor(attack_state)))+geom_point()

#attacks per year
attacks_by_year <- terror_df %>% 
  group_by(year) %>% 
  summarise(count=n())
ggplot(attacks_by_year, aes(x=year, y=count))+geom_point()+stat_smooth()

# multivariate boxplot
boxplot(nepalTerror_df$attacktype1, nepalTerror_df$targtype1)

#2.1 Terrorist attacks on Nepal by ATTACK type
ggplot(terror_df,aes(x = year))+ labs(title =" Terrorist attacks on Nepal by attack type", x = "Years", y = "Number of Attacks") + 
  geom_bar(colour = "grey19", fill = "tomato3") + facet_wrap(~attacktype) + theme(axis.text.x = element_text(hjust = 1, size = 12))+
  theme(strip.text = element_text(size = 16, face = "bold"))

#2.2 Yearwise terrorist attacks by ATTACK type
ggplot(data=terror_df, aes(x=year,fill=attacktype)) + geom_bar() + ggtitle("Yearly terrorist attacks by attack type")+         
  labs(x = "Years", y = "Number of Attacks")

#3.1 By TARGET type
clean_terror <- terror_df[which(terror_df$target_sector !='.'), ] 
ggplot(clean_terror, aes(x = year))+ labs(title =" Terrorist attacks on Nepal by TARGET type", x = "Years", y = "Number of Attacks") + 
  geom_bar(colour = "grey19", fill = "tomato3") + facet_wrap(~target_sector, ncol = 4) + theme(axis.text.x = element_text(hjust = 1, size = 12))+
  theme(strip.text = element_text(size = 16, face = "bold"))

#3.2 Yearwise terrorist attacks by TARGET type
ggplot(data=clean_terror, aes(x=year,fill=target_sector)) + geom_bar() + ggtitle("Yearly terrorist attacks by TARGET type")+         
  labs(x = "Years", y = "Number of Attacks")

#4.1 By WEAPON type
ggplot(terror_df, aes(x = year))+ labs(title =" Terrorist attacks on Nepal by WEAPON type", x = "Years", y = "Number of Attacks") + 
  geom_bar(colour = "grey19", fill = "tomato3") + 
  facet_wrap(~weapon, ncol = 2) + theme(axis.text.x = element_text(hjust = 1, size = 12))+ theme(strip.text = element_text(size = 15, face = "bold"))

#4.2 Yearwise terrorist attacks by WEAPON type
ggplot(data=terror_df, aes(x=year,fill=weapon)) + 
  geom_bar() + ggtitle("Yearly terrorist attacks by WEAPON type")+ 
  labs(x = "Years", y = "Number of Attacks")

#5.1 By Terroris Groups
ggplot(terror_df, aes(x = year))+ labs(title =" Terrorist attacks on Nepal by Terrorist GROUP", x = "Years", y = "Number of Attacks") + 
  geom_bar(colour = "grey19", fill = "skyblue") + 
  facet_wrap(~attacker, ncol = 10, scales = "free_y") + theme(axis.text.x = element_text(hjust = 1))+
  theme(strip.text = element_text(size = 11, face = "bold"))


#Number of killing per year
dfk <- terror_df %>% filter(no_of_kills > 0)
treemap(dfk, 
        index=c("year"),
        vSize = "no_of_kills",  
        palette = "Reds",  
        title="Killings in Nepal Terrorism by year", 
        type="value",
        title.legend = "Number of killed",
        fontsize.title = 14 
)

#by city
treemap(dfk, 
        index=c("city"), 
        vSize = "no_of_kills",  
        palette = "Reds",  
        title="Killings in Nepal Terrorism by city", 
        fontsize.title = 14,
        type="value",
        title.legend = "Number of killed",

)

#by year and dev region
dfyr <- dfk %>% group_by(year,dev_region) %>% summarise(no_of_kills = sum(no_of_kills)) %>% ungroup()
colnames(dfyr)<-c("Year","dev_region","no_of_kills")
ggplot(data = dfyr, aes(x = Year, y = no_of_kills, colour = dev_region)) +       
  geom_line() + geom_point() + theme_bw()


# Number of killing yearly by terrorist group
dfyrk <- dfk %>% group_by(year,attacker) %>% summarise(no_of_kills = sum(no_of_kills)) %>% ungroup()
dfyrku <- head(arrange(dfyrk,desc(no_of_kills)), n = 40)
colnames(dfyrku)<-c("Year","attacker","no_of_kills")
ggplot(data = dfyrku, aes(x = Year, y = no_of_kills, colour = attacker))+geom_line() + geom_point() + theme_bw()



#visualization of terrorism in Nepal according to targets, and total casualities and injuries. Before visualizing the targets are trimmed and regrouped.
terror_df_empty_removed <- terror_df[complete.cases(terror_df),]
terror_df_empty_removed$target_sector <- as.character(terror_df_empty_removed$target_sector)
govIndx <- which(terror_df_empty_removed$target_sector %in% c("Government (General)","Government (Diplomatic)"))
terror_df_empty_removed[govIndx,'target_sector'] <- 'Government'

govIndx <- which(terror_df_empty_removed$target_sector %in% c('Transportation','Airports & Aircraft'))
terror_df_empty_removed[govIndx,'target_sector'] <- 'Transportation'

govIndx <- which(terror_df_empty_removed$target_sector %in% c('Police','Military'))
terror_df_empty_removed[govIndx,'target_sector'] <- 'ArmedForces'

govIndx <- which(terror_df_empty_removed$target_sector %in% "Private Citizens & Property")
terror_df_empty_removed[govIndx,'target_sector'] <- 'Citizens'

govIndx <- which(terror_df_empty_removed$target_sector %in% c("Food or Water Supply","Telecommunication", "Utilities"))
terror_df_empty_removed[govIndx,'target_sector'] <- 'Infrastructure'

govIndx <- which(terror_df_empty_removed$target_sector %in% c("Violent Political Party","Terrorists/Non-State Militia"))
terror_df_empty_removed[govIndx,'target_sector'] <- 'Rebel Party'

govIndx <- which(terror_df_empty_removed$target_sector %in% "Religious Figures/Institutions")
terror_df_empty_removed[govIndx,'target_sector'] <- 'Religious'

govIndx <- which(terror_df_empty_removed$target_sector %in% "Journalists & Media")
terror_df_empty_removed[govIndx,'target_sector'] <- 'Media'

govIndex <- which(terror_df_empty_removed$target_sector %in% "Educational Institution")
terror_df_empty_removed[govIndx,'target_sector'] <- 'Education'

terror_df_empty_removed$target_sector <- as.factor(terror_df_empty_removed$target_sector)

tempData <- terror_df_empty_removed %>% select(target_sector, no_of_kills,no_of_injured) %>% group_by(target_sector) %>% summarize(Casualties = sum(no_of_kills), attactCount= n(), Injured = sum(no_of_injured))
tempData <- tempData[order(-tempData[,4]),]

seque <- seq(length(tempData$target_sector),1)
tempData <- cbind(tempData, size=seque)


plot <- ggplot(tempData, aes(x=Casualties, y=Injured, color=target_sector)) + geom_point(size=tempData$size, alpha=0.6)
plot <- plot + labs(title='Total  injuries/kills according to target sector.', x='Total  Kills', y='Total Injuried')
plot <- plot +  guides(color=FALSE) 
plot <- ggplotly(plot)
hide_legend(plot)




#Map Plot
terror_dfll <- terror_df %>%
  filter(!is.na(latitude) & !is.na(longitude))
t.test(data =kill_by_weapon, no_of_kills ~ weapon )
m <- leaflet(terror_dfll) %>%
  addTiles('https://{s}.tile.thunderforest.com/spinal-map/{z}/{x}/{y}.png?',
           attribution='&copy; <a href="http://www.thunderforest.com/">Thunderforest</a>, &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%  # Add default OpenStreetMap map tiles
  setView(84.5, 28.5, zoom= 7)
m %>% addCircleMarkers(lat= ~latitude, lng = ~longitude,
                             radius = ~ifelse(terror_dfll$attack_state == 1, 5, 6), color =  ~ifelse(terror_dfll$attack_state != 1, "#FF0000", "#92CD00"),
                             popup=paste0("<div class=\"table-title\">
<h3>Terrorism Attack Details</h3>
</div>
<table class=\"table-fill\">
<thead>
<tr>
<th class=\"text-left\">MetaData Name</th>
<th class=\"text-left\">MetaData Value</th>
</tr>
</thead>
<tbody class=\"table-hover\">
<tr>
<td class=\"text-left\">Event Date</td>
<td class=\"text-left\">",terror_dfll$day,'/',terror_dfll$month,'/',terror_dfll$year,"</td>
</tr>
<tr>
<td class=\"text-left\">City/Area</td>
<td class=\"text-left\">",terror_dfll$city,"</td>
</tr>
<tr>
<td class=\"text-left\">Status</td>
<td class=\"text-left\">",terror_dfll$attack_state,"</td>
</tr>
<tr>
<td class=\"text-left\">Type</td>
<td class=\"text-left\">",terror_dfll$attack_type,"</td>
</tr>
<tr>
<td class=\"text-left\">Targets</td>
<td class=\"text-left\">",terror_dfll$target,"</td>
</tr>
<tr>
<td class=\"text-left\">Weapon Used</td>
<td class=\"text-left\">",terror_dfll$weapon,"</td>
</tr>
<tr>
<td class=\"text-left\">Injuried</td>
<td class=\"text-left\">",terror_dfll$n.injured,"</td>
</tr>
<tr>
<td class=\"text-left\">Casulities</td>
<td class=\"text-left\">",terror_dfll$deathtoll,"</td>
</tr>
</tbody>
</table>"),stroke = FALSE, fillOpacity = 0.5)


#Map plot 2
#Clustering the terror attacks for more fine gnalular view.

clusterMap <- leaflet() %>% 
  addTiles('https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png',
           attribution='&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> &copy; <a href="http://cartodb.com/attributions">CartoDB</a>') 

clusterMap %>% addMarkers(data=terror_dfll,popup=paste0("<div class=\"table-title\">
<h3>Terrorism Attack Details</h3>
</div>
<table class=\"table-fill\">
<thead>
<tr>
<th class=\"text-left\">MetaData Name</th>
<th class=\"text-left\">MetaData Value</th>
</tr>
</thead>
<tbody class=\"table-hover\">
<tr>
<td class=\"text-left\">Event Date</td>
<td class=\"text-left\">",terror_dfll$day,'/',terror_dfll$month,'/',terror_dfll$year,"</td>
</tr>
<tr>
<td class=\"text-left\">City/Area</td>
<td class=\"text-left\">",terror_dfll$city,"</td>
</tr>
<tr>
<td class=\"text-left\">Status</td>
<td class=\"text-left\">",terror_dfll$attack_state,"</td>
</tr>
<tr>
<td class=\"text-left\">Type</td>
<td class=\"text-left\">",terror_dfll$attack_type,"</td>
</tr>
<tr>
<td class=\"text-left\">Targets</td>
<td class=\"text-left\">",terror_dfll$target_sector,"</td>
</tr>
<tr>
<td class=\"text-left\">Weapon Used</td>
<td class=\"text-left\">",terror_dfll$weapon,"</td>
</tr>
<tr>
<td class=\"text-left\">Injuried</td>
<td class=\"text-left\">",terror_dfll$no_of_injured,"</td>
</tr>
<tr>
<td class=\"text-left\">Casulities</td>
<td class=\"text-left\">",terror_dfll$no_of_kills,"</td>
</tr>
</tbody>
</table>"), clusterOptions = markerClusterOptions())


#Statistical Testing
#T-test
kill_by_weapon <- terror_df %>%
  select (weapon, no_of_kills) %>%
  filter(tolower(weapon)=="firearms" | tolower(weapon)=="explosives")
t.test(data =kill_by_weapon, no_of_kills ~ weapon )

#Machine Learning
#Linear regression
#Simple linear regression
aby <- terror_df %>% 
  group_by(year) %>% 
  summarise(count=n())
ggplot(aby, aes(x=year, y=count))+geom_point()+geom_smooth(method = "lm")
cor(aby$year, aby$count)
model_simple = lm(aby$count ~ aby$year) #count = 3.55xcount - 7066.83
summary(model_simple)

#two variables
#multi linear regression

terror_df_factored <- terror_df %>%
  select(no_of_kills, attack_state, attacktype, target_sector, attacker, weapon, dev_region, year)
terror_df_factored$attacktype <- as.factor(terror_df_factored$attacktype)
terror_df_factored$target_sector <- as.factor(terror_df_factored$target_sector)
terror_df_factored$attacker <- as.factor(terror_df_factored$attacker)
terror_df_factored$weapon <- as.factor(terror_df_factored$weapon)
terror_df_factored$dev_region <- as.factor(terror_df_factored$dev_region)
terror_df_factored$attack_state <- as.factor(terror_df_factored$attack_state)

terror_df_factored <- terror_df_factored[complete.cases(terror_df_factored),]

reg_model <- lm(no_of_kills ~ attacktype + target_sector + attacker + weapon  + dev_region + year + attack_state,
             data = terror_df_factored)
anova(reg_model)
summary(reg_model)

#prediction
predict(reg_model, terror_df_factored[1:30, c('attacktype', 'target_sector', 'attacker', 'weapon', 'dev_region', 'year', 'attack_state')])
predict(reg_model, terror_df_factored[1:30, c('attacktype', 'target_sector', 'attacker', 'weapon', 'dev_region', 'year', 'attack_state')], interval = "confidence")
reg_model$residuals



#Classification predicting attack state (Logistic Regression)
#terror_df$attack_state <- as.factor(terror_df$attack_state)
#class(terror_df$attack_state)
td <- na.omit(terror_df_factored)
sample_n(td, 3)
set.seed(123)
training.samples <- td$attack_state %>%
  createDataPartition(p = 0.9, list = FALSE)
train.data  <- td[training.samples, ]
test.data <- td[-training.samples, ]

log_model <- glm(attack_state ~ attacktype + target_sector + attacker + weapon  + dev_region + year + no_of_kills,
                 data = train.data, family="binomial")
anova(log_model)
summary(log_model)
predict_values <- predict(log_model, test.data[1:30, c('attacktype', 'target_sector', 'attacker', 'weapon', 'dev_region', 'year', 'no_of_kills')], type="response")
prediction <- round(predict_values)
real <- test.data[1:30, "attack_state"]
confusionMatrix(data=as.factor(round(predict(log_model, train.data[, c('attacktype', 'target_sector', 'attacker', 'weapon', 'dev_region', 'year', 'no_of_kills')], type="response"))),reference =  train.data$attack_state)


#Text analysis of motive of attacks
terror_df$motive <- tolower(terror_df$motive)
known_df <- terror_df %>%
  filter(motive != "unknown")
er_df <-known_df %>%
  filter(motive != "")

text <- sample(er_df$motive, nrow(er_df)/2)
specificWords <- c("The", "Unknown", "attack", "specific", "motive", "spy", "unknown", "claimed", "targeted",
                   "carried", "noted", "incident", "stated", "responsibility", "the", 'maoist', 'election', 'bombing', 'police')
text<-sapply(text, function(x) gsub("\n"," ",x))
myCorpus<-VCorpus(VectorSource(text))


myCorpusClean <- myCorpus %>%
  tm_map(content_transformer(removeNumbers)) %>% 
  tm_map(content_transformer(removePunctuation)) %>%
  tm_map(content_transformer(removeWords),tidytext::stop_words$word) %>%
  tm_map(content_transformer(removeWords),specificWords)
myDtm = TermDocumentMatrix(myCorpusClean,
                           control = list(minWordLength = 3))
freqTerms <- findFreqTerms(myDtm, lowfreq=1)
m <- as.matrix(myDtm)
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wctop <-wordcloud(d$word, d$freq, min.freq=5, colors=brewer.pal(9,"Set1"))
