SportsDrinks <- read.csv('SportsDrinks.csv')
View(SportsDrinks)

################### ALLSPORT CHERRY SLAM #############


table(SportsDrinks$BRAND)

ggplot(SportsDrinks, aes(x=BRAND, y=logmove, fill=factor(SEASON))) +
  geom_bar(stat='identity', position='dodge')

Feat <- sqldf("Select * from SportsDrinks where Feat=1")

ggplot(Feat, aes(x=BRAND, y=logmove, fill=factor(SEASON))) +
  geom_bar(stat='identity', position='dodge')


NoFeat <- sqldf("Select * from SportsDrinks where Feat=0")

ggplot(NoFeat, aes(x=BRAND, y=logmove, fill=factor(SEASON))) +
  geom_bar(stat='identity', position='dodge')


ggplot(SportsDrinks, aes(x=BRAND, y=logmove, fill=factor(Feat))) +
  geom_bar(stat='identity', position='dodge')+scale_fill_manual(values = c('yellow','cyan'))


a <- lm(formula = logmove ~    ETHNIC + INCOME + 
     NOCAR + NWHITE  + REM + RETIRED 
      + INCOME  + NOCAR + 
     NWHITE  + REM     
       , data = SportsDrinks)

summary(a)

cc <- cut(SportsDrinks$logprice,3, labels = c('low','medium','high'))

bb <- sqldf('Select logmove from SportsDrinks order by logmove')

Demographics <- sqldf("Select AGE60,ETHNIC,INCOME,NOCAR,NWHITE,RETIRED,WORKWOM from SportsDrinks")
Demographics$logprice <- cc
View(Demographics)

DemoMelt <- melt(Demographics, id='logprice')
View(DemoMelt)

ggplot(DemoMelt, aes(x=factor(logprice),y=variable, fill=value)) + geom_tile() + scale_fill_gradient(low="yellow", high = "orange")

#################################

w <- cut(SportsDrinks$WORKWOM,3, labels = c('low','medium','high'))
e <- cut(SportsDrinks$ETHNIC,3, labels = c('low','medium','high'))
i <- cut(SportsDrinks$INCOME,3, labels = c('low','medium','high'))

df <- data.frame(w,e,i)
View(df)
colnames(df) <- c("WorkWoman","Ethnic","Income")

W <- length(df$WorkWoman[df$WorkWoman =="high"])
E <- length(df$Ethnic[df$Ethnic =="high"])
I <- length(df$Income[df$Income =="high"])

test <- c(W,E,I)
label <- c("WorkWoman","Ethnic","Income")

pct <- round(test/sum(test)*100)
label <- paste(label,pct) #add percentages to labels
label<-paste(label,"%",sep="")

pie(test,labels=label,main = "Demographics" ,col = rainbow(length(test)))



#################################

A <- length(SportsDrinks$BRAND[SportsDrinks$BRAND =="ALLSPORT CHERRY SLAM"])
B <- length(SportsDrinks$BRAND[SportsDrinks$BRAND =="ALL SPORT LEMON LIME"])
C <- length(SportsDrinks$BRAND[SportsDrinks$BRAND =="POWERADE TIDAL BURST"])


a <- c(A,B,C)
label <- c("Cherry","Lemon","Tidal")

pct <- round(a/sum(a)*100)
label <- paste(label,pct) #add percentages to labels
label<-paste(label,"%",sep="")

pie(a,labels=label,main = "Sports Drinks" ,col = rainbow(length(a)))

ggplot(a, aes(x=SEASON, y=logmove, size=factor(Feat), col=BRAND,
                 group=logprice))+geom_jitter(width = 0.4, height = 1) + geom_abline()

ggplot(a, aes(x=factor(Feat),y=logmove)) + geom_bar(stat='identity', position = 'dodge')

SportsDrinksChicagoCherrySlam <- subset(SportsDrinksChicago, BRAND=='ALLSPORT CHERRY SLAM')
SportsDrinksChicagoCherrySlam
View(SportsDrinksChicagoCherrySlam)

a <- lm(logmove ~ logprice+PROFIT+AGE9+AGE60+ETHNIC+EDUC+NOCAR+INCOME+HHSINGLE+HHLARGE+WORKWOM+HVAL150
        +SINGLE+RETIRED+UNEMP+NWHITE+POVERTY,data = SportsDrinksChicagoCherrySlam)

summary(a)

b<- lm(logmove ~ Week+logprice+Feat, data=SportsDrinksChicagoCherrySlam)
summary(b)

####################  ALL SPORT LEMON LIME ##########
SportsDrinksChicago <- subset(SportsDrinks, CITY=='OAK LAWN' & STORE==8)
View(SportsDrinksChicago)

table(SportsDrinksChicago$BRAND)

SportsDrinksChicagoCherrySlam <- subset(SportsDrinksChicago, BRAND=='ALL SPORT LEMON LIME')
SportsDrinksChicagoCherrySlam
View(SportsDrinksChicagoCherrySlam)

a <- lm(logmove ~ logprice+PROFIT+AGE9+AGE60+ETHNIC+EDUC+NOCAR+INCOME+HHSINGLE+HHLARGE+WORKWOM+HVAL150
        +SINGLE+RETIRED+UNEMP+NWHITE+POVERTY,data = SportsDrinksChicagoCherrySlam)

summary(a)

b<- lm(logmove ~ Week+logprice+Feat, data=SportsDrinksChicagoCherrySlam)
summary(b)

View(SportsDrinksChicago)

sqldf("Select BRAND,SEASON,Feat,avg(logmove),avg(logprice) from SportsDrinks group by SEASON,BRAND,Feat ") -> aa
colnames(aa) <- c('BRAND','SEASON','Feat','AVGlogmove','AVGlogprice')
aa

ggplot(aa, aes(x=BRAND, y=AVGlogmove, fill=factor(Feat))) + geom_bar(stat='identity')

sqldf('Select BRAND,Feat,count(BRAND) from SportsDrinksChicago group by BRAND,Feat')

sqldf("Select WEEK,BRAND,avg(logmove) from SportsDrinksChicago group by WEEK,BRAND having BRAND=='ALL SPORT LEMON LIME' ")

sqldf("Select BRAND,WEEK,max(logmove),Feat from SportsDrinksChicago")

sqldf("Select BRAND,WEEK,min(logmove),Feat from SportsDrinksChicago")

sqldf("Select feat,logmove,price,BRAND,WEEK from SportsDrinksChicago where WEEK in (311,355) group by WEEK,BRAND")

sqldf('Select MOVE,PRICE,BRAND,WEEK from SportsDrinksChicago where Feat=1 and STORE=12 group by WEEK,BRAND')

 a <- data.frame(sqldf('Select CITY,BRAND,count(MOVE)  from SportsDrinks group by CITY,BRAND'))
View(a)
colnames(a) <- c('City','Brand','Demand')

ggplot(a,aes(x=Brand,y=Demand))+geom_bar(stat = 'identity')

sqldf('Select CITY,count(CITY) from SportsDrinks group by CITY order by count(CITY) ')


b <- data.frame(sqldf("Select CITY,BRAND,count(MOVE) from SportsDrinks where CITY in ('CHICAGO','BUFFALO GROVE','MOUNT PROSPECT',
                      'NAPERVILLE','OAK LAWN') group by CITY,BRAND "))
View(b)
colnames(b) <- c('City','Brand','Demand')


ggplot(data=b,aes(x=City,y=Demand, fill=Brand,color=Brand))+geom_bar(stat='identity',position = 'dodge')

str(b)

c <- lm(logmove~ AGE9+AGE60+ZIP+ETHNIC+EDUC+NOCAR+INCOME+HHSINGLE+HHLARGE+WORKWOM+HVAL150+SINGLE+RETIRED
        +UNEMP+NWHITE+POVERTY+DRTIME5+SSTRDIST+SSTRVOL+CPDIST5+CPWVOL5
        , data=SportsDrinksChicago)

summary(c)

d<- lm(logmove~SSTRDIST+BRAND, data=SportsDrinksChicago)
summary(d)

str(SportsDrinksChicago)

################### ALL SPORT LEMON LIME ##########
aa$Feat <- ifelse(aa$Feat==1,'On Sale','Not on Sale')

aa <- sqldf("Select Feat,BRAND,CITY,count(MOVE),avg(logmove) from SportsDrinks group by Feat,BRAND,CITY having  
            CITY in ('CHICAGO','ADDISON','BLOOMINGDALE','OAK LAWN')")

View(aa)
colnames(aa) <- c('Feat','BRAND','CITY','MOVE','logmove')

ggplot(aa, aes(x=CITY, y=logmove, fill=BRAND,color=Feat)) + geom_bar(stat='identity',position = 'dodge') +
scale_colour_manual(values=c("black","red")) +
  scale_fill_manual(values=c("light blue","pink",'cornsilk')) +
   ggtitle('Demand of brand in different cities with Feat=1 and Feat=0')
 

sqldf("Select BRAND,CITY,count(MOVE),logmove from SportsDrinks group by BRAND,CITY having BRAND='ALLSPORT CHERRY SLAM' 
 order by count(MOVE) desc")

sqldf("Select BRAND,CITY,count(MOVE),logmove from SportsDrinks group by BRAND,CITY having BRAND='POWERADE TIDAL BURST' 
 order by count(MOVE) desc")


SportsDrinks %>% filter(BRAND=='POWERADE TIDAL BURST') %>% ggplot(aes(x=BRAND,y=MOVE))+geom_point()


SportsDrinks %>% filter(BRAND=='POWERADE TIDAL BURST' & STORE==95 & Feat==1)  %>%
ggplot(aes(x=logprice,y=logmove))+geom_point()+geom_abline(intercept = 3, slope = -4.8)

SportsDrinksChicago %>% filter(BRAND=='POWERADE TIDAL BURST' & STORE==95 & Feat==0)  %>%
  ggplot(aes(x=logprice,y=logmove))+geom_point()+geom_abline(intercept = 3, slope = -4.8)

SportsDrinksChicago %>% filter(BRAND=='POWERADE TIDAL BURST'  & Feat==1)  
  
  
SportsDrinks %>% lm(formula= logmove ~ logprice + BRAND + Feat + CITY, data= SportsDrinks ) 
  
  
long <- data.frame(subset(SportsDrinksChicago, BRAND=='ALL SPORT LEMON LIME'))
View(long)
View(long_feat_1)

#long_feat_1 <- subset(SportsDrinksChicago, Feat==1 )
long_melt <- SportsDrinksChicago[,c('BRAND','logmove','Feat')]
View(long_melt)
#a <- melt(long_melt,id='BRAND')
#View(a)

ggplot(data=long_melt,aes(x=BRAND, y=logmove), fill=Feat, color=Feat,alpha=Feat)+
geom_bar(stat="identity",position ="dodge") +
  scale_colour_manual(values=c("darkgreen","red")) +
  scale_fill_manual(values=c("lightgreen","pink")) +
  scale_alpha_manual(values=c(.8, .2))


table(long_feat_1$STORE)

#####Correlation#####
#install.packages('corrplot')
library(corrplot)

SportsDrinksTidal <- subset(SportsDrinks, BRAND=='POWERADE TIDAL BURST')
a <-data.frame(cor(SportsDrinksTidal$logmove,SportsDrinksTidal$logprice) )# -0.3797761
b<- data.frame(cor(SportsDrinksTidal$logmove,SportsDrinksTidal$Feat==1))  # 0.4106942
c <- data.frame(cor(SportsDrinksTidal$logmove,SportsDrinksTidal$Feat==0))  # -0.4106942
d <- data.frame(a,b,c)
d
colnames(d) <- c(1,2,3)
d


SportsDrinksTidal <- subset(SportsDrinks, BRAND=='ALLSPORT CHERRY SLAM')
e <-cor(SportsDrinksTidal$logmove,SportsDrinksTidal$logprice)  # -0.4309513
f <- cor(SportsDrinksTidal$logmove,SportsDrinksTidal$Feat==1)  # 0.4787544
g <- cor(SportsDrinksTidal$logmove,SportsDrinksTidal$Feat==0)  # -0.4787544
x <- data.frame(e,f,g)
x
colnames(x) <- c(1,2,3)
x

SportsDrinksTidal <- subset(SportsDrinks, BRAND=='ALL SPORT LEMON LIME')
h <- cor(SportsDrinksTidal$logmove,SportsDrinksTidal$logprice)  # -0.4042745
i <-cor(SportsDrinksTidal$logmove,SportsDrinksTidal$Feat==1)   # 0.3525673
j <- cor(SportsDrinksTidal$logmove,SportsDrinksTidal$Feat==0)   # -0.3525673
y <- data.frame(h,i,j)
colnames(y) <- c(1,2,3)
y

z <- rbind(d,x,y)
z














