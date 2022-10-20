#Carbon budget trends in octocorals: a literature review with data reassessment 
#and a conceptual framework to understand their resilience to environmental changes
#Patrick Derviche, André Menegotto and Paulo da Cunha Lana

#Package
library("car")
library("dplyr")
library("ggplot2")

#Set working directory
#Datasets are in folder 'Variables'






#### I. General description ####

#1.1 Checking values

#Download data
data <- read.csv('data.csv', h=T, sep = ';')
posData <- which(rowSums(data[,c('Hmean','Pmean','Rmean','CHARmean','CZARmean','SfG')], na.rm=T)>0)

#Taxa
taxa <- data$Species
taxa[which(data$rank=='Genus')] <- data$Genus[which(data$rank=='Genus')]
taxa[which(data$rank=='Family')] <- data$Family[which(data$rank=='Family')]
taxa[which(data$rank=='Order')] <- data$Order[which(data$rank=='Order')]
unique(na.omit(taxa)) #169 taxa
unique(na.omit(taxa[posData])) #62 taxa with carbon budget data

#Taxa by symbiosis
x <- cbind(taxa[posData], data$Symbiont[posData])
x <- x[duplicated(x[,1])==F,]
table(x[,2])

#Studies by year
year <- data[posData,]
year <- year[duplicated(year$study.ID)==F,]
sum(year$Year>=2000)/length(year$Year) #67%

#Studies in deep waters
deep <- data[posData,]
unique(na.omit(deep$Authors[as.numeric(deep$DepthMin)>50]))

#Studies, Taxa and Species by realm
unique(na.omit(data$Realm[posData]))

realm <- data[posData,]
realm <- realm[duplicated(realm$study.ID)==F,]
sort(table(realm$Realm))/nrow(realm)

x <- cbind(taxa[posData], data$Realm[posData])
x <- x[duplicated(x[,1])==F,]
sort(table(x[,2]))/nrow(x)

x <- cbind(data$Species[posData], data$Realm[posData])
x <- x[duplicated(x[,1])==F,]
x <- x[!is.na(x[,1]),]
round(sort(table(x[,2]))/nrow(x), 2)

# Gorgonian and soft coral phenotypes
unique(data$Order[posData]) #All species analyzed are either gorgonian or soft coral

gorgonian <- c('Calcaxonia','Holaxonia','Scleraxonia')
posG <- which(!is.na(match(data$Suborder, gorgonian)))
posSC <- which(data$Order=="Alcyonacea")
posSC <- setdiff(posSC, posG)

gorgs <- rep(NA, nrow(data))
gorgs[posG] <- 'Gorgonian'
gorgs[posSC] <- 'Soft coral'

x <- cbind(taxa[posData], gorgs[posData], data$Realm[posData])
x <- x[duplicated(x[,1])==F,]
round(sort(table(x[,2]))/nrow(x), 2) #Gorg: 44%, Soft coral: 56%
table(x[,3],x[,2])




#1.2. Creating the Figures

#Fig 1. Variation trends in the total number of indexed, 
#peer-reviewed studies on octocoral feeding ecology per year

#Reading the data
data <- read.csv(file="Studies.csv",header=TRUE,sep=";",dec=".")

#Creating the figure
Fig1 <-ggplot(data=data, aes(x=Year, y=n))+
  geom_bar(stat="identity")+
  geom_col(aes(color = carbon, fill = carbon), position = position_stack()) +
  theme_minimal(base_size = 12) +
  scale_color_manual(values = c("lightblue","steelblue"))+
  scale_fill_manual(values = c("lightblue","steelblue"))+
  xlab("Year") + ylab("Number of studies per year")+
  scale_x_continuous(breaks=seq(1960,2020,5))+
  theme(axis.text.x = element_text(color="black",size=12, angle=310))+
  theme(axis.text.y = element_text(color="black",size=12))+ theme(legend.position = "none")
Fig1

#Clean the R environment
rm(list=ls(all=TRUE))

#Fig 2. Rc- Respiration carbon demand
Rc <- read.csv(file="RcSelected.csv",header=TRUE,sep=";",dec=".")
Rc<-na.omit(Rc)
summary(Rc)
mean(Rc$Rmean)
sd(Rc$Rmean)
#n = 60
#Rc = 187.5 ? 188.5 ?g C g AFDW-1 hour-1

Rc.zoo <- filter(Rc, Symbiont == "Zooxanthellate")
summary(Rc.zoo)
#Zooxanthellate species:48

Rc.azoo <- filter(Rc, Symbiont == "Azooxanthellate")
summary(Rc.azoo)
#Azooxanthellate species:12

#Creating the figure
legend <- expression(Respiration~carbon~demand~(?g~C~g~AFDW^-1~hour^-1))
Fig.Rc <-ggplot(data=Rc, aes(x = reorder(UsedName,-Rmean), y = Rmean, fill=Symbiont))+
  geom_bar(stat="identity") +     
  scale_fill_manual(values=c("steelblue","lightgreen"), name=" ")+
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))

Fig.Rc

#Clean the R environment
rm(list=ls(all=TRUE))

#Fig 3. Hc - Heterotrophic carbon input
Hc <- read.csv(file="HcSelected.csv",header=TRUE,sep=";",dec=".")
Hc<-na.omit(Hc)
summary(Hc)
mean(Hc$Hmean)
sd(Hc$Hmean)
#n = 34
#80.6 ? 184.8 ?g C g AFDW-1 hour-1

Hc.zoo <- filter(Hc, Symbiont == "Zooxanthellate")
summary(Hc.zoo)
mean(Hc.zoo$Hmean)
sd(Hc.zoo$Hmean)
#Zooxanthellate species: 23
#19.1 ? 286.8 ?g C g AFDW-1 hour-1

Hc.azoo <- filter(Hc, Symbiont == "Azooxanthellate")
summary(Hc.azoo)
mean(Hc.azoo$Hmean)
sd(Hc.azoo$Hmean)
#Azooxanthellate species: 23
#209.0 ? 40.2 ?g C g AFDW-1 hour-1

#Creating the figure
legend <- expression(Ingestion~rates~(?g~C~g~AFDW^-1~hour^-1))
Fig.Hc<-ggplot(data=Hc, aes(x = reorder(UsedName,-Hmean), y = Hmean, fill=Symbiont))+
  geom_bar(stat="identity") +     
  scale_fill_manual(values=c("steelblue","lightgreen"), name=" ")+
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))
Fig.Hc

#Clean the R environment
rm(list=ls(all=TRUE))

#Fig 4.CHAR - Contribution of heterotrophically acquired carbon to animal respiration
CHAR <- read.csv(file="CHARselected.csv",header=TRUE,sep=";",dec=".")
CHAR <-na.omit(CHAR)
summary(CHAR)
mean(CHAR$CHARmean)
sd(CHAR$CHARmean)
#n = 31
#72.8 ? 116.0 %

CHAR.zoo <- filter(CHAR, Symbiont == "Zooxanthellate")
summary(CHAR.zoo)
mean(CHAR.zoo$CHARmean)
sd(CHAR.zoo$CHARmean)
#Zooxanthellate species: 21
#30.0 ? 32.6 %

CHAR.azoo <- filter(CHAR, Symbiont == "Azooxanthellate")
summary(CHAR.azoo)
mean(CHAR.azoo$CHARmean)
sd(CHAR.azoo$CHARmean)
#Azooxanthellate species: 10
#162.8 ? 171.0 %

#Creating the figure
Fig.CHAR<-ggplot(data=CHAR, aes(x = reorder(UsedName,-CHARmean), y = CHARmean, fill=Symbiont))+
  geom_bar(stat="identity") +     
  scale_fill_manual(values=c("steelblue","lightgreen"), name=" ")+
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab("CHAR (%)")+
  geom_hline(yintercept=100, linetype="dashed", color = "black")+ 
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))
Fig.CHAR

#Clean the R environment
rm(list=ls(all=TRUE))

#Fig 5.Pc - Photosynthetic carbon input
Pc <- read.csv(file="PcSelected.csv",header=TRUE,sep=";",dec=".")
Pc<-na.omit(Pc)
summary(Pc)
mean(Pc$Pmean)
sd(Pc$Pmean)
#n = 41
#276.7 ? 278.1 ?g C g AFDW-1 hour-1

#Creating the figure
legend <- expression(Photosynthetic~carbon~input~(?g~C~g~AFDW^-1~hour^-1))
Fig.Pc <-ggplot(data=Pc, aes(x = reorder(UsedName,-Pmean), y = Pmean))+
  geom_bar(stat="identity",fill="lightgreen") + 
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))
Fig.Pc

#Clean the R environment
rm(list=ls(all=TRUE))

#Fig 6.CZAR - Contribution of zooxanthellae to animal respiration
CZAR <- read.csv(file="CZARselected.csv",header=TRUE,sep=";",dec=".")
CZAR<-na.omit(CZAR)
summary(CZAR)
mean(CZAR$CZARmean)
sd(CZAR$CZARmean)
#n = 62
#156.7 ? 113.9 %

#Creating the figure
Fig.CZAR<-ggplot(data=CZAR, aes(x = reorder(UsedName,-CZARmean), y = CZARmean))+
  geom_bar(stat="identity",fill="lightgreen") +     
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab("CZAR (%)")+
  geom_hline(yintercept=100, linetype="dashed", color = "black")+ 
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))
Fig.CZAR

#Clean the R environment
rm(list=ls(all=TRUE))

#Fig 7.SfG - Scope for growth
SfG<- read.csv(file="SfGselected.csv",header=TRUE,sep=";",dec=".")
SfG<-na.omit(SfG)
summary(SfG)
mean(SfG$SfG)
sd(SfG$SfG)
#n = 24
#209.0 ? 40.2 ?g C g AFDW-1 hour-1

SfG.zoo <- filter(SfG, Symbiont == "Zooxanthellate")
summary(SfG.zoo)
mean(SfG.zoo$SfG)
sd(SfG.zoo$SfG)
#Zooxanthellate species: 18
#23.2 ? 28.2 ?g C g AFDW-1 hour-1

SfG.azoo <- filter(SfG, Symbiont == "Azooxanthellate")
summary(SfG.azoo)
mean(SfG.azoo$SfG)
sd(SfG.azoo$SfG)
#Azooxanthellate species: 6
#209.0 ? 40.2 ?g C g AFDW-1 hour-1

#Creating the figure
legend <- expression(Carbon~available~"for"~growth~and~reproduction~(?g~C~g~AFDW^-1~hour^-1))
Fig.SfG<-ggplot(data=SfG, aes(x = reorder(UsedName,-SfG), y = SfG, fill=Symbiont))+
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("steelblue","lightgreen"), name=" ")+
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ 
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))
Fig.SfG

#Remove all objects
rm(list = ls())







##### II. Statistical analysis #####

############# 1. Assessing the difference of  in carbon budgets
#between zooxanthellate and azooxanthellate octocorals

#1.1. Resiratory carbon demand (Rc)
#Download data
rc <- read.csv(file="RcSelected.csv",header=TRUE,sep=";",dec=".")
rc <- na.omit(rc)

#Shapiro-Wilk's test
shapiro.test(rc$Rmean) #p-value < 0.05, we can not assume the normality
qqPlot(rc$Rmean) #As some of the points fall outside the reference line, we can not assume normality

#Levene's test
leveneTest(Rmean ~ Symbiont, data=rc)  #p-value > 0.05, we can assume the homogeneity of variance 

#Transformation
rc$Rmean <- log10(rc$Rmean)  #We applied log10 transformation to try to achieve the normal distribution
#did not work

#Therefore, we decided to use the nonparametric Kruskal-Wallis test
kruskal.test(Rmean ~ Symbiont, data = rc)
#No significant differences
#χ2(1) = 0.04, p = 0.84

#Data distribution
boxplot(Rmean ~ Symbiont, data=rc)
aggregate(rc$Rmean, by=list(rc$Symbiont), mean)
aggregate(rc$Rmean, by=list(rc$Symbiont), sd)

#Remove all objects
rm(list = ls())



#1.2. Heterotrophic carbon input (Hc)
#Download data
hc <- read.csv(file="HcSelected.csv",header=TRUE,sep=";",dec=".")
hc <- na.omit(hc)

#Shapiro-Wilk's test
shapiro.test(hc$Hmean) #p-value < 0.05, we can not assume the normality
qqPlot(hc$Hmean) #As some of the points fall outside the reference line, we can not assume normality

#Levene's test
leveneTest(Hmean ~ Symbiont, data=hc)  #p-value < 0.05, we can not assume the homogeneity of variance

#Transformation
hc$Hmean <- log10(hc$Hmean)  #We applied log10 transformation to achieve the normal distribution
#did work
#p-value = 0.25 (Shapiro-Wilk's)
#p-value = 0.39 (Levene's test)

#ANOVA
model.hc <- aov(Hmean ~ Symbiont, data=hc)
summary(model.hc) #Significant differences
#F(1,32) = 15.52, p = 0.000415 ***

#Data distribution
boxplot(Hmean ~ Symbiont, data=hc)
aggregate(hc$Hmean, by=list(hc$Symbiont), mean)
aggregate(hc$Hmean, by=list(hc$Symbiont), sd)

#Remove all objects
rm(list = ls())



#1.3. contribution of heterotrophically acquired carbon to octocoral respiration (CHAR)
#Download data
char <- read.csv(file="CHARSelected.csv",header=TRUE,sep=";",dec=".")
char <- char[!is.na(char$selected),]

#Shapiro-Wilk's test
shapiro.test(char$CHARmean) #p-value < 0.05, we can not assume the normality
qqPlot(char$CHARmean) #As some of the points fall outside the reference line, we can not assume normality

#Levene's test
leveneTest(CHARmean ~ Symbiont, data=char)  #p-value < 0.05, we can not assume the homogeneity of variance

#Transformation
char$CHARmean <- log10(char$CHARmean)  #We applied log10 transformation to achieve the normal distribution
#did work
#p-value = 0.09 (Shapiro-Wilk's)
#p-value = 0.29 (Levene's test)

#ANOVA
model.char <- aov(CHARmean ~ Symbiont, data=char)
summary(model.char) #Significant differences
#F(1,30) = 15.53, p = 0.00045 ***

#Data distribution
boxplot(CHARmean ~ Symbiont, data=char)
aggregate(char$CHARmean, by=list(char$Symbiont), mean)
aggregate(char$CHARmean, by=list(char$Symbiont), sd)

#Remove all objects
rm(list = ls())




#1.4. C budget available for growth and reproduction (SfG)
#Download data
sfg <- read.csv(file="SfGSelected.csv",header=TRUE,sep=";",dec=".")
sfg <- na.omit(sfg)

#Shapiro-Wilk's test
shapiro.test(resid(aov(SfG ~ Symbiont, data=sfg))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(SfG ~ Symbiont, data=sfg))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(SfG ~ Symbiont, data=sfg)  #p-value < 0.05, we can not assume homogeneity of variance

#Transformations (Positive skew with negative values)
hist(sfg$SfG)
table(sfg$Symbiont)

#We applied adjusted log10 transformation to achieve the normal distribution
#did not work (for homogeneity of variance)
#sfg$SfG <- log10(sfg$SfG +1-min(sfg$SfG))

#Therefore, we decided to use the nonparametric Kruskal-Wallis test
kruskal.test(SfG ~ Symbiont, data = sfg)
#No significant differences
#χ2(1) = 0.11, p = 0.74

#Data distribution
boxplot(SfG ~ Symbiont, data=sfg)
aggregate(sfg$SfG, by=list(sfg$Symbiont), mean)
aggregate(sfg$SfG, by=list(sfg$Symbiont), sd)

#Remove all objects
rm(list = ls())











############# 2. Assessing the difference of  in carbon budgets
#between ‘gorgonians’ and ‘soft corals’ phenotypes

#Download data
data <- read.csv('data.csv', h=T, sep = ';')

#Define gorgonian groups
unique(data$Suborder[data$Order=="Alcyonacea"])

gorgonian <- c('Calcaxonia','Holaxonia','Scleraxonia')
posG <- which(!is.na(match(data$Suborder, gorgonian)))
unique(data$Suborder[posG])

posSC <- which(data$Order=="Alcyonacea")
posSC <- setdiff(posSC, posG)
unique(data$Suborder[posSC])

gorgs <- rep(NA, nrow(data))
gorgs[posG] <- 'Gorgonian'
gorgs[posSC] <- 'Soft coral'





#2.1 Resiratory carbon demand (Rc)
rc <- read.csv(file="RcSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$Rmean))
nrow(rc)==length(pos)
all.equal(data$Rmean[pos], rc$Rmean)
RCselecs <- data[pos[!is.na(rc$selected)],]
RCselecs$Group <- gorgs[pos[!is.na(rc$selected)]]

#Shapiro-Wilk's test
shapiro.test(resid(aov(Rmean ~ Group, data=RCselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(Rmean ~ Group, data=RCselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(Rmean ~ Group, data=RCselecs)  #p-value > 0.05, we can assume homogeneity of variance 

#Transformations (Positive skew)
hist(RCselecs$Rmean)
table(RCselecs$Group)

#We applied sqrt transformation to try to achieve the normal distribution
#did not work
#RCselecs$Rmean <- sqrt(RCselecs$Rmean)

#We applied log10 transformation to achieve the normal distribution
#did not work
#RCselecs$Rmean <- log10(RCselecs$Rmean)

#We applied inverse transformation to try to achieve the normal distribution 
#did not work
#RCselecs$Rmean <- 1/(RCselecs$Rmean)

#Therefore, we decided to use the nonparametric Kruskal-Wallis test
kruskal.test(Rmean ~ Group, data = RCselecs)
#No significant differences
#χ2(1) = 0.42, p = 0.52

#Data distribution
boxplot(Rmean ~ Group, data = RCselecs)
aggregate(RCselecs$Rmean, by=list(RCselecs$Group), range)





#2.2. Heterotrophic carbon input (Hc)
#Download data
hc <- read.csv(file="HcSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$Hmean))
nrow(hc)==length(pos)
all.equal(data$Hmean[pos], hc$Hmean)
HCselecs <- data[pos[!is.na(hc$selected)],]
HCselecs$Group <- gorgs[pos[!is.na(hc$selected)]]

#Shapiro-Wilk's test
shapiro.test(resid(aov(Hmean ~ Group, data=HCselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(Hmean ~ Group, data=HCselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(Hmean ~ Group, data=HCselecs)  #p-value > 0.05, we can assume homogeneity of variance 

#Transformations (Positive skew)
hist(HCselecs$Hmean)
table(HCselecs$Group)

#We applied sqrt transformation to try to achieve the normal distribution
#did not work
#HCselecs$Hmean <- sqrt(HCselecs$Hmean)

#We applied log10 transformation to achieve the normal distribution
#did work
HCselecs$Hmean <- log10(HCselecs$Hmean)

#ANOVA
model.hc <- aov(Hmean ~ Group, data=HCselecs)
summary(model.hc)
#No significant differences
#F(1,32) = 0.02, p = 0.9

#Data distribution
boxplot(Hmean ~ Group, data=HCselecs)
aggregate(HCselecs$Hmean, by=list(HCselecs$Group), mean)
aggregate(HCselecs$Hmean, by=list(HCselecs$Group), sd)




#2.3. Photosynthetic carbon input (Pc)
#Download data
pc <- read.csv(file="PcSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$Pmean))
nrow(pc)==length(pos)
all.equal(data$Pmean[pos], pc$Pmean)
PCselecs <- data[pos[!is.na(pc$selected)],]
PCselecs$Group <- gorgs[pos[!is.na(pc$selected)]]

#Shapiro-Wilk's test
shapiro.test(resid(aov(Pmean ~ Group, data=PCselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(Pmean ~ Group, data=PCselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(Pmean ~ Group, data=PCselecs)  #p-value > 0.05, we can assume homogeneity of variance 

#Transformations (Positive skew)
hist(PCselecs$Pmean)
table(PCselecs$Group)

#We applied sqrt transformation to try to achieve the normal distribution
#did not work
#PCselecs$Pmean <- sqrt(PCselecs$Pmean)

#We applied log10 transformation to achieve the normal distribution
#did not work
#PCselecs$Pmean <- log10(PCselecs$Pmean)

#We applied inverse transformation to try to achieve the normal distribution 
#did not work
#PCselecs$Pmean <- 1/(PCselecs$Pmean)

#Therefore, we decided to use the nonparametric Kruskal-Wallis test
kruskal.test(Pmean ~ Group, data=PCselecs)
#No significant differences
#χ2(1) = 0.48, p = 0.4883

#Data distribution
boxplot(Pmean ~ Group, data=PCselecs)
aggregate(PCselecs$Pmean, by=list(PCselecs$Group), range)




#2.4 Contribution of heterotrophically acquired carbon to octocoral respiration (CHAR)
char <- read.csv(file="CHARSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$CHARmean))
nrow(char)==length(pos)
all.equal(data$CHARmean[pos], char$CHARmean)
CHARselecs <- data[pos[!is.na(char$selected)],]
CHARselecs$Group <- gorgs[pos[!is.na(char$selected)]]

#Shapiro-Wilk's test
shapiro.test(resid(aov(CHARmean ~ Group, data=CHARselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(CHARmean ~ Group, data=CHARselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(CHARmean ~ Group, data=CHARselecs)  #p-value > 0.05, we can assume homogeneity of variance

#Transformations (Positive skew)
hist(CHARselecs$CHARmean)
table(CHARselecs$Group)

#We applied sqrt transformation to try to achieve the normal distribution
#did not work
#CHARselecs$CHARmean <- sqrt(CHARselecs$CHARmean)

#We applied log10 transformation to achieve the normal distribution
#did work
CHARselecs$CHARmean <- log10(CHARselecs$CHARmean)

#ANOVA
model.char <- aov(CHARmean ~ Group, data=CHARselecs)
summary(model.char)
#No significant differences
#F(1,30) = 0.24, p = 0.63

#Data distribution
boxplot(CHARmean ~ Group, data=CHARselecs)
aggregate(CHARselecs$CHARmean, by=list(CHARselecs$Group), mean)
aggregate(CHARselecs$CHARmean, by=list(CHARselecs$Group), sd)



#2.5 Contribution of autotrophically acquired carbon to octocoral respiration (CZAR)
#Download data
czar <- read.csv(file="CZARSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$CZARmean))
nrow(czar)==length(pos)
all.equal(data$CZARmean[pos], czar$CZARmean)
CZARselecs <- data[pos[!is.na(czar$selected)],]
CZARselecs$Group <- gorgs[pos[!is.na(czar$selected)]]

#Shapiro-Wilk's test
shapiro.test(resid(aov(CZARmean ~ Group, data=CZARselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(CZARmean ~ Group, data=CZARselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(CZARmean ~ Group, data=CZARselecs)  #p-value > 0.05, we can assume homogeneity of variance

#Transformations (Positive skew)
hist(CZARselecs$CZARmean)
table(CZARselecs$Group)

#We applied sqrt transformation to try to achieve the normal distribution
#did not work
#CZARselecs$CZARmean <- sqrt(CZARselecs$CZARmean)

#We applied log10 transformation to achieve the normal distribution
#did not work
#CZARselecs$CZARmean <- log10(CZARselecs$CZARmean)

#We applied inverse transformation to try to achieve the normal distribution 
#did not work
#CZARselecs$CZARmean <- 1/(CZARselecs$CZARmean)

#Therefore, we decided to use the nonparametric Kruskal-Wallis test
kruskal.test(CZARmean ~ Group, data=CZARselecs)
#Significant differences
#χ2(1) = 5.73, p = 0.01663 *

#Data distribution
boxplot(CZARmean ~ Group, data=CZARselecs)
aggregate(CZARselecs$CZARmean, by=list(CZARselecs$Group), mean)
aggregate(CZARselecs$CZARmean, by=list(CZARselecs$Group), sd)



##2.6. C budget available for growth and reproduction (SfG)
sfg <- read.csv(file="SfGSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$SfG))
nrow(sfg)==length(pos)
all.equal(data$SfG[pos], sfg$SfG)
SfGselecs <- data[pos[!is.na(sfg$selected)],]
SfGselecs$Group <- gorgs[pos[!is.na(sfg$selected)]]

#Shapiro-Wilk's test
shapiro.test(resid(aov(SfG ~ Group, data=SfGselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(SfG ~ Group, data=SfGselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(SfG ~ Group, data=SfGselecs)  #p-value > 0.05, we can assume homogeneity of variance

#Transformations (Positive skew with negative values)
hist(SfGselecs$SfG)
table(SfGselecs$Group)

#We applied adjusted log10 transformation to achieve the normal distribution
#did not work
#SfGselecs$SfG <- log10(SfGselecs$SfG +1-min(SfGselecs$SfG))

#Therefore, we decided to use the nonparametric Kruskal-Wallis test
kruskal.test(SfG ~ Group, data=SfGselecs)
#No significant differences
#χ2(1) = 0.01, p = 0.9068

#Data distribution
boxplot(SfG ~ Group, data=SfGselecs)
aggregate(SfGselecs$SfG, by=list(SfGselecs$Group), range)

#Remove all objects
rm(list = ls())












############# 3. Assessing the difference of  in carbon budgets
            #    between climatic zones (tropical and temperate)

##### Bioclimatic zones #####
#Assessment of RC, HC, CHAR and Sfg as a function of bioclimatic zones

#Download data
data <- read.csv('data.csv', h=T, sep = ';')

#3.1 Resiratory carbon demand (Rc)
#Select data
rc <- read.csv(file="RcSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$Rmean))
nrow(rc)==length(pos)
all.equal(data$Rmean[pos], rc$Rmean)
RCselecs <- data[pos[!is.na(rc$selected)],]
RCselecs <- RCselecs[RCselecs$Climate!='Polar' & !is.na(RCselecs$Climate),]

#Shapiro-Wilk's test
shapiro.test(resid(aov(Rmean ~ Climate, data=RCselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(Rmean ~ Climate, data=RCselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(Rmean ~ Climate, data=RCselecs)  #p-value > 0.05, we can assume homogeneity of variance 

#Transformations (Positive skew)
hist(RCselecs$Rmean)
table(RCselecs$Climate)

#We applied sqrt transformation to try to achieve the normal distribution
#did not work (for homogeneity of variance)
#RCselecs$Rmean <- sqrt(RCselecs$Rmean)

#We applied log10 transformation to achieve the normal distribution
#did not work
#RCselecs$Rmean <- log10(RCselecs$Rmean)

#We applied inverse transformation to try to achieve the normal distribution 
#did not work
#RCselecs$Rmean <- 1/(RCselecs$Rmean)

#Therefore, we decided to use the nonparametric Kruskal-Wallis test
kruskal.test(Rmean ~ Climate, data = RCselecs)
#No significant differences
#χ2(1) = 0.03, p = 0.87

#Data distribution
boxplot(Rmean ~ Climate, data = RCselecs)
aggregate(RCselecs$Rmean, by=list(RCselecs$Climate), mean)
aggregate(RCselecs$Rmean, by=list(RCselecs$Climate), sd)

#Remove all objects
#rm(list = ls())



#3.2. Heterotrophic carbon input (Hc)
#Select data
hc <- read.csv(file="HcSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$Hmean))
nrow(hc)==length(pos)
all.equal(data$Hmean[pos], hc$Hmean)
HCselecs <- data[pos[!is.na(hc$selected)],]
HCselecs <- HCselecs[HCselecs$Climate!='Polar' & !is.na(HCselecs$Climate),]

#Shapiro-Wilk's test
shapiro.test(resid(aov(Hmean ~ Climate, data=HCselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(Hmean ~ Climate, data=HCselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(Hmean ~ Climate, data=HCselecs)  #p-value < 0.05, we can not assume homogeneity of variance 

#Transformations (Positive skew)
hist(HCselecs$Hmean)
table(HCselecs$Climate)

#We applied sqrt transformation to try to achieve the normal distribution
#did not work
#HCselecs$Hmean <- sqrt(HCselecs$Hmean)

#We applied log10 transformation to achieve the normal distribution
#did work
HCselecs$Hmean <- log10(HCselecs$Hmean)

#ANOVA
model.hc <- aov(Hmean ~ Climate, data=HCselecs)
summary(model.hc)
#Significant differences
#F(1,30) = 9.44, p = 0.0045 **

#Data distribution
boxplot(Hmean ~ Climate, data=HCselecs)
aggregate(HCselecs$Hmean, by=list(HCselecs$Climate), mean)
aggregate(HCselecs$Hmean, by=list(HCselecs$Climate), sd)

#Remove all objects
#rm(list = ls())



#3.3. Photosynthetic carbon input (Pc)
#Only tropical data





#3.4 Contribution of heterotrophically acquired carbon to octocoral respiration (CHAR)
#Select data
char <- read.csv(file="CHARSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$CHARmean))
nrow(char)==length(pos)
all.equal(data$CHARmean[pos], char$CHARmean)
CHARselecs <- data[pos[!is.na(char$selected)],]
CHARselecs <- CHARselecs[CHARselecs$Climate!='Polar' & !is.na(CHARselecs$Climate),]

#Shapiro-Wilk's test
shapiro.test(resid(aov(CHARmean ~ Climate, data=CHARselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(CHARmean ~ Climate, data=CHARselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(CHARmean ~ Climate, data=CHARselecs)  #p-value < 0.05, we can not assume homogeneity of variance

#Transformations (Positive skew)
hist(CHARselecs$CHARmean)
table(CHARselecs$Climate)

#We applied sqrt transformation to try to achieve the normal distribution
#did not work (for homogeneity of variance)
#CHARselecs$CHARmean <- sqrt(CHARselecs$CHARmean)

#We applied log10 transformation to achieve the normal distribution
#did not work
#CHARselecs$CHARmean <- log10(CHARselecs$CHARmean)

#We applied inverse transformation to try to achieve the normal distribution 
#did not work
#CHARselecs$CHARmean <- 1/(CHARselecs$CHARmean)

#Therefore, we decided to use the nonparametric Kruskal-Wallis test
kruskal.test(CHARmean ~ Climate, data=CHARselecs)
#No significant differences
#χ2(1) = 3.78, p = 0.05

#Data distribution
boxplot(CHARmean ~ Climate, data=CHARselecs)
aggregate(CHARselecs$CHARmean, by=list(CHARselecs$Climate), mean)
aggregate(CHARselecs$CHARmean, by=list(CHARselecs$Climate), sd)

#Remove all objects
#rm(list = ls())





#3.5 Contribution of autotrophically acquired carbon to octocoral respiration (CZAR)
#Select data
czar <- read.csv(file="CZARSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$CZARmean))
nrow(czar)==length(pos)
all.equal(data$CZARmean[pos], czar$CZARmean)
CZARselecs <- data[pos[!is.na(czar$selected)],]
table(CZARselecs$Climate)

#CZAR by realm

#Download data
data <- read.csv('data.csv', h=T, sep = ';')

#Select data
czar <- read.csv(file="CZARSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$CZARmean))
nrow(czar)==length(pos)
all.equal(data$CZARmean[pos], czar$CZARmean)
CZARselecs <- data[pos[!is.na(czar$selected)],]

# Isolate tropical Pacific and Atlantic
table(CZARselecs$Realm)
CZARselecs$Realm[grep('Pacific',CZARselecs$Realm)] <- 'Indo-Pacific'
CZARselecs$Realm[grep('Temperate',CZARselecs$Realm)] <- NA

#Shapiro-Wilk's test
shapiro.test(resid(aov(CZARmean ~ Realm, data=CZARselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(CZARmean ~ Realm, data=CZARselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(CZARmean ~ Realm, data=CZARselecs)  #p-value > 0.05, we can assume homogeneity of variance

#Transformations (Positive skew)
hist(CZARselecs$CZARmean)
table(CZARselecs$Realm)

#We applied sqrt transformation to try to achieve the normal distribution
#did not work (for homogeneity of variance)
#CZARselecs$CZARmean <- sqrt(CZARselecs$CZARmean)

#We applied log10 transformation to achieve the normal distribution
#did not work
#CZARselecs$CZARmean <- log10(CZARselecs$CZARmean)

#We applied inverse transformation to try to achieve the normal distribution 
#did not work
#CZARselecs$CZARmean <- 1/(CZARselecs$CZARmean)

#Therefore, we decided to use the nonparametric Kruskal-Wallis test
kruskal.test(CZARmean ~ Realm, data=CZARselecs)
#Significant differences
#χ2(1) = 10.88, p value < 0.01

#Data distribution
boxplot(CZARmean ~ Realm, data=CZARselecs)
aggregate(CZARselecs$CZARmean, by=list(CZARselecs$Realm), mean)
aggregate(CZARselecs$CZARmean, by=list(CZARselecs$Realm), sd)


##3.6. C budget available for growth and reproduction (SfG)
#Select data
sfg <- read.csv(file="SfGSelected.csv",header=TRUE,sep=";",dec=".")
pos <- which(!is.na(data$SfG))
nrow(sfg)==length(pos)
all.equal(data$SfG[pos], sfg$SfG)
SfGselecs <- data[pos[!is.na(sfg$selected)],]
SfGselecs <- SfGselecs[SfGselecs$Climate!='Polar' & !is.na(SfGselecs$Climate),]

#Shapiro-Wilk's test
shapiro.test(resid(aov(SfG ~ Climate, data=SfGselecs))) #p-value < 0.05, we can not assume normality of the residuals
qqPlot(resid(aov(SfG ~ Climate, data=SfGselecs))) #As some of the points fall outside the reference line, we can not assume normality of the residuals

#Levene's test
leveneTest(SfG ~ Climate, data=SfGselecs)  #p-value < 0.05, we can not assume homogeneity of variance

#Transformations (Positive skew with negative values)
hist(SfGselecs$SfG)
table(SfGselecs$Climate)

#We applied adjusted log10 transformation to achieve the normal distribution
#did not work
#SfGselecs$SfG <- log10(SfGselecs$SfG +1-min(SfGselecs$SfG))

#Therefore, we decided to use the nonparametric Kruskal-Wallis test
kruskal.test(SfG ~ Climate, data=SfGselecs)
#Significant differences
#χ2(1) = 5.25, p = 0.02195*

#Data distribution
boxplot(SfG ~ Climate, data=SfGselecs)
aggregate(SfGselecs$SfG, by=list(SfGselecs$Climate), mean)
aggregate(SfGselecs$SfG, by=list(SfGselecs$Climate), sd)

#Remove all objects
rm(list = ls())













############# 4.Beta-diversity. Assessing the species turnover 
            # among biogeographic realms and bioclimatic zones
library(betapart)

#Download data
data <- read.csv('data.csv', h=T, sep = ';')
posData <- which(rowSums(data[,c('Hmean','Pmean','Rmean','CHARmean','CZARmean','SfG')], na.rm=T)>0)

#Select only rows with biogeographic information
posNA <- which(is.na(data$Realm) | data$Realm=='')
dataNew <- data[-posNA,]
#posNA <- which(rowSums(dataNew[,c('Hmean','Pmean','Rmean','CHARmean','CZARmean','SfG')], na.rm = T)==0)
#dataNew <- dataNew[-posNA,]
length(unique(dataNew$study.ID))
length(na.omit(unique(dataNew$Species)))

#4.1. Turnover by realm
commMat <- reshape::cast(dataNew, Species ~ Realm)
commMat <- commMat[!is.na(commMat$Species),]
rownames(commMat) <- commMat$Species
commMat <- commMat[,-1]
commMat <- commMat[rowSums(commMat)>0,]
commMat <- ifelse(commMat>0,1,0)

bp <- beta.pair(t(commMat), index.family = "sorensen")
mean(bp$beta.sim) #.96
sort(bp$beta.sim)

#4.2 Turnover by bioclimatic zone
commMat <- reshape::cast(dataNew, Species ~ Climate)
commMat <- commMat[!is.na(commMat$Species),]
rownames(commMat) <- commMat$Species
commMat <- commMat[,-1]
commMat <- commMat[rowSums(commMat)>0,]
commMat <- ifelse(commMat>0,1,0)

bp <- beta.pair(t(commMat), index.family = "sorensen")
mean(bp$beta.sim) #.94
sort(bp$beta.sim)

#END
