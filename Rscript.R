#Carbon budget trends in octocorals: a literature review with data reassessment and a conceptual framework to understand their resilience to environmental changes
#Patrick Derviche123, André Menegotto4,5, Paulo Lana1
#1 Laboratório de Ecologia Marinha, Centro de Estudos do Mar, Universidade Federal do Paraná, Pontal do Paraná 83255-976 PO Box 61, Brazil
#2 Laboratório de Ecologia de Peixes Marinhos, Departamento de Ciências Agrárias e Biológicas, Universidade Federal do Espírito Santo, São Mateus 29932-540, Brazil
#3 Programa de Pós-graduação em Oceanografia Ambiental, Universidade Federal do Espírito Santo, Vitória, Brazil
#4 Departamento de Ecologia, Universidade Federal de Goiás, Goiânia 74690-900, Brazil
#5 Departamento de Ecología, Universidad Autónoma de Madrid, Madrid 28049, Spain
#Corresponding author: patrickderviche@gmail.com (P. Derviche)


#Set working directory
#Datasets are in folder 'Variables'

#Download packages
library(ggplot2)
library("dplyr")

############# Fig 1. Variation trends in the total number of indexed, peer-reviewed studies on octocoral feeding ecology per year

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

#Figure
Fig1

#Clean the R environment
rm(list=ls(all=TRUE))

############# Fig 2. Rc- Respiration carbon demand

#Rc
data <- read.csv(file="dataUpdated.csv",header=TRUE,sep=";",dec=".")
Rc<-subset(data, select = c(UsedName, Rmean, Symbiont, Authors, Year, Title, Journal, study.ID))
Rc<-na.omit(Rc)
str(Rc)

#Check the data manually
#write.table(Rc,"RcRaw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
Rc <- read.csv(file="RcSelected.csv",header=TRUE,sep=";",dec=".")
Rc<-na.omit(Rc)

summary(Rc)
mean(Rc$Rmean)
sd(Rc$Rmean)
#n = 60
#Rc = 187.5 ± 188.5 µg C g AFDW-1 hour-1

Rc.zoo <- filter(Rc, Symbiont == "Zooxanthellate")
summary(Rc.zoo)
#Zooxanthellate species:48

Rc.azoo <- filter(Rc, Symbiont == "Azooxanthellate")
summary(Rc.azoo)
#Azooxanthellate species:12

#ANOVA
anova <- aov(Rmean ~ Symbiont, data=Rc)
summary(anova)

#Creating the figure
legend <- expression(Respiration~carbon~demand~(µg~C~g~AFDW^-1~hour^-1))
Fig.Rc <-ggplot(data=Rc, aes(x = reorder(UsedName,-Rmean), y = Rmean, fill=Symbiont))+
  geom_bar(stat="identity") +     
  scale_fill_manual(values=c("steelblue","lightgreen"), name=" ")+
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))

#Figure
Fig.Rc

#Clean the R environment
rm(list=ls(all=TRUE))

#Reading new data
Rc <- read.csv(file="RcSelectedClimate.csv",header=TRUE,sep=";",dec=".")
Rc.tropical <- filter(Rc, Climate == "Tropical")
Rc.temperate <- filter(Rc, Climate == "Temperate")
Rc.polar <- filter(Rc, Climate == "Polar")
mean(Rc.tropical$Rmean)
sd(Rc.tropical$Rmean)
mean(Rc.temperate$Rmean)
sd(Rc.temperate$Rmean)
mean(Rc.polar$Rmean)
sd(Rc.polar$Rmean)
#Tropical = 217.8 µg C g AFDW-1 hour-1
#Temperate = 168.9 µg C g AFDW-1 hour-1
#Polar = 1.1 µg C g AFDW-1 hour-1

############# Fig 3. Hc - Heterotrophic carbon input

#Hc
data <- read.csv(file="dataUpdated.csv",header=TRUE,sep=";",dec=".")
Hc<-subset(data, select = c(UsedName, Hmean, Symbiont, Hconsidering, Authors, Year, Title, Journal, study.ID))
Hc<-na.omit(Hc)
str(Hc)

#Check the data manually
#write.table(Hc,"HcRaw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
Hc <- read.csv(file="HcSelected.csv",header=TRUE,sep=";",dec=".")
Hc<-na.omit(Hc)

summary(Hc)
mean(Hc$Hmean)
sd(Hc$Hmean)
#n = 34
#80.6 ± 184.8 µg C g AFDW-1 hour-1

Hc.zoo <- filter(Hc, Symbiont == "Zooxanthellate")
summary(Hc.zoo)
mean(Hc.zoo$Hmean)
sd(Hc.zoo$Hmean)
#Zooxanthellate species: 23
#19.1 ± 286.8 µg C g AFDW-1 hour-1

Hc.azoo <- filter(Hc, Symbiont == "Azooxanthellate")
summary(Hc.azoo)
mean(Hc.azoo$Hmean)
sd(Hc.azoo$Hmean)
#Azooxanthellate species: 23
#209.0 ± 40.2 µg C g AFDW-1 hour-1

#Creating the figure
legend <- expression(Ingestion~rates~(µg~C~g~AFDW^-1~hour^-1))
Fig.Hc<-ggplot(data=Hc, aes(x = reorder(UsedName,-Hmean), y = Hmean, fill=Symbiont))+
  geom_bar(stat="identity") +     
  scale_fill_manual(values=c("steelblue","lightgreen"), name=" ")+
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))

#Figure
Fig.Hc

#Clean the R environment
rm(list=ls(all=TRUE))

############# Fig 4.CHAR - Contribution of heterotrophically acquired carbon to animal respiration

#CHAR
data <- read.csv(file="dataUpdated.csv",header=TRUE,sep=";",dec=".")
CHAR<-subset(data, select = c(UsedName, CHARmean, Symbiont, Hconsidering, Authors, Year, Title, Journal, study.ID))
CHAR<-na.omit(CHAR)
str(CHAR)

#Check the data manually
#write.table(CHAR,"CHARraw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
CHAR <- read.csv(file="CHARselected.csv",header=TRUE,sep=";",dec=".")
CHAR <-na.omit(CHAR)

summary(CHAR)
mean(CHAR$CHARmean)
sd(CHAR$CHARmean)
#n = 31
#72.8 ± 116.0 %

CHAR.zoo <- filter(CHAR, Symbiont == "Zooxanthellate")
summary(CHAR.zoo)
mean(CHAR.zoo$CHARmean)
sd(CHAR.zoo$CHARmean)
#Zooxanthellate species: 21
#30.0 ± 32.6 %

CHAR.azoo <- filter(CHAR, Symbiont == "Azooxanthellate")
summary(CHAR.azoo)
mean(CHAR.azoo$CHARmean)
sd(CHAR.azoo$CHARmean)
#Azooxanthellate species: 10
#162.8 ± 171.0 %

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

#Figure
Fig.CHAR

#Clean the R environment
rm(list=ls(all=TRUE))

############# Fig 5.Pc - Photosynthetic carbon input

#Pc
data <- read.csv(file="dataUpdated.csv",header=TRUE,sep=";",dec=".")
Pc<-subset(data, select = c(UsedName, Pmean, Symbiont, Authors, Year, Title, Journal, study.ID))
Pc<-na.omit(Pc)
str(Pc)

#Check the data manually
#write.table(Pc,"PcRaw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
Pc <- read.csv(file="PcSelected.csv",header=TRUE,sep=";",dec=".")
Pc<-na.omit(Pc)

summary(Pc)
mean(Pc$Pmean)
sd(Pc$Pmean)
#n = 41
#276.7 ± 278.1 µg C g AFDW-1 hour-1

#Creating the figure
legend <- expression(Photosynthetic~carbon~input~(µg~C~g~AFDW^-1~hour^-1))
Fig.Pc <-ggplot(data=Pc, aes(x = reorder(UsedName,-Pmean), y = Pmean))+
  geom_bar(stat="identity",fill="lightgreen") + 
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))

#Figure
Fig.Pc

#Clean the R environment
rm(list=ls(all=TRUE))

############# Fig 6.CZAR - Contribution of zooxanthellae to animal respiration

#CZAR
data <- read.csv(file="dataUpdated.csv",header=TRUE,sep=";",dec=".")
CZAR<-subset(data, select = c(UsedName, CZARmean, Authors, Year, Title, Journal, study.ID))
CZAR<-na.omit(CZAR)
str(CZAR)

#Check the data manually
#write.table(CZAR,"CZARraw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
CZAR <- read.csv(file="CZARselected.csv",header=TRUE,sep=";",dec=".")
CZAR<-na.omit(CZAR)

summary(CZAR)
mean(CZAR$CZARmean)
sd(CZAR$CZARmean)
#n = 62
#156.7 ± 113.9 %

#Creating the figure
Fig.CZAR<-ggplot(data=CZAR, aes(x = reorder(UsedName,-CZARmean), y = CZARmean))+
  geom_bar(stat="identity",fill="lightgreen") +     
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab("CZAR (%)")+
  geom_hline(yintercept=100, linetype="dashed", color = "black")+ 
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))

#Figure
Fig.CZAR

#Clean the R environment
rm(list=ls(all=TRUE))

############# Fig 7.SfG - Scope for growth

#SfG
data <- read.csv(file="dataUpdated.csv",header=TRUE,sep=";",dec=".")
SfG<-subset(data, select = c(UsedName, SfG, Symbiont, Authors, Year, Title, Journal, study.ID))
SfG<-na.omit(SfG)
str(SfG)

#Check the data manually
#write.table(SfG,"SfGraw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
SfG<- read.csv(file="SfGselected.csv",header=TRUE,sep=";",dec=".")
SfG<-na.omit(SfG)

summary(SfG)
mean(SfG$SfG)
sd(SfG$SfG)
#n = 24
#209.0 ± 40.2 µg C g AFDW-1 hour-1

SfG.zoo <- filter(SfG, Symbiont == "Zooxanthellate")
summary(SfG.zoo)
mean(SfG.zoo$SfG)
sd(SfG.zoo$SfG)
#Zooxanthellate species: 18
#23.2 ± 28.2 µg C g AFDW-1 hour-1

SfG.azoo <- filter(SfG, Symbiont == "Azooxanthellate")
summary(SfG.azoo)
mean(SfG.azoo$SfG)
sd(SfG.azoo$SfG)
#Azooxanthellate species: 6
#209.0 ± 40.2 µg C g AFDW-1 hour-1

#Creating the figure
legend <- expression(Carbon~available~"for"~growth~and~reproduction~(µg~C~g~AFDW^-1~hour^-1))
Fig.SfG<-ggplot(data=SfG, aes(x = reorder(UsedName,-SfG), y = SfG, fill=Symbiont))+
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("steelblue","lightgreen"), name=" ")+
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ 
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))

#Figure
Fig.SfG

#Clean the R environment
rm(list=ls(all=TRUE))

#End