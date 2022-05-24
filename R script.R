#Set working directory
setwd("C:/Users/patri/OneDrive/Documentos/PGSISCO/Review/MABI/Variables")

#Package
library(ggplot2)
library("dplyr")

############# 1. Rc

#Rc
data <- read.csv(file="data.csv",header=TRUE,sep=";",dec=".")
Rc<-subset(data, select = c(Species, Rmean, Symbiont, Authors, Year, Title, Journal, study.ID))
Rc<-na.omit(Rc)
str(Rc)

#Check the data manually
#write.table(Rc,"Rc raw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
Rc <- read.csv(file="Rc selected.csv",header=TRUE,sep=";",dec=".")
Rc<-na.omit(Rc)

summary(Rc)
#n = 60

Rc.zoo <- filter(Rc, Symbiont == "Zooxanthellate")
summary(Rc.zoo)
#Zooxanthellate species:48

Rc.azoo <- filter(Rc, Symbiont == "Azooxanthellate")
summary(Rc.azoo)
#Azooxanthellate species:12


#Creating the figure
legend <- expression(Respiration~carbon~demand~(µgC~gAFDW^-1~hour^-1))
Fig.Rc <-ggplot(data=Rc, aes(x = reorder(Species,-Rmean), y = Rmean, fill=Symbiont))+
  geom_bar(stat="identity") +     
  scale_fill_manual(values=c("steelblue","lightgreen"), name=" ")+
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))

#Figure
Fig.Rc

#Clear the R environment
rm(list=ls(all=TRUE))



############# 2. Hc

#Hc
data <- read.csv(file="data.csv",header=TRUE,sep=";",dec=".")
Hc<-subset(data, select = c(Species, Hmean, Symbiont, Hconsidering, Authors, Year, Title, Journal, study.ID))
Hc<-na.omit(Hc)
str(Hc)

#Check the data manually
#write.table(Hc,"Hc raw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
Hc <- read.csv(file="Hc selected.csv",header=TRUE,sep=";",dec=".")
Hc<-na.omit(Hc)

summary(Hc)
#n = 34

Hc.zoo <- filter(Hc, Symbiont == "Zooxanthellate")
summary(Hc.zoo)
#Zooxanthellate species: 23

Hc.azoo <- filter(Hc, Symbiont == "Azooxanthellate")
summary(Hc.azoo)

#Creating the figure
legend <- expression(Ingestion~rates~(µgC~gAFDW^-1~hour^-1))
Fig.Hc<-ggplot(data=Hc, aes(x = reorder(Species,-Hmean), y = Hmean, fill=Symbiont))+
  geom_bar(stat="identity") +     
  scale_fill_manual(values=c("steelblue","lightgreen"), name=" ")+
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))

#Figure
Fig.Hc

#Clear the R environment
rm(list=ls(all=TRUE))



############# 3.CHAR

#CHAR
data <- read.csv(file="data.csv",header=TRUE,sep=";",dec=".")
CHAR<-subset(data, select = c(Species, CHARmean, Symbiont, Hconsidering, Authors, Year, Title, Journal, study.ID))
CHAR<-na.omit(CHAR)
str(CHAR)

#Check the data manually
#write.table(CHAR,"CHAR raw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
CHAR <- read.csv(file="CHAR selected.csv",header=TRUE,sep=";",dec=".")
CHAR <-na.omit(CHAR)

summary(CHAR)
#n = 31

CHAR.zoo <- filter(CHAR, Symbiont == "Zooxanthellate")
summary(CHAR.zoo)
#Zooxanthellate species: 21

CHAR.azoo <- filter(CHAR, Symbiont == "Azooxanthellate")
summary(CHAR.azoo)
#Azooxanthellate species: 10

#Creating the figure
Fig.CHAR<-ggplot(data=CHAR, aes(x = reorder(Species,-CHARmean), y = CHARmean, fill=Symbiont))+
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

#Clear the R environment
rm(list=ls(all=TRUE))



############# 4.Pc

#Pc
data <- read.csv(file="data.csv",header=TRUE,sep=";",dec=".")
Pc<-subset(data, select = c(Species, Pmean, Symbiont, Authors, Year, Title, Journal, study.ID))
Pc<-na.omit(Pc)
str(Pc)

#Check the data manually
#write.table(Pc,"Pc raw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
Pc <- read.csv(file="Pc selected.csv",header=TRUE,sep=";",dec=".")
Pc<-na.omit(Pc)

summary(Pc)
#n = 41

#Creating the figure
legend <- expression(Photosynthetic~carbon~input~(µgC~gAFDW^-1~hour^-1))
Fig.Pc <-ggplot(data=Pc, aes(x = reorder(Species,-Pmean), y = Pmean))+
  geom_bar(stat="identity",fill="lightgreen") + 
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab(legend)+
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))

#Figure
Fig.Pc

#Clear the R environment
rm(list=ls(all=TRUE))



############# 5.CZAR

#CZAR
data <- read.csv(file="data.csv",header=TRUE,sep=";",dec=".")
CZAR<-subset(data, select = c(Species, CZARmean, Authors, Year, Title, Journal, study.ID))
CZAR<-na.omit(CZAR)
str(CZAR)

#Check the data manually
#write.table(CZAR,"CZAR raw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
CZAR <- read.csv(file="CZAR selected.csv",header=TRUE,sep=";",dec=".")
CZAR<-na.omit(CZAR)

summary(CZAR)
#n = 62

#Creating the figure
Fig.CZAR<-ggplot(data=CZAR, aes(x = reorder(Species,-CZARmean), y = CZARmean))+
  geom_bar(stat="identity",fill="lightgreen") +     
  coord_flip()+
  theme_minimal(base_size = 12)+
  xlab(" ") + ylab("CZAR (%)")+
  geom_hline(yintercept=100, linetype="dashed", color = "black")+ 
  theme(axis.text.x = element_text(color="black",size=12))+
  theme(axis.text.y = element_text(color="black",size=12,face = "italic"))

#Figure
Fig.CZAR

#Clear the R environment
rm(list=ls(all=TRUE))



############# 6.SfG

#SfG
data <- read.csv(file="data.csv",header=TRUE,sep=";",dec=".")
SfG<-subset(data, select = c(Species, SfG, Symbiont, Authors, Year, Title, Journal, study.ID))
SfG<-na.omit(SfG)
str(SfG)

#Check the data manually
#write.table(SfG,"SfG raw.csv", sep=";", dec=".",row.names = F) 

#Reading new data
SfG<- read.csv(file="SfG selected.csv",header=TRUE,sep=";",dec=".")
SfG<-na.omit(SfG)

summary(SfG)
#n = 25

SfG.zoo <- filter(SfG, Symbiont == "Zooxanthellate")
summary(SfG.zoo)
#Zooxanthellate species: 18

SfG.azoo <- filter(SfG, Symbiont == "Azooxanthellate")
summary(SfG.azoo)
#Azooxanthellate species: 6

#Creating the figure
legend <- expression(Carbon~available~"for"~growth~and~reproduction~(µgC~gAFDW^-1~hour^-1))
Fig.SfG<-ggplot(data=SfG, aes(x = reorder(Species,-SfG), y = SfG, fill=Symbiont))+
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

#Clear the R environment
rm(list=ls(all=TRUE))



############# 7. Total species
data <- read.csv(file="data.csv",header=TRUE,sep=";",dec=".")
species<-subset(data, select = c(Species, Symbiont, Authors, Year, Title, Journal, study.ID, Rmean, Hmean, CHARmean,Pmean, CZARmean, SfG))


#End
