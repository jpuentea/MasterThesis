# MASTER THESIS: EXPLORING THE INFLUENCE OF BROWSING ACTIVITY IN FACEBOOK ADVERTISEMENTS.
# This script aims to analyze the data obtained in the different experiments.



# Set up ------------------------------------------------------------------


Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_191')
install.packages("RSQLite")

library("RSQLite")
library("xlsx")
library(ggplot2)
library(shiny)
library(dplyr)
library(DT)

#######################################
#FUNCTIONS USED IN THE SCRIPT:

#Function to get and print number of pages visited by the user:

infoPages <- function(user){
  visitedBy <- visited[which(visited$Id == user), ]
  Numberpages = nrow(visitedBy)
  print(paste("Number of pages visited by",user,Numberpages))
  return(visitedBy)
}


#Function to obtain ads of an advertiser
adsof<- function(advertiser){
  ads = AdsInfo[which(AdsInfo$Advertiser == advertiser ),]
  print(paste("Number of ads of",advertiser,": ",nrow(ads)))
  return(ads)
}

#Function to obtain ads oriented only to one sex

sexoriented <- function(advertiser,sex){
  advertiserappearances <- AdsInfo[which(AdsInfo$Advertiser == advertiser),]
  result <- advertiserappearances[grepl(paste(sex), advertiserappearances$Explanation), ]
}

#Function to obtain ads from a certain date

adsAfterDate <- function(date){
  return(AdsInfo[which(AdsInfo$TimeStamp > date),])
}



#Function to obtain ads between two selected dates. The dates must be inserted following the pattern YYYY-MM-DD

adsBetweenDates <- function(from, to){
  aux <- AdsInfo[which((AdsInfo$TimeStamp >= from)& (AdsInfo$TimeStamp < to)),]
  return(aux[which(!is.na(aux$Advertiser)),])
}


#Function to obtain pages between two selected dates.The dates must be inserted following the pattern YYYY-MM-DD

pagesBetweenDates <- function(from, to){
  aux <- visited[which((visited$TimeStamp >= from)& (visited$TimeStamp < to)),]
  return(aux[which(!is.na(aux$PageVisited)),])

}


#Function to obtain percentage

percentage <- function(total, part){
  
  
  percent <- ( length(part)/ nrow(total)) * 100
  return(round(percent,digits = 2))
  
}

#Function to analyze difference on number of ads of a certain type

adsDifference <- function(user1,user2, time, topic){
  if(time ==""){
   a <- AdsInfo 
  }else{
    a <- adsAfterDate(time)
  }
  
  
}

######################################


# Importing databases -----------------------------------------------------


con = dbConnect(SQLite(), dbname="pagesvisited.db")
con2 = dbConnect(SQLite(), dbname="interestsBrowsingAndDetection.db")
print("List of Tables in your dbs: ")
print("In pagesVisited...")
print(dbListTables(con))
print("In interests...")
print(dbListTables(con2))

#profiles <- c("Mark","Lisa","RoberStinto")
#users <- c("Marc Roses","Lisa Thompson","Rober Stinto")



# Get visited table as a dataframe ----------------------------------------



visited = dbGetQuery(con, 'select * from visited')
print(visited)



-# #Obtain information about the pages visited by each user. ---------------


visitedByMarc <- infoPages("Mark")
visitedByLisa = infoPages("Lisa")
visitedByRober = infoPages("RoberStinto")
visitedByIsabelle = infoPages("Isabelle Pontichelli")
visitedByIgnatius= infoPages("Ignatius")
visitedByThomas= infoPages("Thomas Mertens")
visitedByAstrid= infoPages("Astrid neels")
visitedByHenry= infoPages("Henry")
visitedByGustavo= infoPages("Gustavo")
visitedByAlanSteven= infoPages("Alan Steven")
visitedBySelenaMatthews= infoPages("Selena Matthews")
visitedByReedScott= infoPages("Reed Scott")
visitedByMeryClarck= infoPages("Mery Clark")
visitedBySamSmith= infoPages("Sam Smith")

# Get adsFinal table as a dataframe ---------------------------------------


AdsInfo = dbGetQuery(con2, 'SELECT * FROM adsFinal ')
str(AdsInfo)


AdsTags = dbGetQuery(con2, 'SELECT * from advertiserInfoFinal')
AdsInfo$sex<-NA

#Creation of a single dataset containing all the Information about ads received and advertiser's info

for(i in 1:nrow(AdsInfo)){
  try(AdsInfo[i, "AdvertiserTags"] <- AdsTags$Tags[which(AdsTags$Advertiser == AdsInfo[i,"Advertiser"])])
  try(AdsInfo[i, "AdvertiserKeywords"] <- AdsTags$Keywords[which(AdsTags$Advertiser == AdsInfo[i,"Advertiser"])])
  try(AdsInfo[i, "AdvertiserDescription"] <- AdsTags$Description[which(AdsTags$Advertiser == AdsInfo[i,"Advertiser"])])
  if(grepl("Lisa Thompson|Astrid Neels|Carmen Aller|Isabelle Ponticelli|Selena Matthews|Mery Clarck",AdsInfo[i,2])){
    AdsInfo[i,"sex"] = "Female"
  }else{
    AdsInfo[i,"sex"] = "Male"
  }
 
  
}

#Cleaning data, deleting NA appearances.
AdsInfo <- AdsInfo[which(!is.na(AdsInfo$Advertiser)),]


# Division of advertisers by user -----------------------------------------

AdsOfMarc = AdsInfo[which(AdsInfo$UserId == "Marc Roses"),]

AdsOfLisa = AdsInfo[which(AdsInfo$UserId == "Lisa Thompson"),]

AdsOfRoberStinto = AdsInfo[which(AdsInfo$UserId == "Rober Stinto"),]

AdsOfIsabelle = AdsInfo[which(AdsInfo$UserId == "Isabelle Ponticelli"),]

AdsOfIgnatius = AdsInfo[which(AdsInfo$UserId == "Ignatius Smith"),]

AdsOfThomas = AdsInfo[which(AdsInfo$UserId == "Thomas Mertens"),]

AdsOfAstrid = AdsInfo[which(AdsInfo$UserId == "Astrid Neels"),]

AdsOfHenry = AdsInfo[which(AdsInfo$UserId == "Henry Williams"),]

AdsOfGustavo = AdsInfo[which(AdsInfo$UserId == "Gustavo Rueda Pérez"),]

AdsOfAlanSteven = AdsInfo[which(AdsInfo$UserId == "Alan Steven"),]

AdsOfSelenaMatthews = AdsInfo[which(AdsInfo$UserId == "Selena Matthews"),]

AdsOfReedScott = AdsInfo[which(AdsInfo$UserId == "Reed Scott"),]

AdsOfMeryClarck = AdsInfo[which(AdsInfo$UserId == "Mery Clarck"),]

AdsOfSamSmith = AdsInfo[which(AdsInfo$UserId == "Sam Smith"),]


NumberAdsOfMarc = nrow(AdsOfMarc)
NumberAdsOfLisa = nrow(AdsOfLisa)
NumberAdsOfRoberStinto =nrow(AdsOfRoberStinto)
NumberAdsOfIsabelle = nrow(AdsOfIsabelle)
NumberAdsOfIgnatius = nrow(AdsOfIgnatius)
NumberAdsOfThomas = nrow(AdsOfThomas)
NumberAdsOfAstrid = nrow(AdsOfAstrid)
NumberAdsOfHenry = nrow(AdsOfHenry)
NumberAdsOfGustavo = nrow(AdsOfGustavo)
NumberAdsOfAlanSteven = nrow(AdsOfAlanSteven)
NumberAdsOfSelenaMatthews = nrow(AdsOfSelenaMatthews)
NumberAdsOfReedScott =nrow(AdsOfReedScott)
NumberAdsOfMeryClarck = nrow(AdsOfMeryClarck)
NumberAdsOfSamSmith = nrow(AdsOfSamSmith)




sprintf("Number of ads received by Lisa: %i",NumberAdsOfLisa )
sprintf("Number of ads received by Marc: %i",NumberAdsOfMarc )
sprintf("Number of ads received by Rober Stinto: %i",NumberAdsOfRoberStinto) 
sprintf("Number of ads received by Isabelle: %i",NumberAdsOfIsabelle)
sprintf("Number of ads received by Ignatius: %i",NumberAdsOfIgnatius) 
sprintf("Number of ads received by Thomas Mertens: %i",NumberAdsOfThomas) 
sprintf("Number of ads received by Astrid Neels: %i",NumberAdsOfAstrid)
sprintf("Number of ads received by Henry Williams: %i",NumberAdsOfHenry) 
sprintf("Number of ads received by Gustavo: %i",NumberAdsOfGustavo)
sprintf("Number of ads received by Alan Steven: %i",NumberAdsOfAlanSteven) 
sprintf("Number of ads received by Selena Matthews: %i",NumberAdsOfSelenaMatthews)
sprintf("Number of ads received by Reed Scott: %i",NumberAdsOfReedScott) 
sprintf("Number of ads received by Mery Clarck: %i",NumberAdsOfMeryClarck)
sprintf("Number of ads received by Sam Smit: %i",NumberAdsOfSamSmith)




######################### FIRST EXPERIMENT ################

######------------MARC AND LISA -----------------######


#### MARC ####


#Cars and Shops advertisers presented to Marc 

#This experiment was conducted FROM 18/2/2019 TO 24/4/2

AdsExperiment1Test1<- adsBetweenDates("2019-02-18","2019-04-24")
PagesExperiment1Test1 <- pagesBetweenDates("2019-02-18","2019-04-24")
PagesExperiment1Test1 <- PagesExperiment1Test1[which(PagesExperiment1Test1$Id == "Mark" | PagesExperiment1Test1$Id == "Lisa"),]
print(paste("Total number of pages visited in Test 1 of Experiment 1: ",nrow(PagesExperiment1Test1)))
AdsExperiment1Test1<- AdsExperiment1Test1[which(AdsExperiment1Test1$UserId == "Marc Roses" | AdsExperiment1Test1$UserId == "Lisa Thompson"),]
print(paste("Total number of advertisements received in Test 1 of Experiment 1: ",nrow(AdsExperiment1Test1)))

PagesExperiment1Test1 <- PagesExperiment1Test1[which(PagesExperiment1Test1$Id == "Mark" | PagesExperiment1Test1$Id == "Lisa"),]
PagesOfMarcExperiment = PagesExperiment1Test1[which(PagesExperiment1Test1$Id == "Mark"),]
print(paste("Total number of pages visited by the MALE in Test 1 of Experiment 1: ",nrow(PagesOfMarcExperiment)))
AdsOfMarcExperiment = AdsExperiment1Test1[which(AdsExperiment1Test1$UserId == "Marc Roses"),]
NumberAdsOfMarcExperiment = nrow(AdsOfMarcExperiment)
print(paste("Total number of advertisements displayed to the MALE in Test 1 of Experiment 1: ",NumberAdsOfMarcExperiment))

PagesOfLisaExperiment = PagesExperiment1Test1[which(PagesExperiment1Test1$Id == "Lisa"),]
print(paste("Total number of pages visited by the FEMALE in Test 1 of Experiment 1: ",nrow(PagesOfLisaExperiment)))
AdsOfLisaExperiment = AdsExperiment1Test1[which(AdsExperiment1Test1$UserId == "Lisa Thompson"),]
NumberAdsOfLisaExperiment = nrow(AdsOfLisaExperiment)
print(paste("Total number of advertisements displayed to the FEMALE in Test 1 of Experiment 1: ",NumberAdsOfLisaExperiment))


# CARS MARC ---------------------------------------------------------------



AdsOfCarsExperimentTotal <-grepl(" car |vehicle|car | car,|automobile|Cars", paste(AdsExperiment1Test1$AdvertiserTags,AdsExperiment1Test1$AdText,AdsExperiment1Test1$AdvertiserKeywords,AdsExperiment1Test1$AdvertiserDescription),ignore.case = TRUE)
AdsOfCarsMarcTotal <- grepl(" car |vehicle|car | car,|automobile|Cars", paste(AdsOfMarcExperiment$AdvertiserTags,AdsOfMarcExperiment$AdText,AdsOfMarcExperiment$AdvertiserKeywords,AdsOfMarcExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfMarcExperiment$isCar <- AdsOfCarsMarcTotal
AdsOfMarcExperiment$isCar <- as.factor(AdsOfMarcExperiment$isCar)
AdsExperiment1Test1$isCar <- AdsOfCarsExperimentTotal
AdsExperiment1Test1$isCar <- as.factor(AdsExperiment1Test1$isCar)

print(paste("Total number of Cars of Experiment1: ", length(which(AdsExperiment1Test1$isCar == TRUE))))
print(paste("Total percentage of Cars of Experiment1: ", percentage( AdsExperiment1Test1,which(AdsExperiment1Test1$isCar == TRUE))))



print(paste("Total number of Cars displayed to the Male in test 1 of Experiment1: ", length(which(AdsOfCarsMarcTotal == TRUE))))

#Plotting percentages
  
percentage(AdsOfMarcExperiment,which(AdsOfCarsMarcTotal == TRUE))
AdsMarcCarsPlot <- AdsOfMarcExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isCar))) + geom_bar(position = "fill", color = "grey40",width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Car related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Car advertisements received by Marc")+
  labs( x="",y = "Percentage", fill = "Car Ad")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=12),legend.title = element_text(size=20), legend.text = element_text(size=16))
AdsMarcCarsPlot



# SHOPS MARC --------------------------------------------------------------

AdsOfShopsExperimentTotal <- grepl("shop|clothe|clothing|fashion", paste(AdsExperiment1Test1$AdvertiserTags,AdsExperiment1Test1$AdText,AdsExperiment1Test1$AdvertiserKeywords,AdsExperiment1Test1$AdvertiserDescription),ignore.case = TRUE)

AdsOfShopsMarcTotal <- grepl("shop|clothe|clothing|fashion", paste(AdsOfMarcExperiment$AdvertiserTags,AdsOfMarcExperiment$Adtext,AdsOfMarcExperiment$AdvertiserKeywords,AdsOfMarcExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfMarcExperiment$isShop <- AdsOfShopsMarcTotal
AdsOfMarcExperiment$isShop <- as.factor(AdsOfMarcExperiment$isShop)
AdsExperiment1Test1$isShop <- AdsOfShopsExperimentTotal
AdsExperiment1Test1$isShop <- as.factor(AdsExperiment1Test1$isShop)

print(paste("Total number of Shopping advertisements of Experiment1: ", length(which(AdsExperiment1Test1$isShop == TRUE))))

print(paste("Total number of Shopping displayed to the Male in test 1 of Experiment1: ", length(which(AdsOfShopsMarcTotal == TRUE))))



AdsMarcShopssPlot <- AdsOfMarcExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isShop))) + geom_bar(position = "fill", color = "grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Shop related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Shop advertisements received by Marc")+
  labs( x="",y = "Percentage", fill = "Shop Ad")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=12),legend.title = element_text(size=20), legend.text = element_text(size=16))
AdsMarcShopssPlot

# variation of cars Marc --------------------------------------------------


#Analising variation of ads of Cars presented to Marc
datesCars <- c()
datesCars <- c(datesCars,AdsOfMarcExperiment[which(AdsOfMarcExperiment$isCar == "TRUE"),1])
datesCars <- sort(datesCars)
datesCars <- gsub(" .*","",datesCars)

carvariation <- data.frame(Date=character(), Amount=integer())
for(i in 1:length(datesCars)){
  if(sum(carvariation$Date == datesCars[i]) == 0){
    carvariation <- rbind(carvariation, data.frame(Date = datesCars[i], Amount= sum(datesCars == datesCars[i])))
    carvariation
    }
}
carvariation
carvariation$Date <- as.Date(carvariation$Date, "%Y-%m-%d")
plot(Amount ~ Date, carvariation, xaxt = "n", type = "l")
axis(1, carvariation$Date, format(carvariation$Date, "%b %d"), cex.axis = .7)


#### LISA 

# Cars and Shops presented to Lisa 


AdsOfLisaExperiment = AdsExperiment1Test1[which(AdsExperiment1Test1$UserId == "Lisa Thompson"),]
NumberAdsOfLisaExperiment = nrow(AdsOfLisaExperiment)
print(paste("Total number of advertisements displayed to the FEMALE in Test 1 of Experiment 1: ",NumberAdsOfLisaExperiment))


# CARS LISA ---------------------------------------------------------------


AdsOfCarsLisaTotal <- grepl(" car |vehicle|car | car,|automobile|gasoline|Cars", paste(AdsOfLisaExperiment$AdvertiserTags,AdsOfLisaExperiment$AdText,AdsOfLisaExperiment$AdvertiserKeywords,AdsOfLisaExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfLisaExperiment$isCar <- AdsOfCarsLisaTotal
AdsOfLisaExperiment$isCar <- as.factor(AdsOfLisaExperiment$isCar)

print(paste("Total number of Cars advertisements displayed to the Female in test 1 of Experiment1: ", length(which(AdsOfCarsLisaTotal == TRUE))))


percentage(AdsOfLisaExperiment,which(AdsOfCarsLisaTotal == TRUE))


AdsLisaCarsPlot <- AdsOfLisaExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isCar))) + geom_bar(position = "fill", color = "grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Car related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Car advertisements received by Lisa")+
  labs( x="",y = "Percentage", fill = "Car Ad")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=12),legend.title = element_text(size=20), legend.text = element_text(size=16))
AdsLisaCarsPlot

# ggplot(AdsOfLisaExperiment, aes(x=UserId, fill=isCar))+
#   geom_bar(bimwidth=0.5)+
#   
#   ggtitle("Car Ads Lisa")+
#   xlab("Title")+
#   ylab("Total Count")+
#   labs(fill="isCar")



# SHOPS LISA --------------------------------------------------------------


AdsOfShopsLisaTotal <- grepl("shop|clothe|clothing|fashion",  paste(AdsOfLisaExperiment$AdvertiserTags,AdsOfLisaExperiment$AdText,AdsOfLisaExperiment$AdvertiserKeywords,AdsOfLisaExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfLisaExperiment$isShop <- AdsOfShopsLisaTotal
AdsOfLisaExperiment$isShop <- as.factor(AdsOfLisaExperiment$isShop)

print(paste("Total number of Shopping advertisements displayed to the Female in test 1 of Experiment1: ", length(which(AdsOfShopsLisaTotal == TRUE))))


percentage(AdsOfLisaExperiment,which(AdsOfShopsLisaTotal == TRUE))

#Plotting percentages
 


AdsLisaShopsPlot <- AdsOfLisaExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isShop))) + geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Shop related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Shop advertisements received by Lisa")+
  labs( x="",y = "Percentage", fill = "Shop Ad")+
  theme(plot.title = element_text(hjust = 0.5))
AdsLisaShopsPlot


# SUMMery OF EXPERIMENT ---------------------------------------------------



sprintf("Percentage of advertisements of Cars to Marc %g%%",percentage(AdsOfMarcExperiment,which(AdsOfCarsMarcTotal == TRUE)))
sprintf("Percentage of advertisements of Cars to Lisa %g%%",percentage(AdsOfLisaExperiment,which(AdsOfCarsLisaTotal == TRUE)))

sprintf("Percentage of advertisements of Shops to Marc %g%%",percentage(AdsOfMarcExperiment,which(AdsOfShopsMarcTotal == TRUE)))
sprintf("Percentage of advertisements of Shops to Lisa %g%%",percentage(AdsOfLisaExperiment,which(AdsOfShopsLisaTotal == TRUE)))


### PLOTS OF CARS AND SHOPS ADS OF BOTH USERS TOGHETER.

CarsBothPlot <- AdsExperiment1Test1 %>%
  ggplot(aes(x=factor(UserId), fill=factor(isCar))) + geom_bar(position = "fill",color="grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Car related ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Percentage of car related ads")+
  labs(x = "", y = "Percentage")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=16),legend.title = element_text(size=20), legend.text = element_text(size=16))
CarsBothPlot

ShopsBothPlot <- AdsExperiment1Test1 %>%
  ggplot(aes(x=factor(UserId), fill=factor(isShop))) + geom_bar(position = "fill", color= "grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Shop related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Shopping related advertisements")+
  labs(x = "User Name", y = "Percentage")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=16),legend.title = element_text(size=20), legend.text = element_text(size=16))
ShopsBothPlot

#############

#Ads specifically of cars (the word Car appears in its Facebook label or in its web page keywords)

AdsSpecificCarsExperiment <- grepl("Car", paste(AdsExperiment1Test1$AdvertiserTags),ignore.case = TRUE)
AdsExperiment1Test1$SpecificCar <- AdsSpecificCarsExperiment
NumberAdsMarcSpecificCars <- length(which(AdsExperiment1Test1$UserId == "Marc Roses" & AdsExperiment1Test1$SpecificCar == TRUE))
NumberAdsLisaSpecificCars <- length(which(AdsExperiment1Test1$UserId == "Lisa Thompson" & AdsExperiment1Test1$SpecificCar == TRUE))

print(paste("Ads specific of cars: ",length(AdsExperiment1Test1[which(AdsExperiment1Test1$SpecificCar == TRUE),"Advertiser"])))

CarSpecificAdvertisers <- unique(AdsExperiment1Test1[which(AdsExperiment1Test1$SpecificCar == TRUE),"Advertiser"])
CarSpecificAdvertisers

pievalues <- c(NumberAdsMarcSpecificCars,NumberAdsLisaSpecificCars)
pielabels<-c("Marc", "Lisa")

#PIE GRAPH TO SEE NUMBER OF SPECIF ADS OF CARS FOR EACH USER.
pie(pievalues, pievalues , main = "Advertisers specifically of Cars",border="white",cex=1.2 ,col = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  legend("topright", c("Marc", "Lisa"),cex=1.2,fill = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"),)

#Ads specifically of Shops (the word Car appears in its Facebook label or in its web page keywords)

AdsSpecificShopsExperiment <- grepl("Shop", paste(AdsExperiment1Test1$AdvertiserTags, AdsExperiment1Test1$AdvertiserKeywords),ignore.case = TRUE)
AdsExperiment1Test1$SpecificShop <- AdsSpecificShopsExperiment
NumberAdsMarcSpecificShops <- length(which(AdsExperiment1Test1$UserId == "Marc Roses" & AdsExperiment1Test1$SpecificShop == TRUE))
NumberAdsLisaSpecificShops <- length(which(AdsExperiment1Test1$UserId == "Lisa Thompson" & AdsExperiment1Test1$SpecificShop == TRUE))

print(paste("Ads specific of Shops: ",length(AdsExperiment1Test1[which(AdsExperiment1Test1$SpecificShop == TRUE),"Advertiser"])))

ShopspecificAdvertisers <- unique(AdsExperiment1Test1[which(AdsExperiment1Test1$SpecificShop == TRUE),"Advertiser"])
ShopspecificAdvertisers

pievalues <- c(NumberAdsMarcSpecificShops,NumberAdsLisaSpecificShops)
pielabels<-c("Marc", "Lisa")

#PIE GRAPH TO SEE NUMBER OF SPECIF ADS OF Shops FOR EACH USER.
pie(pievalues, pievalues , main = "Advertisers specifically of Shops",border="white", col = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  legend("topright", c("Marc", "Lisa"),cex=0.8,fill = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))





#############

#Number of unique car related advertisers for each user:
UniqueAdsCarMarc <- unique(AdsOfMarcExperiment[which(AdsOfMarcExperiment$isCar == TRUE),"Advertiser"])
UniqueAdsCarLisa <- unique(AdsOfLisaExperiment[which(AdsOfLisaExperiment$isCar == TRUE),"Advertiser"])

uniqueAdsCarMarcDataFrame <- data.frame("Advertiser" = UniqueAdsCarMarc, "Count" = 0)
for(i in 1:length(UniqueAdsCarMarc)){
  uniqueAdsCarMarcDataFrame[which(uniqueAdsCarMarcDataFrame$Advertiser == UniqueAdsCarMarc[i]), "Count"] = nrow(AdsOfMarcExperiment[which(AdsOfMarcExperiment$Advertiser == UniqueAdsCarMarc[i]),])
}
uniqueAdsCarMarcDataFrame <- uniqueAdsCarMarcDataFrame[order(uniqueAdsCarMarcDataFrame$Count, decreasing = TRUE),]

AdvertisersMarcCarsGraph <-ggplot( uniqueAdsCarMarcDataFrame[which(uniqueAdsCarMarcDataFrame$Count > 1),], aes( Advertiser, Count))+geom_col(width  = 0.6, fill ="#29487D")+
  ggtitle("Advertisers displayed to the Male more than once")+
  theme(plot.title = element_text(hjust = 0.5, size = 18), axis.title = element_text(size=16), axis.text = element_text(size=13),legend.title = element_text(size=16), legend.text = element_text(size=13))


uniqueAdsCarLisaDataFrame <- data.frame("Advertiser" = UniqueAdsCarLisa, "Count" = 0)
for(i in 1:length(UniqueAdsCarLisa)){
  uniqueAdsCarLisaDataFrame[which(uniqueAdsCarLisaDataFrame$Advertiser == UniqueAdsCarLisa[i]), "Count"] = nrow(AdsOfLisaExperiment[which(AdsOfLisaExperiment$Advertiser == UniqueAdsCarLisa[i]),])
}
uniqueAdsCarLisaDataFrame <- uniqueAdsCarLisaDataFrame[order(uniqueAdsCarLisaDataFrame$Count, decreasing = TRUE),]

AdvertisersLisaCarsGraph <-ggplot( uniqueAdsCarLisaDataFrame, aes( Advertiser, Count))+geom_col(width  = 0.6, fill ="#29487D")+
  ggtitle("Advertisers displayed to the Female")+
  theme(plot.title = element_text(hjust = 0.5, size = 18), axis.title = element_text(size=16), axis.text = element_text(size=13),legend.title = element_text(size=16), legend.text = element_text(size=13))

#Number of unique car of shop

UniqueAdsShopMarc <- unique(AdsOfMarcExperiment[which(AdsOfMarcExperiment$isShop == TRUE),"Advertiser"])
UniqueAdsShopLisa <- unique(AdsOfLisaExperiment[which(AdsOfLisaExperiment$isShop == TRUE),"Advertiser"])

uniqueshops <- data.frame("User"=c("Marc Roses", "Lisa Thompson"), "Count"=c(length(UniqueAdsShopMarc),length(UniqueAdsShopLisa)))

NumberShoppingAds <-ggplot( uniqueshops, aes( User, Count))+geom_col(width  = 0.5, fill ="#29487D")+
  ggtitle("Number of different advertisers related to Shopping")+
  theme(plot.title = element_text(hjust = 0.5, size = 18), axis.title = element_text(size=16), axis.text = element_text(size=13) )+
  coord_flip()
NumberShoppingAds

#Number of advertisers related to cars received by each user NOT TARGETTING GENDER.

AdsExperiment1Test1$TargetedAd <- grepl(" men | women ", AdsExperiment1Test1$Explanation)
AdsExperiment1Test1$isCartNotarget <- AdsExperiment1Test1$isCar == TRUE & AdsExperiment1Test1$TargetedAd == FALSE
CarsBothPlotNotTarget <- AdsExperiment1Test1 %>%
  ggplot(aes(x=factor(UserId), fill=factor(isCartNotarget))) + geom_bar(position = "fill", color="grey40", width= 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Car related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Car related advertisements not target")+
  labs(x = "User Name", y = "Percentage", fill = "Car Ad")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=15),legend.title = element_text(size=20), legend.text = element_text(size=16))
CarsBothPlotNotTarget

AdsExperiment1Test1$isShopNotarget <- AdsExperiment1Test1$isShop == TRUE & AdsExperiment1Test1$TargetedAd == FALSE

ShopsBothPlotNotTarget <- AdsExperiment1Test1 %>%
  ggplot(aes(x=factor(UserId), fill=factor(isShopNotarget))) + geom_bar(position = "fill", color= "grey40", width=0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Shop related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Shopping related advertisements no target")+
  labs(x = "User Name", y = "Percentage")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=16),legend.title = element_text(size=20), legend.text = element_text(size=16))
ShopsBothPlotNotTarget

NumberAdsCarsMarcNoTarget <- nrow(AdsOfMarcExperiment[!grepl(" men ", AdsOfMarcExperiment$Explanation) & AdsOfMarcExperiment$isCar == TRUE, ])
NumberAdsCarsLisaNoTarget <- nrow(AdsOfLisaExperiment[!grepl(" women ", AdsOfLisaExperiment$Explanation) & AdsOfLisaExperiment$isCar == TRUE, ])

NumberAdsShopsMarcNoTarget <- nrow(AdsExperiment1Test1[which(AdsExperiment1Test1$isShopNotarget == TRUE & AdsExperiment1Test1$UserId == "Marc Roses"),])
NumberAdsShopLisaNoTarget <- nrow(AdsExperiment1Test1[which(AdsExperiment1Test1$isShopNotarget == TRUE & AdsExperiment1Test1$UserId == "Lisa Thompson"),])

######

# TOTAL NUMBER OF CAR AND SHOP  AND THE GRAPH OF EACH USER.

AdsOfCarsExperiment <- AdsExperiment1Test1[which(AdsExperiment1Test1$isCar == TRUE),]
GraphNumberCars <-ggplot(AdsOfCarsExperiment, aes( x="Car-related advertisements", fill=factor(UserId))) + geom_bar(width  = 0.4, color="grey40")+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Total number of ads related to cars")+
  coord_cartesian(ylim=c(0,77))+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=16),legend.title = element_text(size=20), legend.text = element_text(size=16))

GraphNumberCars

AdsOfShopsExperiment <- AdsExperiment1Test1[which(AdsExperiment1Test1$isShop == TRUE),]

GraphNumberShops <-ggplot(AdsOfShopsExperiment, aes( x="Shopping-related advertisements", fill=factor(UserId))) + geom_bar(width  = 0.4, color="grey40")+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Total number of ads related to Shopping")+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=16),legend.title = element_text(size=20), legend.text = element_text(size=16))

GraphNumberShops

AdsOfShopsExperimentNotTarget <- AdsExperiment1Test1[which(AdsExperiment1Test1$isShopNotarget == TRUE),]

GraphNumberShopsNotTarget <-ggplot(AdsOfShopsExperimentNotTarget, aes( x= "Shoppping-related advertisements no target", fill=factor(UserId))) + geom_bar(width  = 0.4, color="grey40")+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Total number of ads related to Shopping")+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=16),legend.title = element_text(size=20), legend.text = element_text(size=16))

GraphNumberShopsNotTarget



# ADS ONLY PRESENT MARC OR LISA -------------------------------------------



# Advertisers only present in Marc or Lisa 
advertisersMarc <- unique(AdsOfMarcExperiment$Advertiser)
advertisersLisa <- unique(AdsOfLisaExperiment$Advertiser)
advertisersonlyMarc <- c()
advertisersonlyLisa <- c()
for(i in 1:length(advertisersMarc)){
  
  if((length(which(advertisersLisa %in% advertisersMarc[i])) == 0) ){
    advertisersonlyMarc <- c(advertisersonlyMarc,advertisersMarc[i])
    
  }


}


for(i in 1:length(advertisersLisa)){
  
  if((length(which(advertisersMarc %in% advertisersLisa[i])) == 0) ){
    advertisersonlyLisa <- c(advertisersonlyLisa,advertisersLisa[i])
    
  }
  
  
}

advertisersonlyMarcBackup <- c()
# only are valid if this advertisement has been shown more than two times and it is not gender filetered.
for(i in 1:length(advertisersonlyMarc)){
  
  explanation <- AdsOfMarcExperiment$Explanation[which(AdsOfMarcExperiment$Advertiser == advertisersonlyMarc[i])]
  if(!grepl("men",explanation) && length(which(AdsOfMarcExperiment$Advertiser == advertisersonlyMarc[i])) > 3){
    advertisersonlyMarcBackup <- c(advertisersonlyMarcBackup, advertisersonlyMarc[i])
    
  }
}
advertisersonlyMarc <- advertisersonlyMarcBackup
advertisersonlyMarc

advertisersonlyLisaBackup <- c()

# only are valid if this advertisement has been shown more than two times and it is not gender filetered.
for(i in 1:length(advertisersonlyLisa)){
  
  explanation <- AdsOfLisaExperiment$Explanation[which(AdsOfLisaExperiment$Advertiser == advertisersonlyLisa[i])]
  if(!grepl("women",explanation) && length(which(AdsOfLisaExperiment$Advertiser == advertisersonlyLisa[i])) > 3){
    advertisersonlyLisaBackup <- c(advertisersonlyLisaBackup, advertisersonlyLisa[i])
    
  }
}
advertisersonlyLisa <- advertisersonlyLisaBackup
advertisersonlyLisa


# ANALYZING ADVERTISERS ONLY SHOWN TO MARC:
for(i in 1:length(advertisersonlyMarc)){
  print(paste("Advertiser: ",advertisersonlyMarc[i]))
  print("Explanation provided to the user:")
  print(unique(AdsOfMarcExperiment$Explanation[which(AdsOfMarcExperiment$Advertiser == advertisersonlyMarc[i])]))
  cat("\n")
  #print images shown
  myurl <- unique(AdsOfMarcExperiment$AdvertImg[which(AdsOfMarcExperiment$Advertiser == advertisersonlyMarc[i])])
  for(i in 1:length(myurl)){
    if(!is.na(myurl[i]) && myurl[i] !=""){
      browseURL(myurl[i])
      
    }
  }
  
}

# ANALYZING ADVERTISERS ONLY SHOWN TO LISA:
for(i in 1:length(advertisersonlyLisa)){
  print(paste("Advertiser: ",advertisersonlyLisa[i]))
  print("Explanation provided to the user:")
  print(unique(AdsOfLisaExperiment$Explanation[which(AdsOfLisaExperiment$Advertiser == advertisersonlyLisa[i])]))
  cat("\n")
  #print images shown
  myurl <- unique(AdsOfLisaExperiment$AdvertImg[which(AdsOfLisaExperiment$Advertiser == advertisersonlyLisa[i])])
  for(i in 1:length(myurl)){
    if(!is.na(myurl[i]) && myurl[i] !=""){
      browseURL(myurl[i])
      
    }
  }
  
}




######------------ISABELLE AND IGNATIUS -----------------######

# Baby and Fitness advertisements presented to Isabelle

AdsExperiment1Test2<- adsBetweenDates("2019-04-22", "2019-05-26")
PagesExperiment1Test2 <- pagesBetweenDates("2019-04-22", "2019-05-26")
PagesExperiment1Test2 <- PagesExperiment1Test2[which(PagesExperiment1Test2$Id == "Ignatius" | PagesExperiment1Test2$Id == "Isabelle Pontichelli"),]
print(paste("Total number of pages visited in Test 2 of Experiment 1: ",nrow(PagesExperiment1Test2)))
AdsExperiment1Test2<- AdsExperiment1Test2[which(AdsExperiment1Test2$UserId == "Ignatius Smith" | AdsExperiment1Test2$UserId == "Isabelle Ponticelli"),]
print(paste("Total number of advertisements received in Test 2 of Experiment 1: ",nrow(AdsExperiment1Test2)))

print(paste("Total number of pages visited in Test 2 of Experiment 1: ",nrow(PagesExperiment1Test2)))
PagesOfIgnatiusExperiment = PagesExperiment1Test2[which(PagesExperiment1Test2$Id == "Ignatius"),]
print(paste("Total number of pages visited by the MALE in Test 2 of Experiment 1: ",nrow(PagesOfIgnatiusExperiment)))
AdsOfIgnatiusExperiment = AdsExperiment1Test2[which(AdsExperiment1Test2$UserId == "Ignatius Smith"),]
NumberAdsOfIgnatiusExperiment = nrow(AdsOfIgnatiusExperiment)
print(paste("Total number of advertisements displayed to the MALE in Test 2 of Experiment 1: ",NumberAdsOfIgnatiusExperiment))

PagesOfIsabelleExperiment = PagesExperiment1Test2[which(PagesExperiment1Test2$Id == "Isabelle Pontichelli"),]
print(paste("Total number of pages visited by the FEMALE in Test 2 of Experiment 1: ",nrow(PagesOfIsabelleExperiment)))
AdsOfIsabelleExperiment = AdsExperiment1Test2[which(AdsExperiment1Test2$UserId == "Isabelle Ponticelli"),]
NumberAdsOfIsabelleExperiment = nrow(AdsOfIsabelleExperiment)
print(paste("Total number of advertisements displayed to the FEMALE in Test 2 of Experiment 1: ",NumberAdsOfIsabelleExperiment))


AdsOfBabiesExperimentTotal <- grepl("baby|kid|babies|child|newborn|mother|mum|father|dad", paste(AdsExperiment1Test2$AdvertiserTags,AdsExperiment1Test2$Advertiser,AdsExperiment1Test2$AdText,AdsExperiment1Test2$AdvertiserKeywords,AdsExperiment1Test2$AdvertiserDescription),ignore.case = TRUE)


AdsOfBabiesIsabelleTotal <- grepl("baby|kid|babies|child|newborn|mother|mum|father|dad", paste(AdsOfIsabelleExperiment$AdvertiserTags,AdsOfIsabelleExperiment$Advertiser,AdsOfIsabelleExperiment$AdText,AdsOfIsabelleExperiment$AdvertiserKeywords,AdsOfIsabelleExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfIsabelleExperiment$isBaby <- AdsOfBabiesIsabelleTotal
AdsOfIsabelleExperiment$isBaby <- as.factor(AdsOfIsabelleExperiment$isBaby)
AdsExperiment1Test2$isBaby <- AdsOfBabiesExperimentTotal
AdsExperiment1Test2$isBaby <- as.factor(AdsExperiment1Test2$isBaby)

print(paste("Total number of Babies advertisements of Experiment1: ", length(which(AdsExperiment1Test2$isBaby == TRUE))))

print(paste("Total number of Baby displayed to the Female in test 1 of Experiment1: ", length(which(AdsOfBabiesIsabelleTotal == TRUE))))


percentage(AdsOfIsabelleExperiment,which(AdsOfBabiesIsabelleTotal == TRUE))


AdsIsabelleBabiesPlot <- AdsOfIsabelleExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isBaby))) + geom_bar(position = "fill", color = "grey40")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Babies related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Babies advertisements received by Isabelle")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5))
AdsIsabelleBabiesPlot



AdsOfFitnessExperimentTotal <- grepl(" fit |fitness|protein|vitamin|muscle", paste(AdsExperiment1Test2$AdvertiserTags,AdsExperiment1Test2$Advertiser,AdsExperiment1Test2$AdText,AdsExperiment1Test2$AdvertiserKeywords,AdsExperiment1Test2$AdvertiserDescription),ignore.case = TRUE)


AdsOfFitnessIsabelleTotal <- grepl(" fit |fitness|protein|vitamin|muscle", paste(AdsOfIsabelleExperiment$AdvertiserTags,AdsOfIsabelleExperiment$Advertiser,AdsOfIsabelleExperiment$AdText,AdsOfIsabelleExperiment$AdvertiserKeywords,AdsOfIsabelleExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfIsabelleExperiment$isFitness <- AdsOfFitnessIsabelleTotal
AdsOfIsabelleExperiment$isFitness <- as.factor(AdsOfIsabelleExperiment$isFitness)
AdsExperiment1Test2$isFitness <- AdsOfFitnessExperimentTotal
AdsExperiment1Test2$isFitness <- as.factor(AdsExperiment1Test2$isFitness)

print(paste("Total number of Fitness advertisements of Experiment1: ", length(which(AdsExperiment1Test2$isFitness == TRUE))))

print(paste("Total number of Fitness displayed to the Female in test 1 of Experiment1: ", length(which(AdsOfFitnessIsabelleTotal == TRUE))))

AdsIsabelleFitnessPlot <- AdsOfIsabelleExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isFitness))) + geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Fitness related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Fitness advertisements received by Isabelle")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5))
AdsIsabelleFitnessPlot

#### Baby and Fitness advertisements presented to Ignatius


AdsOfBabiesIgnatiusTotal <- grepl("baby|kid|babies|child|newborn|mother|mum|father|dad", paste(AdsOfIgnatiusExperiment$AdvertiserTags,AdsOfIgnatiusExperiment$Advertiser,AdsOfIgnatiusExperiment$AdText,AdsOfIgnatiusExperiment$AdvertiserKeywords,AdsOfIgnatiusExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfIgnatiusExperiment$isBaby <- AdsOfBabiesIgnatiusTotal
AdsOfIgnatiusExperiment$isBaby <- as.factor(AdsOfIgnatiusExperiment$isBaby)
AdsExperiment1Test2$isBaby <- AdsOfBabiesExperimentTotal
AdsExperiment1Test2$isBaby <- as.factor(AdsExperiment1Test2$isBaby)

print(paste("Total number of Babies advertisements of Experiment1: ", length(which(AdsExperiment1Test2$isBaby == TRUE))))

print(paste("Total number of Baby displayed to the Female in test 1 of Experiment1: ", length(which(AdsOfBabiesIgnatiusTotal == TRUE))))



percentage(AdsOfIgnatiusExperiment,which(AdsOfBabiesIgnatiusTotal == TRUE))

AdsIgnatiusBabiesPlot <- AdsOfIgnatiusExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isBaby))) + geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Babies related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Babies advertisements received by Ignatius")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5))
AdsIgnatiusBabiesPlot





AdsOfFitnessIgnatiusTotal <- grepl(" fit |fitness|protein|vitamin|muscle", paste(AdsOfIgnatiusExperiment$AdvertiserTags,AdsOfIgnatiusExperiment$Advertiser,AdsOfIgnatiusExperiment$AdText,AdsOfIgnatiusExperiment$AdvertiserKeywords,AdsOfIgnatiusExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfIgnatiusExperiment$isFitness <- AdsOfFitnessIgnatiusTotal
AdsOfIgnatiusExperiment$isFitness <- as.factor(AdsOfIgnatiusExperiment$isFitness)
AdsExperiment1Test2$isFitness <- AdsOfFitnessExperimentTotal
AdsExperiment1Test2$isFitness <- as.factor(AdsExperiment1Test2$isFitness)

print(paste("Total number of Fitness advertisements of Experiment1: ", length(which(AdsExperiment1Test2$isFitness == TRUE))))

print(paste("Total number of Fitness displayed to the Male in test 1 of Experiment1: ", length(which(AdsOfFitnessIgnatiusTotal == TRUE))))

AdsIgnatiusFitnessPlot <- AdsOfIgnatiusExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isBaby))) + geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Fitness related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Fitness advertisements received by Ignatius")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5))
AdsIgnatiusFitnessPlot


######

# TOTAL NUMBER OF BABIES AND FITNESS  AND THE GRAPH OF EACH USER.

AdsOfFitnessExperiment <- AdsExperiment1Test2[which(AdsExperiment1Test2$isFitness == TRUE),]
GraphNumberFitness <-ggplot(AdsOfFitnessExperiment, aes( x="Fitness-related advertisements", fill=factor(UserId))) + geom_bar(width  = 0.5, color="grey40")+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Total number of ads related to Fitness")+
  scale_y_continuous(breaks = seq(0, 30, by = 5))+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5))

GraphNumberFitness

AdsOfBabiesExperiment <- AdsExperiment1Test2[which(AdsExperiment1Test2$isBaby == TRUE),]

GraphNumberBabies <-ggplot(AdsOfBabiesExperiment, aes( x="Babies-related advertisements", fill=factor(UserId))) + geom_bar(width  = 0.4 ,color = "grey40")+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Total number of ads related to babies")+
  coord_cartesian(ylim=c(0,100))+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5))

GraphNumberBabies

#In percentage:
GraphPercentageRelatedBabies <- AdsOfBabiesExperiment %>%
  ggplot(aes(x="Baby-related ads", fill=factor(UserId))) + geom_bar(position = "fill",width = 0.4, color = "grey40")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("User", values = c("Isabelle Ponticelli" = "#29487D", "Ignatius Smith" ="#D4D8E8"))+
  ggtitle("Distribution of ads related to babies")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=16),legend.title = element_text(size=20), legend.text = element_text(size=16))
GraphPercentageRelatedBabies


#### Ads specifically of babies

AdsSpecificallyBabies <- grepl("baby", paste(AdsExperiment1Test2$AdvertiserTags,AdsExperiment1Test2$Advertiser,AdsExperiment1Test2$AdvertiserKeywords),ignore.case = TRUE)
AdsExperiment1Test2$isSpecificBaby <- AdsSpecificallyBabies
print(paste("Total number of ads specifically babies:", length(which(AdsExperiment1Test2$isSpecificBaby == TRUE)) ))
print(paste("Total number of ads specifically babies Female:", length(which(AdsExperiment1Test2$isSpecificBaby == TRUE & AdsExperiment1Test2$UserId == "Isabelle Ponticelli")) ))
print(paste("Total number of ads specifically babies Male:", length(which(AdsExperiment1Test2$isSpecificBaby == TRUE & AdsExperiment1Test2$UserId == "Ignatius Smith")) ))


AdsSpecificBabiesExperiment <- AdsExperiment1Test2[which(AdsExperiment1Test2$isSpecificBaby == TRUE),]

GraphNumberSpecificBabies <-ggplot(AdsSpecificBabiesExperiment , aes( x="Ads specific of babies", fill=factor(UserId))) + geom_bar(width  = 0.4, color = "grey40")+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Total number of ads specific of babies")+
  coord_cartesian(ylim=c(0,60))+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5))

GraphNumberSpecificBabies

GraphPercentageSpecificBabies <- AdsSpecificBabiesExperiment %>%
  ggplot(aes(x="Baby-specific ads", fill=factor(UserId))) + geom_bar(position = "fill",width = 0.4, color = "grey40")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("User", values = c("Isabelle Ponticelli" = "#29487D", "Ignatius Smith" ="#D4D8E8"))+
  ggtitle("Distribution of ads specific of babies")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=16),legend.title = element_text(size=20), legend.text = element_text(size=16))
GraphPercentageSpecificBabies

SpecificBabyAdvertisers <- unique(AdsSpecificBabiesExperiment$Advertiser)

GraphPercentageSpecificBabiesAdvertisers<- ggplot(AdsSpecificBabiesExperiment, aes(x=factor(Advertiser), fill=factor(UserId))) + geom_bar(position = "fill")+
  scale_fill_manual("User", values = c("Isabelle Ponticelli" = "#29487D", "Ignatius Smith" ="#D4D8E8"))+
  ggtitle("Distribution of ads specific of babies")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5))
GraphPercentageSpecificBabiesAdvertisers



AdvertisersSpecificBabiesIgnatius <- unique(AdsSpecificBabiesExperiment[which(AdsSpecificBabiesExperiment$UserId == "Ignatius Smith"), "Advertiser"])
AdvertisersSpecificBabiesIsabelle <- unique(AdsSpecificBabiesExperiment[which(AdsSpecificBabiesExperiment$UserId == "Isabelle Ponticelli"), "Advertiser"])

IgnatiusBabiesdataframe <- data.frame("Advertiser"=AdvertisersSpecificBabiesIgnatius,"UserId"= "Ignatius Smith","Count"=0)
IsabelleBabiesdataframe <- data.frame("Advertiser"=AdvertisersSpecificBabiesIsabelle,"UserId"= "Isabelle Ponticelli","Count"=0)
TotalbabiesSpecificdataframe <- rbind(IgnatiusBabiesdataframe,IsabelleBabiesdataframe)


for(i in 1:length(AdvertisersSpecificBabiesIgnatius)){
  TotalbabiesSpecificdataframe[which(TotalbabiesSpecificdataframe$UserId == "Ignatius Smith" & TotalbabiesSpecificdataframe$Advertiser == AdvertisersSpecificBabiesIgnatius[i]), "Count"] = nrow(AdsOfBabiesExperiment[which(AdsOfBabiesExperiment$UserId == "Ignatius Smith" & AdsOfBabiesExperiment$Advertiser == AdvertisersSpecificBabiesIgnatius[i]),])
}
for(i in 1:length(AdvertisersSpecificBabiesIsabelle)){
  TotalbabiesSpecificdataframe[which(TotalbabiesSpecificdataframe$UserId == "Isabelle Ponticelli" & TotalbabiesSpecificdataframe$Advertiser == AdvertisersSpecificBabiesIsabelle[i]), "Count"] = nrow(AdsOfBabiesExperiment[which(AdsOfBabiesExperiment$UserId == "Isabelle Ponticelli" & AdsOfBabiesExperiment$Advertiser == AdvertisersSpecificBabiesIsabelle[i]),])
}


ggplot(TotalbabiesSpecificdataframe, aes(x = reorder(Advertiser, Count), Count, fill=factor(UserId)))+
  geom_bar(stat = "identity", position = "dodge", color = "grey40", width = 0.5) +
  scale_fill_manual("User", values = c("Isabelle Ponticelli" = "#29487D", "Ignatius Smith" ="#D4D8E8"))+
  labs( x="" ,y = "Count", fill = "User")+
  scale_y_continuous(breaks=seq(0, 12, 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 12),axis.title = element_text(size=16), plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of ads of each advertiser")

#Analysis of the images of the ads not shown to the male:


advertisersonlyIgnatius <- c()
advertisersonlyIsabelle <- c()
for(i in 1:length(AdvertisersSpecificBabiesIgnatius)){
  
  if((length(which(AdvertisersSpecificBabiesIsabelle %in% AdvertisersSpecificBabiesIgnatius[i])) == 0) ){
    advertisersonlyIgnatius <- c(advertisersonlyIgnatius,AdvertisersSpecificBabiesIgnatius[i])
  }
}

for(i in 1:length(AdvertisersSpecificBabiesIsabelle)){
  
  if((length(which(AdvertisersSpecificBabiesIgnatius %in% AdvertisersSpecificBabiesIsabelle[i])) == 0) ){
    advertisersonlyIsabelle <- c(advertisersonlyIsabelle,AdvertisersSpecificBabiesIsabelle[i])
  }
}

urlOnlyIsabelle <- c()
for(i in 1: length(advertisersonlyIsabelle)){
  urlOnlyIsabelle <- c(urlOnlyIsabelle,unique(AdsExperiment1Test2$AdvertImg[which(AdsExperiment1Test2$Advertiser == advertisersonlyIsabelle[i])]))
}
  
for(i in 1:length(urlOnlyIsabelle)){
  if(!is.na(urlOnlyIsabelle[i]) && urlOnlyIsabelle[i] !=""){
    browseURL(urlOnlyIsabelle[i])
    
  }
}

urlIgnatius <- c()
for(i in 1: length(AdvertisersSpecificBabiesIgnatius)){
  urlIgnatius <- c(urlIgnatius,unique(AdsExperiment1Test2$AdvertImg[which(AdsExperiment1Test2$Advertiser == AdvertisersSpecificBabiesIgnatius[i])]))
}

for(i in 1:length(urlIgnatius)){
  if(!is.na(urlIgnatius[i]) && urlIgnatius[i] !=""){
    browseURL(urlIgnatius[i])
    
  }
}

#--------Extra analysis Images fitness

advertisersFitnessIgnatius <- unique(AdsOfIgnatiusExperiment[which(AdsOfIgnatiusExperiment$isFitness == TRUE), "Advertiser"])
urlFitnessIgnatius <- c()
for(i in 1: length(advertisersFitnessIgnatius)){
  urlFitnessIgnatius <- c(urlFitnessIgnatius,unique(AdsExperiment1Test2$AdvertImg[which(AdsExperiment1Test2$Advertiser == advertisersFitnessIgnatius[i] & AdsExperiment1Test2$UserId == "Ignatius Smith")]))
}

for(i in 1:length(urlFitnessIgnatius)){
  if(!is.na(urlFitnessIgnatius[i]) && urlFitnessIgnatius[i] !=""){
    browseURL(urlFitnessIgnatius[i])
    
  }
}

advertisersFitnessIsabelle <- unique(AdsOfIsabelleExperiment[which(AdsOfIsabelleExperiment$isFitness == TRUE), "Advertiser"])
urlFitnessIsabelle <- c()
for(i in 1: length(advertisersFitnessIsabelle)){
  urlFitnessIsabelle <- c(urlFitnessIsabelle,unique(AdsExperiment1Test2$AdvertImg[which(AdsExperiment1Test2$Advertiser == advertisersFitnessIsabelle[i] & AdsExperiment1Test2$UserId == "Isabelle Ponticelli")]))
}

for(i in 1:length(urlFitnessIsabelle)){
  if(!is.na(urlFitnessIsabelle[i]) && urlFitnessIsabelle[i] !=""){
    browseURL(urlFitnessIsabelle[i])
    
  }
}



######------------THOMAS AND ASTRID -----------------######

#---------THOMAS----------
#### Health and Betting advertisements presented to Thomas


AdsExperiment1Test3<- adsBetweenDates("2019-05-05", "2019-05-25")
PagesExperiment1Test3 <- pagesBetweenDates("2019-05-05", "2019-05-25")
PagesExperiment1Test3 <- PagesExperiment1Test3[which(PagesExperiment1Test3$Id == "Thomas Mertens" | PagesExperiment1Test3$Id == "Astrid neels"),]
print(paste("Total number of pages visited in Test 3 of Experiment 1: ",nrow(PagesExperiment1Test3)))
AdsExperiment1Test3<- AdsExperiment1Test3[which(AdsExperiment1Test3$UserId == "Thomas Mertens" | AdsExperiment1Test3$UserId == "Astrid Neels"),]
print(paste("Total number of advertisements received in Test 3 of Experiment 1: ",nrow(AdsExperiment1Test3)))

PagesOfThomasExperiment = PagesExperiment1Test3[which(PagesExperiment1Test3$Id == "Thomas Mertens"),]
print(paste("Total number of pages visited by the MALE in Test 3 of Experiment 1: ",nrow(PagesOfThomasExperiment)))
AdsOfThomasExperiment = AdsExperiment1Test3[which(AdsExperiment1Test3$UserId == "Thomas Mertens"),]
NumberAdsOfThomasExperiment = nrow(AdsOfThomasExperiment)
print(paste("Total number of advertisements displayed to the MALE in Test 3 of Experiment 1: ",NumberAdsOfThomasExperiment))

PagesOfAstridExperiment = PagesExperiment1Test3[which(PagesExperiment1Test3$Id == "Astrid neels"),]
print(paste("Total number of pages visited by the FEMALE in Test 3 of Experiment 1: ",nrow(PagesOfAstridExperiment)))
AdsOfAstridExperiment = AdsExperiment1Test3[which(AdsExperiment1Test3$UserId == "Astrid Neels"),]
NumberAdsOfAstridExperiment = nrow(AdsOfAstridExperiment)
print(paste("Total number of advertisements displayed to the FEMALE in Test 2 of Experiment 1: ",NumberAdsOfAstridExperiment))

AdsOfHealthExperimentTotal <- grepl("Health|vitamin|pain|relief| cure|ache |pharmacy", paste(AdsExperiment1Test3$AdvertiserTags,AdsExperiment1Test3$Advertiser,AdsExperiment1Test3$AdText,AdsExperiment1Test3$AdvertiserKeywords,AdsExperiment1Test3$AdvertiserDescription),ignore.case = TRUE)



AdsOfHealthThomasTotal <- grepl("Health|vitamin|pain|relief| cure|ache |pharmacy", paste(AdsOfThomasExperiment$AdvertiserTags,AdsOfThomasExperiment$Advertiser,AdsOfThomasExperiment$AdText,AdsOfThomasExperiment$AdvertiserKeywords,AdsOfThomasExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfThomasExperiment$isHealth <- AdsOfHealthThomasTotal
AdsOfThomasExperiment$isHealth <- as.factor(AdsOfThomasExperiment$isHealth)
AdsExperiment1Test3$isHealth <- AdsOfHealthExperimentTotal
AdsExperiment1Test3$isHealth <- as.factor(AdsExperiment1Test3$isHealth)

print(paste("Total number of Health advertisements of Experiment1: ", length(which(AdsExperiment1Test3$isHealth == TRUE))))

print(paste("Total number of Health displayed to the Male in test 3 of Experiment1: ", length(which(AdsOfHealthThomasTotal == TRUE))))



percentage(AdsOfThomasExperiment,which(AdsOfHealthThomasTotal == TRUE))

AdsThomasHealthPlot <- AdsOfThomasExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isHealth))) + geom_bar(position = "fill", color = "grey40")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Health related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Health advertisements received by Thomas")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5))
AdsThomasHealthPlot





AdsOfBettingsExperimentTotal <- grepl("betting|luck| bet |casino| bid ", paste(AdsExperiment1Test3$AdvertiserTags,AdsExperiment1Test3$Advertiser,AdsExperiment1Test3$AdText,AdsExperiment1Test3$AdvertiserKeywords,AdsExperiment1Test3$AdvertiserDescription),ignore.case = TRUE)



AdsOfBettingsThomasTotal <- grepl("betting|luck| bet |casino| bid ", paste(AdsOfThomasExperiment$AdvertiserTags,AdsOfThomasExperiment$Advertiser,AdsOfThomasExperiment$AdText,AdsOfThomasExperiment$AdvertiserKeywords,AdsOfThomasExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfThomasExperiment$isBettings <- AdsOfBettingsThomasTotal
AdsOfThomasExperiment$isBettings <- as.factor(AdsOfThomasExperiment$isBettings)
AdsExperiment1Test3$isBettings <- AdsOfBettingsExperimentTotal
AdsExperiment1Test3$isBettings <- as.factor(AdsExperiment1Test3$isBettings)

print(paste("Total number of Bettings advertisements of Experiment1: ", length(which(AdsExperiment1Test3$isBettings == TRUE))))

print(paste("Total number of Bettings displayed to the Male in test 3 of Experiment1: ", length(which(AdsOfBettingsThomasTotal == TRUE))))

percentage(AdsOfThomasExperiment,which(AdsOfBettingsThomasTotal == TRUE))

AdsThomasBettingsPlot <- AdsOfThomasExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isBettings))) + geom_bar(position = "fill", color = "grey40")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Bettings related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Bettings advertisements received by Thomas")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5))
AdsThomasBettingsPlot


#### Health and Betting advertisements presented to Astrid

#-------ASTRID---------


AdsOfHealthAstridTotal <- grepl("Health|vitamin|pain|relief| cure|ache |pharmacy", paste(AdsOfAstridExperiment$AdvertiserTags,AdsOfAstridExperiment$Advertiser,AdsOfAstridExperiment$AdText,AdsOfAstridExperiment$AdvertiserKeywords,AdsOfAstridExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfAstridExperiment$isHealth <- AdsOfHealthAstridTotal
AdsOfAstridExperiment$isHealth <- as.factor(AdsOfAstridExperiment$isHealth)
AdsExperiment1Test3$isHealth <- AdsOfHealthExperimentTotal
AdsExperiment1Test3$isHealth <- as.factor(AdsExperiment1Test3$isHealth)

print(paste("Total number of Health advertisements of Experiment1: ", length(which(AdsExperiment1Test3$isHealth == TRUE))))

print(paste("Total number of Health displayed to the Female in test 3 of Experiment1: ", length(which(AdsOfHealthAstridTotal == TRUE))))



percentage(AdsOfAstridExperiment,which(AdsOfHealthAstridTotal == TRUE))

AdsAstridHealthPlot <- AdsOfAstridExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isHealth))) + geom_bar(position = "fill", color = "grey40")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Health related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Health advertisements received by Astrid")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5))
AdsAstridHealthPlot



AdsOfBettingsAstridTotal <- grepl("betting|luck| bet |casino| bid ", paste(AdsOfAstridExperiment$AdvertiserTags,AdsOfAstridExperiment$Advertiser,AdsOfAstridExperiment$AdText,AdsOfAstridExperiment$AdvertiserKeywords,AdsOfAstridExperiment$AdvertiserDescription),ignore.case = TRUE)
AdsOfAstridExperiment$isBettings <- AdsOfBettingsAstridTotal
AdsOfAstridExperiment$isBettings <- as.factor(AdsOfAstridExperiment$isBettings)
AdsExperiment1Test3$isBettings <- AdsOfBettingsExperimentTotal
AdsExperiment1Test3$isBettings <- as.factor(AdsExperiment1Test3$isBettings)

print(paste("Total number of Bettings advertisements of Experiment1: ", length(which(AdsExperiment1Test3$isBettings == TRUE))))

print(paste("Total number of Bettings displayed to the Male in test 3 of Experiment1: ", length(which(AdsOfBettingsAstridTotal == TRUE))))

percentage(AdsOfAstridExperiment,which(AdsOfBettingsAstridTotal == TRUE))

AdsAstridBettingsPlot <- AdsOfAstridExperiment %>%
  ggplot(aes(x=factor(UserId), fill=factor(isBettings))) + geom_bar(position = "fill", color = "grey40")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Bettings related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Bettings advertisements received by Astrid")+
  labs( x="",y = "Percentage", fill = "Baby Ad")+
  theme(plot.title = element_text(hjust = 0.5))
AdsAstridBettingsPlot


#------EXTRA ANALYSIS

##-NO TARGETING-##
#Number of advertisers related to betting and health received by each user NOT TARGETTING GENDER.

AdsExperiment1Test3$TargetedAd <- grepl(" men | women ", AdsExperiment1Test3$Explanation)
AdsExperiment1Test3$isBettingNotarget <- AdsExperiment1Test3$isBetting == TRUE & AdsExperiment1Test3$TargetedAd == FALSE
BettingBothPlotNotTarget <- AdsExperiment1Test3 %>%
  ggplot(aes(x=factor(UserId), fill=factor(isBettingNotarget))) + geom_bar(position = "fill", color="grey40")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Betting related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Betting related advertisements without gender target")+
  labs(x = "User Name", y = "Percentage", fill = "Betting Ad")+
  theme(plot.title = element_text(hjust = 0.5))
BettingBothPlotNotTarget

AdsExperiment1Test3$isHealthNotarget <- AdsExperiment1Test3$isHealth == TRUE & AdsExperiment1Test3$TargetedAd == FALSE
AdsExperiment1Test3$isBettingNotarget <- AdsExperiment1Test3$isBettings == TRUE & AdsExperiment1Test3$TargetedAd == FALSE


HealthBothPlotNotTarget <- AdsExperiment1Test3 %>%
  ggplot(aes(x=factor(UserId), fill=factor(isHealthNotarget))) + geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Healthping related Ad", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Health related advertisements not target")+
  labs(x = "User Name", y = "Percentage", fill = "Healthping Ad")+
  theme(plot.title = element_text(hjust = 0.5))
HealthBothPlotNotTarget

NumberAdsBettingThomasNoTarget <- nrow(AdsExperiment1Test3[which(AdsExperiment1Test3$isBettingNotarget == TRUE & AdsExperiment1Test3$UserId == "Thomas Mertens"),])
NumberAdsBettingAstridNoTarget <- nrow(AdsExperiment1Test3[which(AdsExperiment1Test3$isBettingNotarget == TRUE & AdsExperiment1Test3$UserId == "Astrid Neels"),])

NumberAdsHealthThomasNoTarget <- nrow(AdsExperiment1Test3[which(AdsExperiment1Test3$isHealthNotarget == TRUE & AdsExperiment1Test3$UserId == "Thomas Mertens"),])
NumberAdsHealthAstridNoTarget <- nrow(AdsExperiment1Test3[which(AdsExperiment1Test3$isHealthNotarget == TRUE & AdsExperiment1Test3$UserId == "Astrid Neels"),])

print(paste("Total number of betting ads no targeting", nrow(AdsExperiment1Test3[which(AdsExperiment1Test3$isBettingNotarget == TRUE),])))
print(paste("Number ads betting Thomas no target: ", NumberAdsBettingThomasNoTarget))
print(paste("Number ads betting Astrid no target: ", NumberAdsBettingAstridNoTarget))


print(paste("Total number of Health ads no targeting", nrow(AdsExperiment1Test3[which(AdsExperiment1Test3$isHealthNotarget == TRUE),])))

print(paste("Number ads Health Thomas no target: ", NumberAdsHealthThomasNoTarget))
print(paste("Number ads Health Astrid no target: ", NumberAdsHealthAstridNoTarget))


AdsOfBettingsExperiment <- AdsExperiment1Test3[which(AdsExperiment1Test3$isBettings == TRUE),]

GraphNumberBettings <-ggplot(AdsOfBettingsExperiment, aes( x="Betting-related advertisements", fill=factor(UserId))) + geom_bar(width  = 0.4, color= "grey40")+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Total number of gambling-related ads.")+
  coord_cartesian(ylim=c(0,125))+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5))

GraphNumberBettings

GraphPercentageBettingsTarget <- AdsOfBettingsExperiment %>%
  ggplot(aes(x="", fill=factor(UserId))) + geom_bar(position = "fill", color="grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("User", values = c("Thomas Mertens" = "#29487D", "Astrid Neels" ="#D4D8E8"))+
  ggtitle("Gambling-related ads display to each user")+
  labs(x = "Gambling ads", y = "Percentage", fill = "Gambling Ads")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=16))
GraphPercentageBettingsTarget

AdsOfBettingsNoTargetExperiment <- AdsExperiment1Test3[which(AdsExperiment1Test3$isBettingNotarget == TRUE),]

GraphNumberBettingsNoTarget <-ggplot(AdsOfBettingsNoTargetExperiment, aes( x="Betting-related advertisements", fill=factor(UserId))) + geom_bar(width  = 0.5, color= "grey40")+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Total number of ads related to BettingsNoTarget")+
  coord_cartesian(ylim=c(0,80))+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5))

GraphNumberBettingsNoTarget

GraphPercentageBettingsNOTarget <- AdsOfBettingsNoTargetExperiment %>%
  ggplot(aes(x="", fill=factor(UserId))) + geom_bar(position = "fill", color="grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("User", values = c("Thomas Mertens" = "#29487D", "Astrid Neels" ="#D4D8E8"))+
  ggtitle("Gambling-related ads display to each user no targeting")+
  labs(x = "Gambling ads", y = "Percentage", fill = "Betting Ad")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=16))
GraphPercentageBettingsNOTarget



AdsOfHealthsExperiment <- AdsExperiment1Test3[which(AdsExperiment1Test3$isHealth == TRUE),]

GraphNumberHealths <-ggplot(AdsOfHealthsExperiment, aes( x="Healthping-related advertisements", fill=factor(UserId))) + geom_bar(width  = 0.4, color = "grey40")+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Total number of ads related to Healthping")+
  coord_cartesian(ylim=c(0,120))+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5))

GraphNumberHealths

GraphPercentageHealthTarget <- AdsOfHealthsExperiment %>%
  ggplot(aes(x="", fill=factor(UserId))) + geom_bar(position = "fill", color="grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("User", values = c("Thomas Mertens" = "#29487D", "Astrid Neels" ="#D4D8E8"))+
  ggtitle("Health-related ads displayed to each user")+
  labs(x = "Health ads", y = "Percentage", fill = "Health Ad")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=18), legend.title = element_text(size=20), legend.text = element_text(size=14), axis.text = element_text(size = 14) )
GraphPercentageHealthTarget

AdsOfHealthsExperimentNotTarget <- AdsExperiment1Test3[which(AdsExperiment1Test3$isHealthNotarget == TRUE),]

GraphNumberHealthsNotTarget <-ggplot(AdsOfHealthsExperimentNotTarget, aes( x= "Healthping-related advertisements not target", fill=factor(UserId))) + geom_bar(width  = 0.4, color="grey40")+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Total number of ads related to Healthping")+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5))

GraphNumberHealthsNotTarget

GraphPercentageHealthNOTarget <- AdsOfHealthsExperimentNotTarget %>%
  ggplot(aes(x="", fill=factor(UserId))) + geom_bar(position = "fill", color="grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("User", values = c("Thomas Mertens" = "#29487D", "Astrid Neels" ="#D4D8E8"))+
  ggtitle("Health-related ads displayed to each user no targeting")+
  labs(x = "Health ads", y = "Percentage", fill = "Health Ad")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=18), legend.title = element_text(size=20), legend.text = element_text(size=14), axis.text = element_text(size = 14) )
GraphPercentageHealthNOTarget

#--------Extra analysis Images Health


urlHealthThom <- c(unique(AdsExperiment1Test3[which(AdsExperiment1Test3$Advertiser=="Tonbienetre" & AdsExperiment1Test3$UserId == "Thomas Mertens" ) ,"AdvertImg"]))


for(i in 1:length(urlHealthThom)){
  if(!is.na(urlHealthThom[i]) && urlHealthThom[i] !=""){
    browseURL(urlHealthThom[i])
    
  }
}

urlHealthAstrid <- c(unique(AdsExperiment1Test3[which(AdsExperiment1Test3$Advertiser=="Tonbienetre" & AdsExperiment1Test3$UserId == "Astrid Neels" ) ,"AdvertImg"]))


for(i in 1:length(urlHealthAstrid)){
  if(!is.na(urlHealthAstrid[i]) && urlHealthAstrid[i] !=""){
    browseURL(urlHealthAstrid[i])
    
  }
}

urlHealthThom <- c(unique(AdsExperiment1Test3[which(AdsExperiment1Test3$Advertiser=="SkinnyLove" & AdsExperiment1Test3$UserId == "Thomas Mertens" ) ,"AdvertImg"]))


for(i in 1:length(urlHealthThom)){
  if(!is.na(urlHealthThom[i]) && urlHealthThom[i] !=""){
    browseURL(urlHealthThom[i])
    
  }
}

urlHealthAstrid <- c(unique(AdsExperiment1Test3[which(AdsExperiment1Test3$Advertiser=="SkinnyLove" & AdsExperiment1Test3$UserId == "Astrid Neels" ) ,"AdvertImg"]))


for(i in 1:length(urlHealthAstrid)){
  if(!is.na(urlHealthAstrid[i]) && urlHealthAstrid[i] !=""){
    browseURL(urlHealthAstrid[i])
    
  }
}

#-Extra analyisis Gambling

GamblingDataFrame <- AdsExperiment1Test3[which(AdsExperiment1Test3$isBettings == TRUE),]

GamblingDataFrame$isCasino <- grepl("casino ", paste(GamblingDataFrame$AdvertiserTags,GamblingDataFrame$Advertiser,GamblingDataFrame$AdText,GamblingDataFrame$AdvertiserKeywords,GamblingDataFrame$AdvertiserDescription),ignore.case = TRUE)

GraphPercentageCasino <- GamblingDataFrame %>%
  ggplot(aes(x=UserId, fill=factor(isCasino))) + geom_bar(position = "fill", color="grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("User", values = c("TRUE" = "#29487D", "FALSE" ="#D4D8E8"))+
  ggtitle("Percentage of gambling ads related to Casino")+
  labs(x = "Casino-related ads", y = "Percentage", fill = "Health Ad")+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=12),legend.title = element_text(size=20), legend.text = element_text(size=16))
GraphPercentageCasino


###PLOT THREE EXPERIMENTS TOGETHER.

GroupExperiments <- AdsInfo
GroupExperiments$StereotypicalMen <- c("NULL")
GroupExperiments$StereotypicalWomen <- c("NULL")
GroupExperiments[grepl(" fit |fitness|protein|vitamin|muscle", paste(GroupExperiments$AdvertiserTags,GroupExperiments$Advertiser,GroupExperiments$AdText,GroupExperiments$AdvertiserKeywords,GroupExperiments$AdvertiserDescription),ignore.case = TRUE) & (GroupExperiments$UserId == "Ignatius Smith" | GroupExperiments$UserId == "Isabelle Ponticelli") & GroupExperiments$TimeStamp > "2019-04-22" & GroupExperiments$TimeStamp < "2019-05-22","StereotypicalMen"] = "FITNESS"
GroupExperiments[grepl("betting|luck| bet |casino| bid ", paste(GroupExperiments$AdvertiserTags,GroupExperiments$Advertiser,GroupExperiments$AdText,GroupExperiments$AdvertiserKeywords,GroupExperiments$AdvertiserDescription),ignore.case = TRUE) & (GroupExperiments$UserId == "Thomas Mertens" | GroupExperiments$UserId == "Astrid Neels")& GroupExperiments$TimeStamp > "2019-05-05" & GroupExperiments$TimeStamp < "2019-05-22","StereotypicalMen"] = "GAMBLING"
GroupExperiments[grepl(" car |vehicle|car | car,|automobile|gasoline|Cars", paste(GroupExperiments$AdvertiserTags,GroupExperiments$AdText,GroupExperiments$AdvertiserKeywords,GroupExperiments$AdvertiserDescription),ignore.case = TRUE) & (GroupExperiments$UserId == "Marc Roses" | GroupExperiments$UserId == "Lisa Thompson")& GroupExperiments$TimeStamp > "2019-02-18" & GroupExperiments$TimeStamp < "2019-04-24","StereotypicalMen"] = "CARS"

GroupExperiments[grepl("Health|vitamin|pain|relief| cure|ache |pharmacy", paste(GroupExperiments$AdvertiserTags,GroupExperiments$Advertiser,GroupExperiments$AdText,GroupExperiments$AdvertiserKeywords,GroupExperiments$AdvertiserDescription),ignore.case = TRUE) & (GroupExperiments$UserId == "Thomas Mertens" | GroupExperiments$UserId == "Astrid Neels")& GroupExperiments$TimeStamp > "2019-05-05" & GroupExperiments$TimeStamp < "2019-05-22","StereotypicalWomen"] = "HEALTH"
GroupExperiments[grepl("baby|kid|babies|child|newborn|mother|mum|father|dad", paste(GroupExperiments$AdvertiserTags,GroupExperiments$Advertiser,GroupExperiments$AdText,GroupExperiments$AdvertiserKeywords,GroupExperiments$AdvertiserDescription),ignore.case = TRUE) & (GroupExperiments$UserId == "Ignatius Smith" | GroupExperiments$UserId == "Isabelle Ponticelli")& GroupExperiments$TimeStamp > "2019-04-22" & GroupExperiments$TimeStamp < "2019-05-22","StereotypicalWomen"] = "BABIES"
GroupExperiments[grepl("shop|clothe|clothing|fashion",  paste(GroupExperiments$AdvertiserTags,GroupExperiments$AdText,GroupExperiments$AdvertiserKeywords,GroupExperiments$AdvertiserDescription),ignore.case = TRUE) & (GroupExperiments$UserId == "Marc Roses" | GroupExperiments$UserId == "Lisa Thompson")& GroupExperiments$TimeStamp > "2019-02-18" & GroupExperiments$TimeStamp < "2019-04-24","StereotypicalWomen"] = "SHOPS"

GroupExperimentsMen <- GroupExperiments[which(GroupExperiments$StereotypicalMen != "NULL"),]
GroupExperimentsWomen <- GroupExperiments[which(GroupExperiments$StereotypicalWomen != "NULL"),]


GraphExperimentsMen <- GroupExperimentsMen %>%
  ggplot(aes(x=StereotypicalMen, fill=factor(sex))) + geom_bar(position = "fill", color="grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("User", values = c("Male" = "#29487D", "Female" ="#D4D8E8"))+
  ggtitle("Topics stereotipically related to men")+
  labs(x = "Topic", y = "Percentage", fill = "Health Ad")+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=12),legend.title = element_text(size=20), legend.text = element_text(size=16))
GraphExperimentsMen

GraphExperimentsWomen <- GroupExperimentsWomen %>%
  ggplot(aes(x=StereotypicalWomen, fill=factor(sex))) + geom_bar(position = "fill", color="grey40", width = 0.4)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("User", values = c("Male" = "#29487D", "Female" ="#D4D8E8"))+
  ggtitle("Topics stereotipically related to Women")+
  labs(x = "Topic", y = "Percentage", fill = "Health Ad")+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5, size = 22), axis.title = element_text(size=20), axis.text = element_text(size=12),legend.title = element_text(size=20), legend.text = element_text(size=16))
GraphExperimentsWomen

######################### SECOND EXPERIMENT


# Robert Stinto -----------------------------------------------------------

# This User clicked an ad of Hostinger the 19/3/2019

firstPagesRobert <- pagesBetweenDates("2019-02-20","2019-03-19","RoberStinto" )

######################### THIRD EXPERIMENT###################

# Analysis of advertisements targetting only to men or women -----------------



# ANALYSIS OF THE ADVERTISEMENTS TARGETTING ONLY TO MEN OR WOMEN.
BoolAdsTargettingWomen <- grepl(" women ", AdsInfo$Explanation, ignore.case = TRUE)
table(BoolAdsTargettingWomen)
AdsTargettingWomen <- AdsInfo[BoolAdsTargettingWomen,]
advertisersTargettingWomen <- c(unique(AdsTargettingWomen$Advertiser))
sprintf("We have found %d advertisers that target only women",length(advertisersTargettingWomen))

BoolAdsTargettingMen <- grepl(" men ", AdsInfo$Explanation,ignore.case = TRUE)
table(BoolAdsTargettingMen)
AdsTargettingMen <- AdsInfo[BoolAdsTargettingMen,]
advertisersTargettingMen <- c(unique(AdsTargettingMen$Advertiser))
sprintf("We have found %d advertisers that target only men",length(advertisersTargettingMen))


# Analizing advertisers that TARGET both gender, To find this advertiser we found if the advertiser is present in
# both arrays.
DiscriminatoryAdvertisers <- c()
for (i in 1:length(advertisersTargettingMen)) {
  
  if(length(which(advertisersTargettingWomen == advertisersTargettingMen[i]))==1){
    DiscriminatoryAdvertisers <- c(DiscriminatoryAdvertisers,advertisersTargettingMen[i])
    
  }
  
}
DiscriminatoryAdvertisers


# Analysing advertisers received only by one gender DUE TO FACEBOOK AD DELIVERY SYSTEM.
AdsNoTargettingGender <- AdsInfo
BoolAdsTargettingWomen <- grepl(" women ", AdsNoTargettingGender$Explanation, ignore.case = TRUE)
AdsNoTargettingGender <- AdsNoTargettingGender[!BoolAdsTargettingWomen, ]

BoolAdsTargettingMen <- grepl(" men ", AdsNoTargettingGender$Explanation, ignore.case = TRUE)
AdsNoTargettingGender <- AdsNoTargettingGender[!BoolAdsTargettingMen, ]

uniqueAdvertisers<- unique(AdsNoTargettingGender$Advertiser)

FemaleAudience <- c()
MaleAudience <- c()


#We only take into account advertisers shown more than two times
for(i in 1:length(uniqueAdvertisers)){
  AdsOfAdvertiser <- nrow(AdsInfo[which(AdsInfo$Advertiser == uniqueAdvertisers[i]),])
  AudienceSex <- unique(AdsNoTargettingGender$sex[which(AdsNoTargettingGender$Advertiser == uniqueAdvertisers[i])])
  if((length(AudienceSex)==1) && (AdsOfAdvertiser > 4)){
    if(AudienceSex[1] == "Female"){
      FemaleAudience<- c(FemaleAudience,uniqueAdvertisers[i])
    }else if(AudienceSex[1] == "Male"){
      MaleAudience <- c(MaleAudience,uniqueAdvertisers[i])
    }
    
  }else if(length(AudienceSex) == 2){
    
    if(!exists("NoTargettingReceivedBothSexes")){
      NoTargettingReceivedBothSexes <- AdsInfo[which(AdsInfo$Advertiser == uniqueAdvertisers[i]),]
    }
    NoTargettingReceivedBothSexes <- rbind(NoTargettingReceivedBothSexes,AdsInfo[which(AdsInfo$Advertiser == uniqueAdvertisers[i]),] )
    
  }
  AudienceSex[""]
  
}
#Creation of the datasets of ads Received only by men or women decided by FACEBOOK

AdsReceivedOnlyByWomen<- AdsInfo[which(AdsInfo$Advertiser == FemaleAudience[1]),]

for(i in 2:length(FemaleAudience)){
  AdsReceivedOnlyByWomen <- rbind(AdsReceivedOnlyByWomen, AdsInfo[which(AdsInfo$Advertiser == FemaleAudience[i]),])
}
AdsReceivedOnlyByMen<- AdsInfo[which(AdsInfo$Advertiser == MaleAudience[1]),]

for(i in 2:length(MaleAudience)){
  AdsReceivedOnlyByMen <- rbind(AdsReceivedOnlyByMen, AdsInfo[which(AdsInfo$Advertiser == MaleAudience[i]),])
}


#Analysis of no targetting advertisements

#Check if the image received is different
NoTargetAdvertisers <- unique(NoTargettingReceivedBothSexes$Advertiser)

for(i in 1:length(NoTargetAdvertisers)){
  ImagesMen <- unique(AdsInfo$AdvertImg[which(AdsInfo$Advertiser == NoTargetAdvertisers[i] & AdsInfo$sex == "Male")])
  ImagesWomen <- unique(AdsInfo$AdvertImg[which(AdsInfo$Advertiser == NoTargetAdvertisers[i] & AdsInfo$sex == "Female")])
  for(i in 1:length(ImagesMen)){
    if(length(which(ImagesWomen == ImagesMen[i])) != 1){
      if(!exists("DiscriminationImages")){
        DiscriminationImages <- head(AdsInfo[which(AdsInfo$AdvertImg == ImagesMen[i]),],1)
      }else{
        DiscriminationImages <- rbind(DiscriminationImages,head(AdsInfo[which(AdsInfo$AdvertImg == ImagesMen[i]),],1))
      }
    }
  }
  
  for(i in 1:length(ImagesWomen)){
    if(length(which(ImagesMen == ImagesWomen[i])) != 1){
      if(!exists("DiscriminationImages")){
        DiscriminationImages <- head(AdsInfo[which(AdsInfo$AdvertImg == ImagesWomen[i]),],1)
      }else{
        DiscriminationImages <- rbind(DiscriminationImages,head(AdsInfo[which(AdsInfo$AdvertImg == ImagesWomen[i]),],1))
      }
    }
  }
}

DiscriminationImages <- DiscriminationImages[which(DiscriminationImages$AdvertImg != ""),]
BoolAdsTargettingWomen <- grepl(" women ", DiscriminationImages$Explanation, ignore.case = TRUE)
DiscriminationImages <- DiscriminationImages[!BoolAdsTargettingWomen, ]

BoolAdsTargettingMen <- grepl(" men ", DiscriminationImages$Explanation, ignore.case = TRUE)
DiscriminationImages <- DiscriminationImages[!BoolAdsTargettingMen, ]



# #Analizing discrimination of Wish advertiser ----------------------------

wishAds <- adsof("Wish")
toMen <- sexoriented("Wish"," men ")
sprintf("Advertisers to men: %d",nrow(toMen))
toWomen <- sexoriented("Wish"," women ")
sprintf("Advertisements to women: %d",nrow(toWomen))

for(i in 1:length(unique(toMen$AdvertImg))){
  if(!is.na(unique(toMen$AdvertImg)[i]) && unique(toMen$AdvertImg)[i] !=""){
    browseURL(unique(toMen$AdvertImg)[i])
    
  }
}
unique(toMen$Explanation)

for(i in 1:length(unique(toWomen$AdvertImg))){
  if(!is.na(unique(toWomen$AdvertImg)[i]) && unique(toWomen$AdvertImg)[i] !=""){
    browseURL(unique(toWomen$AdvertImg)[i])
    
  }
}
unique(toWomen$Explanation)




###### Analizing Winter is Coming advertiser -----------------------------------




adsOfGOT <- adsof("Winter is Coming")
unique(adsOfGOT$UserId)
adsOfGOTtoMen <- sexoriented("Winter is Coming", "men")
sprintf("Advertisers targeted to men: %d",nrow(adsOfGOTtoMen))
sprintf("Advertisers targeted to women: %d",nrow(sexoriented("Winter is Coming", "women")))
print("Explanation received in ads shown to men: ")
unique(adsOfGOTtoMen$AdText)
restofAdsGOT <- adsOfGOT[!grepl("men", adsOfGOT$Explanation), ]
print("Explanations show in the rest of ads: ")
unique(restofAdsGOT$AdText)

# Analysis of the images

TextToWomen <- unique(restofAdsGOT[which(restofAdsGOT$sex == "Female"),"AdText"]) 
TextToMen <- unique(restofAdsGOT[which(restofAdsGOT$sex == "Male"),"AdText"]) 



###### Analising advertisement of house work ---



jobAds <- AdsInfo[grepl("job|work part-time", paste(AdsInfo$AdvertiserTags,AdsInfo$AdvertiserKeywords,AdsInfo$AdvertiserDescription,AdsInfo$AdText),ignore.case = TRUE),]


print(paste("Number of work-related advertisements: ",nrow(jobAds)))

print(paste("Advertisements targeting Women:" ,nrow(jobAds[grepl(" women ", jobAds$Explanation),])))
print(paste("Advertisements targeting men:" , nrow(jobAds[grepl(" men ", jobAds$Explanation),])))

advertisersOfferingJObs <- unique(jobAds$Advertiser)

advertiserJobOnlyOneSex <- c()
for (advertiser in advertisersOfferingJObs) {
  if(length(unique(jobAds[which(jobAds$Advertiser == advertiser), "sex"])) == 1 & nrow(jobAds[which(jobAds$Advertiser== advertiser),])> 2){
    advertiserJobOnlyOneSex <- c(advertiserJobOnlyOneSex, advertiser)
    
  }
  
}


# Analysing advertisements related to VideoGames --------------------------

videogameAds <- AdsInfo[grepl("video game", paste(AdsInfo$AdvertiserTags,AdsInfo$AdvertiserKeywords,AdsInfo$AdvertiserDescription,AdsInfo$AdText),ignore.case = TRUE),]
videogameAds <- videogameAds[which(videogameAds$UserId == "Ignatius Smith" | videogameAds$UserId == "Isabelle Pontichelli" | videogameAds$UserId == "Astrid Neels" | videogameAds$UserId == "Thomas Mertens" | videogameAds$UserId == "Marc Roses" | videogameAds$UserId == "Lisa Thompson" ),]


#Gender of videogame
print(paste("Total number of videogame-related ads: ", nrow(videogameAds)))
print(paste("Number of videogame ads to men: ", nrow(videogameAds[which(videogameAds$sex == "Male"),])))
print(paste("Number of videogame ads to women: ", nrow(videogameAds[which(videogameAds$sex == "Female"),])))


videogameNoTarget <- videogameAds[which(!grepl(" women ", videogameAds$Explanation, ignore.case = TRUE)),]
videogameNoTarget <- videogameNoTarget[which(!grepl(" men ", videogameAds$Explanation, ignore.case = TRUE)),]

ggplot(videogameNoTarget, aes( x= "Videogame-related advertisements", fill=factor(sex))) + geom_bar(width  = 0.4)+
  scale_fill_manual(values=c("#D4D8E8", "#29487D"))+
  ggtitle("Distribution of audience receiving the ads by sex")+
  labs(x="",y= "Total count", fill = "User Id")+
  theme(plot.title = element_text(hjust = 0.5))

