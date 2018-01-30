library(geojsonio)
library(dplyr)
library(tidyr)
library(rgdal)
library(shinydashboard)
library(leaflet)

library(ggplot2)
library(ggmap)

# Prostate.cancer = read.csv("/Users/XiaoJia/downloads/Prostatecancer.csv",stringsAsFactors = FALSE)[,-1]
# 
# Breast.cancer = read.csv("/Users/XiaoJia/downloads/Breastcancer.csv",stringsAsFactors = FALSE)[,-1]
# 
# bc=Breast.cancer %>% mutate(country1 = strsplit(Breast.cancer$Locations,split = ', ' ))
# pc=Prostate.cancer %>% mutate(country1 = strsplit(Prostate.cancer$Locations,split = ', ' ))
# 
# 
# pc = pc %>% select(-URL)
# bc = bc %>% select(-URL)
# nrow(bc)
# #bc$Conditions<-rep("Breast Cancer",nrow(bc))
# bc$Conditions<-"Breast Cancer"
# pc$Conditions<-"Prostate Cancer"
# 
# #table(as.factor(bc$Conditions))
# data_2disease<-rbind(bc,pc)
# 
# #data_2disease$country1[length(data_2disease$country1[[1]])]
# # Long_Lat = function(X){
# #   library(ggmap)
# #   temp<-geocode(X)
# #   lst.result<-list(long=temp[1,1],
# #                    lat=temp[1,2])
# #   #  lon<-temp[1,1]
# #   #  lat<-temp[1,2]
# #   return(lst.result)
# # }
# # temp<-as.numeric(geocode(Breast.cancer$Locations[1]))
# # temp
# # res <- revgeocode(temp, output="more")
# # dim(res)
# # res$country
# #data_2disease[which(data_2disease$country=="Islamic Republic of"),]
# #Breast.cancer$longtitude<-sapply(Breast.cancer$Locations,FUN = Long_Lat)[1,1]
# data_2disease$country<-sapply(data_2disease$country1,function(x) tail(x,1))
# data_2disease$country
# data_2disease_1<-data_2disease[-which(data_2disease$country=="character(0)"),]
# data_2disease_1$country<-unlist(data_2disease_1$country)
# data_2disease_1$country
# 
# data_2disease_1$country[which(data_2disease_1$country=="Republic of")]="South Korea"
# data_2disease_1$country[which(data_2disease_1$country=="Czechia")]="Czech Republic"
# data_2disease_1$country[which(data_2disease_1$country=="United States")]="United States of America"
# data_2disease_1$country[which(data_2disease_1$country=="Russian Federation")]="Russia"
# data_2disease_1$country[which(data_2disease_1$country=="Serbia")]="Republic of Serbia"
# data_2disease_1$country[which(data_2disease_1$country=="Hong Kong")]="China"
# data_2disease_1$country[which(data_2disease_1$country=="Islamic Republic of")]="Iran"
# 
# 
# #data_2disease$country=gsub("Republic of","Korea",fixed = TRUE,data_2disease$country)
# table(as.factor(data_2disease_1$country))
# 
# data_2disease_1$country<-sapply(data_2disease_1$country,function(x) unlist(strsplit(x,split = "|",fixed = TRUE))[1])
# 
# ###### Enrollment to
# data_2disease_1$Enrollment<-as.numeric(data_2disease_1$Enrollment)
# 
# 
# 
# table(as.factor(unlist(data_2disease_1$country)))
# 
# sum(gsub("Republic of","Korea",pc$country)=="Korea")
# dim(gsub("Republic of","Korea",pc$country))
# #sum(A==" Korea")
# 
# table(data_2disease_1$Conditions)
# 
# facilities = read.csv("/Users/Xiaojia/downloads/20180111_pipe-delimited-export/facilities.csv", header = TRUE)[,-1]
# trial_sites<-left_join(data_2disease_1,facilities,by=c("NCT.Number"= "nct_id"))
# trial_sites$Sponsor.Collaborators<-sapply(trial_sites$Sponsor.Collaborators,function(x) unlist(strsplit(x,split = "|",fixed = TRUE))[1])#取sponsor里面的第一个
# 
# no_use<-c("Title","Acronym","Study.Results","Outcome.Measures",
#           "Gender","Age","Study.Designs","Other.IDs","Primary.Completion.Date","Last.Verified",
#           "First.Submitted","First.Posted","Results.First.Submitted","Results.First.Posted","Last.Update.Submitted",
#           "Last.Update.Posted","Interventions")
# trial_sites<- trial_sites[,!names(trial_sites)%in%no_use]
# 
# # list all unique cities avoid deplicate download from google
# a = unique(trial_sites$city)
# a<-as.character(a[complete.cases(a)])###a[!is.na(a)]
# #!is.na();complete.cases()  ### remove na
# 
# trial_sites = trial_sites %>% select(-name)
# trial_sites = trial_sites %>% select(-state)
# trial_sites = trial_sites %>% select(-zip)
# 
# 
# # WorldCountry <-geojsonio::geojson_read("/Users/XiaoJia/Downloads/world.geo.json-master_2/countries.geo.json", what = "sp")
# # data_Map <- WorldCountry
# # qpal <- colorQuantile("Blues", data_Map$total_Trials, n = 7)
# # data_Map@data <- left_join(data_Map@data, country_level, by = c('name' = 'country'))
# # country_level<-data_2disease_1%>%group_by(country)%>%summarise(total_Trials=n())
# # country_level$country[!country_level$country %in% data_Map$name]
# # data_Map@data <- left_join(data_Map@data, country_level, by = c('name' = 'country'))
# trial_sites = trial_sites %>% select(-Locations)
# trial_sites = trial_sites %>% select(-country.x)
# trial_sites = trial_sites %>% select(-country.y)
# trial_sites = trial_sites %>% select(-country1)
# 
# #saveRDS(c(trial_sites, ), 'data.RDS')
# #readRDS('data.RDS')
trial_sites<-read.csv("trial_sites.csv",stringsAsFactors = TRUE,na.strings = "")[,-1]
data_2disease_1<-read.csv("data_2disease_1.csv",stringsAsFactors = FALSE,na.strings = "")[,-1]
information=c("Recruitment","Phases","Study.Type","Sponsor.Collaborators")
data_2disease_1_withROR<-read.csv("data_2disease_1_withROR.csv",stringsAsFactors = FALSE,na.strings = "")[,-1]
data_2disease_1_withROR$ROR<-as.numeric(data_2disease_1_withROR$ROR)
sponsor_selected_names<-c("Alliance for Clinical Trials in Oncology","AstraZeneca","Dana-Farber Cancer Institute","Hoffmann-La Roche","M.D. Anderson Cancer Center","Mayo Clinic","Memorial Sloan Kettering Cancer Center", "National Cancer Institute (NCI)","Novartis Pharmaceuticals","Pfizer","Bayer","Ferring Pharmaceuticals","Sidney Kimmel Comprehensive Cancer Center","University Health Network", "Toronto","University of California, San Francisco")
trial_sites_withROR<-read.csv("trial_sites_withROR.csv",stringsAsFactors = FALSE,na.strings = "")[,-1]
C = trial_sites_withROR%>%group_by(city,Conditions)%>%summarise(n = n())
C = inner_join(B, C, by=c("city"="city"))

