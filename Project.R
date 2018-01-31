library(dplyr)
library(ggplot2)
library(ggmap)

Prostate.cancer = read.csv("/Users/XiaoJia/downloads/Prostatecancer.csv",stringsAsFactors = FALSE)[,-1]

Breast.cancer = read.csv("/Users/XiaoJia/downloads/Breastcancer.csv",stringsAsFactors = FALSE)[,-1]

bc=Breast.cancer %>% mutate(country1 = strsplit(Breast.cancer$Locations,split = ', ' ))
pc=Prostate.cancer %>% mutate(country1 = strsplit(Prostate.cancer$Locations,split = ', ' ))


pc = pc %>% select(-URL)
bc = bc %>% select(-URL)
nrow(bc)
#bc$Conditions<-rep("Breast Cancer",nrow(bc))
bc$Conditions<-"Breast Cancer"
pc$Conditions<-"Prostate Cancer"

#table(as.factor(bc$Conditions))
data_2disease<-rbind(bc,pc)

#data_2disease$country1[length(data_2disease$country1[[1]])]
# Long_Lat = function(X){
#   library(ggmap)
#   temp<-geocode(X)
#   lst.result<-list(long=temp[1,1],
#                    lat=temp[1,2])
#   #  lon<-temp[1,1]
#   #  lat<-temp[1,2]
#   return(lst.result)
# }
# temp<-as.numeric(geocode(Breast.cancer$Locations[1]))
# temp
# res <- revgeocode(temp, output="more")
# dim(res)
# res$country
#data_2disease[which(data_2disease$country=="Islamic Republic of"),]
#Breast.cancer$longtitude<-sapply(Breast.cancer$Locations,FUN = Long_Lat)[1,1]
data_2disease$country<-sapply(data_2disease$country1,function(x) tail(x,1))
data_2disease$country
data_2disease_1<-data_2disease[-which(data_2disease$country=="character(0)"),]
data_2disease_1$country<-unlist(data_2disease_1$country)
data_2disease_1$country

data_2disease_1$country[which(data_2disease_1$country=="Republic of")]="South Korea"
data_2disease_1$country[which(data_2disease_1$country=="Czechia")]="Czech Republic"
data_2disease_1$country[which(data_2disease_1$country=="United States")]="United States of America"
data_2disease_1$country[which(data_2disease_1$country=="Russian Federation")]="Russia"
data_2disease_1$country[which(data_2disease_1$country=="Serbia")]="Republic of Serbia"
data_2disease_1$country[which(data_2disease_1$country=="Hong Kong")]="China"
data_2disease_1$country[which(data_2disease_1$country=="Islamic Republic of")]="Iran"


#data_2disease$country=gsub("Republic of","Korea",fixed = TRUE,data_2disease$country)
table(as.factor(data_2disease_1$country))

data_2disease_1$country<-sapply(data_2disease_1$country,function(x) unlist(strsplit(x,split = "|",fixed = TRUE))[1])

###### Enrollment to
data_2disease_1$Enrollment<-as.numeric(data_2disease_1$Enrollment)



table(as.factor(unlist(data_2disease_1$country)))

sum(gsub("Republic of","Korea",pc$country)=="Korea")
dim(gsub("Republic of","Korea",pc$country))
sum(A==" Korea")

table(data_2disease_1$Conditions)

facilities = read.csv("/Users/Xiaojia/downloads/20180111_pipe-delimited-export/facilities.csv", header = TRUE)[,-1]
trial_sites<-left_join(data_2disease_1,facilities,by=c("NCT.Number"= "nct_id"))
#取sponsor里面的第一个
trial_sites$Sponsor.Collaborators<-sapply(trial_sites$Sponsor.Collaborators,
                                          function(x) unlist(strsplit(x,split = "|",fixed = TRUE))[1])
data_2disease_1_withROR$Sponsor.Collaborators<-sapply(data_2disease_1_withROR$Sponsor.Collaborators,
                                              function(x) unlist(strsplit(x,split = "|",fixed = TRUE))[1])#取sponsor里面的第一

no_use<-c("Title","Acronym","Study.Results","Outcome.Measures",
          "Gender","Age","Study.Designs","Other.IDs","Primary.Completion.Date","Last.Verified",
          "First.Submitted","First.Posted","Results.First.Submitted","Results.First.Posted","Last.Update.Submitted",
          "Last.Update.Posted")
trial_sites<- trial_sites[,!names(trial_sites)%in%no_use]

trial_sites= trial_sites %>% select(-Title)
trial_sites= trial_sites %>% select(-Acronym)
trial_sites= trial_sites %>% select(-Study.Results)
trial_sites= trial_sites %>% select(-Conditions)
trial_sites= trial_sites %>% select(-Outcome.Measures)
trial_sites= trial_sites %>% select(-Gender)
trial_sites= trial_sites %>% select(-Age)
trial_sites= trial_sites %>% select(-Study.Designs)
trial_sites= trial_sites %>% select(-Other.IDs)
trial_sites= trial_sites %>% select(-Primary.Completion.Date)
trial_sites= trial_sites %>% select(-Last.Verified)
trial_sites= trial_sites %>% select(-First.Submitted)
trial_sites= trial_sites %>% select(-First.Posted)
trial_sites= trial_sites %>% select(-Results.First.Submitted)
trial_sites= trial_sites %>% select(-Results.First.Posted)
trial_sites= trial_sites %>% select(-Last.Update.Submitted)
trial_sites= trial_sites %>% select(-Last.Update.Posted)

# list all unique cities avoid deplicate download from google
a = unique(trial_sites$city)
a<-as.character(a[complete.cases(a)])###a[!is.na(a)]
!is.na();complete.cases()  ### remove na

#long=c()
#lat=c()
for (i in 5502:length(a)){
  temp=geocode(a[i])
  long[i]=temp[1,1]
  lat[i]=temp[1,2]
  Sys.sleep(2)
}
longtitude = long
latitude = lat
trial_sites= trial_sites %>% select(-Title)

B=data.frame(city=a[3695:5501],long=long[3695:5501],lat=lat[3695:5501])
city_long_lat<-rbind(A,B)

trial_sites_withROR<-left_join(trial_sites_withROR,city_long_lat,by=c("city"="city"))
write.csv(trial_sites_withROR,"trial_sites_withROR.csv")

length(long)




output$hist1 <- renderPlot({
  ggplot(data=trial_sites) + 
    geom_histogram(aes_string(x=input$select_hist), stat = 'count') +
    geom_histogram(aes(fill = phase))
})

output$hist2 <- renderPlot({
  ggplot(data=trial_sites[trial_sites$Conditions == "Prostate_Cancer"]) + 
    geom_histogram(aes_string(x=input$select_hist), stat = 'count')
})

write.csv(trial_sites,"trial_sites.csv")
write.csv(data_2disease_1,"data_2disease_1.csv")

temp_df<-data.frame("longtitude"=longitude,"Latitude"=latitude)
write.csv(temp_df,"long_lat_3694.csv")
no_use<-c("Title","Acronym","Study.Results","Outcome.Measures",
          "Gender","Age","Study.Designs","Other.IDs","Primary.Completion.Date","Last.Verified",
          "First.Submitted","First.Posted","Results.First.Submitted","Results.First.Posted","Last.Update.Submitted",
          "Last.Update.Posted")
data_2disease_1<- data_2disease_1[,!names(data_2disease_1)%in%no_use]
data_2disease_1 = data_2disease_1 %>% select(-Locations)
data_2disease_1 = data_2disease_1 %>% select(-country.x)
data_2disease_1 = data_2disease_1 %>% select(-country.y)
data_2disease_1 = data_2disease_1 %>% select(-country1)
##### get a Google map
map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')
####plot US map
ggmap(map) + geom_point(
  aes(x=longitude, y=latitude, show_guide = TRUE, colour=as.numeric(RoR)), 
  data=trial_sites, alpha=0.1, size = 5, na.rm = T)  + 
  scale_color_gradient(low="beige", high="blue")


ggmap(map) + geom_point(
  aes(x=longitude, y=latitude, show_guide = TRUE, colour=as.numeric(RoR)), 
  data=INTN_MM_site_Census_complete, alpha=0.1, size = 6, na.rm = T)  + 
  scale_color_gradient(low="yellow", high="red")

ggmap(map) + geom_point(
  aes(x=longitude, y=latitude, show_guide = TRUE, colour=RoR.Ranking..H.M.L.), 
  data=INTN_MM_site_Census_complete, size = 5,na.rm = T)  + 
  scale_color_brewer(palette = "Spectral")


year_cap<-function(x){
  if(is.na(as.yearmon(x,"%b %d, %Y")) == TRUE){
    return(format(as.yearmon(x,"%b %Y"),"%Y"))
    } else{
      return(format(as.yearmon(x,"%b %d, %Y"),"%Y"))
    }
  }


month_cap = function(x){
  if (is.na(as.yearmon(x,"%b %d, %Y")) == TRUE){
    return(format(as.yearmon(x,"%b %Y"),"%m"))
  } else{
    return(format(as.yearmon(x,"%b %d, %Y"),"%m"))
  }
}
#trial_sites$Start.Date<-as.character(trial_sites$Start.Date)
#trial_sites$Completion.Date<-as.character(trial_sites$Completion.Date)
#### create new column to show the start year
start_year<-unlist(lapply(data_2disease_1$Start.Date,
                   function(x) year_cap(x)))
start_year
Completion_year<-unlist(lapply(data_2disease_1$Completion.Date,
                        function(x) year_cap(x)))

data_2disease_1$start_year<-as.numeric(start_year)
data_2disease_1$Completion_year<-as.numeric(Completion_year)

#### create new column to show the start month

start_month<-unlist(lapply(data_2disease_1$Start.Date,
                          function(x) month_cap(x)))
start_month
Completion_month<-unlist(lapply(data_2disease_1$Completion.Date,
                        function(x) month_cap(x)))

data_2disease_1$start_month<-as.numeric(start_month)
data_2disease_1$Completion_month<-as.numeric(Completion_month)


data_2disease_1$Enrollment<-as.numeric(as.character(data_2disease_1$Enrollment))
data_2disease_1_withROR<-data_2disease_1%>%mutate(duration=(Completion_year-start_year)*12+(Completion_month-start_month))%>%
  mutate(ROR=Enrollment/duration)C,

write.csv(data_2disease_1_withROR,"data_2disease_1_withROR.csv")
#join the data fram when get all the lat and long
B=data.frame(city=a[3695:5501],long=long[3695:5501],lat=lat[3695:5501])
city_long_lat<-rbind(A,B)
trial_sites_withROR<-left_join(trial_sites_withROR,city_long_lat,by=c("city"="city"))
ggplot(data = data_2disease_1_withROR) + 
  geom_boxplot(aes(x= Phases, y=ROR, fill=Conditions))+ylim(c(0,1))
