# Create a continuous palette function
#pal <- colorNumeric(palette = "Blues",
#                    domain = data_2disease$Enrollment)

#qpal <- colorQuantile("Blues", data_2disease$Enrollment, n = 7)
#map %>%
#  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
#              color = ~qpal(gdp_md_est))
#
country_level<-data_2disease_1_withROR%>%
  group_by(country)%>%
  summarise(total_Trials=n())
  country_level$country[!country_level$country %in% data_Map$name]
  
  temp<-data_2disease_1_withROR%>%
    group_by(Conditions,Sponsor.Collaborators)%>%
    summarise(Num_Trials=n(),RecuitPerformace=mean(as.numeric(ROR),na.rm = TRUE))
  
#country_level$country<-as.factor(country_level$country)
  shinyServer(function(input, output){
    data_Map <- WorldCountry#[WorldCountry$name %in% country_level$country, ]
    qpal <- colorQuantile("Blues", data_Map$total_Trials, n = 7)
    data_Map@data <- left_join(data_Map@data, country_level, by = c('name' = 'country'))
#    binpal <- colorBin('Blues', data_Map$total_Trials, 9, pretty = TRUE)
#############   Sidebar 1 Leaflet Map
    output$map1 <- renderLeaflet({
      leaflet(data_Map) %>% setView(lng = 2.213749, lat = 46.22764, zoom = 1.3) %>%
        addPolygons(data = data_Map, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                    color = ~qpal(total_Trials)) %>% 
        addLegend("bottomleft", pal = qpal, values = ~total_Trials, title = "total_Trials", opacity = 1)
      })
    
    #############    Sidebar 2 World map2
    output$map2 <- renderLeaflet({
      #leaflet(C) %>% setView(lng = -94.0589, lat = 42.3601, zoom = 4) %>%
        #addTiles() %>% addMarkers(
       # clusterOptions = markerClusterOptions()
      #)
      pal <- colorFactor(c("navy", "red"), domain = c("Breast Cancer", "Prostate Cancer"))
      
      leaflet() %>%
        addTiles(
          'https://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}'
        ) %>% setView(lng = -94.0589, lat = 42.3601, zoom = 4)%>%
        addCircleMarkers(data=C%>%filter(Conditions=="Breast Cancer"),
          group = "Breast Cancer",
          #clusterOptions = markerClusterOptions(),
          radius = ~ lapply(C$n, function(x) sqrt(x)*3),
          color = 'orange',
          stroke = FALSE, fillOpacity = 0.5, clusterOptions = markerClusterOptions()
        ) %>%
    addCircleMarkers(data=C%>%filter(Conditions=="Prostate Cancer"),
        group = "Prostate Cancer",
        #clusterOptions = markerClusterOptions(),
        radius = ~ lapply(C$n, function(x) sqrt(x)*3),
        color = 'navy',
        stroke = FALSE, fillOpacity = 0.5,clusterOptions = markerClusterOptions()
      )%>%
      addLayersControl(
      baseGroups = c("Breast Cancer", "Prostate Cancer"),
      options = layersControlOptions(collapsed = FALSE)
    )
    })
      # ##### get a Google map
      # world_map<-get_map(location='world', 
      #                 zoom=4, 
      #                 maptype = "terrain",
      #                 source='google', 
      #                 color='color')
      # , colour=as.numeric(RoR)
      # ggmap(world_map) + geom_point(data=B,
      #   aes(x=long, y=lat, show_guide = TRUE))
      #   data=trial_sites_withROR, alpha=0.1, size = 5, na.rm = T)+
      #   scale_color_gradient(low="beige", high="blue")
    #})
#############    Sidebar 3 US map
    ##output$map3<- renderPlot({
      ##### get a Google map
      #map_us<-get_map(location='united states', 
                      #zoom=4, 
                      #maptype = "terrain",
                      #source='google', 
                      #color='color')
      #ggmap(map_us) + geom_point(
        #aes(x=long, y=lat, show_guide = TRUE, colour=as.numeric(RoR)), 
       # data=trial_sites_withROR, alpha=0.1, size = 5, na.rm = T)+
        #scale_color_gradient(low="beige", high="blue")
    #})
#########################  Sidebar 4: Histogram
    output$hist1 <- renderPlot({
      if (input$ct=="Sponsor.Collaborators") {
        df_1 <- temp[temp$Conditions=="Breast Cancer",] %>% top_n(10, Num_Trials)

        ggplot(data = df_1, aes(x=reorder(Sponsor.Collaborators,Num_Trials),
                                y=Num_Trials,
                                fill = Sponsor.Collaborators)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label=Num_Trials), size=3.5, vjust=-.5) +
          theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
          labs(title="Breast Cancer")
      } else{
        df_1 <- data_2disease_1_withROR[data_2disease_1_withROR$Conditions == "Breast Cancer",]
        ggplot(data=df_1, aes_string(x=input$ct, fill=input$ct)) + 
        geom_histogram(stat = 'count',na.rm = TRUE) +
        theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
        geom_text(stat = 'count', aes(label = ..count..), na.rm=TRUE, vjust=-.5) +
        labs(title="Breast Cancer")
      }
    })
    output$hist2 <- renderPlot({
      if (input$ct=="Sponsor.Collaborators") {
        df_2<-temp[temp$Conditions=="Prostate Cancer",]%>%top_n(10,Num_Trials)
#        temp<-as.data.frame(sort(table(data_2disease_1_withROR$Sponsor.Collaborators),
#                                 decreasing=TRUE)[1:10])
        ggplot(data = df_2, aes(x=reorder(Sponsor.Collaborators,Num_Trials),
                                y=Num_Trials,
                                fill = Sponsor.Collaborators))+
          geom_bar(stat = "identity") +
                   geom_text(aes(label=Num_Trials), size=3.5, vjust=-.5)+
          theme(axis.text.x = element_text(angle = 45,hjust = 1))+
          labs(title="Prostate Cancer")
      } else {
      df_2<-data_2disease_1_withROR[data_2disease_1_withROR$Conditions=="Prostate Cancer",]
      ggplot(data=df_2, aes_string(x=input$ct, fill=input$ct)) + 
        geom_histogram(stat = 'count',na.rm = TRUE)+
        theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
        geom_text(stat = 'count', aes(label = ..count..), na.rm=TRUE, vjust=-.5)+
        labs(title="Prostate Cancer")
      }
    })
    output$boxplot1 <- renderPlot({
      if (input$cy=="Sponsor.Collaborators") {
 #       sponsor_names<-unique(c(df_1$Sponsor.Collaborators,df_2$Sponsor.Collaborators))
        temp_2<-data_2disease_1_withROR[data_2disease_1_withROR$Sponsor.Collaborators%in%sponsor_names,]
        ggplot()+ 
          geom_boxplot(data =temp_2,
                       aes_string(x= input$cy, y='ROR',fill='Conditions'),
                       na.rm = TRUE)+ylim(c(0,1))+
          theme(axis.text.x = element_text(angle = 30,hjust = 1))+labs(title="Sponsor Performances")
      } else {
        ggplot()+ 
          geom_boxplot(data =data_2disease_1_withROR,
                       aes_string(x= input$cy, y='ROR',fill='Conditions'),
                       na.rm = TRUE)+
          ylim(c(0,1))+
          theme(axis.text.x = element_text(angle = 30,hjust = 1))
          
        }
    
    })
}
)
    
    
    