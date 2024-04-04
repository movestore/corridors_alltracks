library("shiny")
library("shinydashboard")
library("fresh")
library("move2")
library("sf")
# library("dplyr")
library("move")
# library("ggplot2")
library("shinyBS") ## to display message when hovering over input element in UI
library("lubridate")
# library("geosphere")
# library("dismo")
library("rgeos")
library("stringr")
library("shinyWidgets")
# library("ggsn")
library("shinycssloaders")
library("maptools")
library("circular")
library("leaflet")
# library(viridis)
# library(plyr)
library("dplyr")

# options(shiny.reactlog=TRUE)

shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)
  tagList(
    titlePanel(h2("Corridors for all track",
                  style='background-color:#46505a;
                color:#dfe7ef;
                     padding-left: 15px')),
    
    fluidRow( ## make the same as in "population corridors"
      column(3,sliderInput(inputId=ns("speedProp"),label="Speed", min=0,max=1, value=0.75, step=0.01),
             bsTooltip(id=ns("speedProp"), title="Proportion of speeds which are high enough to be a valid corridor point (default: speeds that are greater than 75 % of all speeds)", placement = "bottom", trigger = "hover", options = list(container = "body"))),
      column(3,sliderInput(inputId=ns("circProp"),label="Parallelism", min=0,max=1, value=0.25, step=0.01),
             bsTooltip(id=ns("circProp"), title="Proportion of the circular variances that is low enough to be a valid corridor point. Low values indicate that the segments are (near) parallel (default: variances that are lower than 25 % of all variances)", placement = "bottom", trigger = "hover", options = list(container = "body"))), ## maybe change wording to make it simpler: the lower the value, the more parallel are the segments
      column(1, numericInput(ns("timeThin"),"Thin track to X mins", value=0, step=1),
             bsTooltip(id=ns("timeThin"), title="This is specially recommended for high resolution tracks to ease finding regions with parallel segments. Default (=0) no thinning", placement = "bottom", trigger = "hover", options = list(container = "body"))),
      column(1, numericInput(ns("clustDist"),"Cluster radius (m)",value=500), #diameter of the cluster, i.e. width of the corridor
             bsTooltip(id=ns("clustDist"), title="All identified corridor segments that fall within a circle will be grouped as a corridor cluster", placement = "bottom", trigger = "hover", options = list(container = "body"))
      ),
      column(1, numericInput(ns("clustNb"),"Segment number",value=3), #number of segments that corridor should have within a cluster
             bsTooltip(id=ns("clustNb"), title="Minimum number of segments that will define a cluster. Clusters with fewer segments will be excluded", placement = "bottom", trigger = "hover", options = list(container = "body"))
      ),
      column(2, radioButtons(ns("allorsingle"),"Segment number", choices=c("Per track", "Across tracks"),selected="Per track")),
      column(1,actionBttn(ns("updateButton"), label="Update!", style="fill", color="success",icon=icon("redo"),size="md"))# offset=1,
    ),
    withSpinner(leafletOutput(ns("plotnames"), height = "85vh"), type=5, size=1.5,color= "#28b78d") ## color the same as update button
  )
}

shinyModule <- function(input, output, session, data) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- session$ns
  data_out <- reactiveVal(data)
  output$plotnames <- renderLeaflet({
    leaflet() %>% addTiles()
    #   leaflet(coordinates(dataMv)) %>% addTiles()%>%
    #     addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
    #     addProviderTiles("Esri.WorldImagery", group = "Aerial") #%>%
    # addLayersControl(
    #         baseGroups = c("TopoMap","Aerial"),
    #         # overlayGroups = centClust$clusterID,
    #         # overlayGroups = c(inputTable$indiv,paste0("potential corridors-",inputTable$indiv)),
    #         options = layersControlOptions(collapsed = FALSE))
  })
  observeEvent(input$updateButton, {
    #####################
    ### transform data ##
    #####################
    data  <- mutate(data, LocID = 1:nrow(data))
    data  <- mutate(data, corridorBehavior = NA)
    # data  <- mutate(data, corridorBehaviorClusterID = NA)
    data_out <- reactiveVal(data)
    dataMv2 <- data
    if(st_is_longlat(dataMv2)){dataMv2 <- dataMv2}else{dataMv2 <- st_transform(dataMv2,crs="EPSG:4326")}  ## to not change projection of original data
    dataMv <- moveStack(to_move(dataMv2))
    
    output$plotnames <- renderLeaflet({
      #########################
      ##### plot functions ####
      #########################
      noCorrPlot_all <- function(inputTable){
        ## to create the text box no corridors found
        tag.map.title <- tags$style(HTML(".leaflet-control.map-title {
          transform: translate(-50%,20%);
          position: fixed !important;
          left: 50%;
          text-align: center;
          padding-left: 10px; 
          padding-right: 10px; 
          background: rgba(255,255,255,0.75);
          font-weight: bold;
          font-size: 28px;
          color: black;
          }"))
        title <- tags$div(
          tag.map.title, HTML("No corridors found")
        ) 
        
        ind <- unique(inputTable$indiv)
        
        map1 <- leaflet(inputTable) %>% addTiles()%>%
          addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
          addProviderTiles("Esri.WorldImagery", group = "Aerial") #%>% 
        for(x in seq_along(ind)){
          i <- ind[x]
          corrDFInd <- inputTable[inputTable$indiv==i,]
          corrDFr <- which(corrDFInd$burstId%in%c("corridor"))
          map1 <- map1 %>%
            addPolylines(lng = corrDFInd$long,lat = corrDFInd$lat,weight=2, opacity=0.5, layerId=i,group=i, color="black")
        }
        map1 %>%
          addControl(title, position = "topleft", className="map-title") %>% # to add the ext box of corridors not found
          addScaleBar(position="bottomright",
                      options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
          addLayersControl(
            baseGroups = c("TopoMap","Aerial"),
            # overlayGroups = c("indiv","corr"),
            options = layersControlOptions(collapsed = FALSE))
      }
      
      midPts_all <- function(inputTable, r, ns, trks){### calculating clusters to define which segments belong "to the same corridor"
        if(trks=="Across tracks"){ #"Per track", "Across tracks" #trks= RVupdate$allorsingle
          ## corridor segment midpoints
          midCrPts <- inputTable[inputTable$burstId=="corridor",c("segMid_x","segMid_y","LocID")]
          midCrPts <- midCrPts[complete.cases(midCrPts),]
          coordinates(midCrPts) <- ~segMid_x+segMid_y
          projection(midCrPts) <- projection(dataMv)
          ## distance matrix
          dist <- distm(midCrPts, fun=distGeo)
          ## clustering
          hc <- hclust(as.dist(dist), method="single") # complete single
          # define clusters based on a tree "height" cutoff distance (distance between corridor clusters) and add them to the SpDataFrame
          midCrPts$clusterID <- cutree(hc, h=r/2) # r=RVupdate$clustDist
          # add nb of corridor segments per cluster to SpDataFrame
          tbCount <- data.frame(table(midCrPts$clusterID))
          midCrPts$nbCorrInClust <- NA
          for(j in tbCount$Var1){
            midCrPts$nbCorrInClust[midCrPts$clusterID==j] <- tbCount$Freq[tbCount$Var1==j]
          }
          ## remove those with lesss than X points
          midCrPts <- midCrPts[midCrPts$nbCorrInClust>ns,] #ns=RVupdate$clustNb
          midCrPts2 <- do.call("rbind",midCrPts)
          return(midCrPts2)
        }
        if(trks=="Per track"){ #"Per track", "Across tracks"
          midPts_L <- lapply(split(inputTable, inputTable$indiv), function(trkTB)tryCatch({
            ## corridor segment midpoints
            midCrPts <- trkTB[trkTB$burstId=="corridor",c("segMid_x","segMid_y","LocID")]
            midCrPts <- midCrPts[complete.cases(midCrPts),]
            coordinates(midCrPts) <- ~segMid_x+segMid_y
            projection(midCrPts) <- projection(dataMv)
            ## distance matrix
            dist <- distm(midCrPts, fun=distGeo)
            ## clustering
            hc <- hclust(as.dist(dist), method="single") # complete single
            # define clusters based on a tree "height" cutoff distance (distance between corridor clusters) and add them to the SpDataFrame
            midCrPts$clusterID <- cutree(hc, h=r/2) # r=RVupdate$clustDist
            # add nb of corridor segments per cluster to SpDataFrame
            tbCount <- data.frame(table(midCrPts$clusterID))
            midCrPts$nbCorrInClust <- NA
            for(j in tbCount$Var1){
              midCrPts$nbCorrInClust[midCrPts$clusterID==j] <- tbCount$Freq[tbCount$Var1==j]
            }
            ## remove those with lesss than X points
            midCrPts <- midCrPts[midCrPts$nbCorrInClust>ns,] #ns=RVupdate$clustNb
            return(midCrPts)
          },error=function(e) NULL))
          midPts_L[sapply(midPts_L, is.null)] <- NULL
          midPts_L2 <- lapply(midPts_L, function(x){
            if(nrow(x)==0){NULL}else{x}
          })
          midPts_L2[sapply(midPts_L2, is.null)] <- NULL
          midCrPts1 <- do.call("rbind",midPts_L2)
          if(is.null(midCrPts1)){midCrPts <- midPts_L[[1]]}else{midCrPts <- midPts_L2}
          midCrPts2 <- do.call("rbind",midCrPts)
          return(midCrPts2)
        }
      }
      
      corrPlot_all <- function(inputTable,midCrPts){
        # get the centroid coords for each cluster
        centClust <- data.frame(x=NA,y=NA,clusterID=unique(midCrPts$clusterID))
        for(i in unique(midCrPts$clusterID)){
          centClust[centClust$clusterID==i,c("x","y")] <- gCentroid(subset(midCrPts, clusterID == i))@coords
        }
        
        inputTable <- merge(inputTable, midCrPts@data, by="LocID", all.x=T)
        ## only selected corridors acording to clustNb and clustDist are assigned with corridor
        inputTable$burstId <- "no.corridor"
        inputTable$burstId[inputTable$LocID%in%midCrPts@data$LocID] <- "corridor"
        corrDF_rv(inputTable)
        # corrDFr <- which(indDF$burstId%in%c("corridor")) ## to plot all corridors at once
        
        # ## color palette so clusters can be identified
        # selClusterIDs <- unique(indDF$clusterID)[complete.cases(unique(indDF$clusterID))]
        # pal <- colorFactor(
        #   palette = "magma",
        #   domain = selClusterIDs
        # )
        
        ind <- unique(inputTable$indiv)
        cols <- rainbow(n=length(ind))
        
        map1 <- leaflet(inputTable) %>% addTiles()%>%
          addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
          addProviderTiles("Esri.WorldImagery", group = "Aerial") #%>% 
        for(x in seq_along(ind)){
          i <- ind[x]
          corrDFInd <- inputTable[inputTable$indiv==i,]
          corrDFr <- which(corrDFInd$burstId%in%c("corridor"))
          map1 <- map1 %>%
            addPolylines(lng = corrDFInd$long,lat = corrDFInd$lat,weight=2, opacity=0.5, layerId=i,group=i, color="black")
          for(n in corrDFr){
            map1 <- map1 %>%
              addPolylines(data=corrDFInd[n:(n+1),],lng=~long,lat=~lat, color=cols[x],weight = 6,opacity = 0.7, group=paste0("potential corridors-",i))
          }
          
        }
        # for(n in corrDFr){ ## to plot all corridors at once
        #   map1 <- map1 %>%
        #   addPolylines(data=indDF[n:(n+1),],lng=~long,lat=~lat, color=~pal(indDF$clusterID[n]),weight = 6,opacity = 0.8, group="potential corridors")
        # }
        # for(s in selClusterIDs){
        #   corrLocs <- which(indDF$clusterID%in%s)
        #   for(n in corrLocs){
        #     map1 <- map1 %>%
        #       addPolylines(data=indDF[n:(n+1),],lng=~long,lat=~lat, color=~pal(indDF$clusterID[n]),weight = 6,opacity = 0.8, group=paste0("potential corridors cl:",s))
        #   }
        # }
        map1 %>% 
          # addCircles(data=centClust, lng=~x, lat=~y, group=~clusterID, radius=RVupdate$clustDist/2, color="gold") %>% #radius=RVupdate$clustDist
          addScaleBar(position="bottomright",
                      options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
          addLayersControl(
            baseGroups = c("TopoMap","Aerial"),
            overlayGroups = c(inputTable$indiv,paste0("potential corridors-",inputTable$indiv)),
            options = layersControlOptions(collapsed = TRUE)) 
      }
      
      # corrPlot_all <- function(inputTable){
      #   ind <- unique(corrDF$indiv)
      #   cols <- rainbow(n=length(ind))
      #   map1 <- leaflet(inputTable) %>% addTiles()%>%
      #     addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      #     addProviderTiles("Esri.WorldImagery", group = "Aerial") #%>% 
      #   for(x in seq_along(ind)){
      #     i <- ind[x]
      #     corrDFInd <- inputTable[inputTable$indiv==i,]
      #     corrDFr <- which(corrDFInd$burstId%in%c("corridor"))
      #     map1 <- map1 %>%
      #       addPolylines(lng = corrDFInd$long,lat = corrDFInd$lat,weight=2, opacity=0.5, layerId=i,group=i, color="black")
      #     for(n in corrDFr){
      #       map1 <- map1 %>%
      #         addPolylines(data=corrDFInd[n:(n+1),],lng=~long,lat=~lat, color=cols[x],weight = 6,opacity = 0.7, group=paste0("potential corridors-",i))
      #     }
      #   }
      #   map1 %>% 
      #     # addCircles(data=centClust, lng=~x, lat=~y, group=~clusterID, radius=medSegleng*2, color="red") %>%
      #     addScaleBar(position="bottomright",
      #                 options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
      #     addLayersControl(
      #       baseGroups = c("TopoMap","Aerial"),
      #       # overlayGroups = centClust$clusterID,
      #       overlayGroups = c(inputTable$indiv,paste0("potential corridors-",inputTable$indiv)),
      #       options = layersControlOptions(collapsed = FALSE)) 
      # }
      
      
      
      ################
      ## end plot F ##
      ###############
      
      
      
      
      
      # if(!input$updateButton){
      
      # dataMvSubTime <- dataMv ## no thining by time
      # corrDF_L <- lapply(split(dataMvSubTime), function(y){
      #   # corridorCalc <- corridor(x=y, speedProp=.75, circProp=.25, plot=FALSE)
      #   corridorCalc <- corridor(x=y, speedProp=input$speedProp, circProp=input$circProp, plot=FALSE)
      #   indDF <- data.frame(long=coordinates(corridorCalc)[,1],lat=coordinates(corridorCalc)[,2],burstId=c(as.character(burstId(corridorCalc)),NA),LocID=corridorCalc$LocID, indiv=namesIndiv(corridorCalc),segMid_x=corridorCalc@data$segMid_x, segMid_y=corridorCalc@data$segMid_y)
      #   return(indDF)
      # })
      # corrDF <- do.call("rbind",corrDF_L)
      # corrYorN <- which(corrDF$burstId%in%c("corridor"))
      # corrDF_rv <- reactiveVal(corrDF)
      ##################################################
      ### default plot all individuals -- NO corridors ##
      ##################################################
      # if(length(corrYorN)<=1){  ## if levels do not contain "corridor" OR if there is only 1 corridor point identified it cannot be clustered and gives an error
      #   ## to create the text box no corridors found
      #   noCorrPlot_all(isolate(corrDF_rv()))
      # } else { ## there are corridors found
      #   ####################################################
      #   ### default plot all individuals -- with corridors ##
      #   ####################################################
      #   mP <- midPts_all(isolate(corrDF_rv()),r=500,ns=3,trks="Per track")
      #   if(length(unique(mP$clusterID))>=1){
      #     corrPlot_all(isolate(corrDF_rv()),mP)
      #   }else{
      #     noCorrPlot_all(isolate(corrDF_rv()))
      #   }
      # }
      # }
      #################################
      ### updated plot all individuals ##
      ##################################
      # observeEvent(input$updateButton, {
      # dataMvSubTime <- dataMv ## no thining by time
      if(input$timeThin==0){dataMvSubTime <- dataMv ## no thining by time
      } else {
        dataMvSubTime <- dataMv[!duplicated(paste0(dataMv@trackId,round_date(timestamps(dataMv), paste0(input$timeThin," aminutes")))),]
      }
      corrDF_L <- lapply(split(dataMvSubTime), function(y){
        corridorCalc <- corridor(x=y, speedProp=input$speedProp, circProp=input$circProp, plot=FALSE)#, minNBsegments = 4)
        indDF <- data.frame(long=coordinates(corridorCalc)[,1],
                            lat=coordinates(corridorCalc)[,2],
                            burstId=c(as.character(burstId(corridorCalc)),NA),
                            LocID=corridorCalc$LocID, 
                            indiv=namesIndiv(corridorCalc),
                            segMid_x=corridorCalc@data$segMid_x, 
                            segMid_y=corridorCalc@data$segMid_y)
        return(indDF)
      })
      corrDF <- do.call("rbind",corrDF_L)
      corrYorN <- which(corrDF$burstId%in%c("corridor"))
      corrDF_rv <- reactiveVal(corrDF)
      ##################################################
      ### updated plot all individuals -- NO corridors ##
      ##################################################
      if(length(corrYorN)<=1){  ## if levels do not contain "corridor" OR if there is only 1 corridor point identified it cannot be clustered and gives an error
        noCorrPlot_all(isolate(corrDF_rv()))  
        
      } else { ## there are corridors found
        ####################################################
        ### updated plot all individuals -- with corridors ##
        ####################################################
        mP <- midPts_all(isolate(corrDF_rv()),r=input$clustDist,ns=input$clustNb,trks=input$allorsingle)
        if(length(unique(mP$clusterID))>=1){
          corrPlot_all(isolate(corrDF_rv()),mP)
        }else{
          noCorrPlot_all(isolate(corrDF_rv()))
        }
      }
    })
  })
  
  # observe({ ## THIS OBSERVE DOES NOT WORK!!! as asingle corridors cannot be selected in plot
  #   selected_groups <- req(input[[paste0(plotnames[i],"_groups")]])
  #   selClusts <- as.numeric(gsub("potential corridors cl:", "", selected_groups, perl=TRUE))
  #   selClusts <- selClusts[complete.cases(selClusts)]
  #   selLocIDs <- corrDF_rv$LocID[corrDF_rv$clusterID%in%selClusts]
  #   data$corridorBehavior[data$LocID%in%selLocIDs] <- "corridor"
  #   data_out(data)
  # })
  
  
  # observe({ 
  #   selected_groups <- req(input[[paste0(plotnames[i],"_groups")]])
  #   selClusts <- as.numeric(gsub("potential corridors cl:", "", selected_groups, perl=TRUE))
  #   selClusts <- selClusts[complete.cases(selClusts)]
  #   indDFmod <- indDF_rv()
  #   selLocIDs <- indDFmod$LocID[indDFmod$clusterID%in%selClusts]
  #   data$corridorBehavior[data$LocID%in%selLocIDs] <- "corridor"
  #   data_out(data)
  # })    
  
  return(data_out)
  
}
