#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(leaflet)
library(ncdf4)
library(rgdal)
library(raster)
library(shinyWidgets)

ab <- c("tn","tx","tg","rr")
indno <- 32


idNA <- function(file_name,var_name)
{
  nc <- nc_open(file_name) 
  val <- ncvar_get(nc,var_name)
  n <- nc$dim[[1]]$len
  m <- nc$dim[[2]]$len  
  for(i in 4:n)
  {
    for(j in 4:m)
    {
      l=which(is.na(val[i,j,]))
      if (length(l)>0)
        print(paste("lon:",i," lat:",j," day: ",l,sep=""))
    }
  }
}


timeseries <- function(startDate,no)
{
  sd <- as.Date(startDate)
  ts <- c()
  for(i in 0:(no-1))
  {
    t <- sd+i
    ts <- c(ts,as.character(t))
  }
  df <- data.frame(ye=as.numeric(format(as.Date(ts),"%Y")),mo=as.numeric(format(as.Date(ts),"%m")),day=as.numeric(format(as.Date(ts),"%d")))
  return (df)
  
}

numberOfDays <- function(date) {
  date <- as.Date(date)
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}


construct <- function(basename,abbrev,startingYear,endingYear,startingMonth,endingMonth,session)
{
  
  
  nc <- nc_open(paste(basename,"_",ab[3],".nc",sep="")) 
  tdf <- timeseries("1950-01-01",nc$dim[[3]]$len)
  val <- ncvar_get(nc,ab[3])
  nc_close(nc)
  n <- nc$dim[[1]]$len
  m <- nc$dim[[2]]$len
  
  prog <- 8/(n*m)
  
  diff <- 0
  for (ye in startingYear:endingYear)
    for (mo in startingMonth:endingMonth)
      diff <- diff + numberOfDays(paste(ye,"-",mo,"-28",sep=""))
  
  
  A <- array(rep(0,n*m*indno),c(n,m,indno))
  print("Mean of mean T")  
  for(i in 1:n)
  {
    for(j in 1:m)
    {
      
      progress <<- progress + prog
      updateProgressBar(session=session, id="pb", value=progress)
      
      vdf <- data.frame(tdf,value=val[i,j,])
      
      fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
      
      A[i,j,1] <- mean(fvdf)
      A[i,j,2] <- sum(fvdf[fvdf<=(-20)])/diff
      A[i,j,3] <- sum(fvdf[fvdf<=(-10) & fvdf>(-20)])/diff
      A[i,j,4] <- sum(fvdf[fvdf<=0 & fvdf>(-10)])/diff
      A[i,j,5] <- sum(fvdf[fvdf<=10 & fvdf>0])/diff
      A[i,j,6] <- sum(fvdf[fvdf<=20 & fvdf>10])/diff
      A[i,j,7] <- sum(fvdf[fvdf<=30 & fvdf>20])/diff
      A[i,j,8] <- sum(fvdf[fvdf>30])/diff
    }
  }
  print("Mean of min T")  
  nc <- nc_open(paste(basename,"_",ab[1],".nc",sep="")) 
  val <- ncvar_get(nc,ab[1])
  nc_close(nc)
  for(i in 1:n)
  {
    for(j in 1:m)
    {
      
      progress <<- progress + prog
      updateProgressBar(session=session, id="pb", value=progress)      
      
      vdf <- data.frame(tdf,value=val[i,j,])
      fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
      
      A[i,j,9] <- mean(fvdf)
      A[i,j,10] <- sum(fvdf[fvdf<=(-20)])/diff
      A[i,j,11] <- sum(fvdf[fvdf<=(-10) & fvdf>(-20)])/diff
      A[i,j,12] <- sum(fvdf[fvdf<=0 & fvdf>(-10)])/diff
      A[i,j,13] <- sum(fvdf[fvdf<=10 & fvdf>0])/diff
      A[i,j,14] <- sum(fvdf[fvdf>10])/diff
    }
  }
  print("Mean of max T")  
  nc <- nc_open(paste(basename,"_",ab[2],".nc",sep="")) 
  val1 <- ncvar_get(nc,ab[2])
  nc_close(nc)
  for(i in 1:n)
  {
    for(j in 1:m)
    {
      
      progress <<- progress + prog
      updateProgressBar(session=session, id="pb", value=progress)
      
      vdf <- data.frame(tdf,value=val1[i,j,])
      fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
      
      A[i,j,15] <- mean(fvdf)
      A[i,j,16] <- sum(fvdf[fvdf<(-10)])/diff
      A[i,j,17] <- sum(fvdf[fvdf<=0 & fvdf>(-10)])/diff
      A[i,j,18] <- sum(fvdf[fvdf<=10 & fvdf>0])/diff
      A[i,j,19] <- sum(fvdf[fvdf<=20 & fvdf>10])/diff
      A[i,j,20] <- sum(fvdf[fvdf<=30 & fvdf>20])/diff
      A[i,j,21] <- sum(fvdf[fvdf>30])/diff      
    }
  }  
  
  print("Mean of daily variation")  
  for(i in 1:n)
  {
    for(j in 1:m)
    {
      
      progress <<- progress + prog
      updateProgressBar(session=session, id="pb", value=progress)
      
      vdf <- data.frame(tdf,value=val1[i,j,]-val[i,j,])
      fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
      
      A[i,j,22] <- mean(fvdf)
      A[i,j,23] <- sum(fvdf[fvdf<5])/diff
      A[i,j,24] <- sum(fvdf[fvdf>=5 & fvdf<10])/diff
      A[i,j,25] <- sum(fvdf[fvdf>=10 & fvdf<15])/diff
      A[i,j,26] <- sum(fvdf[fvdf>=15 & fvdf<20])/diff
      A[i,j,27] <- sum(fvdf[fvdf>=20])/diff
    }
  }    
  
  
  print("Precipitation")  
  nc <- nc_open(paste(basename,"_",ab[4],".nc",sep="")) 
  val <- ncvar_get(nc,ab[4])
  nc_close(nc)
  for(i in 1:n)
  {
    for(j in 1:m)
    {
      
      progress <<- progress + prog
      updateProgressBar(session=session, id="pb", value=progress)
      
      vdf <- data.frame(tdf,value=val[i,j,])
      fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
      
      A[i,j,28] <- mean(fvdf)
      A[i,j,29] <- sum(fvdf[fvdf==0])/diff
      A[i,j,30] <- sum(fvdf)/(diff-A[i,j,29])
      A[i,j,31] <- sum(fvdf>=10)/diff
      A[i,j,32] <- sum(fvdf>=20)/diff     
    }
  }  
  
  
  return (A)
}


getRowColumn <- function(nc,lon,lat)
{
  llon <- ncvar_get(nc,nc$dim[[1]]$name)
  llat <- ncvar_get(nc,nc$dim[[2]]$name)
  r <- which(lon<=llon)[1]
  c <- which(lat<=llat)[1]
  return (c(r,c))
}


extract <- function(basename,abbrev,startingYear,endingYear,startingMonth,endingMonth,lon,lat,session)
{
  nc <- nc_open(paste(basename,"_",ab[3],".nc",sep="")) 
  tdf <- timeseries("1950-01-01",nc$dim[[3]]$len)
  p <- getRowColumn(nc,lon,lat)
  i <- p[1]
  j <- p[2]  
  val <- ncvar_get(nc,ab[3])
  nc_close(nc)
  n <- nc$dim[[1]]$len
  m <- nc$dim[[2]]$len
  diff <- 0
  for (ye in startingYear:endingYear)
    for (mo in startingMonth:endingMonth)
      diff <- diff + numberOfDays(paste(ye,"-",mo,"-28",sep=""))

  A <- c()
  
  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  print("Average T")  
  
  
  vdf <- data.frame(tdf,value=val[i,j,])
  fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
  
  A <- c(A,mean(fvdf),sum(fvdf[fvdf<=(-20)])/diff,sum(fvdf[fvdf<=(-10) & fvdf>(-20)])/diff,sum(fvdf[fvdf<=0 & fvdf>(-10)])/diff,sum(fvdf[fvdf<=10 & fvdf>0])/diff,sum(fvdf[fvdf<=20 & fvdf>10])/diff,sum(fvdf[fvdf<=30 & fvdf>20])/diff,sum(fvdf[fvdf>30])/diff)

  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  
  print("Min T")  
  nc <- nc_open(paste(basename,"_",ab[1],".nc",sep="")) 
  val <- ncvar_get(nc,ab[1])
  nc_close(nc)
  vdf <- data.frame(tdf,value=val[i,j,])
  fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
  
  A <- c(A, mean(fvdf),sum(fvdf[fvdf<=(-20)])/diff,sum(fvdf[fvdf<=(-10) & fvdf>(-20)])/diff,sum(fvdf[fvdf<=0 & fvdf>(-10)])/diff,sum(fvdf[fvdf<=10 & fvdf>0])/diff,sum(fvdf[fvdf>10])/diff)
  
  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  
  print("Max T")  
  nc <- nc_open(paste(basename,"_",ab[2],".nc",sep="")) 
  val1 <- ncvar_get(nc,ab[2])
  nc_close(nc)
  vdf <- data.frame(tdf,value=val1[i,j,])
  fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
  
  A <- c(A,mean(fvdf),sum(fvdf[fvdf<(-10)])/diff,sum(fvdf[fvdf<=0 & fvdf>(-10)])/diff,sum(fvdf[fvdf<=10 & fvdf>0])/diff,sum(fvdf[fvdf<=20 & fvdf>10])/diff,sum(fvdf[fvdf<=30 & fvdf>20])/diff,sum(fvdf[fvdf>30])/diff)

  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  
  print("Daily variation")  
  vdf <- data.frame(tdf,value=val1[i,j,]-val[i,j,])
  fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
  
  A <- c(A,mean(fvdf),sum(fvdf[fvdf<5])/diff,sum(fvdf[fvdf>=5 & fvdf<10])/diff,sum(fvdf[fvdf>=10 & fvdf<15])/diff,sum(fvdf[fvdf>=15 & fvdf<20])/diff,sum(fvdf[fvdf>=20])/diff)

  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  
  print("Precipitation")  
  nc <- nc_open(paste(basename,"_",ab[4],".nc",sep="")) 
  val <- ncvar_get(nc,ab[4])
  nc_close(nc)
  vdf <- data.frame(tdf,value=val[i,j,])
  fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
  
  A <- c(A,mean(fvdf),sum(fvdf[fvdf==0])/diff)
  A <- c(A,sum(fvdf)/(diff-A[29]))
  A <- c(A,sum(fvdf>=10)/diff, sum(fvdf>=20)/diff)

  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  
  return (A)
}

cosim <- function(A,B)
{
  return (sum(A*B)/(sqrt(sum(A^2))*sqrt(sum(B^2))))
}

simMap <- function(A,B)
{
  n <- length(A[,1,1])
  m <- length(A[1,,1])
  C <- array(rep(0,n*m),c(n,m))
  for(i in 1:n)
  {
    for(j in 1:m)
    {
      C[i,j] <- cosim(A[i,j,],B)
    }
  }
  return (C)
}

getLimits <- function(basename,abbrev)
{
  nc <- nc_open(paste(basename,"_",ab[1],".nc",sep="")) 
  lon <- nc$dim[[1]]$vals
  lat <- nc$dim[[2]]$vals
  nc_close(nc)
  return (c(min(lon)-0.05,min(lat)-0.05, max(lon)+0.05, max(lat)+0.05))
}

analyze <- function(loc,rect,basename,abbrev,target_startingYear,target_endingYear,target_startingMonth,target_endingMonth,base_startingYear,base_endingYear,base_startingMonth,base_endingMonth,lon,lat,name,session)
{
  
  
  indices <- construct(basename,abbrev,target_startingYear,target_endingYear,target_startingMonth,target_endingMonth,session)
  map <- simMap(indices,loc)
  r1=raster(t(map),xmn=rect[1],xmx=rect[3],ymn=rect[2],ymx=rect[4],crs=CRS("+init=epsg:4326"))
  
  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  return (r1)
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$style(type = "text/css","h2 {text-align: center; font-size: 20px; font-weight: bold;} 
                                  h4 {font-size:14px; color: darkblue; font-weight: bold;} 
                                  label {font-size: 12px;}  
                                  input {width: 100px !important;}
                                  .shinyDirectories {}
                                  #fname { width: 160px !important;}
                                  #banalyze, #bsave { width: 200px !important; background: darkblue; color: white;}
                                  #mymap {cursor: pointer;}
               "),
    
    titlePanel("ClimShift"),
    sidebarPanel(
      fluidRow(column(4,
        h4("Base period"),          
        numericInput("bsyear","Starting year",value=1961,min=1900,max=2022),  
        numericInput("bsmonth","Starting month",value=1,min=1,max=12),
        numericInput("bfyear","Ending year",value=1990,min=1900,max=2022),  
        numericInput("bfmonth","Ending month",value=12,min=1,max=12),
        h4("Base location"),
        numericInput("lon","Longitude",value=23.12,min=22,max=25,step=0.01),  
        numericInput("lat","Latitude",value=45.12,min=44,max=47,step=0.01)
      ),
      column(8,
             h4("Analysis period"),              
             numericInput("asyear","Starting year",value=1991,min=1900,max=2022),  
             numericInput("asmonth","Starting month",value=1,min=1,max=12),
             numericInput("afyear","Ending year",value=2021,min=1900,max=2022),  
             numericInput("afmonth","Ending month",value=12,min=1,max=12),
             h4("Data files"),
             textInput("fname","Filename template"),
             actionButton("folder", "Choose folder"),
             #shinyDirButton('folder', 'Állományok mappája', 'Válasszon mappát', FALSE),
             br(),
             hr(),
             br(),
             actionButton("banalyze", "Analyse!"),
             br(),
             br(),
             progressBar(id="pb",value=0, title="Progress", display_pct=T),
             textOutput("baseArea"),
             br(),
             textOutput("analysisArea"),
             br(),
             textOutput("commonArea"),
             br(),
             actionButton("bsave", "Save the results")
             )
    )),
    mainPanel(
      leafletOutput("mymap",height=900)
    )
    
   
  )
    
# Define server logic required to draw a histogram
server <- function(input, output, session) {


  
  observeEvent(input$folder, {
    fld <<- choose.dir()
  })
  
  
  observeEvent(input$banalyze, {
    lon <<- input$lon
    lat <<- input$lat
    progress <<- 0
    name <<- paste(fld,"\\",input$fname,sep="")
    progress <<- progress + 4
    updateProgressBar(session=session, id="pb", value=progress)
    loc <- extract(name,ab,input$bsyear,input$bfyear,input$bsmonth,input$bfmonth,lon,lat,session)
    
    rect <- getLimits(name,ab)
    
    progress <<- progress + 1
    updateProgressBar(session=session, id="pb", value=progress)
    
    base <<-analyze(loc,rect,name,ab,input$bsyear,input$bfyear,input$bsmonth,input$bfmonth,input$bsyear,input$bfyear,input$bsmonth,input$bfmonth,lon,lat,name,session)
    rez <<-analyze(loc,rect,name,ab,input$asyear,input$afyear,input$asmonth,input$afmonth,input$bsyear,input$bfyear,input$bsmonth,input$bfmonth,lon,lat,name,session)
    
    pal <- colorBin(c("#f7fbff","#9ecae1","#4292c6","#2171b5","#08519c","#01ff01"), bins=c(0,0.99,0.992,0.994,0.996,0.998,1), na.color = "transparent",pretty=FALSE)

    leafletProxy("mymap") %>% addRasterImage(flip(rez), group="Analysis_period", colors=pal, opacity = 0.5) 
    leafletProxy("mymap") %>% addRasterImage(flip(base), group="Base_period", colors=pal, opacity = 0.5)
    leafletProxy("mymap") %>%  addLegend(pal=pal, values = c(0,0.99,0.992,0.994,0.996,0.998,1), title = "Similarity index")
    
    
    cellno <- ncell(base)
    
    
    bArea <- length(which(values(base)>0.998 & values(base)<=1))
    
    updateProgressBar(session=session, id="pb", value=97)
    
    aArea <- length(which(values(rez)>0.998 & values(rez)<=1))
    
    updateProgressBar(session=session, id="pb", value=98)
    
    r <- (values(base)>0.998 & values(base)<=1) * (values(rez)>0.998 & values(rez)<=1)
    cArea <- sum(r,na.rm=TRUE)
    
    updateProgressBar(session=session, id="pb", value=99)
    
    output$baseArea <- renderText(paste("Base period cell number: ",bArea," (",round(bArea/cellno*100,1),"% of total cells)",sep=""))
    output$analysisArea <- renderText(paste("Analysed period cell number: ",aArea," (",round(aArea/cellno*100,1),"% of total cells)",sep=""))
    output$commonArea <- renderText(paste("Common cell number with highest similarity: ",cArea," (",round(cArea/bArea*100,1),"% of base period cells)",sep=""))
    
    updateProgressBar(session=session, id="pb", value=100)
    print("over")
  })
  
  observeEvent(input$bsave, {
    writeRaster(flip(rez),paste(name,"_lon",lon,"_lat",lat,"_B",input$bsyear,"_",input$bfyear,"_",input$bsmonth,"_",input$bfmonth,"_A",input$asyear,"_",input$afyear,"_",input$asmonth,"_",input$afmonth,sep=""),overwrite=TRUE)
    writeRaster(flip(base),paste(name,"_lon",lon,"_lat",lat,"_B",input$bsyear,"_",input$bfyear,"_",input$bsmonth,"_",input$bfmonth,sep=""),overwrite=TRUE)
  })
  
  observe({
    click = input$mymap_click
    if(is.null(click))
      return()
    
    updateNumericInput(session,"lon",value = click$lng)
    updateNumericInput(session,"lat",value = click$lat)
    leafletProxy("mymap") %>% clearMarkers()
    leafletProxy("mymap") %>% addCircleMarkers(lat=click$lat,lng=click$lng,fillColor="red",stroke =F,fillOpacity=1,radius=5)
    
    })
  
  output$mymap <- renderLeaflet({
    leaflet() %>% removeTiles(layerId = "rez")
    leaflet() %>% addTiles(group="OSM") %>%  addLayersControl(baseGroups = c("OSM"),overlayGroups = c("Base_period", "Analysis_period"),options = layersControlOptions(collapsed = FALSE))  %>% setView(20.960933,46.585830, zoom=7)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
