library(shiny)
library(shinyjs)
library(shinyFiles)
library(leaflet)
library(ncdf4)
library(rgdal)
library(raster)
library(shinyWidgets)
library(gsubfn)
library(lubridate)

#considered file postfix for minimum, maximum, mean temperature and for precipitation 
#data file format fileprefix_postfix.nc where fileprefix have to be the same for all files
ab <- c("tn","tx","tg","rr")

#number of considered indices
indno <- 32

#
areaBoundary <- c(0,0,0,0)
startYear <- 0
endYear <- 0
startMonth <- 1
endMonth <- 12

fld <- ""

pal <- colorBin(c("#f7fbff","#9ecae1","#4292c6","#2171b5","#08519c","#01ff01"), bins=c(0,0.99,0.992,0.994,0.996,0.998,1), na.color = "transparent",pretty=FALSE)

getMapLimits <- function(folder,basename)
{
  nc <- nc_open(paste(folder,'/',basename,"_tx.nc",sep="")) 
  lon <- ncvar_get(nc,nc$dim[[1]]$name)
  lat <- ncvar_get(nc,nc$dim[[2]]$name)
  nc_close(nc)
  areaBoundary <<- c(min(lat),max(lat),min(lon),max(lon))
}

getDataInterval <- function(folder,basename)
{
  nc <- nc_open(paste(folder,'/',basename,"_tx.nc",sep="")) 
  d1 <- strapplyc(nc$dim[[3]]$units, "[0-9-]{8,}", simplify = TRUE)
  days <- nc$dim[[3]]$len
  d2 <- as.Date(d1) + days
  nc_close(nc)
  startYear <<- year(d1)
  endYear <<- year(d2)
}

checkAllFiles <- function(folder,mask)
{
  ok <- TRUE
  for(i in 1:4)
    ok <- ok & file.exists(paste(folder,"/",mask,"_",ab[i],".nc",sep=""))
  return (ok)
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
  return (c(r,c-1))
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
  
  vdf <- data.frame(tdf,value=val[i,j,])
  fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
  
  A <- c(A,mean(fvdf),sum(fvdf[fvdf<=(-20)])/diff,sum(fvdf[fvdf<=(-10) & fvdf>(-20)])/diff,sum(fvdf[fvdf<=0 & fvdf>(-10)])/diff,sum(fvdf[fvdf<=10 & fvdf>0])/diff,sum(fvdf[fvdf<=20 & fvdf>10])/diff,sum(fvdf[fvdf<=30 & fvdf>20])/diff,sum(fvdf[fvdf>30])/diff)

  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  
  nc <- nc_open(paste(basename,"_",ab[1],".nc",sep="")) 
  val <- ncvar_get(nc,ab[1])
  nc_close(nc)
  vdf <- data.frame(tdf,value=val[i,j,])
  fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
  
  A <- c(A, mean(fvdf),sum(fvdf[fvdf<=(-20)])/diff,sum(fvdf[fvdf<=(-10) & fvdf>(-20)])/diff,sum(fvdf[fvdf<=0 & fvdf>(-10)])/diff,sum(fvdf[fvdf<=10 & fvdf>0])/diff,sum(fvdf[fvdf>10])/diff)
  
  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  
  nc <- nc_open(paste(basename,"_",ab[2],".nc",sep="")) 
  val1 <- ncvar_get(nc,ab[2])
  nc_close(nc)
  vdf <- data.frame(tdf,value=val1[i,j,])
  fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
  
  A <- c(A,mean(fvdf),sum(fvdf[fvdf<(-10)])/diff,sum(fvdf[fvdf<=0 & fvdf>(-10)])/diff,sum(fvdf[fvdf<=10 & fvdf>0])/diff,sum(fvdf[fvdf<=20 & fvdf>10])/diff,sum(fvdf[fvdf<=30 & fvdf>20])/diff,sum(fvdf[fvdf>30])/diff)

  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  
  vdf <- data.frame(tdf,value=val1[i,j,]-val[i,j,])
  fvdf <- vdf$value[vdf$ye>=startingYear & vdf$ye<=endingYear & vdf$mo>=startingMonth & vdf$mo<=endingMonth]
  
  A <- c(A,mean(fvdf),sum(fvdf[fvdf<5])/diff,sum(fvdf[fvdf>=5 & fvdf<10])/diff,sum(fvdf[fvdf>=10 & fvdf<15])/diff,sum(fvdf[fvdf>=15 & fvdf<20])/diff,sum(fvdf[fvdf>=20])/diff)

  progress <<- progress + 1
  updateProgressBar(session=session, id="pb", value=progress)
  
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
    useShinyjs(),
    tags$style(type = "text/css","h2 {text-align: center; font-size: 20px; font-weight: bold;} 
                                  h4 {font-size:14px; color: darkblue; font-weight: bold;} 
                                  label {font-size: 12px;}  
                                  input {width: 100px !important;}
                                  .shinyDirectories {}
                                  #fname { width: 160px !important;}
                                  #banalyze, #bsave { width: 200px !important; background: darkblue; color: white;}
                                  #mymap {cursor: pointer;}
               "),
    
    titlePanel("C L I M S H I F T"),
    sidebarPanel(
      fluidRow(column(4,
        h4("Base period"),          
        numericInput("bsyear","Starting year",value=1961,min=1900,max=2022),  
        numericInput("bsmonth","Starting month",value=1,min=1,max=12),
        numericInput("bfyear","Ending year",value=1990,min=1900,max=2022),  
        numericInput("bfmonth","Ending month",value=12,min=1,max=12),
        h4("Base location"),
        numericInput("lon","Longitude",value=23.12,min=22,max=25,step=0.01),  
        numericInput("lat","Latitude",value=45.12,min=44,max=47,step=0.01),
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
  
  shinyjs::disable("bsyear")
  shinyjs::disable("bfyear")
  shinyjs::disable("bsmonth")
  shinyjs::disable("bfmonth")
  shinyjs::disable("asyear")
  shinyjs::disable("afyear")
  shinyjs::disable("asmonth")
  shinyjs::disable("afmonth")
  shinyjs::disable("lon")
  shinyjs::disable("lat")
  shinyjs::disable("banalyze")
  shinyjs::disable("bsave")
  shinyjs::disable("fname")
  
  
  observeEvent(input$folder, {
    fld <<- choose.dir()
    updateTextInput(session=session,inputId="fname", value="")
    if (!is.na(fld))
    {
      shinyjs::enable("fname")
    }
    else
    {
      shinyjs::disable("fname")
    }
  })
  
  observeEvent(input$fname, {
    if (checkAllFiles(fld,input$fname))
    {
      getMapLimits(fld,input$fname)
      updateNumericInput(session=session,inputId="lat",value=round(mean(areaBoundary[1:2]),digits=2))
      updateNumericInput(session=session,inputId="lon",value=round(mean(areaBoundary[3:4]),digits=2))
      
      leafletProxy("mymap") %>% addRectangles(lng1=areaBoundary[3], lat1=areaBoundary[1], lng2=areaBoundary[4], lat2=areaBoundary[2], fillColor = "transparent",layerId="boundary")
      
      getDataInterval(fld,input$fname)
      
      updateNumericInput(session=session,inputId="bsyear",value=startYear)
      updateNumericInput(session=session,inputId="asyear",value=startYear)
      updateNumericInput(session=session,inputId="bfyear",value=endYear)
      updateNumericInput(session=session,inputId="afyear",value=endYear)
      
      updateNumericInput(session=session,inputId="bsmonth",value=startMonth)
      updateNumericInput(session=session,inputId="asmonth",value=startMonth)
      updateNumericInput(session=session,inputId="bfmonth",value=endMonth)
      updateNumericInput(session=session,inputId="afmonth",value=endMonth)
      
            
      shinyjs::enable("bsyear")
      shinyjs::enable("bfyear")
      shinyjs::enable("bsmonth")
      shinyjs::enable("bfmonth")
      shinyjs::enable("asyear")
      shinyjs::enable("afyear")
      shinyjs::enable("asmonth")
      shinyjs::enable("afmonth")
      shinyjs::enable("lon")
      shinyjs::enable("lat")
      shinyjs::enable("banalyze") 
    }
    else
    {
      leafletProxy("mymap") %>% removeShape("boundary")
      
      shinyjs::disable("bsyear")
      shinyjs::disable("bfyear")
      shinyjs::disable("bsmonth")
      shinyjs::disable("bfmonth")
      shinyjs::disable("asyear")
      shinyjs::disable("afyear")
      shinyjs::disable("asmonth")
      shinyjs::disable("afmonth")
      shinyjs::disable("lon")
      shinyjs::disable("lat")   
      shinyjs::disable("banalyze") 
    }
  })
  
  
  listenCoord <-function()
  {
    list(input$lat,input$lon)
  }
  
  observeEvent(listenCoord(),
               {
                 if (!is.na(input$lon))
                 if (input$lat>=areaBoundary[1] & input$lat<=areaBoundary[2] & input$lon>=areaBoundary[3] & input$lon<=areaBoundary[4])
                 {
                   leafletProxy("mymap") %>% clearMarkers() 
                   leafletProxy("mymap") %>% addCircleMarkers(lat=input$lat,lng=input$lon,fillColor="red",stroke =F,fillOpacity=1,radius=5)
                   shinyjs::enable("banalyze") 
                 }
                 else
                 {
                   leafletProxy("mymap") %>% clearMarkers() 
                   shinyjs::disable("banalyze") 
                 } 
               })
  
  observeEvent(input$banalyze, {
    
    output$baseArea <- renderText("")
    output$analysisArea <- renderText("")
    output$commonArea <- renderText("")
    
    leafletProxy("mymap") %>% clearGroup("Analysis_period")
    leafletProxy("mymap") %>% clearGroup("Base_period")
    
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
    
    leafletProxy("mymap") %>% addRasterImage(flip(rez), group="Analysis_period", colors=pal, opacity = 0.5) 
    leafletProxy("mymap") %>% addRasterImage(flip(base), group="Base_period", colors=pal, opacity = 0.5)

    
    cellno <- ncell(base)
    
    
    bArea <<- length(which(values(base)>0.998 & values(base)<=1))
    
    updateProgressBar(session=session, id="pb", value=97)
    
    aArea <<- length(which(values(rez)>0.998 & values(rez)<=1))
    
    updateProgressBar(session=session, id="pb", value=98)
    
    r <- (values(base)>0.998 & values(base)<=1) * (values(rez)>0.998 & values(rez)<=1)
    cArea <<- sum(r,na.rm=TRUE)
    
    updateProgressBar(session=session, id="pb", value=99)
    
    output$baseArea <- renderText(paste("Base period cell number: ",bArea," (",round(bArea/cellno*100,1),"% of total cells)",sep=""))
    output$analysisArea <- renderText(paste("Analysed period cell number: ",aArea," (",round(aArea/cellno*100,1),"% of total cells)",sep=""))
    output$commonArea <- renderText(paste("Common cell number with highest similarity: ",cArea," (",round(cArea/bArea*100,1),"% of base period cells)",sep=""))
    
    updateProgressBar(session=session, id="pb", value=100)

    shinyjs::enable("bsave") 
    
  })
  
  observeEvent(input$bsave, {
    writeRaster(flip(rez),paste(name,"_lon",lon,"_lat",lat,"_B",input$bsyear,"_",input$bfyear,"_",input$bsmonth,"_",input$bfmonth,"_A",input$asyear,"_",input$afyear,"_",input$asmonth,"_",input$afmonth,sep=""),overwrite=TRUE)
    writeRaster(flip(base),paste(name,"_lon",lon,"_lat",lat,"_B",input$bsyear,"_",input$bfyear,"_",input$bsmonth,"_",input$bfmonth,sep=""),overwrite=TRUE)
    
    fileConn<-file(paste(name,"_lon",lon,"_lat",lat,"_B",input$bsyear,"_",input$bfyear,"_",input$bsmonth,"_",input$bfmonth,"_A",input$asyear,"_",input$afyear,"_",input$asmonth,"_",input$afmonth,".txt",sep=""))
    writeLines(c(paste("Base period cell number:",bArea,sep=""),paste("Analysed period cell number:",aArea,sep=""),paste("Common cell number with highest similarity:",cArea,sep="")), fileConn)
    close(fileConn)
    
    shinyjs::disable("bsave") 
    
    })
  

  
  observe({
    click = input$mymap_click
    if(is.null(click))
      return()
    
    if (click$lat>=areaBoundary[1] & click$lat<=areaBoundary[2] & click$lng>=areaBoundary[3] & click$lng<=areaBoundary[4])
    {
    
    updateNumericInput(session,"lon",value = round(click$lng,digits=2))
    updateNumericInput(session,"lat",value = round(click$lat,digits=2))
    leafletProxy("mymap") %>% clearMarkers()
    leafletProxy("mymap") %>% addCircleMarkers(lat=click$lat,lng=click$lng,fillColor="red",stroke =F,fillOpacity=1,radius=5)
    }
    })
  
  listenPeriod <-function()
  {
    list(input$bsyear,input$bfyear,input$bsmonth,input$bfmonth,input$asyear,input$afyear,input$asmonth,input$afmonth)
  }
  
  observeEvent(listenPeriod(),
               {
                 if (input$bsyear>=startYear & input$bsyear<=endYear & input$asyear>=startYear & input$asyear<=endYear &
                     input$bfyear>=startYear & input$bfyear<=endYear & input$afyear>=startYear & input$afyear<=endYear &
                     input$bsmonth>=startMonth & input$bsmonth<=endMonth & input$asmonth>=startMonth & input$asmonth<=endMonth &
                     input$bfmonth>=startMonth & input$bfmonth<=endMonth & input$afmonth>=startMonth & input$afmonth<=endMonth
                     )
                 {
                   shinyjs::enable("banalyze") 
                 }
                 else
                 {
                   shinyjs::disable("banalyze")  
                 }
               })
  

  output$mymap <- renderLeaflet({
    #leaflet() %>% removeTiles(layerId = "rez")
    leaflet() %>% addTiles(group="OSM") %>%  addLayersControl(baseGroups = c("OSM"),overlayGroups = c("Base_period", "Analysis_period"),options = layersControlOptions(collapsed = FALSE))  %>% setView(20.960933,46.585830, zoom=7) %>%  addLegend(pal=pal, values = c(0,0.99,0.992,0.994,0.996,0.998,1), title = "Similarity index")

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server,options=list(launch.browser=TRUE))
