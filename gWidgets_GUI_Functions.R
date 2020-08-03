#######################################################################################################################
# R gWidget functions
# Created by: Melissa Kimble
# Last Modified: 06/27/2017
#######################################################################################################################
## Libraries
LibraryList<-c("gWidgets","gWidgetstcltk")
for (TheLibrary in LibraryList)
{
  if(TheLibrary %in% rownames(installed.packages()) == FALSE) install.packages(TheLibrary)
  
}
library(gWidgets)
library(gWidgetstcltk) #or gWidgetsGtk2 or gWidgetsrJava or gWidgetsWWW or gWidgetsQt
#install.packages("tkrplot")
#library(tkrplot)
#install.packages("tcltk")
######################################################################################
TheGUI<-function()
{
  options(guiToolkit="tcltk")
  
  # global vars
  TheOutputSPIFolderPath <- NULL                           
  OutputDailyPPTFolderPath <- NULL
  # environment
  TheLocalEnviron <- new.env()                          
  ## The GUI Functions
  #choose.files()
  SetDailyPptFolderPath<-function(h,...) {
    SelectedDailyPPTFolderPath <- gfile(type="selectdir")
    gmessage(paste0("Input directory set to ",SelectedDailyPPTFolderPath))
    ## assign via <<-
    OutputDailyPPTFolderPath <<- SelectedDailyPPTFolderPath
    ## assign to an environment
    TheLocalEnviron$OutputDailyPPTFolderPath <- SelectedDailyPPTFolderPath
  }
  SetSPIFolderPath<-function(h,...) {
    SelectedOutputSPIFolderPath <- gfile(type="selectdir")
    gmessage(paste0("Input directory set to ",SelectedOutputSPIFolderPath))
    ## assign via <<-
    TheOutputSPIFolderPath <<- SelectedOutputSPIFolderPath
    ## assign to an environment
    TheLocalEnviron$TheOutputSPIFolderPath <- SelectedOutputSPIFolderPath
  }
  SetSelectedPpt<-function(h,...) {
    TheOutput<-sprintf("%s", svalue(ThePptCheckBox))
    if (TheOutput==FALSE) DownloadStationData<-"no"
    else DownloadStationData<-"yes"
    #print(TheOutput);message("DL: ",DownloadStationData)
    return(DownloadStationData)
  }
  SetSelectedSPI<-function(h,...) {
    TheOutput<-sprintf("%s", svalue(TheSPICheckBox))
    if (TheOutput==FALSE) CalculateSPI<-"no"
    else CalculateSPI<-"yes"
    #print(TheOutput);message("SPI: ",CalculateSPI)
    return(CalculateSPI)
  }
  SetTheNOAAKey<-function(h,...) {
    TheNOAAKey<-sprintf("%s",svalue(TheNOAAKeyTxt))
    #print(TheNOAAKey)
    return(TheNOAAKey)
  }
  SetSelectedStations<-function(h,...) {
    #TheStationsCat<-paste(svalue(TheStationsTbl),collapse=", ")
    TheStationsCat<-sprintf("%s",svalue(TheStationNamesTbl))
    
    Stations<-c("GHCND:VQW00011640","GHCND:VQC00677600","GHCND:VQC00671980","GHCND:VQC00672551",
                "GHCND:VQW00011624","GHCND:VQC00672560","GHCND:VQC00671740","GHCND:VQC00674900")
    StationNames<-c("STTAirport","STTRedhook","STJCruzBay","STJEastEnd","STXChristainstedAirport",
                    "STXEastHill","STXChristainstedFort","STXMontpellier")
    StationDF<-data.frame(Stations=Stations,StationNames=StationNames)
    StationDF$Stations<-as.character(StationDF$Stations)
    StationDF$StationNames<-as.character(StationDF$StationNames)
    
    TheStationsList<-vector()
    i=1
    for (Station in TheStationsCat)
    {
      #Station="STTAirport"
      TheStationsList[i]<-StationDF$Stations[StationDF$StationNames==Station]
      i=i+1
    }

    #print(TheStationsList)
    return(TheStationsList)
  }
  SetSPIInterval<-function(h,...){
    SPIInterval<-sprintf("%s",svalue(SPIIntervalDrpList))
    SPIIntervalVal<-gsub("SPI", "", SPIInterval)
    SPIIntervalVal<-as.numeric(SPIIntervalVal)
    #TheType<-typeof(SPIIntervalVal)
    #print(paste(SPIIntervalVal,TheType))
    return(SPIIntervalVal)
  }
  TheOKButton<-function(h,...) {
    ## The Input NOAA Key
    TheNOAAKey<-SetTheNOAAKey()
    ## CalculateSPI, Yes or No
    CalculateSPI<-SetSelectedSPI()
    ## MinSPIYear
    MinSPIYear<-sprintf("%s",svalue(MinSPIYrTxt))
    ## MaxSPIYear
    MaxSPIYear<-sprintf("%s",svalue(MaxSPIYrTxt))
    ## Percent monthly coverage cut-off for running SPI. I.e., if you set your cut-off to 50
    ## than SPI calculations will be included for all months that had greater than or equal to months
    ## with ppt data for half of the days in that month.
    PercCovSPICutoff<-sprintf("%s",svalue(PercCovSPICutoffTxt))
    
    SPIInterval<-SetSPIInterval()
 
    ## the stations to compute SPI
    TheStations<-SetSelectedStations()
    ## Download station data, yes or no
    DownloadStationData<-SetSelectedPpt()

    ## output folder path for SPI plots and data files
    if (!(is.null(TheOutputSPIFolderPath))) TheOutputSPIFolderPath<-paste(TheOutputSPIFolderPath,"/",sep="")
    ## the output folder path for downloaded ppt data
    if (!(is.null(OutputDailyPPTFolderPath))) OutputDailyPPTFolderPath<-paste(OutputDailyPPTFolderPath,"/",sep="")
    if (length(TheStations)==0) print("Please select station(s)")
      
    if (DownloadStationData=="no" & CalculateSPI=="no")
    {
      print("No options selected")
    } else if (DownloadStationData=="yes" & is.null(OutputDailyPPTFolderPath) & CalculateSPI=="no") {
      NOAAMsg<-""
      if (TheNOAAKey=="Enter NOAA Key Here")
      {
        # check if there is a NOAA Key Script breaks if absent. 
        browseURL("https://www.ncdc.noaa.gov/cdo-web/token ", browser = getOption("browser"),
                  encodeIfNeeded = FALSE)
        NOAAMsg<-" and enter Personal NOAA Key"
      }
      print(paste("Please select a folder for precipitation data",NOAAMsg,sep=""))
      
    } else if (CalculateSPI=="yes" & is.null(TheOutputSPIFolderPath) & DownloadStationData=="no") {
      print("Please select a folder for SPI data")
    } else if (DownloadStationData=="yes" & is.null(OutputDailyPPTFolderPath) & CalculateSPI=="yes" & is.null(TheOutputSPIFolderPath)) {
      NOAAMsg<-""
      if (TheNOAAKey=="Enter NOAA Key Here")
      {
        # check if there is a NOAA Key Script breaks if absent. 
        browseURL("https://www.ncdc.noaa.gov/cdo-web/token ", browser = getOption("browser"),
                  encodeIfNeeded = FALSE)
        NOAAMsg<-" and enter Personal NOAA Key"
      }
      print(paste("Please select a folder for precipitation and SPI data",NOAAMsg,sep=""))
      
    } else {
      dispose(TheGWindow)
      ClimateDataAPIDailyGUI(TheNOAAKey,CalculateSPI,MinSPIYear,MaxSPIYear,SPIInterval,PercCovSPICutoff,
                             TheOutputSPIFolderPath,TheStations,DownloadStationData,OutputDailyPPTFolderPath)
    }
  }
  ######################################################################################
  ## The GUI
  TheGWindow <- gwindow(title="The SPI and NOAA API GUI", visible=TRUE)
  TheGrpStations <- ggroup(horizontal=FALSE, container=TheGWindow)
  
  TheStationNames<-c("STTAirport","STTRedhook","STJCruzBay","STJEastEnd","STXChristainstedAirport",
                  "STXEastHill","STXChristainstedFort","STXMontpellier")

  glabel("Select Station(s):",container=TheGrpStations)
  TheStationNamesTbl <- gtable(TheStationNames, chosencol = 1, multiple = TRUE, container=TheGrpStations, expand=TRUE)
  size(TheStationNamesTbl) <- c(200,200)
  TheGrpCheckBox1 <- ggroup(container=TheGWindow)
  TheCheckBoxTbl1<-glayout(container=TheGrpCheckBox1,spacing=2)
  TheCheckBoxTbl1[1,1]<-"Would you like to download station data?"
  TheCheckBoxTbl1[2,1]<-(ThePptCheckBox<-gcheckbox("DownloadStationData",checked=TRUE,handler = NULL,container=TheCheckBoxTbl1))
  TheCheckBoxTbl1[2,2]<-(ThePptGrp<-ggroup(container=TheCheckBoxTbl1))
  OutputPptButton<-gbutton("Daily Ppt Output Folder", container=ThePptGrp,handler = SetDailyPptFolderPath)

  TheCheckBoxTbl1[3,1]<-"Personal NOAA Key:"
  TheCheckBoxTbl1[4,1]<-(PptTxtGrp1<-ggroup(container=TheCheckBoxTbl1))
  TheNOAAKeyTxt<-gedit("Enter NOAA Key Here", width=35, expand=TRUE, container=PptTxtGrp1, handler = NULL)
  
  TheGrpCheckBox2 <- ggroup(container=TheGWindow)
  TheCheckBoxTbl2<-glayout(container=TheGrpCheckBox2,spacing=2)
  TheCheckBoxTbl2[5,1]<-"Would you like to download calculate SPI?"
  TheCheckBoxTbl2[6,1]<-(TheSPICheckBox<-gcheckbox("CalculateSPI",checked=TRUE,handler = NULL,container=TheCheckBoxTbl2))
  TheCheckBoxTbl2[6,2]<-(TheSPIButtongrp<-ggroup(container=TheCheckBoxTbl2))
  SPIButton<-gbutton("SPI Output Folder", container=TheSPIButtongrp, handler = SetSPIFolderPath) 
  TheCheckBoxTbl2[6,3]<-(TheSPIPptButtongrp<-ggroup(container=TheCheckBoxTbl2,visible=FALSE))
  InputPptButton<-gbutton("Daily Ppt Input Folder", container=TheSPIPptButtongrp,handler = SetDailyPptFolderPath)
  delete(TheSPIPptButtongrp,InputPptButton)
  
  TheCheckBoxTbl2[8,1]<-"Min SPI Year:"
  TheCheckBoxTbl2[9,1]<-(SPITxtGrp1<-ggroup(container=TheCheckBoxTbl2))
  MinSPIYrTxt <- gedit("1980", width=10, container=SPITxtGrp1, expand=TRUE, handler = NULL)
  TheCheckBoxTbl2[8,2]<-"Max SPI Year:"
  TheCheckBoxTbl2[9,2]<-(SPITxtGrp2<-ggroup(container=TheCheckBoxTbl2))
  MaxSPIYrTxt <- gedit("2016", width=10, container=SPITxtGrp2, expand=TRUE, handler = NULL)
  TheCheckBoxTbl2[10,1]<-"Select SPI Interval:"
  TheCheckBoxTbl2[11,1]<- (SPIDrpGrp1<-ggroup(container=TheCheckBoxTbl2))
  SPIIntervalDrpList <- gdroplist(c("SPI1","SPI2","SPI3","SPI4","SPI5","SPI6","SPI7","SPI8",
                                    "SPI9","SPI10", "SPI11","SPI12","SPI24","SPI48"), container = SPIDrpGrp1)
  TheCheckBoxTbl2[10,2]<-"Percent Cut-off for SPI Monthly PPT Coverage:"
  TheCheckBoxTbl2[11,2]<-(SPITxtGrp3<-ggroup(container=TheCheckBoxTbl2))
  PercCovSPICutoffTxt<-gedit("80", width=5, container=SPITxtGrp3, expand=TRUE, handler = NULL)
  
  addHandlerClicked(ThePptCheckBox, handler=function(h,...) {
    ThePptCheckBoxValue<-svalue(ThePptCheckBox)
    TheSPICheckBoxValue<-svalue(TheSPICheckBox)
    #print(paste(ThePptCheckBoxValue,TheSPICheckBoxValue))
    
    if(ThePptCheckBoxValue==TRUE & TheSPICheckBoxValue==TRUE) {
      add(ThePptGrp,OutputPptButton)
      add(PptTxtGrp1,TheNOAAKeyTxt)
      delete(TheSPIPptButtongrp,InputPptButton)
    } else if (ThePptCheckBoxValue==FALSE & TheSPICheckBoxValue==TRUE) {
      delete(ThePptGrp,OutputPptButton)
      delete(PptTxtGrp1,TheNOAAKeyTxt)
      add(TheSPIPptButtongrp,InputPptButton)
    } else if (ThePptCheckBoxValue==TRUE & TheSPICheckBoxValue==FALSE) {
      add(ThePptGrp,OutputPptButton)
      add(PptTxtGrp1,TheNOAAKeyTxt)
      delete(TheSPIPptButtongrp,InputPptButton)
    } else if (ThePptCheckBoxValue==FALSE & TheSPICheckBoxValue==FALSE) {
      delete(ThePptGrp,OutputPptButton)
      delete(PptTxtGrp1,TheNOAAKeyTxt)
      delete(TheSPIPptButtongrp,InputPptButton)
    }
  })
  addHandlerClicked(TheSPICheckBox, handler=function(h,...) {
    TheSPICheckBoxValue<-svalue(TheSPICheckBox)
    ThePptCheckBoxValue<-svalue(ThePptCheckBox)
    #print(paste(TheSPICheckBoxValue,ThePptCheckBoxValue))
    
    if(ThePptCheckBoxValue==TRUE & TheSPICheckBoxValue==TRUE) {
      add(TheSPIButtongrp,SPIButton)
      add(SPITxtGrp1,MinSPIYrTxt)
      add(SPITxtGrp2,MaxSPIYrTxt)
      add(SPITxtGrp3,PercCovSPICutoffTxt)
      add(SPIDrpGrp1,SPIIntervalDrpList)
      delete(TheSPIPptButtongrp,InputPptButton)
    } else if (ThePptCheckBoxValue==FALSE & TheSPICheckBoxValue==TRUE) {
      add(TheSPIButtongrp,SPIButton)
      add(SPITxtGrp1,MinSPIYrTxt)
      add(SPITxtGrp2,MaxSPIYrTxt)
      add(SPITxtGrp3,PercCovSPICutoffTxt)
      add(SPIDrpGrp1,SPIIntervalDrpList)
      add(TheSPIPptButtongrp,InputPptButton)
    } else if (ThePptCheckBoxValue==TRUE & TheSPICheckBoxValue==FALSE) {
      delete(TheSPIButtongrp,SPIButton)
      delete(SPITxtGrp1,MinSPIYrTxt)
      delete(SPITxtGrp2,MaxSPIYrTxt)
      delete(SPITxtGrp3,PercCovSPICutoffTxt)
      delete(SPIDrpGrp1,SPIIntervalDrpList)
      delete(TheSPIPptButtongrp,InputPptButton)
    } else if (ThePptCheckBoxValue==FALSE & TheSPICheckBoxValue==FALSE) {
      delete(TheSPIButtongrp,SPIButton)
      delete(SPITxtGrp1,MinSPIYrTxt)
      delete(SPITxtGrp2,MaxSPIYrTxt)
      delete(SPITxtGrp3,PercCovSPICutoffTxt)
      delete(SPIDrpGrp1,SPIIntervalDrpList)
      delete(TheSPIPptButtongrp,InputPptButton)
    }
  })
  TheFinalButtons <- ggroup(container=TheGWindow)
  addSpring(TheFinalButtons)
  gbutton("Ok", container=TheFinalButtons,handler = TheOKButton)
  gbutton("dismiss", container=TheFinalButtons, handler = function(h,...) dispose(TheGWindow))
}  

