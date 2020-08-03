# IMPORTANT!
# You need a token to access the data. Go to: https://www.ncdc.noaa.gov/cdo-web/token 
#
# GUI/Script to:
# 1) download GHCND data (accumulated daily precipitation) via rnoaa API
# 2) aggregate to daily totals 
# 3) compute/add new field for % coverage within each month
#     *takes into account variation in total month days and leap year
# 4) format the data
# 5) run the SPEI::spi function based on the specified SPI Interval (1mo, 2mo, etc)
# 6) write the outputs to CSVs & PNGs
#
# Created by: mkimble
# Last Modified: 06/27/2017
# helpful info on rnoaa package: https://ropensci.org/blog/2014/03/13/rnoaa/
######################################################################################

#################################################################################
# Libraries                                                                     #
#################################################################################
LibraryList<-c("rnoaa","digest","rstudioapi")
for (TheLibrary in LibraryList)
{
  if(TheLibrary %in% rownames(installed.packages()) == FALSE) install.packages(TheLibrary)
  
}
library(rstudioapi)
library(rnoaa)
###################################################################################
# Additional files (classes) that have functions that are used in this script.    #
###################################################################################
TheSourceDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
source(paste(TheSourceDir,"/ClimateData_SPI_GUI_Functions.R",sep=""))
source(paste(TheSourceDir,"/gWidgets_GUI_Functions.R",sep=""))

#################################################################################################################################
#################################################################################################################################
# END OF REQUIRED INPUT FIELDS, DONT CHANGE UNLESS YOU KNOW WHAT YOU'RE DOING! (OR SAVE A COPY BEFOREHAND ...)###################
#################################################################################################################################
#################################################################################################################################

#################################################################################
# SPI Option - Would you like to run it?   yes or no                            #
# if yes, specify the following:                                                #
# *PercCovSPICutoff = The cut-off % for months with missing days.               # 
#  1 is equivalent to months with 100% daily precip coverage.                   #
# *MinSPIYear/MaxSPIYear = the year ranges                                      #
# *FileName, PlotTitle, SPIPlotTitle, TheSPIFilePath, TheOutputSPIFilePath =    #
#  These are all components of all of the output files and the plots that SPI   #
#  generates. Keep in mind that this script does not automatically save the     #
#  SPI plots, those must be saved manually!!!!                                  #
#################################################################################
# Summary information                                                                           %Coverage  All : 1980-2015
#TheStation<- "GHCND:VQW00011640";FileName<-"STTAirport" # STTAirport 1953-2016 -                            81% : 91.67%
#TheStation<- "GHCND:VQC00677600";FileName<-"STTRedhook" # STTRedhook 1980-2016 -                            68% : 82.41%
#TheStation<- "GHCND:VQC00671980";FileName<-"STJCruzBay" # STJCruzBay 1972-2016 -                            83% : 93.29%
#TheStation<- "GHCND:VQC00672551";FileName<-"STJEastEnd" # STJEastEnd 1972-2016 -                            84% : 84.26%
#TheStation<- "GHCND:VQW00011624";FileName<-"STXChristainstedAirport" # STXChristainstedAirport 1951-2016  - 86% : 98.38%
#TheStation<- "GHCND:VQC00672560";FileName<-"STXEastHill" # STXEastHill 1972-2016 -                          94% : 92.82%
#TheStation<-"GHCND:VQC00671740";FileName<-"STXChristainstedFort" # STXChristainstedFort 1921-2016 -         48% : 71.53%
#TheStation<-"GHCND:VQC00674900";FileName<-"STXMontpellier" # STXMontpellier 1979-2016 -                     90% : 93.98%
##########################################################################################################################

TheGUI()
ClimateDataAPIDailyGUI<-function(TheNOAAKey,CalculateSPI,MinSPIYear,MaxSPIYear,SPIInterval,PercCovSPICutoff,
                                 TheOutputSPIFolderPath,TheStations,DownloadStationData,OutputDailyPPTFolderPath)
{
  TheNOAAKey<-as.character(TheNOAAKey)
  #message("TheNOAAKey: ",TheNOAAKey, " | TheType: ",typeof(TheNOAAKey))
  
  CalculateSPI<-as.character(CalculateSPI)
  #message("CalculateSPI: ", CalculateSPI, " | TheType: ",typeof(CalculateSPI))
  TheOutputSPIFolderPath<-as.character(TheOutputSPIFolderPath)
  #message("The SPI Data Folder Path: ",TheOutputSPIFolderPath, " | TheType: ",typeof(TheOutputSPIFolderPath))
  
  
  MinSPIYear<-as.numeric(MinSPIYear)
  #message("Min SPI Year: ", MinSPIYear, " | TheType: ",typeof(MinSPIYear))
  
  MaxSPIYear<-as.numeric(MaxSPIYear)
  #message("Max SPI Year: ",MaxSPIYear, " | TheType: ",typeof(MaxSPIYear))
  
  PercCovSPICutoff<-as.numeric(PercCovSPICutoff)
  #message("Percent Cut-off: ",PercCovSPICutoff, "| TheType: ",typeof(PercCovSPICutoff))
  
 
  DownloadStationData<-as.character(DownloadStationData)
  OutputDailyPPTFolderPath<-as.character(OutputDailyPPTFolderPath)
  #message("DownloadStationData: ",DownloadStationData, " | TheType: ",typeof(DownloadStationData),
  #        " | The PPT Data Folder Path: ",OutputDailyPPTFolderPath, " | TheType: ",typeof(OutputDailyPPTFolderPath))
  message("The Stations: ",TheStations, " | TheType: ",typeof(TheStations))
  print(TheStations)

  options(noaakey=TheNOAAKey)

  ## checks if the OutputDataFolderPath exists, if it doesn't it creates it.
  DirectoryExists<-dir.exists(file.path(TheOutputSPIFolderPath))
  if (DirectoryExists==FALSE) dir.create(file.path(TheOutputSPIFolderPath), recursive = TRUE)
  #################################################################################
  # Setup the precip data file locations, filename, and output log file           #
  #################################################################################
  ThePlotOutputFolderPath<-paste(OutputDailyPPTFolderPath,"NCDCPlots/",sep="")
  
  #output log file to keep track of data downloads; if it already exists it appends the existing file. 
  OutputLogFile<-paste(OutputDailyPPTFolderPath,"UpdateLog_SPI.csv",sep="")
  FileName="Daily_ppt_mm_USVI"
  
  ## checks if the OutputDataFolderPath exists, if it doesn't it creates it.
  DirectoryExists<-dir.exists(file.path(ThePlotOutputFolderPath))
  if (DirectoryExists==FALSE) dir.create(file.path(ThePlotOutputFolderPath),recursive = TRUE)
  
  TodayDate<-as.character(format(Sys.Date(), "%Y-%m-%d"))
  FileNameDate<-as.character(format(Sys.Date(), "%Y%m%d"))
  TheScriptStartTime=Sys.time()
  
  TheUpdateLog<-data.frame(STATION=character(),
                           StationName=character(),
                           TodayDate=character(),
                           StartDate=character(),
                           EndDate=character(),
                           DownloadStationData=character(),
                           CalculateSPI=character(),
                           MinSPIYear=double(),
                           MaxSPIYear=double(),
                           SPIInterval=character(),
                           SPIMoPercCov=double(),
                           PercCovSPICutoff=double(),
                           stringsAsFactors=FALSE)
  # create a new dataframe to be filled with the summed data per month
  Daily_ppt_mm_USVI<-data.frame(STATION=character(),
                                StationName=character(),
                                LATITUDE=double(),
                                LONGITUDE=double(),
                                ELEVATION=double(),
                                DATE=character(),
                                PRCP=double(),
                                NumDays=integer(),
                                DaysAvail=integer(),
                                MoPercCov=double(),
                                stringsAsFactors=FALSE)
  #################################################################################
  # SCRIPT START
  #################################################################################
  i=1
  #################################################################################
  # Run API if DownloadStationData =="yes"
  #################################################################################
  if (DownloadStationData=="yes") 
  {
    #################################################################################
    # Download each station
    #################################################################################
    for (TheStation in TheStations) 
    {
      TheFileStartTime<-Sys.time()
      
      #TheStation<-TheStations[1]
      StationInfo<-ncdc_stations(datasetid = "GHCND", locationid = "FIPS:5200", stationid = TheStation)
      StartDate<-StationInfo$data[[2]]
      EndDate<-StationInfo$data[[3]]
      
      StartYear<-as.integer(format.Date(StartDate, "%Y"))
      EndYear<-as.integer(format.Date(EndDate, "%Y"))

      LATITUDE<-StationInfo$data[[4]]
      LONGITUDE<-StationInfo$data[[9]]
      ELEVATION<-StationInfo$data[[1]]
      STATION<-StationInfo$data[[7]]
      StationName<-StationInfo$data[[5]]
      # removes the ", US" from all of the station names - causes issues with formatting.
      StationName<-unlist(strsplit(StationName,","))[1]

      TheStationFileName<-gsub(":", "",TheStation)
      OutputDailypptFile<-paste(OutputDailyPPTFolderPath,FileName,"_",TheStationFileName,".csv",sep="")
     
      # checks if the file already exists, and if it does, skip to the next station.
      FileExists<-file.exists(OutputDailypptFile)
      
      if (FileExists) 
      {
        print (paste("Already Exists: ",OutputDailypptFile, sep=""))
        OldData<-read.csv(OutputDailypptFile, stringsAsFactors=FALSE, sep=",")
        NumRows<-nrow(OldData)
        
        OldData$YEAR<-format(as.Date(OldData$DATE,"%Y-%m-%d"), "%Y")
        TheLastYearDownloaded<-as.integer(OldData$YEAR[NumRows])
      
        TheLastDateDownloaded<-format(as.Date(OldData$DATE[NumRows],"%Y-%m-%d"), "%Y-%m-%d")
  
        if (TheLastDateDownloaded>=EndDate) 
        {
          print(paste("Data is up to date - LastDateDownloaded: ",TheLastDateDownloaded,"| EndDateAPI: ",EndDate,sep=""))
          next
          
        } else 
          {
          print(paste("Data is NOT up to date - LastDateDownloaded: ",TheLastDateDownloaded,"| EndDateAPI: ",EndDate,sep=""))
          
          TheYears<-seq(TheLastYearDownloaded, EndYear)
          }
      } else 
        {
        print (paste("Does not Exist: ",OutputDailypptFile, sep=""))
        print(paste("Creating new dataset - StartYear: ",StartYear,"| EndYear: ",EndYear,sep=""))
        
        # a sequence of years that start and end the same years the station has data.
        TheYears<-seq(StartYear, EndYear)
        }
      ######################################################
      # Download each year individually for the station - limit is 1 year for API
      ######################################################
      for (NumYear in TheYears)
      {
        TheDataAPI<-ncdc(datasetid = "GHCND", stationid = TheStation, datatypeid = "PRCP", startdate = paste(NumYear,"-01-01", sep=""), enddate = paste(NumYear,"-12-31", sep=""), units="metric", limit=1000)
        TotalAvailData<-TheDataAPI$meta$totalCount
        message("DLing data for: ",TheStation,"| Start Date: ", NumYear,"-01-01", "| End Date: ",NumYear,"-12-31")
        # grab the data and convert it into a data.frame
        TheData<-TheDataAPI$data
        TheData<-do.call(cbind.data.frame, TheData)
        NumRow<-nrow(TheData)
        
        ## if numrow==0, then this year had no data, therefore we're going to fill in -9999, or NA rows 
        if (NumRow==0)
        {
          ## sprintf makes a sequence with '01' format, rather than '1'.
          TheMonths<-sprintf("%02d",1:12)
          for (MonthNum in TheMonths)
          {
            DaysAvail=TotalDaysInMonth(MonthNum,NumYear)
            ## sprintf makes a sequence with '01' format, rather than '1'.
            TheDaysAvailSeq<-sprintf("%02d",1:DaysAvail)
            
            for (DayNum in TheDaysAvailSeq)
            {
              DATE=paste(NumYear,"-",MonthNum,"-",DayNum,sep="")
              
              PRCP=-9999
              NumDays=0
              MoPercCov=0
  
              DailypptRow<-c(STATION,StationName,LATITUDE,LONGITUDE,ELEVATION,DATE,PRCP,NumDays,DaysAvail,MoPercCov)
  
              Daily_ppt_mm_USVI[i,]<-DailypptRow
              i=i+1
              #print(paste("TheStation:",STATION,"Lat",LATITUDE,"lon",LONGITUDE,"Elev",ELEVATION,"date",DATE,"PRCP",PRCP,"%Cov",MoPercCov, sep=" "))
            }
          }
        } else
          {
            ThePlotStationOutputFilePath<-paste(ThePlotOutputFolderPath,TheStationFileName,"/", sep="")
            
            ## checks if the OutputDataFolderPath exists, if it doesn't it creates it.
            DirectoryExists<-dir.exists(file.path(ThePlotStationOutputFilePath))
            if (DirectoryExists==FALSE) dir.create(file.path(ThePlotStationOutputFilePath),recursive = TRUE)
            
            ThePlotOutputFile<-paste(ThePlotStationOutputFilePath,FileName,"_",TheStationFileName,"_",NumYear,".png", sep="")
    
            # checks if the file already exists, and if it does, skip to the next station.
          #  PlotFileExists<-file.exists(ThePlotOutputFile)
            
           # if (!PlotFileExists)
          #  {
              ## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
              Cairo(ThePlotOutputFile,type="png",units="in",width=18,height=9,res=300, pointsize=12,bg="white")
              
              TheNDCDPlot<-(ncdc_plot(TheDataAPI, breaks="1 month", dateformat="%b%y"))
              print(TheNDCDPlot)
              dev.off()
          #  }  
            # rename date field to TheDates
            names(TheData)[names(TheData) == 'date'] <- 'TheDates'
            
            # reformat the date field to remove the "T" so that the as.Date function can recognize it as date/time
            TheData$TheDates <- gsub('T', ' ', TheData$TheDates)
            # add new field that's reformatted as mm-dd-yyyy
            TheData$TheDates <- strptime(TheData$TheDates, "%Y-%m-%d %H:%M:%OS",tz="UTC")
            TheData$mmddyyyy<- as.Date(TheData$TheDates,"%Y-%m-%d")
    
            ######################################################
            # Subset the data by month and total the precipitation for the year and station
            ######################################################
           
             ## sprintf makes a sequence with '01' format, rather than '1'.
            TheMonths<-sprintf("%02d",1:12)
              
            for (MonthNum in TheMonths)
            {
              #MonthNum<-TheMonths[1]
              TheMonthData<-subset(TheData, format.Date(TheData$mmddyyyy, "%m")==MonthNum)
              # grab the start and end days for the  month
              NumDays<-nrow(TheMonthData)
              # calculate the % coverage for the month, calls function from ClimateData_SPI_Functions.R
              DaysAvail=TotalDaysInMonth(MonthNum,NumYear)
              
              ## sprintf makes a sequence with '01' format, rather than '1'.
              TheDaysAvailSeq<-sprintf("%02d",1:DaysAvail)
              ## if NumDays==0, then there were no .....
              if (NumDays==0)
              {
                for (DayNum in TheDaysAvailSeq)
                {
                  #DayNum<-TheDaysAvailSeq[1]
                  DATE=paste(NumYear,"-",MonthNum,"-",DayNum,sep="")
                  PRCP=-9999
                  MoPercCov=0
    
                  DailypptRow<-c(STATION,StationName,LATITUDE,LONGITUDE,ELEVATION,DATE,PRCP,NumDays,DaysAvail,MoPercCov)
                  
                  Daily_ppt_mm_USVI[i,]<-DailypptRow
                  #print(paste("TheStation:",STATION,"Lat",LATITUDE,"lon",LONGITUDE,"Elev",ELEVATION,"date",DATE,"PRCP",PRCP,"%Cov",MoPercCov, sep=" "))
                  
                  i=i+1
                }
              } else 
              {
                TheMonthData$dd<-format(as.Date(TheMonthData$mmddyyyy,"%Y-%m-%d"), "%d")
                
                for (DayNum in TheDaysAvailSeq)
                {
                  #DayNum=TheDaysAvailSeq[2]
                  
                  MoPercCov<-round((NumDays/DaysAvail),4)*100
                  DATE<-TheMonthData$mmddyyyy[TheMonthData$dd==DayNum]
                  
                  TheDATELength<-length(DATE)
                  if (TheDATELength==0)
                  {
                    DATE=paste(NumYear,"-",MonthNum,"-",DayNum,sep="")
                    PRCP=-9999
                    DailypptRow<-c(STATION,StationName,LATITUDE,LONGITUDE,ELEVATION,DATE,PRCP,NumDays,DaysAvail,MoPercCov)
                    
                    Daily_ppt_mm_USVI[i,]<-DailypptRow
                    #print(paste("TheStation:",STATION,"Lat",LATITUDE,"lon",LONGITUDE,"Elev",ELEVATION,"date",DATE,"PRCP",PRCP,"%Cov",MoPercCov, sep=" "))
                    
                    i=i+1
                    
                  } else 
                  {
                    PRCP<-TheMonthData$value[TheMonthData$dd==DayNum]
                    DATE<-as.character(DATE)
                    DailypptRow<-c(STATION,StationName,LATITUDE,LONGITUDE,ELEVATION,DATE,PRCP,NumDays,DaysAvail,MoPercCov)
                    
                    Daily_ppt_mm_USVI[i,]<-DailypptRow
                    
                    #print(paste("TheStation:",STATION,"Lat",LATITUDE,"lon",LONGITUDE,"Elev",ELEVATION,"date",DATE,"PRCP",PRCP,"%Cov",MoPercCov, sep=" "))
                    
                    i=i+1
                  }
                }
              }
            }
          }
        }
      
      if (FileExists)
      {
        NumRows<-nrow(OldData)
        
        OldData$yyyymm<-format(as.Date(OldData$DATE,"%Y-%m-%d"), "%Y-%m")
        LastYrMo<-OldData$yyyymm[NumRows]
        
        ## drop the last month from the old data, otherwise the %coverage field will not be calculated correctly as new
        ## days in a month are added
        OldData_sub<-OldData[-which(OldData$yyyymm %in% LastYrMo),]
        
        NumRows<-nrow(OldData_sub)
        
        TheLastDate<-as.Date(OldData_sub$DATE[NumRows],"%Y-%m-%d")
        
        Daily_ppt_mm_USVI_sub<-Daily_ppt_mm_USVI[Daily_ppt_mm_USVI$DATE>TheLastDate & Daily_ppt_mm_USVI$DATE<=EndDate,]
        
        OldData_sub<-OldData_sub[ , -which(names(OldData_sub) %in% c("X","START_DATE","END_DATE","YEAR","yyyymm"))]
        
        CombinedPptData<-rbind(OldData_sub,Daily_ppt_mm_USVI_sub)
        write.csv(CombinedPptData, file=OutputDailypptFile,quote=FALSE,row.names=FALSE)
      } else
      {
        Daily_ppt_mm_USVI_sub<-Daily_ppt_mm_USVI[Daily_ppt_mm_USVI$DATE<=EndDate,]
        
        write.csv(Daily_ppt_mm_USVI_sub,file=OutputDailypptFile,quote=FALSE,row.names=FALSE)
      }
      
      TheScriptEndTime=Sys.time()
      ScriptDuration<-difftime(TheScriptEndTime,TheFileStartTime,"VET",units="mins")
      message(TheStationFileName," - Complete, Time Elapsed: ",round(ScriptDuration,2), " mins")
      
      # Restart the fill and empty the data frame!
      i=1
      Daily_ppt_mm_USVI <- Daily_ppt_mm_USVI[0,]
    } 
    #################################################################################
    # Run API if DownloadStationData =="no"
    #################################################################################
  } else if (DownloadStationData=="no") 
    {
      print("No new precipitation data downloaded")
    }
  
  
  #################################################################################
  # Run SPI if CalculateSPI=="yes" && DownloadStationData =="yes"
  #################################################################################
  j=1
  OutputDailypptFiles <- list.files(path=OutputDailyPPTFolderPath, pattern="*.csv", full.names=T, recursive=FALSE)
  OutputDailypptFiles<-OutputDailypptFiles[OutputDailypptFiles != OutputLogFile]
  for (OutputDailypptFile in OutputDailypptFiles)
  {
    ThePrecipData<-read.csv(OutputDailypptFile, sep=",")
    #ThePrecipData<-read.csv(OutputDailypptFiles[1], sep=",")
    NumRow<-nrow(ThePrecipData)
    
    TheStation<-as.character(ThePrecipData$STATION[1])
    StationName<-as.character(ThePrecipData$StationName[1])
    TheDateField="DATE"
    
    SPIIntervalName<-paste("SPI",SPIInterval,sep="")
    
    StartDate<-as.character(ThePrecipData$DATE[1])
    EndDate<-as.character(ThePrecipData$DATE[NumRow])
    
    StationSelected<-(TheStation %in% TheStations)
    
    if (CalculateSPI=="yes" & StationSelected)
    {
      #print(paste(TheStation, " is selected? ",StationSelected, " AND SPI= ", CalculateSPI,sep=""))
      
      TheSPIFormatFolderPath=TheOutputSPIFolderPath
  
      PercCoverageField="MoPercCov"
      ThePrecipField="PRCP"
      SPIMoPercCov=CalculateSPIFunction(ThePrecipData,TheStation,StationName,TheDateField,MinSPIYear,MaxSPIYear,SPIInterval,PercCovSPICutoff,
                                        PercCoverageField,ThePrecipField,TheOutputSPIFolderPath,TheSPIFormatFolderPath)
      
      UpdateLogRow<-c(TheStation,StationName,TodayDate,StartDate,EndDate,DownloadStationData,CalculateSPI,MinSPIYear,MaxSPIYear,SPIIntervalName,SPIMoPercCov,PercCovSPICutoff)
      TheUpdateLog[j,]<-UpdateLogRow
      j=j+1
    }else {
      #################################################################################
      # Run SPI if CalculateSPI=="no" && DownloadStationData =="yes"
      #################################################################################
      CalculateSPIName="no"
      MinSPIYear=-9999
      MaxSPIYear=-9999
      SPIMoPercCov=-9999
      UpdateLogRow<-c(TheStation,StationName,TodayDate,StartDate,EndDate,DownloadStationData,CalculateSPIName,MinSPIYear,MaxSPIYear,SPIIntervalName,SPIMoPercCov,PercCovSPICutoff)
      TheUpdateLog[j,]<-UpdateLogRow
      j=j+1
    }
    
    print(paste("UPDATE LOG - STATION: ",TheStation," | StationName: ",StationName, sep=""))
    print(paste("TodayDate: ",TodayDate," | StartDate: ",StartDate," | EndDate: ",EndDate," | SPI%Coverage: ",SPIMoPercCov,sep=""))
    
  }
  
  #################################################################################
  # Write data and write/append logfile
  #################################################################################
  
  FileExists<-file.exists(OutputLogFile)
  
  if (FileExists)
  {
    ## read in the pre-existing update log
    OldLogFile<-read.csv(OutputLogFile, sep=",")
    ## combine it with the new update log
    CombinedData<-rbind(TheUpdateLog,OldLogFile)
    ## overwrite the pre-existing update log with the
    ## combined update logs
    write.csv(CombinedData, file=OutputLogFile,quote=FALSE,row.names=FALSE)
  } else
  {
    ## if the file doesn't already exist, then write out a new update log
    write.csv(TheUpdateLog, file=OutputLogFile,quote=FALSE,row.names=FALSE)
  }
  
  #################################################################################
  
  TheScriptEndTime=Sys.time()
  ScriptDuration<-difftime(TheScriptEndTime,TheScriptStartTime,"VET",units="mins")
  message("API & SPI Script Complete, Time Elapsed: ",round(ScriptDuration,2), " mins")
}
