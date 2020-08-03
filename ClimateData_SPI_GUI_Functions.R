#######################################################################################################################
# R SPEI Lib & rnoaa API functions
# Created by: Melissa Kimble
# Last Modified: 06/27/2017
#######################################################################################################################
## Libraries
LibraryList<-c("SPEI","Cairo","RColorBrewer","ggplot2","reshape","xts","data.table")
for (TheLibrary in LibraryList)
{
  if(TheLibrary %in% rownames(installed.packages()) == FALSE) install.packages(TheLibrary)
  
}
library(SPEI)
library(Cairo)
library(RColorBrewer)
library(ggplot2)
library(reshape)
library(xts)
library(data.table)

######################################################################################
## function to return total days available in a month, including leap years
TotalDaysInMonth=function(MonthNum,NumYear)
{
  if (MonthNum=="01" | MonthNum=="03" | MonthNum=="05" | MonthNum=="07" | MonthNum=="08" | MonthNum=="10" | MonthNum=="12")
  {
    DaysAvail=31
  } 
  else if (MonthNum=="04" | MonthNum=="06" | MonthNum=="09" | MonthNum=="11")
  {
    DaysAvail=30
  } 
  else if (MonthNum=="02" &&  NumYear=="1904" |  NumYear=="1908" |  NumYear=="1912" | NumYear=="1916"  | 
           NumYear=="1920" |  NumYear=="1924" |  NumYear=="1928" |  NumYear=="1932" |  NumYear=="1936" | 
           NumYear=="1940" |  NumYear=="1944" |  NumYear=="1948" |  NumYear=="1952" |  NumYear=="1956" | 
           NumYear=="1960" |  NumYear=="1964" |  NumYear=="1968" |  NumYear=="1972" |  NumYear=="1976" | 
           NumYear=="1980" |  NumYear=="1984" |  NumYear=="1988" |  NumYear=="1992" |  NumYear=="1996" |
           NumYear=="2000" |  NumYear=="2004" |  NumYear=="2008" |  NumYear=="2012" |  NumYear=="2016" |  
           NumYear=="2020")
  {
    ##  total days of feb are 29 during leap years
    ## leap years: 1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 
    ## 1940, 1944, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 
    ## 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020.
    DaysAvail=29
  } 
  else 
  {
    DaysAvail=28
  }
  
  return(DaysAvail)
}
######################################################################################
## function to insert row at a particular col/row in a dataframe
InsertRow <- function(InputData, NewRow, RowNum) {
  InputData[seq(RowNum+1,nrow(InputData)+1),] <- InputData[seq(RowNum,nrow(InputData)),]
  InputData[RowNum,] <- NewRow
  InputData
}

######################################################################################
## Function to subset the data based on the specified PPT coverage cut-off and
## add NAs for missing data 
MissingDates2NA_SumMo<-function(ThePrecipData,TheStation,TheDateField,MinSPIYear,MaxSPIYear,PercCovSPICutoff,PercCoverageField,ThePrecipField)
{
  ## subset the dataset by station
  ThePrecipData_Subset=ThePrecipData[(ThePrecipData$STATION == TheStation) , ]
  
  ## setup the DATE fields
  ThePrecipData_Subset$DATE<- format(as.Date(ThePrecipData_Subset[[TheDateField]]), "%Y-%m-%d")
  ThePrecipData_Subset$YEAR<- format(as.Date(ThePrecipData_Subset[[TheDateField]]), "%Y")
  ## subset data by min and max SPI year
  ThePrecipData_Subset<-ThePrecipData_Subset[ThePrecipData_Subset$YEAR>=MinSPIYear & ThePrecipData_Subset$YEAR<=MaxSPIYear,]
  
  ## Change all ppt values where coverage is less than the cutoff to -9999
  ThePrecipData_Subset[[ThePrecipField]][ThePrecipData_Subset[[PercCoverageField]]<PercCovSPICutoff]<- -9999
  
  ## Subset the data to only include Year, Month, and PPT (daily precip)
  TheColNames<-c("DATE",ThePrecipField)
  ThePrecipData_Subset<-ThePrecipData_Subset[ , which(names(ThePrecipData_Subset) %in% TheColNames)]
  
  ## convert -9999 to NA
  ThePrecipData_Subset[ThePrecipData_Subset==-9999]<-NA
  
  ## Add in missing dates as NA
  ThePrecipData_Subset<-read.zoo(ThePrecipData_Subset,header=TRUE, format="%Y-%m-%d")
  ## create empty DF with start/end sequence = ThePrecipData_Subset
  Z_ZeroDimTS<-zoo(x=NULL,seq(start(ThePrecipData_Subset),end(ThePrecipData_Subset),by="day"))
  ThePrecipData_Subset <- merge(ThePrecipData_Subset,Z_ZeroDimTS, all=TRUE)
  
  ## section data based on SPI interval (1mo, 2mo, 3mo, etc)
  EP_1moInterval<-endpoints(ThePrecipData_Subset,'months',k=1)
  ## aggregate (sum) ppt values by SPI Interval; had to convert to NA so that -9999 weren't summed
  ThePrecipData_Agg<-period.apply(x=ThePrecipData_Subset,EP_1moInterval,FUN=sum )
  
  ThePrecipData_Agg<-as.data.frame(ThePrecipData_Agg)
  ThePrecipData_Agg<-data.table::setDT(ThePrecipData_Agg,keep.rownames=TRUE)[]
  
  ## re-enter column names 
  colnames(ThePrecipData_Agg)<-TheColNames
  
  ThePrecipData_Agg$YEAR<-format(as.Date(ThePrecipData_Agg$DATE),"%Y")
  ThePrecipData_Agg$MONTH<-format(as.Date(ThePrecipData_Agg$DATE),"%m")
  ThePrecipData_Agg<-as.data.frame(ThePrecipData_Agg)
  ## Subset the data to only include YEAR, MONTH, and PPT (daily precip)
  TheColNames<-c("YEAR","MONTH",ThePrecipField)
  ThePrecipData_Agg<-ThePrecipData_Agg[,which(names(ThePrecipData_Agg) %in% TheColNames)]
  ## re-arrange columns
  ThePrecipData_Agg<-ThePrecipData_Agg[,TheColNames]
  ## percentage of non NAs
  SPIPercCoverage<-100-((sum(is.na(ThePrecipData_Agg))/(prod(dim(ThePrecipData_Agg))-nrow(ThePrecipData_Agg)))*100)
  # The number of years in the dataset is not equal to the number of years between 1953-2016
  # missing 7 year columns so they need to be added in as NA.
  #(MaxSPIYear-MinSPIYear)-length(AllYears)
  
  return(list(ThePrecipData_Agg,SPIPercCoverage))
}


######################################################################################
## Function to call AddNAMonthsPPT & ReformatDataForSPI# to input into the
## spi function. Add if statement here for 
CalculateSPIFunction<-function(ThePrecipData,TheStation,StationName,TheDateField,MinSPIYear,MaxSPIYear,SPIInterval,PercCovSPICutoff,PercCoverageField,ThePrecipField,
                               TheOutputSPIFolderPath,TheSPIFormatFolderPath)
{
  SPIIntervalName<-paste("SPI",SPIInterval,sep="")
  FileNameDate<-as.character(format(Sys.Date(), "%Y%m%d"))
  TheSPIFormatFolderPath<-paste(TheSPIFormatFolderPath,FileNameDate,"/",SPIIntervalName,"/",sep="")
  TheOutputSPIFolderPath<-paste(TheOutputSPIFolderPath,FileNameDate,"/",SPIIntervalName,"/",sep="")
  
  TheSPIPlotFolderPath<-paste(TheSPIFormatFolderPath,"SPIPlots/",sep="")
  ## checks if the OutputDataFolderPath exists, if it doesn't it creates it.
  DirectoryExists<-dir.exists(file.path(TheSPIPlotFolderPath))
  if (DirectoryExists==FALSE) dir.create(file.path(TheSPIPlotFolderPath),recursive = TRUE)
  ## checks if the OutputDataFolderPath exists, if it doesn't it creates it.
  DirectoryExists<-dir.exists(file.path(TheOutputSPIFolderPath))
  if (DirectoryExists==FALSE) dir.create(file.path(TheOutputSPIFolderPath),recursive = TRUE)
  
  AggOutput<-MissingDates2NA_SumMo(ThePrecipData,TheStation,TheDateField,MinSPIYear,MaxSPIYear,PercCovSPICutoff,PercCoverageField,ThePrecipField)
  
  ThePrecipData_Agg<-AggOutput[[1]]
  SPIPercCoverage<-AggOutput[[2]]
  
  ## Filenames
  TheStationFileName<-gsub(":", "",TheStation)
  FileName=paste(TheStationFileName,"_",MinSPIYear,"_",MaxSPIYear,"_",SPIIntervalName,"_",PercCovSPICutoff, sep="")
  
  TheSPIFile<-paste(TheSPIFormatFolderPath,"MonthlyStation_",FileName,"_PPT_SPI_Format.csv", sep="")
  OutputSPIFile<-paste(TheOutputSPIFolderPath,"MonthlyStation_",FileName,"_PPT_SPI_Output.csv",sep="")
  PlotOutputSPIFile<-paste(TheOutputSPIFolderPath,"MonthlyStation_",FileName,"_PPT_SPI_PlotOutput.csv",sep="")
  
  TheGGPlotFile<-paste(TheSPIPlotFolderPath,"MonthlyStation_",FileName,"_PPT_GGSPI_Plot.png", sep="")
  TheTSSPIPlotFile<-paste(TheSPIPlotFolderPath,"MonthlyStation_",FileName,"_PPT_TSSPI_Plot.png", sep="")
  TheSPIPlotFile<-paste(TheSPIPlotFolderPath,"MonthlyStation_",FileName,"_PPT_SPI_Plot.png", sep="")
  
  ## write the re-formatted SPI to a table
  write.csv(ThePrecipData_Agg,file=TheSPIFile,quote=FALSE,row.names=FALSE)	 
  
  ## plot titles
  TheStationNames<-c("STT CHARLOTTE AMALIE CYRIL E KING AIRPORT","STT REDHOOK BAY","STJ CRUZ BAY","STJ EAST END","STX CHRISTIANSTED HAMILTON FIELD AIRPORT",
                  "STX EAST HILL","STX CHRISTIANSTED FORT","STX MONTPELLIER")
  if (StationName=="REDHOOK BAY ST THOMAS") StationName="REDHOOK BAY"
  StationName<-grep(StationName, TheStationNames,value=TRUE)
  
  SPIIntervalName<-paste("SPI ",SPIInterval,sep="")
  SPIPercCoverageName<-round(SPIPercCoverage,0)
  SPIPlotTitle=paste(SPIIntervalName,": ", MinSPIYear," - ",MaxSPIYear,", ",SPIPercCoverageName, "% SPI Coverage",
                     "\n", StationName," (",TheStation,")",
                     "\n Monthly PPT Coverage Cut-off: ",PercCovSPICutoff,"%", sep="")
  ## One+ month SPI
  #?SPEI::spi
  ## fit = unbiased Weighted Moments ('ub-pwm'), plotting-position PWM ('pp-pwm'), or maximum likelihood ('max-lik')
  ## *fit for the empirical cumulative distribution function; equivalent to ecdf function (?stats::ecdf)
  ## *used in spi package, except you can pick the fit method. This is the initial distribution of the ppt data
  ## distribution = 'log-Logistic', 'Gamma', or 'PearsonIII'
  ## *what the initial distribution is converted to to compute stdevs of ppt from the converted mean
  ## kernel (for computing SPI at scales > 1) = list(type='gaussian', shift=0), list(type='rectangular', shift=0)
  ## *kernel is simply how the values inbetween larger scales are smoothed/averaged
  ## na.rm = TRUE or FALSE  
  ## *If there are NA data in your ppt, set this to TRUE
  PPT_SPI <- SPEI::spi(ThePrecipData_Agg[,ThePrecipField], 
                       SPIInterval,distribution='Gamma',fit='max-lik',
                       kernel=list(type='gaussian', shift=0), 
                       na.rm=TRUE)
  

  xLabel="YEAR"
  yLabel="MONTH"

  ####################################
  ## filled.contour plot
  NumMonths<-length(unique(ThePrecipData_Agg$MONTH))
  TheMonths = 1:NumMonths ## 1:12
  TheYears<-seq(min(ThePrecipData_Agg$YEAR), max(ThePrecipData_Agg$YEAR),by=1)
  NumYears = length(TheYears)
  spi_plot<-t(matrix(unlist(PPT_SPI$fitted), NumMonths, NumYears))  ## reformats the data to be col=months, row=year
  spi_breaks <- c(-2.4, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 
                  1.3, 1.6, 2, 2.4)
  spi_cols <- colorRampPalette(c("darkred", "red", "yellow", 
                                 "lightgrey", "green", "blue", "darkblue"), space = "rgb")
  spi_plot[(spi_plot == Inf)] <- 2.2
  spi_plot[(spi_plot == -Inf)] <- -2.2
  ## SPI Plot
  ## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
  Cairo::Cairo(TheSPIPlotFile,type="png",units="in",width=18,height=6,res=300, pointsize=12,bg="white")
  filled.contour(TheYears, TheMonths, matrix(unlist(spi_plot[,1:NumMonths]), NumYears, NumMonths), 
                 col = spi_cols(11), xlab = xLabel, ylab = yLabel, cex.lab = 1.1, 
                 font.lab = 2, levels = spi_breaks, main = SPIPlotTitle, 
                 las = 2, cex.axis = 0.7, plot.axes = {
                   axis(1, TheYears) 
                   axis(2, TheMonths)
                 })
  dev.off()
  spi_plot<-as.data.frame(spi_plot)
  spi_plot$DATE<-TheYears
  Spi_ColRename<-c(TheMonths,"DATE")
  colnames(spi_plot)<-Spi_ColRename
  spi_plot<-spi_plot[c('DATE',TheMonths)]
  ## write the output to a txt file; matrix of SPI Data
  write.csv(spi_plot,file=OutputSPIFile,quote=FALSE,row.names=FALSE)

  ####################################
  ## reformatted SPI data for ggplot2 plotting
  TheLength<-length(PPT_SPI$fitted)
  ThePrecipData_Agg$SPI<-PPT_SPI$fitted[1:TheLength]
  ####################################
  ## ggplot2 raster plot
  ## color scheme for SPI plots
  myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
  TheSPIPlot<-ggplot(ThePrecipData_Agg, aes(YEAR, MONTH)) +
    geom_raster(aes(fill = SPI)) + 
    scale_fill_gradientn(colours = myPalette(100), limits=c(-3, 3), na.value="transparent") +
    # scale_x_discrete(name ="YEAR", limits=seq(min(PPT_SPI$YEAR), max(PPT_SPI$YEAR),by=1)) +
    ggtitle(SPIPlotTitle) + 
    theme(axis.text.x = element_text(size=16, angle=90,vjust=0.5),
          axis.text.y = element_text(size=16),
          plot.title = element_text(size=16, hjust = 0.5))
  ggsave(TheSPIPlot, file = TheGGPlotFile, scale = 1, width=18,height=6,units="in", dpi = 300)
         #width = par("din")[1], height = par("din")[2], dpi = 300)
  
  ####################################
  ## Time series plot
  ## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
  #ThePrecipData_Agg$MONTH<-as.character(ThePrecipData_Agg$MONTH)
  #ThePrecipData_Agg$MONTH<-sprintf("%02d", as.character(ThePrecipData_Agg$MONTH))
  #ThePrecipData_Agg$YEAR<-as.character(ThePrecipData_Agg$YEAR)
  ThePrecipData_Agg$DATE<-format(as.Date(paste(ThePrecipData_Agg$YEAR,ThePrecipData_Agg$MONTH,"01", sep="-")), "%Y-%m-%d")
  
  TS_ThePrecipData_Agg = data.frame(Time=c(time(ThePrecipData_Agg$DATE)), SPI=c(ThePrecipData_Agg$SPI), TheColor=ifelse(c(ThePrecipData_Agg$SPI)<0,0,1))
  
  TheTSSPIPlot<-ggplot( data=TS_ThePrecipData_Agg, aes(x=Time, y=SPI) )   +
    geom_ribbon(aes(ymax=TheColor*SPI, ymin=0,  fill = "wet"))             +
    geom_ribbon(aes(ymax=0,  ymin=(1-TheColor)*SPI, fill = "dry"))          +
    ggtitle(SPIPlotTitle) + 
    guides(fill=guide_legend(title="SPI")) +
    scale_x_discrete(name ="YEAR", limits=ThePrecipData_Agg$YEAR) +
    theme(axis.text.x = element_text(size=16, angle=90,vjust=0.5),
          axis.text.y = element_text(size=16),
          plot.title = element_text(size=16, hjust = 0.5))
  ggsave(TheTSSPIPlot, file = TheTSSPIPlotFile, scale = 1, width=18,height=6,units="in", dpi = 300)
  
  ## write the output txt file; spi data for ggplot2
  write.csv(ThePrecipData_Agg,file=PlotOutputSPIFile,quote=FALSE,row.names=FALSE)
  return(SPIPercCoverage)
}
