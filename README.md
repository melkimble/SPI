# Features
* Downloads precipitation data from the National Climate Data Center (NCDC) Climate Data Online Web Services
* If the precipitation data has already been downloaded, it checks the last download date and appends the dataset
# Setup
01) Download or update R and RStudio to versions
	* R - 3.4.0 "You Stupid Darkness" 
	* RStudio - 1.0.143

02) Obtain NOAA API Key from this web address
	* https://www.ncdc.noaa.gov/cdo-web/token

03) These R Files must all be in the same folder
	* "gWidgets_GUI_Functions.R"
	* "ClimateData_SPI_GUI_Functions.R"
	* "ClimateData_API_GUI_daily_Script.R"

***********************************************************
# Graphic User Interface
03) Open "ClimateData_API_GUI_daily_Script.R" in RStudio

04) CTRL+A then CTRL+R
	* The SPI and NOAA API GUI should open in a new window

05) Hold CTRL and click on all weather stations of interest
	* To download precipitation data, a Personal NOAA API Key is needed. If this is not provided, a web browser will automatically launch.

06) To update/download & plot precipitation data for selected stations
	* Check "DownloadStationData" 
	* Click the "Select Daily Ppt Output Folder" button
	* Select the output folder for Ppt data from weather stations (saved as CSVs)

08) To calculate & plot SPI 
***********************************************************
# Documentation
The Climate Data API/GUI was developed to capture drought conditions by automating the collection of daily accumulated precipitation and calculating the 
Standardized Precipitation Index at a variety of time-scales and locations. These products serve as an early warning for drought conditions, and are required 
for enrollment in the National Drought Monitor. Inclusion in the Drought Monitor would grant the Territory automatic eligibility for resources and aid from the 
U.S. Department of Agriculture in the event of drought, an outcome of critical importance to agriculture producers in the USVI.

Standardized Precipitation Index (SPI) is a means through which present precipitation events can be monitored based on their consistency or departure from historic 
precipitation. Consistency with historic precipitation is determined by a normalized several-year median of accumulated precipitation for a chosen period. Departure, 
or standard deviations from the median would indicate abnormally dry or wet periods of time for each weather station; where one negative standard deviation would serve 
as an early warning system by signaling the start of a drought event (McKee et al., 1993). Because rain events are strongly influenced by elevation (Hijmans et al., 2005), 
indications of drought are strongly influenced by the location of weather stations in regions where elevation is highly variable, such as in the U.S. Virgin Islands (USVI). 
Due to the lack of fine-scale and long-term climate monitoring data that account for the Territory’s highly-variable landscape, any indication of drought may be biased by 
the few available stations, ignoring impacts to communities where data are not being collected or are insufficient due to gaps in data.

Computation of SPI and the collection of climate data is also part of a collaborative effort with the National Drought and Mitigation Center (NDMC) to place the USVI within 
the scope of accepted standards for inclusion in the Drought Monitor, like those that have been developed for the Pacific Island Nations. Inclusion in the Drought Monitor 
through the development of Territory-specific SPI products would make the Territory automatically eligible for resources and aid from the U.S. Department of Agriculture, an 
outcome of critical importance to agricultural producers in the USVI. At the development of this application, there were no such products available.  

## Methods
The SPEI package in R was utilized to compute the Standardized Precipitation Index for all active stations in the Territory. To calculate the Standard Precipitation Index, 
a cumulative probability distribution function was derived from monthly accumulated precipitation. The SPEI package allows the user to choose a variety of model fit methods, 
which include Unbiased Weighted Moments, Plotting-Position PWM, or Maximum Likelihood. The method chosen for fitting the distributions for the Territory was Maximum Likelihood, 
which maximizes the likelihood of the data when given the model. This initial distribution was then converted to a distribution that can be chosen by the user, and includes 
log-Logistic, Gamma, or Pearson III. The Gamma distribution was chosen for the Territory based on feedback from National Drought Monitor collaborators. Once converted to a Gamma 
distribution, SPI values are equivalent to standard deviations from the converted mean.  For SPI calculations at scales greater than one month, a user defined kernel is used to 
smooth SPI values between aggregated months. The SPEI package allows the user to select the shift and type of kernel used, where the type is limited to rectangular or Gaussian. 
A Gaussian kernel with no shift was selected to smooth aggregated SPI values.

## Calculating SPI
### Min SPI Year / Max SPI Year
Based on feedback from the National Drought Monitor, the desired duration of precipitation data for computing SPI is thirty years, but SPI can be computed at a minimum of fifteen years. 
These fields allow the user to dynamically adjust which years are used when computing SPI. 
### Percent Cut-off for SPI Monthly PPT Coverage
As daily accumulated precipitation is downloaded for each station, the total coverage for each month is computed and appended to the dataset. i.e., if a month only had 15 days with 
precipitation data, then the percent coverage for that month would be approximately 50%. This field is utilized in the Climate Data API/GUI by allowing the user to define criteria for 
months used in the SPI calculation. If a month’s coverage falls below the desired cut-off, then it would not be used in computing SPI.
### Selected SPI Interval
There are a variety of intervals that SPI can be computed at. Included in the Climate Data API/GUI are SPI 1:12, 24, and 48. These are denoted as “SPI#” from a drop-down menu. Each 
are aggregates of a series of months starting at 1, where “SPI1” is one month SPI, “SPI2” is two month SPI, and so on. When SPI is computed for more than 1 month, a user specified kernel 
is used to smooth the values between aggregated months.   
