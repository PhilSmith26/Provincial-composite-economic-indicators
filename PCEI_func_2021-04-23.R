# Comprehensive PCEI program
# April23, 2021

MonR <- "February"
first_date <- "2001-01-01"
last_date <- "2021-02-01"
read_indicators <- TRUE
output_level <- 2 # 0 = minimal; 1 = moderate; 2 = maximum
prov_codes <- c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","CA")
#===============================================================================
# Make sure the packages are loaded and all functions defined (once) before
# calling the PCEI_func function, which is at the end:
#===============================================================================

# The following packages must be loaded and the following functions must be 
# defined before the PCEI_calc function is called.
pkgs <- c("tidyverse","cansim","lubridate","gdata","dataframes2xls",
          "readxl", "writexl","forecast","seasonal","stringr", "ggtext",
          "corrplot")
inst <- lapply(pkgs,library,character.only=TRUE)

# Define some functions
YOY <- function(x) { # year-over-year percentage changes
  y <- 100*(x-lag(x,12))/lag(x,12)
}
YOYRND <- function(x) {
  y <- round(YOY(x),2)
}
MOM <- function(x) { # month-over-month percentage changes
  y <- 100*(x-lag(x,1))/lag(x,1)
}
MOMRND <- function(x) {
  y <- round(MOM(x),2)
}
MA13CXE <- function(x) { # 6-month moving average
  k <- length(x)
  y <- (lag(x,6)+lag(x,5)+lag(x,4)+lag(x,3)+lag(x,2)+lag(x,1)+x+
          lead(x,1)+lead(x,2)+lead(x,3)+lead(x,4)+lead(x,5)+lead(x,6))/13
}
norm <- function(x) { # normalization
  m <- mean(x,na.rm=TRUE)
  sd <- sd(x,na.rm=TRUE)
  y <- (x-m)/sd
}
get_vector <- function(df,x) { # get the Vector, given the Series_name of an indicator
  y <- filter(df,Series_name==x)$Vector
}
get_Series_name <- function(df,x) { # get the Series_name, given the Vector of an indicator
  y <- filter(df,Vector==x)$Series_name
}
# Calculate the number of observations via monnb and mondf
# This function turns a date into a 'monthnumber' relative to an origin
monnb <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));
  y <- lt$year*12 + lt$mon 
} 
# This function computes a month difference as a difference between two monnb's
mondf <- function(d1, d2) {
  y <- monnb(d2) - monnb(d1)
}
# This function retrieves an already SA series from CODR
canseries <- function (vector,first_date,nobs) {
  x_df <- get_cansim_vector(vector,first_date)
  y <- x_df$VALUE
  if (length(y)<nobs) {
    y[(length(y)+1):nobs] <- NA
  }
  if (length(y)>nobs) {
    y <- y[1:nobs]
  } 
  return (y)
}
# This function seasonally adjusts a series
SEASADJ <- function(x,start_year,freq) {
  x1 <- ts(data=x,start=start_year,frequency=freq)
  x1_seas <- seas(x1)
  y <- as.numeric(final(x1_seas))
  if (length(y)<length(x)) {
    y[(length(y)+1):length(x)] <- NA
  }
  return(y)
}
# This function retrieves an NSA series from CODR and seasonally adjusts it
canseriesSA <- function (vector,first_date,styr,nobs) {
  x <- canseries(vector,first_date,nobs)
  xSA <- SEASADJ(x,styr,12)
  y <- as.vector(xSA)
  if (length(y)<nobs) {
    y[(length(y)+1):nobs] <- NA
  }
  return (y)
}

#===============================================================================
# This function calculates a provincial composite economic indicator (PCEI).
# It takes about 15 seconds to run if the indicators data are already
# stored in an rds file. Otherwise it could ~3 minutes if the data 
# must be retrieved from StatCan. It depends on StatCan download speed.
#===============================================================================
PCEI_calc <- function(ProvA,MonR,first_date,last_date,
                      Detrend,read_indicators,output_level) {
setwd(paste0("/Users/philipsmith/Documents/R/Provincial_summary_indicators/",
             "Multi_prov/",ProvA))
if (!dir.exists("Output")) dir.create("Output")
provinces <- c(CA="Canada",NL="Newfoundland and Labrador",
             PE="Prince Edward Island",NS="Nova Scotia",NB="New Brunswick",
             QC="Quebec",ON="Ontario",MB="Manitoba",SK="Saskatchewan",
             AB="Alberta",BC="British Columbia",YK="Yukon",
             NT="Northwest Territories",NU="Nunavut")
province <- provinces[ProvA]
ProvCol <- case_when( ProvA=="CA" ~  8,
                      ProvA=="NL" ~  9,
                      ProvA=="PE" ~ 10,
                      ProvA=="NS" ~ 11,
                      ProvA=="NB" ~ 12,
                      ProvA=="QC" ~ 13,
                      ProvA=="ON" ~ 14,
                      ProvA=="MB" ~ 15,
                      ProvA=="SK" ~ 16,
                      ProvA=="AB" ~ 17,
                      ProvA=="BC" ~ 18 )
start_time <- as.character(Sys.time()) # Save the start time for use later
print(paste0("Starting ",Detrend," calculations for ",province," ..."))
print("Beginning with start-up conditions...")
indicators_file <- paste0("/Users/philipsmith/Documents/R/Provincial_summary_",
       "indicators/Multi_prov/ALL_indicators_feb232021.xls")
avg_growth_rates <- c(CA=2.0,NL=1.8,PE=2.0,NS=1.2,NB=1.1,QC=1.7,ON=1.8,MB=2.2,
                      SK=2.1,AB=2.6,BC=2.7,YK=2.9,NW=0.7,NU=4.6,OC=-0.6)
# All output generated by this program goes in a sub-directory called "Output"
# except R data sets (".rds" files) which are saved in the "rds" sub-directory.
# Output includes the indicators as retrieved, the ARIMA-extended indicators,
# the indicators growth rates, the correlation matrix, the principal components.

# Strategy:
# (1) Set starting conditions and define some functions
# (2) Download necessary data from CODR and create the indicators data frame
# (3) Determine where series extensions are needed at the back and front ends 
# (4) Backcast and forecast the indicators where needed via ARIMA models
# (5) Calculate YoY and MoM indicator growth rates
# (6) Detrend and normalize the indicators
# (7) Calculate the principal components
# (8) Prepare component groups
# (9) Make charts with the PCEI and its indicator groups

nobs <- mondf(first_date,last_date)+1 # number of monthly observations

# nobs_after_YOY is the number of observations after YOY percentage changes
# It is the same number for the MOM case
nobs_after_YOY <- nobs-12 
styr <- as.integer(str_sub(first_date,1,4)) # starting year
enyr <- as.integer(str_sub(last_date,1,4)) # ending year

# Read indicators list on worksheet 1
indicators <- read_xls(indicators_file,skip=1,sheet="indicators") 
# Rename the 'vectors' column for the province to 'Vector'
tmp <- colnames(indicators)
tmp[ProvCol] <- "Vector"
colnames(indicators) <- tmp
# Order the indicator data frame rows alphabetically, based on the v-numbers
indicators0 <- arrange(indicators,Vector)
# Remove the lines that have "NA" in them, that is, vectors that are not
# used for this particular province ("NA" is a character string here)
indicators1 <- filter(indicators0,Vector!="NA")
indicators <- indicators1
# Read deflators list on worksheet 2 and change column name to 'DVector'
deflators <- read_xls(indicators_file,skip=1,sheet="deflators") 
tmp <- colnames(deflators)
tmp[ProvCol] <- "Vector"
colnames(deflators) <- tmp
# Read old data list on worksheet 3 and change column name to 'OVector'
old_data <- read_xls(indicators_file,skip=1,sheet="old_data") 
tmp <- colnames(old_data)
tmp[ProvCol] <- "Vector"
colnames(old_data) <- tmp
print("Indicator spreadsheet successfully read in...")

# (2) Download necessary data from CODR and create the indicators data frame
# ==========================================================================

# Read necessary data from CODR, transform the data, create vseries data frame

# If this was done previously, just read in the vseries data frame
#if (!read_indicators & file.exists("rds/vseries.rds")) { # fast
#  vseries <- readRDS("rds/vseries.rds") 
#  } else { # otherwise read and transform the data to create vseries data frame

print("Reading in the deflators and seasonally adjusting them if necessary...")
# Read in and, if necessary, seasonally adjust the deflators
dseries <- data.frame(REF_DATE=seq(as.Date(first_date),
              as.Date(last_date),by="month"),stringsAsFactors=FALSE)

#alldv <- get_cansim_vector(deflators$Vector,first_date)
## Select just the required columns from this df
#alldv0 <- select(alldv,REF_DATE,VECTOR,VALUE)
## Switch from long- to wide-format, with v-numbers as column names
#alldv0 <- pivot_wider(alldv0,names_from=VECTOR,values_from=VALUE)
## Remove any bottom rows beyond the chosen last_date
#alldv0 <- filter(alldv0,REF_DATE<=as.Date(last_date))
## Arrange the rows in time series order. 2001-01-01 to last date retrieved
#vseries0 <- arrange(allv0,REF_DATE)

for (i in 1:nrow(deflators)) {
  if (deflators$Seas_adj[i]=="A") {
    dseries[,i+1] <- canseries(deflators$Vector[i],first_date,nobs)
    colnames(dseries)[i+1] <- deflators$Series_name[i]
  }
  if (deflators$Seas_adj[i]=="N") {
    dseries[,i+1] <- canseriesSA(deflators$Vector[i],first_date,styr,nobs)
    colnames(dseries)[i+1] <- deflators$Series_name[i]
  }
} 

if (read_indicators) {
  print("Starting data retrieval from StatCan...")    
  # Retrieve all series in a giant long-form df
  # Note that latest possible data are retrieved, so the LFS data in particular
  # will have a later terminal month than the other data
  allv <- get_cansim_vector(indicators$Vector,first_date)
  # Select just the required columns from this df
  allv0 <- select(allv,REF_DATE,VECTOR,VALUE)
  # Switch from long- to wide-format, with v-numbers as column names
  allv0 <- pivot_wider(allv0,names_from=VECTOR,values_from=VALUE)
  # Remove any bottom rows beyond the chosen last_date
  allv0 <- filter(allv0,REF_DATE<=as.Date(last_date))
  # Arrange the rows in time series order. 2001-01-01 to last date retrieved
  vseries0 <- arrange(allv0,REF_DATE)
  # Order the v-number column names alphabetically, so the vseries1 columns 
  # will be ordered the same as the indicators1 rows
  vseries1 <- vseries0[,order(names(vseries0))]
  # Rename the vseries1 columns using the Series_name column of indicators1
  colnames(vseries1)[2:ncol(vseries1)] <- indicators1$Series_name 
  # Now create vseries2 by SA and deflation of vseries1, where needed
  vseries2 <- data.frame(REF_DATE=vseries1$REF_DATE)
  for (i in 1:nrow(indicators1)) {
    print(paste0("i = ",i,", Vector = ",indicators1$Vector[i],
                 ", Name = ",indicators1$Series_name[i],
                 ", Defl = ",indicators1$Deflator[i],
                 ", Seas = ",indicators1$Seas_adj[i]))
    # Case 1 - series needs no deflation (1) and is already SA (A)
    if (indicators1$Deflator[i]==1 & indicators1$Seas_adj[i]=="A") { 
      vseries2[,i+1] <- vseries1[,i+1]
      colnames(vseries2)[i+1] <- indicators1$Series_name[i]
    }
    # Case 2 - series needs no deflation (1) and is NSA (N)
    if (indicators1$Deflator[i]==1 & indicators1$Seas_adj[i]=="N") {
      tmp <- as.numeric(SEASADJ(vseries1[,i+1],styr,12))
      if (length(tmp)<nrow(vseries2)) {
        tmp[(length(tmp)+1):nrow(vseries2)] <- NA
      }
      vseries2[i+1] <- tmp
      colnames(vseries2)[i+1] <- indicators1$Series_name[i]
    }
    # Case 3 - series needs deflation (<100,>1) and is SA (A)
    if (indicators1$Deflator[i]>1 & indicators1$Deflator[i]<100 & 
        indicators1$Seas_adj[i]=="A") {
        tmp1 <- dseries[,indicators1$Deflator[i]]
        tmp2 <- as.numeric(SEASADJ(tmp1,styr,12))
        tmp3 <- 100*vseries1[,i+1]/tmp2
        vseries2[,i+1] <- tmp3
        colnames(vseries2)[i+1] <- indicators1$Series_name[i]
    }
    # Case 4 - series needs deflation (<100,>1) and is NSA (N)
    if (indicators1$Deflator[i]>1 & indicators1$Deflator[i]<100 & 
        indicators1$Seas_adj[i]=="N") {
      tmp1 <- 100*vseries1[,i+1]/dseries[,indicators1$Deflator[i]]
      tmp2 <- as.numeric(SEASADJ(tmp1,styr,12))
      if (length(tmp2)<nrow(vseries2)) {
        tmp2[(length(tmp2)+1):nrow(vseries2)] <- NA
      }
      vseries2[,i+1] <- tmp2
      colnames(vseries2)[i+1] <- indicators1$Series_name[i]
    } else {
    # Otherwise leave the series alone
      vseries2[,i+1] <- vseries1[,i+1]
      colnames(vseries2)[i+1] <- indicators1$Series_name[i]
    }
  }
  # Case 5 - deflation > 100 means special situation, handled below
  # res building permits value, already SA
  BPRES2 <- vseries1$`Building permits, residential`
  # res building permits value - old data, NSA
  if (ProvA=="CA") {
  BPRES1 <- c(2427330,2388908,3274765,3490931,4331835,3937924,3997648,3667929,3393365,3758223,
            3577539,2609720,2530200,2587283,3759293,4588252,4728689,4606990,4655028,4415803,
            3896672,4542617,3977548,2973732,3028307,2793197,3896051,4583037,5015736,5172281,
            5281734,4214891,4715508,4624220,3999725,3447298,2827370,3036513,4524182,5006204,
            5028760,6506456,5333514,4915275,4699191,4815405,5039508,3846203,2827624,3503627,
            5198240,5420465,5851055,6045782,5309170,5919239,5341726,5463796,4811371,5058616,
            3381826,3709444,5689449,5186356,6570539,6521233,5759553,6391045,5797926,6497505,
            6202155,4558791,4918500,3779628,6084141,5991069,8111961,7728367,6549203,6853800,
            6297743,7296334,5871672,4897312,4508118,4801380,5274015,7268883,7557467,7061615,
            6998456,5870290,6866139,5817328,4490667,3923057,3158257,2767168,4454348,4856427,
            5861688,6085977,5249556,5369960,5777760,6660321,5855529,4952381,3898840,4039330,
            6261104,7255505,6918327,7718106,6687071,5966748,7031908,6497025,5428505,4742998,
            3711215,4631118,6922680,5639284,7530636,7677973,6989920,6441444,6068313,6603921,
            6109707,5639440,4447985,5142372,6970257,7024457,8072422,8127257,7393648,7859306,
            6849997,8156281,6256042,4550662,4327391,4576464,6131598,7527611,8501266,7439983,
            8790782,6695064,7112784,7915333,6616058,5206567,5440387,4858381,5882469,6419656,
            7973541,9360158,9951018,6679344,8033094,8150757,6331296,5851977,4532651,4597805,
            6836424,8107640,7685082,9035159,8763394,7644223,7634753,8528090,5930543,5844277,
            4111576,5310272,6482409,7737585,8001491,8018209,7907357,8537039,7508639,9021928,
            7406899,5858345,5282366,5349893,7286148,7214931,9615434,10629999,9389964,8516920,
            8599464,9377461,7451832,6159659)
  } else {
  BPRES1 <- canseriesSA(get_vector(old_data,
      "Building permits, residential old data"),first_date,styr,nobs)
  }
  BPRES <- c(BPRES1[1:120],BPRES2[121:nobs])
  # non-res building permits value, already SA
  BPNRES2 <- vseries1$`Building permits, non-residential`
  # non-res building permits value - old data
  if (ProvA=="CA") {
  BPNRES1 <- c(1256318.2,1074895.5,1327020.0,1644707.9,1700773.3,1538610.5,1925546.0,1610346.7,1549798.0,
               1757610.7,1585640.9,1239017.4,1082258.8, 969256.0,1207091.9,1339538.5,1504523.2,1820983.9,
               1900477.1,1590422.5,1467352.3,1789598.5,1724079.5,1272759.6,1291133.2,1155610.8,1243489.1,
               1713269.1,1723761.6,1938012.9,2089226.3,1541215.9,1661967.4,1709551.4,1338541.4,1338844.7,
                969572.7,1108562.7,1216663.0,1394373.5,1528744.9,2190053.8,1921341.6,1591796.4,1570004.2,
               1694727.1,2014455.2,1507153.7,1076043.4,1270990.5,2081381.8,1781179.9,1881607.6,2096897.4,
               1942267.7,2490127.6,1963562.3,2040601.3,1811055.8,1574699.5,1216392.6,1393026.4,2073210.0,
               1683299.6,2333844.7,2457418.2,2083711.5,2353019.3,2262420.1,2598260.1,2698939.8,2028382.7,
               2196318.4,1635152.9,2354463.3,2001677.1,3245393.6,2964612.8,2326422.2,2666255.5,2275557.7,
               2887368.0,2258284.8,2003225.6,2192199.1,1868700.8,1936326.3,2696423.1,3035722.9,2876028.5,
               2765467.5,2316610.6,3362574.9,2525720.6,2004692.2,1962389.1,1936292.1,1352350.8,2309226.6,
               2178500.1,2575994.4,2685005.8,2080234.7,2141014.9,2054113.5,2750481.5,2129098.2,2018614.1,
               1423382.4,1604737.6,2109403.4,2955114.7,2506025.8,3344587.7,2871087.8,2199160.2,2906517.8,
               2859120.3,2354820.7,1782310.4,1347777.0,2566969.3,2793439.2,1819042.2,2957673.9,3126431.2,
               2796103.2,2499754.9,2212422.9,2823976.8,2343970.2,2193345.2,1550238.1,2209689.7,2966961.4,
               2839630.0,2881175.7,2892362.8,2647668.9,3342986.1,2392727.8,3799505.6,2635913.7,2249727.3,
               1641934.0,1992921.7,2636159.1,2687101.0,2951521.0,2910659.4,4079929.1,2544091.6,2702271.0,
               3056093.8,2708547.3,2431265.5,2008661.2,2174434.5,2292438.9,2266228.5,2960838.6,4430309.7,
               4401948.5,2444799.9,3086010.0,3173517.0,2232403.3,2439730.9,1699958.1,1638554.6,2441602.4,
               3178056.0,2989708.6,3547676.6,3075305.5,2846731.9,2971124.5,3038541.6,2191632.1,2136621.6,
               1584258.8,2437068.6,2289104.1,2697397.0,2804335.6,2957528.1,3167224.0,3533532.6,2258389.2,
               2634742.4,2451266.4,1805111.3,1789774.1,1878115.2,2499071.8,2796736.1,3278243.7,4185895.4,
               3729674.9,2983650.0,3457658.9,3771378.2,2784991.0,2195521.6)
  } else {
  BPNRES1 <- canseriesSA(get_vector(old_data,
      "Building permits, non-residential old data"),first_date,styr,nobs)
  }
  BPNRES <- c(BPNRES1[1:120],BPNRES2[121:nobs])
  EPG2 <- vseries1$`Electric power generation`
  # electric power production (megawatt hours)
  EPG1 <- canseriesSA(get_vector(old_data,"Electric power generation old data"),
      first_date,styr,nobs) 
  EPG <- c(EPG1[1:84],EPG2[85:nobs])
  vseries2$`Building permits, residential` <- BPRES
  vseries2$`Building permits, non-residential` <- BPNRES
  vseries2$`Electric power generation` <- EPG
  if (!dir.exists("rds")) dir.create("rds")
  saveRDS(vseries2,file="rds/vseries2.rds")
} else {
  vseries2 <- readRDS(file="rds/vseries2.rds")
}

# Fix for Quebec CMHC housing starts = 0 in April 2020
# Causes divide-by-zero problems for % chg calculations later
if (ProvA == "QC") vseries2$`CMHC housing starts`[232] <- 30.000

if (output_level>1) {
  write_xlsx(vseries2,path=paste0("Output/",ProvA,
                                  "_indicators_as_read_from_STC.xlsx"))
}
# end of read_indicators commands
orig_growth12 <- mutate_if(vseries2,is.numeric,YOYRND)
orig_growth12 <- orig_growth12[-c(1:12),] # Remove NAs
orig_growth01 <- mutate_if(vseries2,is.numeric,MOMRND)
orig_growth01 <- orig_growth01[-c(1:12),] # Make same range as YOY
if (output_level>0) {
  write_xlsx(orig_growth12,path=paste0("Output/Orig_indic_growth12_",
                                       ProvA,".xlsx"))
  write_xlsx(orig_growth01,path=paste0("Output/Orig_indic_growth01_",
                                       ProvA,".xlsx"))
}
# Calculate and save cumulative % change during COVID-19 period
if (Detrend=="MoM") {
  Atemp <- filter(orig_growth01,REF_DATE>=as.Date("2020-03-01"))
  Btemp <- Atemp
  for (j in 2:ncol(Atemp)) {
    for (i in 2:nrow(Atemp)) {
      Btemp[i,j] <- round(100*((1+Atemp[i,j]/100)*(1+Btemp[i-1,j]/100)-1),2)
    }
  }
  if (output_level>1) {
    write_xlsx(Btemp,path=paste0("Output/",ProvA,"_",Detrend,
          "_cumulative_%chg_from_Feb2020.xlsx"))
  }
}

print("Indicators have been acquired/saved. Now identifying missing values...")
indicators_groups <- indicators1$Ind_grp
nind <- length(indicators_groups) # The number of indicators

# (3) Determine where series extensions are needed at the back and front ends 
# ===========================================================================

# Initialize a data frame, dts, to hold start and end points for missing data
# for every indicator in vseries
dts <- data.frame(matrix(as.numeric(NA),nrow=4,ncol=ncol(vseries2)-1))
nobs <- nrow(vseries2)
nvar <- ncol(vseries2)-1 # number of indicators (excluding REF_DATE of course)
REF_DATE <- as.Date(vseries2$REF_DATE) # saving REF_DATE for later

# Fill in the dts data frame
# Row 1 = NA if the data are there, otherwise first row (at start) where data are missing
# Row 2 = NA if the data are there, otherwise last row (at start) where data are missing
# Row 3 = NA if the data are there, otherwise first row (at end) where data are missing
# Row 4 = NA if the data are there, otherwise last row (at end) where data are missing
# NOTE: The programming assumes there are no NAs embedded within vseries2, i.e.
#       all NAs are at the beginning or the end of an indicator
for (j in 2:(nvar+1)) { # iterating over the vseries2 df, with nvar indicators
  flag <- 0 # means we are looking for NAs at start of series
  for (i in 1:nobs) { # iterating over the vseries2 df, with nobs rows
    if (i==1 & !is.na(vseries2[i,j])) {
      dts[1,j-1] <- NA
      flag <- 1 # means we are looking for NAs at end of series
    }
    if (is.na(vseries2[i,j]) & flag==0) {
      dts[1,j-1] <- 1
      dts[2,j-1] <- i
    }
    if (!is.na(vseries2[i,j]) & flag==0) {
      flag <- 1 # means we are looking for NAs at end of series
    }
    if (is.na(vseries2[i,j]) & flag==1) {
      dts[3,j-1] <- i
      flag <- 2 # means we found the first NA at end of series
    }  
    if (is.na(vseries2[i,j]) & flag==2) {
      dts[4,j-1] <- i
      flag <- 2
    }
  }
}
colnames(dts) <- colnames(vseries2[2:ncol(vseries2)])

# (4) Backcast and forecast the indicators, where needed, via ARIMA models
# ========================================================================

# Function to backcast or forecast a series with NAs at start or end of series.
# x is an indicator vector (eg. v66449943) starting at first_date and ending at
# last_date. Some of its elements may be NA, at the beginning
# and/or the end but not in the middle. If observations are missing in the 
# middle the indicator cannote be used in this program. dt1, dt2, dt3 and dt4 
# are numbers indicating the observations to be backcasted (between dt1 and dt2)
# and to be forecasted (between dt3 and dt4). If any of dt1, dt2, dt3, dt4 are
# NA there is no backcast or forecast to be done. REF_DATE is a vector
# with the same length as x with the dates corresponding to x.
EXTEND <- function(x,REF_DATE,dt1,dt2,dt3,dt4) {
  freq <- 12
  if (!is.na(dt1)) {
    bfct = dt2-dt1+1 # number of months to backcast
    revx <- ts(rev(x),frequency=12)
    revx <- as.numeric(revx[1:(length(revx)-dt2)])
    fc <- forecast(auto.arima(revx),bfct)
    revx1 <- c(revx,fc$mean) # extend with forecasts (in fc$mean)
    m <- month(as.POSIXlt(REF_DATE[1],format="%Y-%m-%d"))
    y <- year(as.POSIXlt(REF_DATE[1],format="%Y-%m-%d"))
    x <- as.numeric(ts(rev(revx1),start=c(y,m),frequency=12))
  }
  if (!is.na(dt3)) {
    ffct <- dt4-dt3+1 # number of months to forecast
    x <- x[1:(dt3-1)]
    fc <- forecast(auto.arima(x),ffct)
    x <- c(x,fc$mean)
  }
  return(x)
}

# Convert vseries2 and dts to matrix form after extracting REF_DATE
REF_DATE <- vseries2$REF_DATE
vseries3 <- vseries2[,2:ncol(vseries2)]
vseries3 <- as.matrix(vseries3) # EXTEND doesn't work with data frames
dts1 <- as.matrix(dts) # EXTEND doesn't work with data frames

# Now use EXTEND to fill in the missing values in the indicators
vseries4 <- vseries3
for (j in 1:(ncol(vseries3))) {
  if ( (!is.na(dts1[1,j])) | (!is.na(dts1[3,j])) ) {
    vseries4[,j] <- EXTEND(vseries3[,j],REF_DATE,dts1[1,j],dts1[2,j],
                           dts1[3,j],dts1[4,j])
  }
}
vseries4 <- as.data.frame(vseries4) # from matrix back to data frame
vseries4 <- mutate(vseries4,REF_DATE=REF_DATE) # add back the REF_DATE column
vseries4 <- select(vseries4,REF_DATE,everything()) # and put it at the front
if (output_level>0) {
  write_xlsx(vseries4,path=paste0("Output/indicators_after_ARIMA_",
  ProvA,"_",Detrend,".xlsx"))
}
print("Missing values identified and estimates substituted. Now normalizing...")
# (5) Calculate the YOY and MOM indicator growth rates
# ====================================================

# Calculate growth rates and mean 12-month growth rates for each series
growth12 <- mutate_if(vseries4,is.numeric,YOYRND)
growth12 <- growth12[-c(1:12),]
growth01 <- mutate_if(vseries4,is.numeric,MOMRND)
growth01 <- growth01[-c(1:12),]
if (output_level>0) {
  write_xlsx(growth12,path=paste0("Output/Extended_indic_growth12_",
             ProvA,".xlsx"))
  write_xlsx(growth01,path=paste0("Output/Extended_indic_growth01_",
             ProvA,".xlsx"))
}
growthxdate <- select(growth12,-REF_DATE)
Mean_growth <- apply(growthxdate,2,mean) # 2 = apply across columns (rows = 1)
Mean_growth <- round(Mean_growth,2)
Mean_growth <- data.frame(names(Mean_growth),Mean_growth)
#rownames(Mean_growth) <- c()
#Mean_growth <- rename(Mean_growth,replace=c("names.Mean_growth."="Series_name"))
if (output_level>0) {
  write_xlsx(Mean_growth,path=paste0("Output/Mean_growth_extend_indic_",
                                     ProvA,".xlsx"))
}

# (6) Detrend and normalize the indicators
# ========================================
if (Detrend=="YoY") {
  tseries <- mutate_if(vseries4,is.numeric,YOY)
} else if (Detrend=="MoM") {
  tseries <- mutate_if(vseries4,is.numeric,MOM)
}
tseries <- mutate_if(tseries,is.numeric,norm)
if (output_level>1) {
  roundPS <- function(x) {round(x,2)}
  tseries1 <- mutate_if(tseries,is.numeric,roundPS)
  write_xlsx(tseries1,path=paste0("Output/Detrended_and_normalized_indicators_",
                                 Detrend,"_",ProvA,".xlsx"))
}

# NOTE: Calculations from here on are based on the tseries df, containing
# the detrended and normalized indicators.
# In the case of MoM there are now data from Feb 2001 to last_date = nobs-1
# In the case of YoY there are now data from Jan 2002 to last_date = nobs-12
# We remove all of 2001 in both cases, i.e. nobs-12 observations regardless.

# (7) Calculate the principal components
# ======================================

# Information about principal components in R:
# http://www.gastonsanchez.com/visually-enforced/how-to/2012/06/17/PCA-in-R/

# http://stats.stackexchange.com/questions/143905/ddg#143949 
# "In PCA, you split a covariance (or correlation) matrix into scale part 
# (eigenvalues) and direction part (eigenvectors). You may then endow 
# eigenvectors with the scale: loadings. So, loadings thus become comparable 
# by magnitude with the covariances/correlations observed between the variables, 
# - because what had been drawn out from the variables' covariation now returns 
# back - in the form of the covariation between the variables and the principal 
# components. Actually, loadings are the covariances/correlations between the 
# original variables and the unit-scaled components. This answer shows 
# geometrically what loadings are and what are coefficients associating 
# components with variables in PCA or factor analysis."
print("Now calculating principal components...")
pcdf <- tseries # create the data frame for principal components analysis (PCA)
REF_DATE <- tseries$REF_DATE # save REF_DATE for use later
pcdf <- select(pcdf,-REF_DATE) # remove REF_DATE from the data frame
pcdf <- pcdf[-c(1:12),] # remove NAs due to YOY detrend calculation
REF_DATE <- REF_DATE[13:length(REF_DATE)] # save REF_DATE for use later
# princomp calculates PCs. The output is a list of 7 items called pc ==>
# (1) pc$sdev - standard deviations of the PCs (that is, of the eigenvalues)
# (2) pc$loadings - the linear combination wts (coefficients or eigenvectors)
#                whereby unit-scaled components define or "load" a variable
# (3) pc$center - the means subtracted (should be zeros because of norm)
# (4) pc$scale - the scalings applied (should be ones because of norm)
# (5) pc$n.obs - number of observations in pcdf
# (6) pc$scores - the principal components
# (7) pc$call - the call which is "princomp(x = pcdf, cor = TRUE)" in this case
pc = princomp(pcdf, cor = TRUE) # Calculate PCs
sdev <- pc$sdev # sqrt of eigenvalues = standard deviations
variances <- pc$sdev^2 # eigenvalues = variances
# Show percentage of variance due to 1st PC, in the console
print(paste0("Variance share of first principal component for ",ProvA,
             " = ",round(100*variances[1]/sum(variances),2)," %"),quote=FALSE)
text_results <- c(NULL,NULL,NULL,NULL,NULL)
text_results[1] <- paste0(ProvA,"_",Detrend)
text_results[2] <- as.character(round(100*variances[1]/sum(variances),2))
text_results[3] <- avg_growth_rates[ProvA]
# Collect variance results in a data frame and save
var_PrinCompOne <- data.frame(Standard_deviation=pc$sdev,Variance=variances,
                              Variance_share=100*variances/sum(variances))
if (output_level>0) {
  roundPS <- function(x) {round(x,2)}
  var_PrinCompOne1 <- mutate_if(var_PrinCompOne,is.numeric,roundPS)
  write_xlsx(var_PrinCompOne1,path=paste0("Output/PC_variance_results_",
                                         ProvA,"_",Detrend,".xlsx"))
}
# unclass returns a copy of its argument with its class attribute removed
# loadings is a df with nind coefficients (loadings) in the rows and 
# nind principal components in the columns. Only the first column matters here.
loadings <- as.data.frame(unclass(pc$loadings))
# PCs is a df with nobs-12 rows and nind principal components. Only column 1
# for the first principal component matters here.
PCs <- as.data.frame(unclass(pc$scores))
pc1 <- pc$scores[,1] # the 1st PC is the PCEI
# The mean of pc1 is zero and the standard deviation is the variance of 1st PC
# I tried with and without this next line, and the results were the same
#pc1 <- (pc1-mean(pc1))/sd(pc1) # normalize (although mean is zero already)
if (ProvA=="SK") pc1 <- -pc1
PrinCompOne <- data.frame(REF_DATE,pc1)
corr_PCs <- round(cor(loadings),2) # correlation matrix for PCs
corr_series <- round(as.data.frame(cor(pcdf)),2) # correlation matrix for series
if (output_level>0) {
  roundPS <- function(x) {round(x,2)}
  PCs1 <- mutate_if(PCs,is.numeric,roundPS)
  loadings1 <- mutate_if(loadings,is.numeric,roundPS)
  corr_series1 <- mutate_if(corr_series,is.numeric,roundPS)
  write_xlsx(PCs1,path=paste0("Output/indicators_PCs_",
                             ProvA,"_",Detrend,".xlsx"))
  write_xlsx(loadings1,path=paste0("Output/indicators_PC_loadings_",
                                  ProvA,"_",Detrend,".xlsx"))
  write_xlsx(corr_series,path=paste0("Output/indicators_correlations_",
                                     ProvA,"_",Detrend,".xlsx"))
}
# Create and save a correlation matrix chart for the indicators
# I removed this because it is too dense and unreadable.
#png(height=1200,width=1200,pointsize=10,filename=
#      paste0("Output/correlation_chart_",ProvA,"_",Detrend,".png"))
#corrplot(cor(pcdf),method="color",order="AOE",type="upper",
#         tl.col="black",addCoef.col="black")
#dev.off()
# Get first PC weights
wts <- unclass(pc$loadings) # class is "matrix" / "array", 53x53
wts_pc1 <- wts[,1] # class is "numeric", 1x53
if (ProvA=="SK") wts_pc1 <- -wts_pc1
# (8) Prepare component groups
# =============================
# Typically a PCEI is calculated both as a total and as the sum of a small
# number of groups. The following code sets up the groups. In the example
# here, four groups are defined. You can have more or
# fewer than this, but it is best to keep the number of groups small. Once you
# decide on your groups, you must choose three-character labels for each of them
# and record these labels in the indicators_group column of the indicators
# spreadsheet. Then modify the example code by substituting your own groups
# in the proper spots. 
print("Now setting up principal component groups...")
# Create the `Indicators Indicators_group` sub-PCs
wts_pc1_lab <- rep(0,length(indicators_groups))
wts_pc1_hhd <- rep(0,length(indicators_groups))
wts_pc1_prd <- rep(0,length(indicators_groups))
wts_pc1_inv <- rep(0,length(indicators_groups))
for (i in 1:length(wts_pc1)) {
  if (indicators_groups[i]=="lab") {
    wts_pc1_lab[i] <- wts_pc1[i]
  } else if (indicators_groups[i]=="hhd") {
    wts_pc1_hhd[i] <- wts_pc1[i]
  } else if (indicators_groups[i]=="prd") {
    wts_pc1_prd[i] <- wts_pc1[i]
  } else if (indicators_groups[i]=="inv") {
    wts_pc1_inv[i] <- wts_pc1[i] }
}
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><  
# %*% operator is used for matrix multiplication
`Labour market` <- as.matrix(pcdf) %*% wts_pc1_lab
`Household expenditure and income` <- as.matrix(pcdf) %*% wts_pc1_hhd
`Production` <- as.matrix(pcdf) %*% wts_pc1_prd
`Investment` <- as.matrix(pcdf) %*% wts_pc1_inv
`Labour market` <- norm(`Labour market`)
`Household expenditure and income` <- norm(`Household expenditure and income`)
`Production` <- norm(`Production`)
`Investment` <- norm(`Investment`)
PC_sum1 <- `Labour market`+
  `Household expenditure and income`+
  Production+
  Investment
(mean(PC_sum1))
(sd(PC_sum1))
PC_sum <- norm(PC_sum1)
text_results[4] <- round(PC_sum[length(PC_sum)],2)
text_results[5] <- round(PC_sum[length(PC_sum)]+avg_growth_rates[ProvA],2)
`Labour market` <- `Labour market`/sd(PC_sum1)
`Household expenditure and income` <- `Household expenditure and income`/sd(PC_sum1)
`Production` <- `Production`/sd(PC_sum1)
`Investment` <- `Investment`/sd(PC_sum1)
wts_data <- PrinCompOne
wts_data <- mutate(wts_data,PC_sum=`Labour market`+
                     `Household expenditure and income`+
                     `Production`+
                     `Investment`)
wts_data$REF_DATE <- as.Date(wts_data$REF_DATE)

first_date <- as.Date(first_date)
last_date <- as.Date(last_date)
first_year <- getYear(first_date)
first_month <- getMonth(first_date)
first_day <- getDay(first_date)
first_year_after <- as.numeric(first_year)+1
first_date_after <- as.Date(paste0(as.character(first_year_after),"-",
                                   first_month,"-",
                                   first_day))
# Build a long-form data frame with the indicator groups
PCs <- data.frame(REF_DATE=rep(seq(first_date_after,last_date,by="month"),4))
PCs <- mutate(PCs,`Indicators group: `=
                c(rep("Labour market",nobs_after_YOY),
                  rep("Household expenditure and income",nobs_after_YOY),
                  rep("Production",nobs_after_YOY),
                  rep("Investment",nobs_after_YOY)))
PCs <- mutate(PCs,value=c(`Labour market`,`Household expenditure and income`,
                          `Production`,`Investment`))

# Make data columns for the covid months for purposes of chart display
wts_data <- mutate(wts_data,covid_months1=NA)
# 218 is Feb 2020
c1 <- which(colnames(wts_data)=="covid_months1")
c2 <- which(colnames(wts_data)=="PC_sum")
wts_data[218:nrow(wts_data),c1] <- wts_data[218:nrow(wts_data),c2]
wts_data <- mutate(wts_data,covid_month_names1=NA)
c3 <- which(colnames(wts_data)=="covid_month_names1")
if (Detrend=="YoY") {
  wts_data[218:nrow(wts_data),c3] <- paste0(as.character(filter(wts_data,
    REF_DATE>as.Date("2020-01-01"))$REF_DATE,format="%b")," ",
    round(wts_data$PC_sum[218:nrow(wts_data)]+avg_growth_rates[ProvA],1),"%") 
  wts_data <- mutate(wts_data,y1=MA13CXE(PC_sum)+avg_growth_rates[ProvA])
  wts_data <- mutate(wts_data,y2=PC_sum+avg_growth_rates[ProvA])
  wts_data <- mutate(wts_data,y3=covid_months1+avg_growth_rates[ProvA])
  wts_data <- mutate(wts_data,y4=PC_sum)
  wts_data <- mutate(wts_data,y5=covid_months1)
  wts_data <- mutate(wts_data,y6=NA)
  c4 <- which(colnames(wts_data)=="y6")
  wts_data[218:nrow(wts_data),c4] <- paste0(as.character(filter(wts_data,
    REF_DATE>as.Date("2020-01-01"))$REF_DATE,format="%b")," ",
    round(wts_data$PC_sum[218:nrow(wts_data)],1),"%")  
} else { # Detrend == "MoM"
  wts_data[218:nrow(wts_data),c3] <- paste0(as.character(filter(wts_data,
    REF_DATE>as.Date("2020-01-01"))$REF_DATE,format="%b")," ",
    round(wts_data$PC_sum[218:nrow(wts_data)]+avg_growth_rates[ProvA]/12,1),"%")
  wts_data <- mutate(wts_data,y1=MA13CXE(PC_sum)+avg_growth_rates[ProvA]/12)
  wts_data <- mutate(wts_data,y2=PC_sum+avg_growth_rates[ProvA]/12)
  wts_data <- mutate(wts_data,y3=covid_months1+avg_growth_rates[ProvA]/12)
  wts_data <- mutate(wts_data,y4=PC_sum)
  wts_data <- mutate(wts_data,y5=covid_months1)
  wts_data <- mutate(wts_data,y6=NA)
  c4 <- which(colnames(wts_data)=="y6")
  wts_data[218:nrow(wts_data),c4] <- paste0(as.character(filter(wts_data,
    REF_DATE>as.Date("2020-01-01"))$REF_DATE,format="%b")," ",
    round(wts_data$PC_sum[218:nrow(wts_data)],1),"%")
}

# (9) Make charts with the PCEI and its indicator groups
# =======================================================

# Chart 1. The PCEI over its entire 18-year+ time range.
# The chart shows growth deviations from long-term trend, either the
# annual trend or the monthly (annual/12) trend.
print("Now making chart 1...")
p_xtitle <- "" # for all charts
MA13_end_date <- REF_DATE[224] # August 2019 + 6 lead months gives February 2020
title1 <- paste0(province," composite economic indicator, January 2002 to ",
                 MonR," ",enyr)
title2 <- paste0("This PCEI averages ",nind," monthly economic indicators, ",
                 "constructed to have zero mean and unit variance.")
if (Detrend=="YoY") {
  title3 <- paste0("A value of +1 means 12-month growth is one percentage point ",
                   "above trend. The trend annual real growth rate ",
                   "for 2002-2018 = ",avg_growth_rates[ProvA],"%.")
} else {
  title3 <- paste0("A value of +1 means 1-month growth is one percentage point ",
                   "above trend. The trend monthly real growth rate ",
                   "for 2002-2018 = ",
                   round(as.numeric(avg_growth_rates[ProvA])/12,2),"%.")  
}
p_title <- paste0("<b><span style = 'color:navyblue;'>
                  <span style = 'font-size:16pt'>",
                  title1,"</span></b><br>",
                  "<b><span style = 'color:black;'>
                  <span style = 'font-size:12pt'>",
                  title2,"</span></b><br>",
                  "<b><span style = 'color:black;'>
                  <span style = 'font-size:12pt'>",
                  title3,"</span></b>")
p_title_background_colour <- "wheat"
p_recession_colour <- "slategray1"
p_xtitle <- ""
if (Detrend=="YoY") {
  p_ytitle <- "Percentage above or below 12-month growth trend"
} else {
  p_ytitle <- "Percentage above or below 1-month growth trend"
}
p_caption <- paste0("Derived as the first principal component of ",nind,
            " Statistics Canada monthly time series.\n@PhilSmith26")
p_savetitle <- paste0("Output/PCEI_1_",ProvA,"_",Detrend,"_18Y.jpg")

lsize <- 1.2
p1 <- ggplot() +
  geom_bar(data=PCs,aes(x=REF_DATE,y=value,fill=`Indicators group: `),
           stat="identity") +
  geom_line(data=wts_data,aes(x=REF_DATE,y=PC_sum),size=0.8,colour="navyblue") +
  #geom_text(label="Above\ntrend\ngrowth",aes(x=as.Date("2022-01-01"),y=0.6),
  #          colour="black",size=3,angle=0) +
  #geom_text(label="Below\ntrend\ngrowth",aes(x=as.Date("2022-01-01"),y=-0.6),
  #          colour="black",size=3,angle=0) +
  #geom_segment(aes(x=as.Date("2021-04-01"),xend=as.Date("2021-04-01"),y=0.1,
  #                 yend=0.45),colour="black",
  #             size=0.5,arrow=arrow(length=unit(0.2,"cm"))) +
  #geom_segment(aes(x=as.Date("2021-04-01"),xend=as.Date("2021-04-01"),y=-0.1,
  #                 yend=-0.45),colour="black",
  #             size=0.5,arrow=arrow(length=unit(0.2,"cm"))) +
  geom_hline(yintercept=0,size=0.9,colour="black") +
  scale_x_date(breaks=c(as.Date("2002-01-01"),as.Date("2005-01-01"),
                        as.Date("2008-01-01"),as.Date("2011-01-01"),
                        as.Date("2014-01-01"),as.Date("2017-01-01"),
                        last_date),labels=
                 c("Jan 2002","Jan 2005","Jan 2008","Jan 2011",
                   "Jan 2014","Jan 2017",
                   paste0(str_sub(MonR,1,3)," ",enyr))) +
  scale_y_continuous(breaks=c(seq(-12,12,by=1)),
                     labels=c("-12%","-11%","-10%","-9%","-8%","-7%","-6%",
                              "-5%","-4%","-3%","-2%","-1%","0",
                              "1%","2%","3%","4%","5%","6%",
                              "7%","8%","9%","10%","11%","12%")) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(plot.title = element_text(size=14,face="bold")) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  labs(title = p_title,
       caption = p_caption,
       x = p_xtitle, y = p_ytitle) +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(
          size = 13,
          lineheight = 1,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          margin = margin(0, 0, 5.5, 0),
          fill = p_title_background_colour)
  ) +
  scale_fill_brewer(palette="Spectral") +
  theme(legend.position="top") +
  guides(fill=guide_legend(reverse=TRUE))
p1
ggsave(p_savetitle,p1,height=9,width=16,dpi=300) # save as a .jpg file

# Chart 2. The PCEI over  a short, 5-year+ time range.
# The chart shows growth deviations from long-term trend, either the
# annual trend or the monthly (annual/12) trend.
print("Now making chart 2...")
title1 <- paste0(province," composite economic indicator, January 2015 to ",
                 MonR," ",enyr)
title2 <- paste0("This composite indicator averages ",nind,
                 " monthly economic indicators, ",
                 "constructed to have zero mean and unit variance.")
if (Detrend=="YoY") {
  title3 <- paste0("A value of +1 means 12-month growth is one percentage point ",
                   "above trend. The trend annual real growth rate ",
                   "for 2002-2018 = ",avg_growth_rates[ProvA],"%.")
} else {
  title3 <- paste0("A value of +1 means 1-month growth is one percentage point ",
                   "above trend. The trend monthly real growth rate ",
                   "for 2002-2018 = ",
                   round(as.numeric(avg_growth_rates[ProvA])/12,2),"%.")  
}
p_title <- paste0("<b><span style = 'color:navyblue;'>
                  <span style = 'font-size:16pt'>",
                  title1,"</span></b><br>",
                  "<b><span style = 'color:black;'>
                  <span style = 'font-size:12pt'>",
                  title2,"</span></b><br>",
                  "<b><span style = 'color:black;'>
                  <span style = 'font-size:12pt'>",
                  title3,"</span></b>")
p_title_background_colour <- "wheat"
p_recession_colour <- "slategray1"
p_xtitle <- ""
if (Detrend=="YoY") {
  p_ytitle <- "Percentage above or below 12-month growth trend"
} else {
  p_ytitle <- "Percentage above or below 1-month growth trend"
}
p_caption <- paste0("Derived as the first principal component of ",nind,
          " Statistics Canada monthly time series.\n@PhilSmith26")
p_savetitle <- paste0("Output/PCEI_2_",ProvA,"_",Detrend,"_5Y.jpg")
lsize <- 1.2
p2<- ggplot() +
  geom_bar(data=filter(PCs,REF_DATE>as.Date("2014-12-01")),
           aes(x=REF_DATE,y=value,fill=`Indicators group: `),stat="identity") +
  geom_line(data=filter(wts_data,REF_DATE>as.Date("2014-12-01")),
            aes(x=REF_DATE,y=PC_sum),size=0.8,colour="navyblue") +
  #geom_text(label="Above\ntrend\ngrowth",aes(x=as.Date("2021-01-01"),y=0.7),
  #          colour="black",size=4,angle=0) +
  #geom_text(label="Below\ntrend\ngrowth",aes(x=as.Date("2021-01-01"),y=-0.7),
  #          colour="black",size=4,angle=0) +
  #geom_segment(aes(x=as.Date("2020-11-01"),xend=as.Date("2020-11-01"),y=0.2,
  #                 yend=0.6),colour="black",
  #             size=0.6,arrow=arrow(length=unit(0.2,"cm"))) +
  #geom_segment(aes(x=as.Date("2020-11-01"),xend=as.Date("2020-11-01"),y=-0.2,
  #                 yend=-0.6),colour="black",
  #             size=0.6,arrow=arrow(length=unit(0.2,"cm"))) +
  geom_hline(yintercept=0,size=0.9,colour="black") +
  scale_x_date(breaks=c(as.Date("2015-01-01"),as.Date("2016-01-01"),
                        as.Date("2017-01-01"),as.Date("2018-01-01"),
                        as.Date("2019-01-01"),as.Date("2020-01-01"),last_date),
               labels=c("Jan 2015","Jan 2016","Jan 2017","Jan 2018","Jan 2019",
                        "Jan 2020",paste0(str_sub(MonR,1,3)," ",enyr)),
               limits=c(as.Date("2015-01-01"),as.Date("2021-04-01"))) +
  scale_y_continuous(breaks=c(seq(-12,12,by=1)),
                     labels=c("-12%","-11%","-10%","-9%","-8%","-7%","-6%",
                              "-5%","-4%","-3%","-2%","-1%","0",
                              "1%","2%","3%","4%","5%","6%",
                              "7%","8%","9%","10%","11%","12%")) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(plot.title = element_text(size=14,face="bold")) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  labs(title = p_title,
       caption = p_caption,
       x = p_xtitle, y = p_ytitle) +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(
          size = 13,
          lineheight = 1,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          margin = margin(0, 0, 5.5, 0),
          fill = p_title_background_colour)
  ) +
  scale_fill_brewer(palette="Spectral") +
  theme(legend.position="top") +
  guides(fill=guide_legend(reverse=TRUE))
p2
ggsave(p_savetitle,p2,height=9,width=16,dpi=300) 

# Chart 3. The PCEI over  a long, 18-year+ time range.
# The chart shows growth deviations from long-term trend, either the
# annual trend or the monthly (annual/12) trend, plus a 13-month centred
# moving-average (smoothed) line..
print("Now making chart 3...")
title1 <- paste0(province," composite economic indicator, January 2002 to ",
                 MonR," ",enyr)
title2 <- paste0("This PCEI averages ",nind," monthly economic indicators, ",
                 "constructed to have zero mean and unit variance.")
if (Detrend=="YoY") {
  title3 <- paste0("A value of +1 means 12-month growth is one percentage ",
                   "point above trend. The trend annual real growth rate ",
                   "for 2002-2018 = ",avg_growth_rates[ProvA],"%.")
} else {
  title3 <- paste0("A value of +1 means 1-month growth is one percentage ",
                   "point above trend. The trend monthly real growth rate ",
                   "for 2002-2018 = ",
                   round(as.numeric(avg_growth_rates[ProvA])/12,2),"%.")  
}
title4 <- paste0("The thin black line is the indicator. The solid green ",
                 "line is a centred 13-month moving-average of the indicator.")
p_title <- paste0("<b><span style = 'color:navyblue;'>
                  <span style = 'font-size:16pt'>",
                  title1,"</span></b><br>",
                  "<b><span style = 'color:black;'>
                  <span style = 'font-size:12pt'>",
                  title2,"</span></b><br>",
                  "<b><span style = 'color:black;'>
                  <span style = 'font-size:12pt'>",
                  title3,"</span></b><br>",
                  "<b><span style = 'color:black;'>
                  <span style = 'font-size:12pt'>",
                  title4,"</span></b>")
p_title_background_colour <- "wheat"
p_xtitle <- ""
if (Detrend=="YoY") {
  p_ytitle <- "Percentage above or below 12-month growth trend"
} else {
  p_ytitle <- "Percentage above or below 1-month growth trend"
}
p_caption <- paste0("Derived as the first principal component of ",nind,
            " Statistics Canada monthly time series.\n@PhilSmith26")
p_savetitle <- paste0("Output/PCEI_3_",ProvA,"_",Detrend,"_18Y_MA13C.jpg")
lsize <- 1.2
MA13_end_date <- REF_DATE[length(REF_DATE)-2]
p3 <- ggplot() +
  # THIS IS STILL WRONG
  geom_line(data=filter(wts_data,REF_DATE<MA13_end_date),aes(x=REF_DATE,
                        y=MA13CXE(PC_sum)),size=1.5,colour="forestgreen") +
  geom_line(data=wts_data,aes(x=REF_DATE,y=PC_sum),size=0.4,colour="black") +
  #geom_text(label="Above\ntrend\ngrowth",aes(x=as.Date("2022-04-01"),y=0.4),
  #          colour="black",size=4,angle=0) +
  #geom_text(label="Below\ntrend\ngrowth",aes(x=as.Date("2022-04-01"),y=-0.4),
  #          colour="black",size=4,angle=0) +
  #geom_segment(aes(x=as.Date("2021-10-01"),xend=as.Date("2021-10-01"),y=0.2,
  #                 yend=0.5),colour="black",
  #             size=0.5,arrow=arrow(length=unit(0.2,"cm"))) +
  #geom_segment(aes(x=as.Date("2021-10-01"),xend=as.Date("2021-10-01"),y=-0.2,
  #                 yend=-0.5),colour="black",
  #             size=0.5,arrow=arrow(length=unit(0.2,"cm"))) +
  geom_point(data=filter(wts_data,REF_DATE>=as.Date("2020-02-01")),
             aes(x=REF_DATE,y4),colour="black")+
  geom_text(data=wts_data,aes(x=REF_DATE,y=y5,
            label=y6),hjust=-0.1,vjust=0) +
  geom_hline(yintercept=0,size=0.9,colour="black") +
  scale_x_date(breaks=c(as.Date("2002-01-01"),as.Date("2005-01-01"),
                        as.Date("2008-01-01"),as.Date("2011-01-01"),
                        as.Date("2014-01-01"),as.Date("2017-01-01"),
                        last_date),
               labels=c("Jan 2002","Jan 2005","Jan 2008","Jan 2011","Jan 2014",
                        "Jan 2017",paste0(str_sub(MonR,1,3)," ",enyr))) +
  scale_y_continuous(breaks=seq(-20,20,by=1)) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(plot.title = element_text(size=14,face="bold")) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  labs(title = p_title,
       caption = p_caption,
       x = p_xtitle, y = p_ytitle) +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(
          size = 13,
          lineheight = 1,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          margin = margin(0, 0, 5.5, 0),
          fill = p_title_background_colour)
  ) +
  scale_fill_brewer(palette="Spectral") +
  theme(legend.position="top") +
  guides(fill=guide_legend(reverse=TRUE))
p3
ggsave(p_savetitle,p3,height=9,width=16,dpi=300)

# Chart 4. The PCEI over a long, 18-year+ time range.
# The chart shows growth, either the YoY or MoM growth, scaled to the
# average trend annual or the monthly (annual/12) trend, plus a 13-month 
# centred moving-average (smoothed) line.
print("Now making chart 4...")
title1 <- paste0(province," composite economic indicator, January 2002 to ",
                 MonR," ",enyr)
if (Detrend=="YoY") {
  title2 <- paste0("This PCEI averages the 12-month percentage changes of ",
                   nind," monthly economic indicators that are seasonally- ",
                   "and inflation-adjusted.")
  title3 <- paste0("The weights are from the first principal component. The ",
                   "PCEI is scaled to the trend annual real growth ",
                   "rate for 2002-2018, which is ",
                   round(avg_growth_rates[ProvA],1),"%.")
} else {
  title2 <- paste0("This PCEI averages the 1-month percentage changes of ",
                   nind," monthly economic indicators that are seasonally- ",
                   "and inflation-adjusted.")
  title3 <- paste0("The weights are from the first principal component. The ",
                   "PCEI is scaled to the trend monthly real growth ",
                 "rate for 2002-2018, which is ",
                 round(avg_growth_rates[ProvA]/12,2),"%.")    
}
title4 <- paste0("The thin black line is the indicator. The solid green ",
                 "line is a centred 13-month moving-average of the indicator.")
p_title <- paste0("<b><span style = 'color:navyblue;'>
                  <span style = 'font-size:16pt'>",
                  title1,"</span></b><br>",
                  "<b><span style = 'color:black;'>
                  <span style = 'font-size:12pt'>",
                  title2,"</span></b><br>",
                  "<b><span style = 'color:black;'>
                  <span style = 'font-size:12pt'>",
                  title3,"</span></b><br>",
                  "<b><span style = 'color:black;'>
                  <span style = 'font-size:12pt'>",
                  title4,"</span></b>")
p_title_background_colour <- "wheat"
p_xtitle <- ""
p_ytitle <- "Percent"
p_caption <- paste0("Derived as the first principal component of ",nind,
          " Statistics Canada monthly time series.\n@PhilSmith26")
if (Detrend=="YoY") {
  p_savetitle <- paste0("Output/PCEI_4_",ProvA,"_",Detrend,"_18Y_MA13C_Scaled.jpg")
} else {
  p_savetitle <- paste0("Output/PCEI_4_",ProvA,"_",Detrend,"_18Y_MA13C_Scaled.jpg")
}
lsize <- 1.2
p4 <- ggplot() +
  geom_line(data=filter(wts_data,REF_DATE<MA13_end_date),aes(x=REF_DATE,
                        y=y1),size=1.5,colour="forestgreen") +
  geom_line(data=wts_data,aes(x=REF_DATE,
                        y=y2),size=0.4,colour="black") + 
  geom_hline(yintercept=0,size=0.9,colour="black") +
  geom_point(data=wts_data,aes(x=REF_DATE,y3),colour="black")+
  geom_text(data=wts_data,aes(x=REF_DATE,y=y2,
            label=as.character(covid_month_names1)),hjust=-0.1,vjust=0) +
  scale_x_date(breaks=c(as.Date("2002-01-01"),as.Date("2005-01-01"),
                        as.Date("2008-01-01"),as.Date("2011-01-01"),
                        as.Date("2014-01-01"),as.Date("2017-01-01"),
                        last_date),
               labels=c("Jan 2002","Jan 2005","Jan 2008","Jan 2011",
                        "Jan 2014","Jan 2017",
                        paste0(str_sub(MonR,1,3)," ",enyr)),
               limits=c(as.Date("2002-01-01"),as.Date("2021-03-01"))) +
  scale_y_continuous(breaks=seq(-20,20,by=1)) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(plot.title = element_text(size=14,face="bold")) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  labs(title = p_title,
       caption = p_caption,
       x = p_xtitle, y = p_ytitle) +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(
          size = 13,
          lineheight = 1,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          margin = margin(0, 0, 5.5, 0),
          fill = p_title_background_colour)
  ) +
  scale_fill_brewer(palette="Spectral") +
  theme(legend.position="top") +
  guides(fill=guide_legend(reverse=TRUE))
p4
ggsave(p_savetitle,p4,height=9,width=16,dpi=300)

#===============================================================================
# Chart 5. The PCEI over  a long, 18-year+ time range. The chart shows growth,
# either the YoY or MoM growth, scaled to the average annual trend or the 
# monthly (annual/12) trend, plus a 13-month centred moving-average (smoothed) 
# line. The growth numbers for the COVID-19 months are marked on the chart.
print("Now making chart 5 and saving results...")
if (Detrend=="YoY") {
  p_title <- paste0(province," composite economic indicator\n",
                    "12-month percentage change, seasonally adjusted, ",
                    "inflation adjusted\n",
                    "Scaled to average annual economic growth for 2002-2018 = ",
                    round(avg_growth_rates[ProvA],1),"%")
  p_ytitle <- "12-month (year-over-year) % growth trend"
  } else {
  p_title <- paste0(province," composite economic indicator\n",
                "1-month percentage change, seasonally adjusted, ",
                "inflation adjusted\n",
                "Scaled to average monthly economic growth for 2002-2018 = ",
                round(avg_growth_rates[ProvA]/12,2),"%")
  p_ytitle <- "1-month % growth trend"
}
p_caption <- "@PhilSmith26"
p_savetitle <- paste0("Output/PCEI_5_",ProvA,"_",Detrend,"_18Y_NC.jpg")
lsize <- 1.2

p5 <- ggplot() +
  geom_line(data=filter(wts_data,REF_DATE<=MA13_end_date),
            aes(x=REF_DATE,y=y1),
            size=lsize,colour="forestgreen") +
  geom_line(data=wts_data,aes(x=REF_DATE,y=y2),
            size=0.4,colour="black") + 
  geom_text(aes(x=as.Date("2003-01-01"),y=-1.5,
            label="Black = the composite economic indicator"),
            colour="black",size=5,hjust=0) +
  geom_text(aes(x=as.Date("2003-01-01"),y=-1.9,
            label="Green = the smoothed composite economic indicator"),
            colour="forestgreen",size=5,hjust=0) +
  geom_point(data=wts_data,aes(x=REF_DATE,y3),colour="black")+
  geom_text(data=wts_data,aes(x=REF_DATE,y=y2,
            label=as.character(covid_month_names1)),hjust=-0.1,vjust=0) +
  geom_hline(yintercept=0,size=0.5,colour="black") +
  scale_x_date(breaks=c(as.Date("2002-01-01"),as.Date("2005-01-01"),
                        as.Date("2008-01-01"),as.Date("2011-01-01"),
                        as.Date("2014-01-01"),as.Date("2017-01-01"),
                        last_date),
                        labels=c("Jan 2002","Jan 2005","Jan 2008","Jan 2011",
                                 "Jan 2014","Jan 2017",
                                 paste0(str_sub(MonR,1,3)," ",enyr)),
                        limits=c(as.Date("2002-01-01"),last_date+360)) +
  scale_y_continuous(breaks=seq(-20,20,by=1)) +
  scale_fill_brewer(palette="Spectral") +
  theme(axis.text.x = element_text(size=8)) + 
  theme(axis.text.y = element_text(size=8)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(plot.title = element_text(size=14,face="bold")) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  labs(title = p_title,
       caption = p_caption,
       x = NULL, y = p_ytitle) +
  theme(legend.position="none") +
  theme(plot.background=element_rect(fill="lightblue")) +
  guides(fill=guide_legend(reverse=TRUE))
p5
ggsave(p_savetitle,p5,height=9,width=16,dpi=300) # save this chart
saveRDS(p5,paste0("rds/PCEI_",ProvA,"_",Detrend,"_18Y_COMP.rds")) 
saveRDS(text_results,paste0("rds/PCEI_",ProvA,"_",Detrend,"_text_results.rds"))
saveRDS(PC_sum,paste0("rds/PCEI_",ProvA,"_",Detrend,"_PC_sum.rds"))
if (Detrend=="YoY") {
  saveRDS((PC_sum+avg_growth_rates[ProvA]),paste0("rds/PCEI_",ProvA,"_",Detrend,
                                                "_PC_sum_pt.rds"))
} else {
  saveRDS((PC_sum+avg_growth_rates[ProvA]/12),paste0("rds/PCEI_",ProvA,"_",
                                                     Detrend,"_PC_sum_pt.rds"))
}
p5 # Display the last chart before exiting the function

# Report to the console: time to do the calculations 
end_time <- as.character(Sys.time())
(a <- paste0("Starting time was ",start_time))
(b <- paste0("Ending time was ",end_time))
print(a)
print(b)
(total_time <- difftime(end_time,start_time))
} # End of function PCEI_calc
#===============================================================================

# Set province
ProvA <- ""
prov_codes <- c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","CA")
prov_codes <- c("PE","NS","NB","QC","ON","MB","SK","AB","BC","CA")

# Run for one province:
system.time({PCEI_calc(ProvA,MonR,first_date,
                       last_date,"MoM",read_indicators,output_level)}) 
system.time({PCEI_calc(ProvA,MonR,first_date,
                       last_date,"YoY",read_indicators,output_level)}) 

# Or run for all provinces:
Detrend="MoM"
read_indicators <- TRUE
for (prov in prov_codes) {
  system.time({PCEI_calc(prov,MonR,first_date,last_date,
                         Detrend,read_indicators,output_level)})  
}  
Detrend="YoY"
read_indicators <- FALSE
for (prov in prov_codes) {
  system.time({PCEI_calc(prov,MonR,first_date,last_date,
                         Detrend,read_indicators,output_level)})  
}

