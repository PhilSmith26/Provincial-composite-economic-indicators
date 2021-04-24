# Program to combine PCEI information from multiple provinces in charts and 
# tables. This program is for combining MoM-based PCEIs. This version 
# benchmarks the provincial PCEIs to the national monthly GDP.
# Last update: April 23, 2021

reference_month <- "February"
first_date <- "2001-01-01"
last_date <- "2021-02-01"
first_usable_date <- "2002-01-01"
enyr <- "2021"
GDPnotAvailyet <- TRUE
FlashEstimate <- 0.5 # released March 31, 2021

# NOTE _ GDP must be updated correctly at line 101

pkgs <- c("tidyverse","cansim","lubridate","gdata","dataframes2xls",
          "readxl", "writexl","forecast","seasonal","stringr", "ggtext",
          "corrplot","gt","ggpubr")
inst <- lapply(pkgs,library,character.only=TRUE)

SEASADJ <- function(x) { 
  x1 <- ts(data=x,start=2002,frequency=12)
  x1_seas <- seas(x1)
  x1_sa <- final(x1_seas)
  y <- as.numeric(x1_sa)
}

MA13CXE <- function(x) { # 13-month centred moving average
  k <- length(x)
  y <- (lag(x,6)+lag(x,5)+lag(x,4)+lag(x,3)+lag(x,2)+lag(x,1)+x+
          lead(x,1)+lead(x,2)+lead(x,3)+lead(x,4)+lead(x,5)+lead(x,6))/13
}

prov_names <- c(AT="Atlantic Canada",CE="Central Canada",
                WE="Western Canada",CA="Canada")

# Steps in this program:
# (1) Create vectors for average GDP shares and average GDP growth rates
# (2) Retrieve MoM PCEI values that were calculated and stored elsewhere
# (3) Calculate implicit PCEIs for higher-level aggregates
# (4) Generate charts and tables

# These are the 2019 shares; formerly were the average GDP shares 2002-2018
shares_AT <- c(NL=0.2770,PE=0.05935,NS=0.3651,NB=0.2986)
shares_CE <- c(QC=0.3405,ON=0.6595)
shares_WE <- c(MB=0.09030,SK=0.1015,AB=0.4318,BC=0.3764)
shares_CA <- c(NL=0.0154,PE=0.0033,NS=0.0203,NB=0.0166,QC=0.2003,
               ON=0.3880,MB=0.0321,SK=0.0361,AB=0.1535,BC=0.1338,
               AT=0.0556,CE=0.5883,WE=0.3555,CA=1.000)

# Average real GDP growth rate 2002-2018
avg_growth_rates <- c(NL=1.28,PE=2.21,NS=0.99,NB=0.97,QC=1.67,ON=1.66,
  MB=2.01,SK=1.98,AB=2.67,BC=2.74,CA=1.94,AT=1.11,CE=1.66,WE=2.54)

# Retrieve the PCEIs for 10 provinces, WITH AVG MONTHLY GROWTH ADDED IN
PC_sum_NL <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
            "summary_indicators/Multi_prov/NL/rds/PCEI_NL_MoM_PC_sum_pt.rds"))
PC_sum_PE <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
            "summary_indicators/Multi_prov/PE/rds/PCEI_PE_MoM_PC_sum_pt.rds"))
PC_sum_NS <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
            "summary_indicators/Multi_prov/NS/rds/PCEI_NS_MoM_PC_sum_pt.rds"))
PC_sum_NB <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
            "summary_indicators/Multi_prov/NB/rds/PCEI_NB_MoM_PC_sum_pt.rds"))
PC_sum_QC <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
            "summary_indicators/Multi_prov/QC/rds/PCEI_QC_MoM_PC_sum_pt.rds"))
PC_sum_ON <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
            "summary_indicators/Multi_prov/ON/rds/PCEI_ON_MoM_PC_sum_pt.rds"))
PC_sum_MB <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
            "summary_indicators/Multi_prov/MB/rds/PCEI_MB_MoM_PC_sum_pt.rds"))
PC_sum_SK <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
            "summary_indicators/Multi_prov/SK/rds/PCEI_SK_MoM_PC_sum_pt.rds"))
PC_sum_AB <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
            "summary_indicators/Multi_prov/AB/rds/PCEI_AB_MoM_PC_sum_pt.rds"))
PC_sum_BC <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
            "summary_indicators/Multi_prov/BC/rds/PCEI_BC_MoM_PC_sum_pt.rds"))

# Calculate implicit not-yet-benchmarked aggregates
PC_sum_AT <- shares_AT["NL"]*PC_sum_NL + shares_AT["PE"]*PC_sum_PE + 
             shares_AT["NS"]*PC_sum_NS + shares_AT["NB"]*PC_sum_NB
PC_sum_CE <- shares_CE["QC"]*PC_sum_QC + shares_CE["ON"]*PC_sum_ON
PC_sum_WE <- shares_WE["MB"]*PC_sum_MB + shares_WE["SK"]*PC_sum_SK + 
             shares_WE["AB"]*PC_sum_AB + shares_WE["BC"]*PC_sum_BC
PC_sum_CA <- shares_CA["NL"]*PC_sum_NL + shares_CA["PE"]*PC_sum_PE + 
             shares_CA["NS"]*PC_sum_NS + shares_CA["NB"]*PC_sum_NB + 
             shares_CA["QC"]*PC_sum_QC + shares_CA["ON"]*PC_sum_ON + 
             shares_CA["MB"]*PC_sum_MB + shares_CA["SK"]*PC_sum_SK + 
             shares_CA["AB"]*PC_sum_AB + shares_CA["BC"]*PC_sum_BC
# Create a data frame containing the PCs
# These results have the average provincial growth rates added in,
# but they have not yet been benchmarked to the monthly GDP growth rate
PCEI_df <- data.frame(REF_DATE=seq.Date(as.Date(first_usable_date),
  as.Date(last_date),by="month"),NL=PC_sum_NL,PE=PC_sum_PE,
  NS=PC_sum_NS,NB=PC_sum_NB,QC=PC_sum_QC,
  ON=PC_sum_ON,MB=PC_sum_MB,SK=PC_sum_SK,
  AB=PC_sum_AB,BC=PC_sum_BC,AT=PC_sum_AT,
  CE=PC_sum_CE,WE=PC_sum_WE,CA=PC_sum_CA)
# Retrieve GDP monthly growth and include it in PCEI_df
GDPdf <- get_cansim_vector("v65201210","2001-12-01")
GDP <- 100*(GDPdf$VALUE/lag(GDPdf$VALUE,1)-1)
GDP <- GDP[2:length(GDP)]
indx <- case_when(
  (reference_month=="December"  & enyr==2020) ~ 228,
  (reference_month=="January"   & enyr==2021) ~ 229,
  (reference_month=="February"  & enyr==2021) ~ 230,
  (reference_month=="March"     & enyr==2021) ~ 231,
  (reference_month=="April"     & enyr==2021) ~ 232,
  (reference_month=="May"       & enyr==2021) ~ 233,
  (reference_month=="June"      & enyr==2021) ~ 234,
  (reference_month=="July"      & enyr==2021) ~ 235,
  (reference_month=="August"    & enyr==2021) ~ 236,
  (reference_month=="September" & enyr==2021) ~ 237,
  (reference_month=="October"   & enyr==2021) ~ 238,
  (reference_month=="November"  & enyr==2021) ~ 239,
  (reference_month=="December"  & enyr==2021) ~ 240)
if (GDPnotAvailyet) GDP[indx] <- FlashEstimate
PCEI_df <- mutate(PCEI_df,GDP=GDP)
# Now benchmark the results to monthly GDP by calculating an additive
# adjustment and adding it to all provincial values
a <- PCEI_df$GDP # StatCan's GDP estimate
b <- PCEI_df$CA # implicit CA estimate by aggregating provincial PCEIs
f1 <- a-b # the additive adjustment
# Benchmark the PCEIs:
PCEI_df1 <- mutate(PCEI_df,across(2:11,function(x) {y <- x+f1}))
PCEI_df1 <- mutate(PCEI_df1,
  AT = shares_AT["NL"]*NL + shares_AT["PE"]*PE + 
       shares_AT["NS"]*NS + shares_AT["NB"]*NB)
PCEI_df1 <- mutate(PCEI_df1,
  CE = shares_CE["QC"]*QC + shares_CE["ON"]*ON)
PCEI_df1 <- mutate(PCEI_df1,
  WE = shares_WE["MB"]*MB + shares_WE["SK"]*SK + 
       shares_WE["AB"]*AB + shares_WE["BC"]*BC)
PCEI_df1 <-  mutate(PCEI_df1,
  CA = shares_CA["NL"]*NL + shares_CA["PE"]*PE + 
       shares_CA["NS"]*NS + shares_CA["NB"]*NB + 
       shares_CA["QC"]*QC + shares_CA["ON"]*ON + 
       shares_CA["MB"]*MB + shares_CA["SK"]*SK + 
       shares_CA["AB"]*AB + shares_CA["BC"]*BC)
# Calculate and save the PCEI results for transmission to the dashboard
PCEI_dashboard <- select(PCEI_df1,-GDP)
rnd <- function(x) {y <- round(x,1)}
PCEI_dashboard <- mutate(PCEI_dashboard,across(1:ncol(PCEI_dashboard),rnd))
savespot <- paste0("/Users/philipsmith/Documents/R/",
  "Provincial_summary_indicators/Multi_prov/AA/PCEI_",last_date,".rds")
saveRDS(PCEI_dashboard,savespot)
test <- readRDS(savespot)
 
#===============================================================================
# Make summary MoM AT table
#===============================================================================
ProvA <- "AT"
prv_nms <- c("Atlantic region",
             "Newfoundland and Labrador",
             "Prince Edward Island",
             "Nova Scotia",
             "New Brunswick")
tbl_df <- data.frame(round(PCEI_df1$AT,1),round(PCEI_df1$NL,1),
                     round(PCEI_df1$PE,1),round(PCEI_df1$NS,1),
                     round(PCEI_df1$NB,1))
tbl_df <- tbl_df[219:nrow(tbl_df),] # 219 is March 2020
tbl_df1 <- as.data.frame(t(tbl_df))
tbl_df1 <- mutate(tbl_df1,prv_nms=prv_nms)
tbl_df1 <- select(tbl_df1,prv_nms,everything())
colnames(tbl_df1) <- c("Region or province","Mar","Apr","May",
  "Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb")

gt_tbl1 <- gt(data=tbl_df1)
gt_tbl1 <- gt_tbl1 %>% 
  tab_options(table.font.size=10) %>%
  tab_header(
    title=md(html("**Atlantic Canada composite economic indicator<br>February 2021**")),
    subtitle=md(html("1-month percentage change<br><br>"))
  ) %>% 
  tab_source_note(
    source_note=md(html(paste0("<br>Based on about 50 seasonally-adjusted and ",
       "price-deflated monthly economic time series.<br><br>",
       "@PhilSmith26")))
  ) %>% 
  cols_align(
    align=c("left"),
    columns=vars(`Region or province`)
  ) %>%
  fmt_number(
    columns=vars(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb),
    decimals=1,
    use_seps=TRUE
  ) %>%
  cols_label(
    `Region or province`="",
    `Mar`=md("**Mar**"),
    `Apr`=md("**Apr**"),
    `May`=md("**May**"),
    `Jun`=md("**Jun**"),
    `Jul`=md("**Jul**"),
    `Aug`=md("**Aug**"),
    `Sep`=md("**Sep**"),
    `Oct`=md("**Oct**"),
    `Nov`=md("**Nov**"),
    `Dec`=md("**Dec**"),
    `Jan`=md("**Jan**"),
    `Feb`=md("**Feb**")
      ) %>%
  data_color(
    columns=vars(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb),
    colors=scales::col_numeric(
      palette=c(
        "lightcyan"),
      domain=c(-100.0,100.0),
      )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(`Region or province`)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=vars(`Region or province`,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb)
    )
  )
gt_tbl1
setwd(paste0("/Users/philipsmith/Documents/R/Provincial_summary_indicators/",
             "Multi_prov/",ProvA,"/Output"))
gtsave(gt_tbl1,"ATCEI_Feb_2021.html")
gtsave(gt_tbl1,"ATCEI_Feb_2021.png")

#===============================================================================
# Make summary MoM CE table
#===============================================================================
ProvA <- "CE"
prv_nms <- c("Central region",
             "Quebec",
             "Ontario")
tbl_df <- data.frame(round(PCEI_df1$CE,1),round(PCEI_df1$QC,1),
                     round(PCEI_df1$ON,1))
tbl_df <- tbl_df[219:nrow(tbl_df),] # 219 is March 2020
tbl_df1 <- as.data.frame(t(tbl_df))
tbl_df1 <- mutate(tbl_df1,prv_nms=prv_nms)
tbl_df1 <- select(tbl_df1,prv_nms,everything())
colnames(tbl_df1) <- c("Region or province","Mar","Apr","May",
  "Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb")

gt_tbl1 <- gt(data=tbl_df1)
gt_tbl1 <- gt_tbl1 %>% 
  tab_options(table.font.size=10) %>%
  tab_header(
    title=md(html("**Central Canada composite economic indicator<br>February 2021**")),
    subtitle=md(html("1-month percentage change<br><br>"))
  ) %>% 
  tab_source_note(
    source_note=md(html(paste0("<br>Based on about 60 seasonally-adjusted and ",
                               "price-deflated monthly economic time series.<br><br>",
                               "@PhilSmith26")))
  ) %>% 
  cols_align(
    align=c("left"),
    columns=vars(`Region or province`)
  ) %>%
  fmt_number(
    columns=vars(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb),
    decimals=1,
    use_seps=TRUE
  ) %>%
  cols_label(
    `Region or province`="",
    `Mar`=md("**Mar**"),
    `Apr`=md("**Apr**"),
    `May`=md("**May**"),
    `Jun`=md("**Jun**"),
    `Jul`=md("**Jul**"),
    `Aug`=md("**Aug**"),
    `Sep`=md("**Sep**"),
    `Oct`=md("**Oct**"),
    `Nov`=md("**Nov**"),
    `Dec`=md("**Dec**"),
    `Jan`=md("**Jan**"),
    `Feb`=md("**Feb**")
  ) %>%
  data_color(
    columns=vars(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb),
    colors=scales::col_numeric(
      palette=c(
        "lightcyan"),
      domain=c(-100.0,100.0),
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(`Region or province`)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=vars(`Region or province`,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb)
    )
  )
gt_tbl1
setwd(paste0("/Users/philipsmith/Documents/R/Provincial_summary_indicators/",
             "Multi_prov/",ProvA,"/Output"))
gtsave(gt_tbl1,"CECEI_Feb_2021.html")
gtsave(gt_tbl1,"CECEI_Feb_2021.png")

#===============================================================================
# Make summary MoM WE table
#===============================================================================
ProvA <- "WE"
prv_nms <- c("Western region",
             "Manitoba",
             "Saskatchewan",
             "Alberta",
             "British Columbia")
tbl_df <- data.frame(round(PCEI_df1$WE,1),round(PCEI_df1$MB,1),
                     round(PCEI_df1$SK,1),round(PCEI_df1$AB,1),
                     round(PCEI_df1$BC,1))
tbl_df <- tbl_df[219:nrow(tbl_df),] # 219 is March 2020
tbl_df1 <- as.data.frame(t(tbl_df))
tbl_df1 <- mutate(tbl_df1,prv_nms=prv_nms)
tbl_df1 <- select(tbl_df1,prv_nms,everything())
colnames(tbl_df1) <- c("Region or province","Mar","Apr","May",
  "Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb")

gt_tbl1 <- gt(data=tbl_df1)
gt_tbl1 <- gt_tbl1 %>% 
  tab_options(table.font.size=10) %>%
  tab_header(
    title=md(html("**Western Canada composite economic indicator<br>February 2021**")),
    subtitle=md(html("1-month percentage change<br><br>"))
  ) %>% 
  tab_source_note(
    source_note=md(html(paste0("<br>Based on about 60 seasonally-adjusted and ",
                               "price-deflated monthly economic time series.<br><br>",
                               "@PhilSmith26")))
  ) %>% 
  cols_align(
    align=c("left"),
    columns=vars(`Region or province`)
  ) %>%
  fmt_number(
    columns=vars(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb),
    decimals=1,
    use_seps=TRUE
  ) %>%
  cols_label(
    `Region or province`="",
    `Mar`=md("**Mar**"),
    `Apr`=md("**Apr**"),
    `May`=md("**May**"),
    `Jun`=md("**Jun**"),
    `Jul`=md("**Jul**"),
    `Aug`=md("**Aug**"),
    `Sep`=md("**Sep**"),
    `Oct`=md("**Oct**"),
    `Nov`=md("**Nov**"),
    `Dec`=md("**Dec**"),
    `Jan`=md("**Jan**"),
    `Feb`=md("**Feb**")
      ) %>%
  data_color(
    columns=vars(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb),
    colors=scales::col_numeric(
      palette=c(
        "lightcyan"),
      domain=c(-100.0,100.0),
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(`Region or province`)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=vars(`Region or province`,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb)
    )
  )
gt_tbl1
setwd(paste0("/Users/philipsmith/Documents/R/Provincial_summary_indicators/",
             "Multi_prov/",ProvA,"/Output"))
gtsave(gt_tbl1,"WECEI_Feb_2021.html")
gtsave(gt_tbl1,"WECEI_Feb_2021.png")

#===============================================================================
# Make summary MoM CA table
#===============================================================================
ProvA <- "CA"
prv_nms <- c("Canada",
             "Newfoundland and Labrador",
             "Prince Edward Island",
             "Nova Scotia",
             "New Brunswick",
             "Quebec",
             "Ontario",
             "Manitoba",
             "Saskatchewan",
             "Alberta",
             "British Columbia")
tbl_df <- data.frame(round(PCEI_df1$CA,1),round(PCEI_df1$NL,1),
                     round(PCEI_df1$PE,1),round(PCEI_df1$NS,1),
                     round(PCEI_df1$NB,1),round(PCEI_df1$QC,1),
                     round(PCEI_df1$ON,1),round(PCEI_df1$MB,1),
                     round(PCEI_df1$SK,1),round(PCEI_df1$AB,1),
                     round(PCEI_df1$BC,1))
tbl_df <- tbl_df[219:nrow(tbl_df),] # 219 is March 2020
tbl_df1 <- as.data.frame(t(tbl_df))
tbl_df1 <- mutate(tbl_df1,prv_nms=prv_nms)
tbl_df1 <- select(tbl_df1,prv_nms,everything())
tbl_df1 <- mutate(tbl_df1,Cumul=100*((1+`219`/100)*(1+`220`/100)*
    (1+`221`/100)*(1+`222`/100)*(1+`223`/100)*(1+`224`/100)*
    (1+`225`/100)*(1+`226`/100)*(1+`227`/100)*(1+`228`/100)*(1+`229`/100)*(1+`230`/100)-1))
colnames(tbl_df1) <- c("Region or province","Mar","Apr","May",
  "Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Cumul")

gt_tbl1 <- gt(data=tbl_df1)
gt_tbl1 <- gt_tbl1 %>% 
  tab_options(table.font.size=10) %>%
  tab_header(
    title=md(html("**Canada composite economic indicator<br>February 2021**")),
    subtitle=md(html("1-month percentage change<br><br>"))
  ) %>% 
  tab_source_note(
    source_note=md(html(paste0("<br>Based on about 60 seasonally-adjusted and ",
                               "price-deflated monthly economic time series.<br><br>",
                               "@PhilSmith26")))
  ) %>% 
  cols_align(
    align=c("left"),
    columns=vars(`Region or province`)
  ) %>%
  fmt_number(
    columns=vars(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb,Cumul),
    decimals=1,
    use_seps=TRUE
  ) %>%
  cols_label(
    `Region or province`="",
    `Mar`=md("**Mar**"),
    `Apr`=md("**Apr**"),
    `May`=md("**May**"),
    `Jun`=md("**Jun**"),
    `Jul`=md("**Jul**"),
    `Aug`=md("**Aug**"),
    `Sep`=md("**Sep**"),
    `Oct`=md("**Oct**"),
    `Nov`=md("**Nov**"),
    `Dec`=md("**Dec**"),
    `Jan`=md("**Jan**"),
    `Feb`=md("**Feb**"),
    `Cumul`=md("**Cumul**")
  ) %>%
  data_color(
    columns=vars(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb,Cumul),
    colors=scales::col_numeric(
      palette=c(
        "lightcyan"),
      domain=c(-100.0,100.0),
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(`Region or province`)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=vars(`Region or province`,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb,Cumul)
    )
  )
gt_tbl1
setwd(paste0("/Users/philipsmith/Documents/R/Provincial_summary_indicators/",
             "Multi_prov/",ProvA,"/Output"))
gtsave(gt_tbl1,"CACEI_BM_Feb_2021.html")
gtsave(gt_tbl1,"CACEI_BM_Feb_2021.png")

#===============================================================================
# Make summary MoM AA table
#===============================================================================
ProvA <- "AA"
prv_nms <- c("Canada",
             "Atlantic Canada",
             "Newfoundland and Labrador",
             "Prince Edward Island",
             "Nova Scotia",
             "New Brunswick",
             "Central Canada",
             "Quebec",
             "Ontario",
             "Western Canada",
             "Manitoba",
             "Saskatchewan",
             "Alberta",
             "British Columbia")
tbl_df <- data.frame(round(PCEI_df1$CA,1),round(PCEI_df1$AT,1),
                     round(PCEI_df1$NL,1),round(PCEI_df1$PE,1),
                     round(PCEI_df1$NS,1),round(PCEI_df1$NB,1),
                     round(PCEI_df1$CE,1),round(PCEI_df1$QC,1),
                     round(PCEI_df1$ON,1),round(PCEI_df1$WE,1),
                     round(PCEI_df1$MB,1),round(PCEI_df1$SK,1),
                     round(PCEI_df1$AB,1),round(PCEI_df1$BC,1))
tbl_df <- tbl_df[219:nrow(tbl_df),] # 219 is March 2020
tbl_df1 <- as.data.frame(t(tbl_df))
tbl_df1 <- mutate(tbl_df1,prv_nms=prv_nms)
tbl_df1 <- select(tbl_df1,prv_nms,everything())
tbl_df1 <- mutate(tbl_df1,Cumul=100*((1+`219`/100)*(1+`220`/100)*
    (1+`221`/100)*(1+`222`/100)*(1+`223`/100)*(1+`224`/100)*
    (1+`225`/100)*(1+`226`/100)*(1+`227`/100)*(1+`228`/100)*(1+`229`/100)*(1+`230`/100)-1))
colnames(tbl_df1) <- c("Region or province","Mar","Apr","May",
  "Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Cumul")

gt_tbl1 <- gt(data=tbl_df1)
gt_tbl1 <- gt_tbl1 %>% 
  tab_options(table.font.size=10,
              container.width = 550) %>%
  tab_header(
    title=md(html("**Canadian composite economic indicator<br>February 2021**")),
    subtitle=md(html("1-month percentage change<br><br>"))
  ) %>% 
  tab_source_note(
    source_note=md(html("@PhilSmith26"))
  ) %>% 
  cols_align(
    align=c("left"),
    columns=vars(`Region or province`)
  ) %>%
  fmt_number(
    columns=vars(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb,Cumul),
    decimals=1,
    use_seps=TRUE
  ) %>%
  cols_label(
    `Region or province`="",
    `Mar`=md("**Mar**"),
    `Apr`=md("**Apr**"),
    `May`=md("**May**"),
    `Jun`=md("**Jun**"),
    `Jul`=md("**Jul**"),
    `Aug`=md("**Aug**"),
    `Sep`=md("**Sep**"),
    `Oct`=md("**Oct**"),
    `Nov`=md("**Nov**"),
    `Dec`=md("**Dec**"),
    `Jan`=md("**Jan**"),
    `Feb`=md("**Feb**"),
    `Cumul`=md("**Cumul**")
  ) %>%
  data_color(
    columns=vars(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb,Cumul),
    #rows=c(3,4,5,6,8,9,11,12,13,14),
    colors=scales::col_numeric(
      palette=c(
        "lightcyan"),
      domain=c(-100.0,100.0),
    )
  ) %>%
  tab_style( # column label style
    style = list(
      cell_fill(color = "bisque3"), # "gray66" or "darkslategray2"
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=vars(`Region or province`,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb,Cumul))
  ) %>%
  tab_style( # Canada line style
    style = list(
      cell_fill(color = "bisque2"), # "gray76"
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = 1)
  ) %>%
  tab_style( # Region line style
    style = list(
      cell_fill(color = "bisque2"), # "gray86"
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = c(2,7,10))
  ) %>%
  tab_style( # province line style
    style = list(
      cell_fill(color = "bisque"), # "gray96"
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = c(3,4,5,6,8,9,11,12,13,14) #columns = vars(`Region or province`)
    )
  ) %>%
  tab_footnote(
    footnote = paste0("Based on about 60 seasonally-adjusted and price-deflated ",
       "monthly economic time series such as LFS unemployment and participation ",
       "rates; LFS hours worked by industry; SEPH employment by industry; retail, ",
       "wholesale, manufacturing and restaurant sales; international trade; ",
       "housing starts; building permits; electric power generation; average ",
       "weekly earnings; and CPI relative prices for eight product groups. The ",
       "monthly percentage changes of these series are weighted by coefficients ",
       "associated with the first principal component. The resulting composite ",
       "index accounts for between 15% (PE) and 31% (ON) of the variance ",
       "in each province. Some provinces have much lower signal-to-noise ",
       "ratios, but the method still seems to identify the overall economic ",
       "trend quite well. The provincial results are constrained so their ",
       "weighted average growth is that of Canada's overall monthly GDP growth. ",
       "Further explanation and methodology discussion at ",
       "www.philipsmith.ca/PCEI"),
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = paste0("Cumulative percentage change between ",
       "the latest month and February (the month ",
       "before the COVID-19 shutdowns began)."),
    locations = cells_column_labels(
      columns=vars(Cumul))
  )
gt_tbl1
setwd(paste0("/Users/philipsmith/Documents/R/Provincial_summary_indicators/",
             "Multi_prov/",ProvA,"/Output"))
gtsave(gt_tbl1,"CACEI_BM_Feb_2021.html")
gtsave(gt_tbl1,"CACEI_BM_Feb_2021.png")

#===============================================================================
# Make chart of the COVID paths
df <- tbl_df
colnames(df) <- c("CA","AT","NL","PE","NS","NB","CE","QC","ON","WE",
  "MB","SK","AB","BC")
cumul <- function(x){
  y <- numeric()
  y[1] <- 100*(1+x[1]/100) # March 2020
  for (i in 2:(nrow(df))) {
    y[i] <- y[i-1]*(1+x[i]/100)
  }
  return(y)
}
df1 <- mutate(df,across(CA:BC,cumul))
df2 <- select(df1,-AT,-CE,-WE)
febline <- rep(100,11)
df2 <- rbind(febline,df2)
df2 <- mutate(df2,mnth=c("Febr","Mar","Apr","May","Jun",
  "Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"))
df2$mnth <- as.factor(df2$mnth)
df2$mnth <- factor(df2$mnth,levels=c("Febr","Mar","Apr","May","Jun",
  "Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"))
df3 <- pivot_longer(df2,cols=CA:BC,names_to="GEO",values_to="VALUE")
df3$lab <- as.character(NA)
df3[133,4] <- "CA"
df3[134,4] <- "NL"
df3[135,4] <- "PE"
df3[136,4] <- "NS"
df3[137,4] <- "NB"
df3[138,4] <- "QC"
df3[139,4] <- "ON"
df3[140,4] <- "MB"
df3[141,4] <- "SK"
df3[142,4] <- "AB"
df3[143,4] <- "BC"
df3 <- rename(df3,"Geography"="GEO")
df3$Geography <- as.factor(df3$Geography)
df3$Geography <- factor(df3$Geography,levels=c("PE","NS","NL","NB","QC",
  "BC","MB","SK","CA","AB","ON"))
mycols <- colors()[c(26,33,264,417,51,91,316,142,100,73,31,135)]
p_title <- "Economic paths during the pandemic"
p_subtitle <- paste0("The lines are provincial composite economic indicators ",
  "built from about 60 monthly economic time series in each province: ",
  "retail, wholesale,\nmanufacturing and restaurant sales, LFS hours worked ",
  "by industry, SEPH employment by industry, exports and imports and some others.\n",
  "The CA line is monthly GDP for Canada. The ten provincial growth paths are ",
  "benchmarked to be consistent with Canada-level monthly GDP.")
p_caption <- "Methodology at www.philipsmith.ca/PCEI. @PhilSmith26"
p_ytitle <- "Percentage change relative to February 2020"
p_savetitle <- "Economic_pandemic_paths_BM.png"
p1 <- ggplot(df3,aes(x=mnth,y=100*(VALUE/100-1),colour=Geography,group=Geography))+geom_line(size=1.2)+
  scale_y_continuous(breaks=c(-20,-15,-10,-5,0,5,10),labels=c("-20%","-15%",
    "-10%","-5%","0","5%","10%"),limits = c(-20,10))+
  geom_hline(yintercept=0,size=0.5,linetype="dashed")+
  scale_colour_manual(values=mycols) +
  theme(legend.position="none")+
  theme(legend.background=element_rect(fill="lightcyan1"))+
  theme(axis.text.x = element_text(size=11)) +
  theme(axis.text.y = element_text(size=11)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(plot.title = element_text(size=20,face="bold")) +
  theme(plot.subtitle = element_text(size=10)) +
  theme(plot.background=element_rect(fill="lightcyan1"))+
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  labs(title = p_title,
       subtitle = p_subtitle,
       caption = p_caption,
       x = NULL, y = p_ytitle) +
  theme(legend.position="right")
p1
ggsave(p_savetitle,p1,height=8,width=10,dpi=300)
                  
