# PCEI annual calculations
# February 23, 2021; updated and much improved April 4, 2021

# This program retrieves the PEA data and builds a by-province df
# of real growth rates, 2013-2019. It also takes the PCEIs, creates
# indexes by multiplying the growth rates together, calculates annual values
# from the indexes, calculates annual growth rates, and compares to the
# PEA growth rates in a chart. 

pkgs <- c("tidyverse","cansim","lubridate","gdata","dataframes2xls",
          "readxl", "writexl","forecast","seasonal","stringr", "ggtext",
          "corrplot","gt","ggpubr")
inst <- lapply(pkgs,library,character.only=TRUE)

last_year <- 2019
last_date <- "2020-12-01"

setwd(paste0("/Users/philipsmith/Documents/R/Provincial_summary_indicators/",
  "Multi_prov/AA/Output"))

# Get PEA K$ data
table01_id <- "36-10-0222-01" 
file_refresh <- TRUE
save_file <- paste0(table01_id,".rds")
if(!file.exists(save_file)|file_refresh){
  table01 <- get_cansim(table01_id,refresh=file_refresh)
  saveRDS(table01,file=save_file)
} else {
  table01 <- readRDS(save_file)
}
(a0 <- unique(table01$REF_DATE))
(a1 <- unique(table01$GEO))
(a2 <- unique(table01$Prices))
(a3 <- unique(table01$Estimates))

v0 <- filter(table01,Prices=="2012 constant prices",
             Estimates=="Gross domestic product at market prices")
v0 <- select(v0,REF_DATE,GEO,VALUE)
v0$REF_DATE <- as.numeric(v0$REF_DATE)
v0 <- filter(v0,REF_DATE>2001)
v1 <- pivot_wider(v0,names_from=GEO,values_from=VALUE)
# Calculate provincial shares of GDP
v1a <- select(v1,REF_DATE,
                 "NL"="Newfoundland and Labrador",
                 "PE"="Prince Edward Island",
                 "NS"="Nova Scotia",
                 "NB"="New Brunswick",
                 "QC"="Quebec",
                 "ON"="Ontario",
                 "MB"="Manitoba",
                 "SK"="Saskatchewan",
                 "AB"="Alberta",
                 "BC"="British Columbia",
                 "CA"="Canada",
                 "YT"="Yukon",
                 "NT"="Northwest Territories",
                 "NU"="Nunavut",
                 "OC"="Outside Canada")
# The prov GDP total (excludes NU, YK, NT and OC)
v1a <- mutate(v1a,CA1=NL+PE+NS+NB+QC+ON+MB+SK+AB+BC+YT+NT+NU+OC,DIF=CA-CA1)
# Result: Provs and terrs add to Canada from 2012 forward (only)
v1a <- filter(v1a,REF_DATE>=2012)
v1a <- select(v1a,-YT,-NT,-NU,-OC,-CA1,-DIF)
v1a <- mutate(v1a,AT=NL+PE+NS+NB)
v1a <- mutate(v1a,CE=QC+ON)
v1a <- mutate(v1a,WE=MB+SK+AB+BC)
# The prov GDP shares
v1b <- mutate(v1a,across(NL:WE,function(x) {y <- 100*x/v1a$CA}))
# The prov GDP average shares over 2012-2019
v1c <- summarise(v1b,across(NL:WE,mean))
# The prov GDP indexes, 2012=100
v2a <- mutate(v1a,across(NL:WE,function(x) {y <- 100*x/x[1]}))
# The prov GDP annual growth rates
v3 <- mutate(v2a,across(NL:WE,function(x) {y <- round(100*(x/lag(x,1)-1),2)}))
# The prov GDP average growth rates for 2013-2019
v4 <- summarise(filter(v3,REF_DATE>2012),across(NL:WE,function(x) {y <- round(mean(x),2)}))

#===============================================================================
# Look at the indexes calculated by multiplying the PCEI growth rates together
# First obtain a data frame with the provincial PCEI estimates
savespot <- paste0("/Users/philipsmith/Documents/R/",
  "Provincial_summary_indicators/Multi_prov/AA/PCEI_",last_date,".rds")
PCEI_df <- readRDS(savespot)
# Now create two-year index series from the PCEI growth rates
pidx <- function(x) { # create index series from growth rate series
  n <- length(x)
  y <- c(100,rep(NA,(n)))
  for (i in 2:n) {
    y[i] <- y[i-1]*(x[i]/100+1)
  }
  y <- y[1:n]
  return(y)
}
# Select a two-year (24-month) period
#Y2012to2013 <- filter(PCEI_df,REF_DATE>as.Date("2011-12-01") & 
#    REF_DATE<as.Date("2014-01-01"))
# Cumulate the growth rates to create indexes
#S2012to2013 <- mutate(Y2012to2013,across(NL:CA,pidx))
# Create annual indexes by averaging the months
#S2012to2013 <- mutate(S2012to2013,Year=year(REF_DATE),Quarter=quarter(REF_DATE))
#A2012to2013 <- S2012to2013 %>% group_by(Year) %>% summarise(across(NL:CA,mean))

# Cumulate the PCEIs from 2012 to 2019
PCEI_cumul_df <- mutate(PCEI_df,across(NL:CA,pidx))
t1 <- mutate(PCEI_cumul_df,Year=year(REF_DATE),Quarter=quarter(REF_DATE))
PCEI_ann_df <- t1 %>% group_by(Year) %>% summarise(across(2:(ncol(t1)-1),mean))
PCEI_ann_gr_df <- mutate(PCEI_ann_df,across(2:(ncol(t1)-1),function(x) {y <- round(100*(x/lag(x)-1),1)}))
PEA <- filter(v3,REF_DATE>2012)
PEA <- rename(PEA,"Year"="REF_DATE")
PEA$Year <- c(2013:2019)
PEA <- select(PEA,Year,NL,PE,NS,NB,QC,ON,MB,SK,AB,BC,AT,CE,WE,CA)
PEAlong <- pivot_longer(PEA,cols=2:15,names_to="prov",values_to="valPEA")
PCEI <- filter(PCEI_ann_gr_df,Year>2012)
PCEIlong <- pivot_longer(PCEI,cols=2:15,names_to="prov",values_to="valPCEI")
PCEIlong <- select(PCEIlong,-Quarter)
PCEIlong <- filter(PCEIlong,Year!=2020)
PCEIlong$valPEA <- PEAlong$valPEA
PCEIlong$prov <- as.factor(PCEIlong$prov)
PCEIlong$prov <- factor(PCEIlong$prov,levels=c(
  "CA","WE","BC","AB","SK","MB","CE","ON","QC","AT","NB","NS","PE","NL")
)

c1 <- ggplot(PCEIlong)+
  geom_col(aes(x=Year,y=valPEA),fill="red",alpha=0.5)+
  geom_col(aes(x=Year,y=valPCEI),fill="blue",alpha=0.5)+
  facet_wrap(~prov)+
  labs(title=paste0("Comparison of the provincial composite economic ",
    "indicators\nwith the provincial economic accounts"),
    subtitle=paste0("Red = provincial economic accounts (36-10-0222-01)\nBlue = ",
    "provincial composite economic indicators\nPurple = overlap"),
    caption=paste0("The red bars represent the provincial economic accounts, ",
    "the blue bars the provincial composite\neconomic indicators and the ",
    "purple the overlap. Source: Statistics Canada table 36-10-0222-01 and\n",
    "calculations by the author. @PhilSmith26"),
    x="",y="Annual percentage change") +
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019))+
  #scale_y_continuous(breaks=),
  #  labels=scales::dollar_format())+
  theme(plot.title = element_text(size=16,face="bold")) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  theme(panel.border = element_rect(fill=NA,colour="black"))+
  theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
  theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
  theme(axis.text.x = element_text(angle=0,hjust=1,size=6)) +
  theme(axis.text.y = element_text(size=6))
c1
ggsave("PEAvsPCEI.png",c1,height=8,width=10,dpi=300)





#===============================================================================
# Other related code, not currently used

g1 <- mutate_if(g0,is.numeric,pidx) # g1 contains the PCEI indexes
g2 <- mutate_if(g1,is.numeric,SEASADJ) #g2 contains the seas adj PCEI indexes
GDPm <- get_cansim_vector("v65201210","2002-01-01")
GDP <- GDPm$VALUE
GDP <- 100*GDP/GDP[1] # monthly GDP vector converted to Jan 2002 = 100
GDPdf <- data.frame(REF_DATE=g2$REF_DATE,GDP=GDP)
ggplot(g1) + # check CA tracking vs GDP[]
  geom_line(aes(x=REF_DATE,y=g1[,12]),colour="black") +
  annotate(geom="text",x=as.Date("2005-01-01"),y=140,label=paste0("Black = ",
    "indicator\nRed = seasonally adjusted indicator\nGreen = monthly GDP")) +
  geom_line(data=g2,aes(x=REF_DATE,y=g2[,12]),colour="red") +
  geom_line(data=GDPdf,aes(x=REF_DATE,y=GDP),colour="forestgreen")
# Now aggregate the monthly to annual to compare to PEA
g3 <- mutate(g2,Yr=year(REF_DATE),Mth=month(REF_DATE))
g4 <- g3 %>% 
  group_by(Yr) %>%
  summarise_if(is.numeric,mean) %>%
  ungroup()
g5 <- filter(g4,Yr<2019) # g5 contains the annual PCEIs based on Jan 2002=100
g5 <- mutate(g5,across(NLp:CAp,IDX))
g5 <- select(g5,-Mth)
g6 <- select(g5,Yr,CAp,NLp,PEp,NSp,NBp,QCp,ONp,MBp,SKp,ABp,BCp)
colnames(g6) <- nms

ggplot() + # This chart compares annual GDP to the PCEIs, 2002=100
  geom_line(data=v3,aes(x=Yr,y=BC),colour="black") +
  geom_line(data=g6,aes(x=Yr,y=BC),colour="red")

# fitObj: object from lm() or glm() or glm.nb();
# adj: whether to calculate the adjusted R-squared.
# type: specifying the extension of R-squared to generalized linear models,
#   v -- variance-function-based (Zhang, 2017),
#   kl -- Kullback-Leibler-divergence-based (Cameron and Windmeijer, 1997),
#   sse -- sum-of-squared-errors-based (Efron, 1978),
#   lr -- likelihood-ratio-statistic-based (Maddala, 1983; Cox & Snell, 1989; Magee, 1990),
#   n -- corrected generalization of 'lr' (Nagelkerke, 1991)
#data: data used to obtain fitObj.
rsq<-function(fitObj,adj=FALSE,type=c('v','kl','sse','lr','n'))
{
  if( is(fitObj,"glm")|is(fitObj,"glmerMod") ) # glm in stats, glmer & glmer.nb in lme4
  {
    type <- type[[1]]
    rsq <- switch(type,
                  v = rsq.v(fitObj,adj=adj), 
                  kl = rsq.kl(fitObj,adj=adj),
                  sse = rsq.sse(fitObj,adj=adj),
                  lr = rsq.lr(fitObj,adj=adj),
                  n = rsq.n(fitObj,adj=adj))
  }
  else if( is(fitObj,"glmmPQL") ) # glmmPQL in MASS, which is also "belongs to "lme" 
    warning("Unsupported object!")
  else if( is(fitObj,"lmerMod")|is(fitObj,"lme") )  # lmer in lme4, lme in nlme
    rsq <- rsq.lmm(fitObj,adj=adj)
  else if( is(fitObj,"lm") )
    rsq <- ifelse(adj,summary(fitObj)$adj.r.squared,summary(fitObj)$r.squared)
  else
    warning("Unsupported object!")
  
  rsq  
}

# Regressions of the GDPs on the PCEIs
fit1 <- lm(v3$ON~g6$ON)
(rsq1 <- rsq(fit1))
fit2 <- lm(v3$ON~lag(g6$ON,2))
(rsq2 <- rsq(fit2))

# COVID-19 path analysis
g1a <- filter(g1,REF_DATE>as.Date("2020-01-01")) # PCEIs NSA
g1a <- mutate(g1a,across(NLp:WEp,IDX))
g2a <- filter(g2,REF_DATE>as.Date("2020-01-01")) # PCEIs SA
g2a <- mutate(g2a,across(NLp:WEp,IDX))

# COVID chart for Atlantic
ProvA <- "AT"
setwd(paste0("/Users/philipsmith/Documents/R/Provincial_summary_indicators/",
             "Multi_prov/",ProvA,"/"))
p_title <- paste0(prov_names[ProvA],":  Composite economic indicators ",
                  "during the COVID-19 period")
p_savetitle <- paste0("Output/PCEI_COVID_path",ProvA,"_MoM.jpg")
p_xtitle <- ""
p_ytitle <- "February 2020 = 100"
p_caption <- "@PhilSmith26"
pp1 <- ggplot(data=g1a) +
  geom_line(aes(x=REF_DATE,y=NLp),size=1.1,colour="red") + 
  geom_line(aes(x=REF_DATE,y=PEp),size=1.1,colour="blue") + 
  geom_line(aes(x=REF_DATE,y=NSp),size=1.1,colour="forestgreen") + 
  geom_line(aes(x=REF_DATE,y=NBp),size=1.1,colour="purple") + 
  geom_line(aes(x=REF_DATE,y=ATp),size=1.1,colour="chocolate1") + 
  geom_line(aes(x=REF_DATE,y=CAp),size=1.1,colour="black") + 
  geom_hline(yintercept=100,linetype="dashed",colour="black") +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=NLp,
            label=as.character("NL")),hjust=-0.3,vjust=0.5,colour="red",size=5,
            position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=PEp,
            label=as.character("PE")),hjust=-0.3,vjust=0,colour="blue",size=5,
            position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=NSp,
            label=as.character("NS")),hjust=-0.3,vjust=0.7,colour="forestgreen",size=5,
            position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=NBp,
            label=as.character("NB")),hjust=-0.3,vjust=-0.7,colour="purple",size=5,
            position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=ATp,
            label=as.character("AT")),hjust=-0.3,vjust=-0.3,colour="chocolate1",size=5,
            position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=CAp,
            label=as.character("CA")),hjust=-0.3,vjust=0,colour="black",size=5,
            position=position_jitter(width=.1,height=.1)) +
  theme(axis.text.x = element_text(size=11)) +
  theme(axis.text.y = element_text(size=11)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(plot.title = element_text(size=20,face="bold")) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  labs(title = p_title,
       caption = p_caption,
       x = NULL, y = p_ytitle) +
  theme(legend.position="none") +
  theme(plot.background=element_rect(fill="lightblue")) +
  guides(fill=guide_legend(reverse=TRUE))
pp1
ggsave(p_savetitle,pp1,height=9,width=16,dpi=300)
saveRDS(pp1,paste0("rds/PCEI_COVID_path",ProvA,".rds"))

# COVID chart for Central Canada
ProvA <- "CE"
setwd(paste0("/Users/philipsmith/Documents/R/Provincial_summary_indicators/",
             "Multi_prov/",ProvA,"/"))
p_title <- paste0(prov_names[ProvA],":  Composite economic indicators ",
                  "during the COVID-19 period")
p_savetitle <- paste0("Output/PCEI_COVID_path",ProvA,"_MoM.jpg")
p_xtitle <- ""
p_ytitle <- "February 2020 = 100"
p_caption <- "@PhilSmith26"
pp2 <- ggplot(data=g1a) +
  geom_line(aes(x=REF_DATE,y=QCp),size=1.1,colour="red") + 
  geom_line(aes(x=REF_DATE,y=ONp),size=1.1,colour="blue") + 
  geom_line(aes(x=REF_DATE,y=CEp),size=1.1,colour="chocolate1") + 
  geom_line(aes(x=REF_DATE,y=CAp),size=1.1,colour="black") + 
  geom_hline(yintercept=100,linetype="dashed",colour="black") +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=QCp,
            label=as.character("QC")),hjust=-0.3,vjust=0,colour="red",size=5) +
            #position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=ONp,
            label=as.character("ON")),hjust=-0.3,vjust=0,colour="blue",size=5) +
            #position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=CEp,
            label=as.character("CE")),hjust=-0.3,vjust=0,colour="chocolate1",size=5) +
            #position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=CAp,
            label=as.character("CA")),hjust=-0.3,vjust=0,colour="black",size=5) +
            #position=position_jitter(width=.1,height=.1)) +
  theme(axis.text.x = element_text(size=11)) +
  theme(axis.text.y = element_text(size=11)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(plot.title = element_text(size=20,face="bold")) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  labs(title = p_title,
       caption = p_caption,
       x = NULL, y = p_ytitle) +
  theme(legend.position="none") +
  theme(plot.background=element_rect(fill="lightblue")) +
  guides(fill=guide_legend(reverse=TRUE))
pp2
ggsave(p_savetitle,pp2,height=9,width=16,dpi=300)
saveRDS(pp2,paste0("rds/PCEI_COVID_path",ProvA,".rds"))

# COVID chart for Western Canada
ProvA <- "WE"
setwd(paste0("/Users/philipsmith/Documents/R/Provincial_summary_indicators/",
             "Multi_prov/",ProvA,"/"))
p_title <- paste0(prov_names[ProvA],":  Composite economic indicators ",
                  "during the COVID-19 period")
p_savetitle <- paste0("Output/PCEI_COVID_path",ProvA,"_MoM.jpg")
p_xtitle <- ""
p_ytitle <- "February 2020 = 100"
p_caption <- "@PhilSmith26"
pp3 <- ggplot(data=g1a) +
  geom_line(aes(x=REF_DATE,y=MBp),size=1.1,colour="red") + 
  geom_line(aes(x=REF_DATE,y=SKp),size=1.1,colour="blue") + 
  geom_line(aes(x=REF_DATE,y=ABp),size=1.1,colour="forestgreen") + 
  geom_line(aes(x=REF_DATE,y=BCp),size=1.1,colour="purple") + 
  geom_line(aes(x=REF_DATE,y=WEp),size=1.1,colour="chocolate1") + 
  geom_line(aes(x=REF_DATE,y=CAp),size=1.1,colour="black") + 
  geom_hline(yintercept=100,linetype="dashed",colour="black") +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=MBp,
            label=as.character("MB")),hjust=-0.3,vjust=-1,colour="red",size=5) +
            #position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=SKp,
            label=as.character("SK")),hjust=-0.3,vjust=0,colour="blue",size=5) +
            #position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=ABp,
            label=as.character("AB")),hjust=-0.3,vjust=0,colour="forestgreen",size=5) +
            #position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=BCp,
            label=as.character("BC")),hjust=-0.3,vjust=0,colour="purple",size=5) +
            #position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=WEp,
            label=as.character("WE")),hjust=-0.3,vjust=-0.7,colour="chocolate1",size=5) +
            #position=position_jitter(width=.1,height=.1)) +
  geom_text(data=filter(g1a,REF_DATE>as.Date("2020-06-01")),aes(x=REF_DATE,y=CAp,
            label=as.character("CA")),hjust=-0.3,vjust=0.4,colour="black",size=5) +
            #position=position_jitter(width=.1,height=.1)) +
  theme(axis.text.x = element_text(size=11)) +
  theme(axis.text.y = element_text(size=11)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(plot.title = element_text(size=20,face="bold")) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  labs(title = p_title,
       caption = p_caption,
       x = NULL, y = p_ytitle) +
  theme(legend.position="none") +
  theme(plot.background=element_rect(fill="lightblue")) +
  guides(fill=guide_legend(reverse=TRUE))
pp3
ggsave(p_savetitle,pp3,height=9,width=16,dpi=300)
saveRDS(pp3,paste0("rds/PCEI_COVID_path",ProvA,".rds"))

# Put some basic information about each PCEI in a df called Bar
# The columns of Bar are: (1) a text identifier, (2) 1st PC variance share,
# (3) average annual GDP growth rate divided by 12, (4) the PCEI and
# (5) column 3 times column 4.
Bar <- data.frame(Prov=character(),Vshr=character(),Agr=character(),
                  PCEI0=character(),PCEIa=character())
Bar[1,] <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
                        "summary_indicators/Multi_prov/NL/rds/PCEI_NL_",
                        "MoM_text_results.rds"))
Bar[2,] <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
                        "summary_indicators/Multi_prov/PE/rds/PCEI_PE_",
                        "MoM_text_results.rds"))
Bar[3,] <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
                        "summary_indicators/Multi_prov/NS/rds/PCEI_NS_",
                        "MoM_text_results.rds"))
Bar[4,] <- readRDS(file=paste0("/Users/philipsmith/Documents/R/Provincial_",
                        "summary_indicators/Multi_prov/NB/rds/PCEI_NB_",
                        "MoM_text_results.rds"))
Bar[5,1] <- "AT_MoM"
Bar[5,2] <- as.character(round(
                        as.numeric(Bar[1,2])*shares_AT["NL"]+
                        as.numeric(Bar[2,2])*shares_AT["PE"]+
                        as.numeric(Bar[3,2])*shares_AT["NS"]+
                        as.numeric(Bar[4,2])*shares_AT["NB"],2))
Bar[5,3] <- round(avg_growth_rates["AT"]/12,2)
Bar[5,4] <- round(last(PC_sum_AT),2)
Bar[5,5] <- round(avg_growth_rates["AT"]/12+last(PC_sum_AT),2)
