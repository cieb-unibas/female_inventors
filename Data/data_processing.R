#######################################################################
# Description:    This script processes patent data from the USPTO    #
#                 and the EPO to investigate female inventor shares   #
#                 across countries and over time.                     #
# Authors:        Matthias Niggli/CIEB UniBasel                       #
#                 Christian Rutzer/CIEB UniBasel                      #
# Last Revised:   01.02.2021                                          #
#######################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("countrycode")
library("viridis")

# directories ------------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"

if(substr(getwd(), nchar(getwd())-15, nchar(getwd())) != "female_inventors"){
  print("make sure your working directory is the GitHub repository. Please specify with setwd().")}else{
    print("Directories are set and packages are loaded")}

##################################
############ Load data ###########
##################################

#### Load OECD data on gender shares of university graduates
grad_dat <- read.csv("Data/oecd_graduates.csv")
names(grad_dat)[1] <- "COUNTRY"
total_year <- grad_dat %>% filter(SEX == "T") %>% 
  select(COUNTRY, FIELD, Field, ISC11_LEVEL, YEAR, Value) %>%
  rename(total_graduates = Value)
grad_dat <- grad_dat %>% filter(SEX == "F") %>% 
  select(COUNTRY, SEX, FIELD, ISC11_LEVEL, YEAR, Value)
grad_dat <- merge(grad_dat, total_year, 
                  by = c("COUNTRY", "FIELD", "ISC11_LEVEL", "YEAR"),
                  all.x = TRUE)
total_year <- NULL
grad_dat <- mutate(grad_dat, female_share_graduates = Value / total_graduates)
grad_dat <- grad_dat[is.nan(grad_dat$female_share_graduates) == FALSE &
                       is.na(grad_dat$female_share_graduates) == FALSE, ]
grad_dat$COUNTRY <- countrycode(grad_dat$COUNTRY, "iso3c", "iso2c")
grad_dat <- rename(grad_dat, Ctry_code = COUNTRY, p_year = YEAR, 
                   N_female_graduates = Value)
grad_dat <- select(grad_dat, - SEX)
print("Data on university graduates ready for analysis")

#### Load patent inventor data with (predicted) gender infromation
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg_gender.rds")) 
print("Properties of patent inventors loaded")

# clean the data
inv_reg$gender <- as.numeric(inv_reg$gender)
inv_reg$p_year <- as.numeric(inv_reg$p_year)
dat <- inv_reg # to keep original inv_reg dataframe
dat <- dat[is.na(dat$gender) == FALSE, ] # drop NA's

print("All data ready for analysis")

##################################
######## DATA PROCESSING #########
##################################

## Overall female share in patent data:
table(dat$gender)/nrow(dat) # 16.05% for all patents

## non-predicted USPTO patent inventors only:
table(dat[is.na(dat$lat)==FALSE, "gender"]) / nrow(dat[is.na(dat$lat)==FALSE, ]) 
# => 15.23% i.e. slightly lower female share among USPTO inventors

## female inventors per country and p_year:
female_inv_shares <- dat %>% 
  group_by(Ctry_code, p_year) %>%
  summarise(total_inventors = n(),
            female_inventors = sum(gender == 0, na.rm = TRUE),
            female_share = female_inventors / total_inventors)%>%
  filter(total_inventors > 30)

## merge female graduate shares and female inventor shares together
female_inv_shares <- merge(female_inv_shares, grad_dat, 
                           by = c("Ctry_code", "p_year"), all = TRUE)

## get Country names
female_inv_shares$Ctry_code <- trimws(female_inv_shares$Ctry_code)
isco2obs <- rownames(female_inv_shares[nchar(female_inv_shares$Ctry_code) == 2, ])
isco2obs <- as.numeric(isco2obs)
female_inv_shares$Country <- NA
female_inv_shares$Country[isco2obs] <- countrycode(female_inv_shares$Ctry_code[isco2obs],
                                                   "iso2c", "country.name.en")
female_inv_shares$Country[-isco2obs] <- countrycode(female_inv_shares$Ctry_code[-isco2obs],
                                                    "iso3c", "country.name.en")

# calculate overall female shares for 2010-2017
tmp <- female_inv_shares %>%
  group_by(Ctry_code, FIELD, ISC11_LEVEL) %>%
  summarise(N_female_graduates_overall = sum(N_female_graduates, na.rm = TRUE),
            total_graduates_overall = sum(total_graduates, na.rm = TRUE),
            female_share_graduates_overall = N_female_graduates_overall / total_graduates_overall,
            female_inventor_share_overall = sum(female_inventors, na.rm = TRUE) /
              sum(total_inventors, na.rm = TRUE))
female_inv_shares <- merge(female_inv_shares, tmp, 
                           by = c("Ctry_code", "FIELD", "ISC11_LEVEL"),
                           all.x =TRUE)
tmp <- NULL
female_inv_shares <- mutate(female_inv_shares,
                            inventor_graduate_ratio = female_share / female_share_graduates_overall)
print("Data ready for plotting")

########################
######## PLOTS #########
########################

## FIGURE 1: FEMALE INVENTOR SHARES ACROSS COUNTRIES
selected_countries <- c("CH", "US", "UK", "DE", "AT", "DK", "FR")

plot_data <- female_inv_shares %>%
  filter(Ctry_code %in% selected_countries,
         p_year >= 1990, p_year < 2017,
         total_inventors > 30) %>% 
  select(p_year, Ctry_code, female_share) %>%
  distinct(p_year, Ctry_code, .keep_all = TRUE)

ggplot(plot_data, aes(x = p_year, y = female_share, color = Ctry_code))+
  geom_line() + geom_point(aes(shape = Ctry_code))+ ylim(c(0, 0.4))+
  labs(y = "Share of Females Inventors", x = "Year",
       shape = "", color = "")+
  scale_color_viridis(option = "inferno", end = 0.9, discrete = TRUE)+
  guides(color = guide_legend(nrow = 1))+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom", legend.direction = "horizontal",
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
# => convert this plot to an aimated graph in the sense of the specialization report


## FIGURE 2: FEMALE INVENTOR SHARES AND FEMALE UNIVERSITY GRADUATES IN NATURAL SCIENCES

# data:
selected_countries <- c("CH", "US", "UK", "DE", "AT", "DK", "FR", "IT", "ES", "PT", "CA",
                        "KR", "FI", "SE", "NO", "NL", "BE", "PL", "CZ", "HU", "SK", "IE",
                        "IL")
plot_data <- female_inv_shares %>%
  filter(Ctry_code %in% selected_countries,
         FIELD == "F05", ISC11_LEVEL == "L7",
         p_year >= 2010,
         total_graduates > 30,
         total_inventors > 30) %>%
  distinct(Ctry_code, .keep_all = TRUE)

# plot with 45degree line (all countries are bad)
ggplot(plot_data, aes(x = female_share_graduates_overall, 
                      y = female_inventor_share_overall))+
  geom_point(aes(size = total_graduates), alpha = 0.5, color = "steelblue")+
  geom_text(aes(label = Ctry_code), size = 3, nudge_y = 0.005, nudge_x = -0.003)+
  xlim(0, 0.75)+
  ylim(0, 0.6)+
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted")+
  labs(x = "Female Graduate Share in Natural Sciences", y = "Female Inventor Share")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))

## with comparison to the average (some countries are even worse)
ggplot(plot_data, aes(x = female_share_graduates_overall, 
                      y = female_inventor_share_overall))+
  geom_point(aes(size = total_graduates), alpha = 0.5, color = "steelblue")+
  geom_text(aes(label = Ctry_code), size = 3, nudge_y = 0.005, nudge_x = -0.003)+
  xlim(0.4, 0.75)+
  ylim(0, 0.4)+
  labs(x = "Female Graduate Share in Natural Sciences", y = "Female Inventor Share")+
  geom_vline(xintercept = mean(plot_data$female_share_graduates_overall), 
             linetype = "dotted")+
  geom_hline(yintercept = mean(plot_data$female_inventor_share_overall), 
             linetype = "dotted")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))





