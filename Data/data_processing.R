#######################################################################
# Description:    This script processes patent data from the USPTO    #
#                 and the EPO to investigate female inventor shares   #
#                 across countries and over time.                     #
# Authors:        Matthias Niggli/CIEB UniBasel                       #
#                 Christian Rutzer/CIEB UniBasel                      #
# Last Revised:   02.02.2021                                          #
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
                   female_graduates = Value)
grad_dat <- select(grad_dat, - SEX)
print("Data on university graduates ready for analysis")

#### Load patent inventor data with (predicted) gender information
pat_dat <- readRDS(paste0(mainDir1, "/created data/inv_reg.rds"))
gender <- readRDS(paste0(mainDir1, "/created data/inv_reg_gender.rds"))
gender <- gender %>% select(name, gender, gender_predicted)
pat_dat <- left_join(pat_dat, gender, by = "name")
print("Patent inventor data loaded loaded")
pat_dat$gender <- as.numeric(pat_dat$gender)
pat_dat$p_year <- as.numeric(pat_dat$p_year)
print("All data ready for analysis")

## Overall female inventor shares :------------------------------------

# (a) female share among unique inventors
plot_dat <- pat_dat %>% filter(gender_predicted == "yes") %>% distinct(name, .keep_all = TRUE)
N <- nrow(plot_dat)
share_female <- nrow(plot_dat[plot_dat$gender == "0", ]) / N
paste("For predicted inventors, female share is:", 
      round(share_female, 3) * 100,
      "% among unique inventors") # 21.8%
plot_dat <- pat_dat %>% filter(gender_predicted == "no") %>% distinct(name, .keep_all = TRUE)
N <- nrow(plot_dat)
share_female <- nrow(plot_dat[plot_dat$gender == "0", ]) / N
paste("For non-predicted inventors, female share is:", 
      round(share_female, 3) * 100, 
      "% among unique inventors") # 13.3%

# (b) female share on invented patents
plot_dat <- pat_dat %>% filter(gender_predicted == "yes") %>% distinct(name, p_key, .keep_all = TRUE)
N <- nrow(plot_dat)
share_female <- nrow(plot_dat[plot_dat$gender == "0", ]) / N
paste("For predicted inventors, female share is:", 
      round(share_female, 3) * 100,
      "% among unique patents") # 17.8%
plot_dat <- pat_dat %>% filter(gender_predicted == "no") %>% distinct(name, p_key, .keep_all = TRUE)
N <- nrow(plot_dat)
share_female <- nrow(plot_dat[plot_dat$gender == "0", ]) / N
paste("For non-predicted inventors, female share is:", 
      round(share_female, 3) * 100,
      "% among unique patents") # 9.4%

# Check the frequency of countries in the two samples to investigate:
plot_dat <- lapply(c("yes", "no"), function(x){
  
  plot_dat <- pat_dat %>% filter(gender_predicted == x) %>% distinct(name, .keep_all = TRUE)
  N <- nrow(plot_dat)
  plot_dat <- plot_dat %>% group_by(Ctry_code) %>% summarize(total_inv = n(),
                                                           Ctry_share = total_inv / N) %>%
  filter(total_inv > 5000) %>% mutate(gender_predicted = x)
  
  return(plot_dat)
  }
  )
plot_dat <- bind_rows(plot_dat)
ggplot(plot_dat, aes(x = Ctry_code, y = Ctry_share, fill = gender_predicted))+
  geom_col(position = "dodge")+labs(x = "Country", y="Sample Share")
# => much more U.S. residents in the non-predicted sample

# check whether country-specifc female shares are similar in both countries:
plot_dat <- lapply(c("yes", "no"), function(x){
  
  plot_dat <- pat_dat %>% filter(gender_predicted == x) %>% distinct(name, .keep_all = TRUE)
  plot_dat <- plot_dat %>% group_by(Ctry_code) %>% summarize(total_inv = n(),
                                                             female_inv = sum(gender == 0, na.rm = TRUE),
                                                             female_share = female_inv / total_inv) %>%
    filter(total_inv > 5000) %>% mutate(gender_predicted = x)
  
  return(plot_dat)
}
)
plot_dat <- bind_rows(plot_dat)
ggplot(plot_dat, aes(x = Ctry_code, y = female_share, fill = gender_predicted))+
  geom_col(position = "dodge")+labs(x = "Country", y="Feamle Share")
# => US has a rather low female share and is over-representeted in non-prediction share.
# => Thus, higher shares in predicted sample could partially be driven by this. However,
# => the algorithm does seem to predict more inventors to be female than actually are...

##################################
######## DATA PROCESSING #########
##################################

## female inventors on patents per country and p_year:
female_inv_shares <- pat_dat %>% 
  group_by(Ctry_code, p_year) %>%
  summarise(total_inventors = n(),
            female_inventors = sum(gender == 0, na.rm = TRUE),
            female_share_inventors = female_inventors / total_inventors)%>%
  filter(total_inventors > 30)

## merge female graduate shares and female inventor shares together
female_inv_shares$Ctry_code <- trimws(female_inv_shares$Ctry_code)
female_inv_shares <- merge(female_inv_shares, grad_dat, 
                           by = c("Ctry_code", "p_year"), all = TRUE)

## get Country names
isco2obs <- rownames(female_inv_shares[nchar(female_inv_shares$Ctry_code) == 2, ])
isco2obs <- as.numeric(isco2obs)
female_inv_shares$Country <- NA
female_inv_shares$Country[isco2obs] <- countrycode(female_inv_shares$Ctry_code[isco2obs],
                                                   "iso2c", "country.name.en")
female_inv_shares$Country[-isco2obs] <- countrycode(female_inv_shares$Ctry_code[-isco2obs],
                                                    "iso3c", "country.name.en")
print("Data ready for plotting")

########################
######## PLOTS #########
########################


## FIGURE 1: FEMALE INVENTOR SHARES ACROSS COUNTRIES
selected_countries <- c("CH", "US", "UK", "DE", "AT", "DK", "FR", "IT", "ES", "PT", "CA",
                        "KR", "FI", "SE", "NO", "NL", "BE", "PL", "CZ", "HU", "SK", "IE",
                        "IL", "JP")
plot_dat <- female_inv_shares %>%
  filter(Ctry_code %in% selected_countries,
         p_year >= 1990, p_year <= 2015,
         total_inventors > 30) %>% 
  select(p_year, Ctry_code, female_share_inventors) %>%
  distinct(p_year, Ctry_code, .keep_all = TRUE)

write.csv(plot_dat, "Data/female_inventor_share.csv")
# selected_countries <- c("US", "DE", "JP", "CH", "IT", "FR")
# plot_dat <- plot_dat %>% filter(Ctry_code %in% selected_countries)
# ggplot(plot_dat, aes(x = p_year, y = female_share_inventors, color = Ctry_code))+
#   geom_line() + geom_point(aes(shape = Ctry_code))+ ylim(c(0, 0.4))+
#   labs(y = "Share of Females Inventors", x = "Year",
#        shape = "", color = "")+
#   scale_color_viridis(option = "inferno", end = 0.9, discrete = TRUE)+
#   guides(color = guide_legend(nrow = 1))+
#   theme(panel.background = element_blank(),
#         panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
#         legend.position = "bottom", legend.direction = "horizontal",
#         axis.line = element_line(),
#         axis.title = element_text(face="bold",size=10))
# # => convert this plot to an aimated graph in the sense of the specialization report


## FIGURE 2: FEMALE INVENTOR SHARES AND FEMALE UNIVERSITY GRADUATES IN NATURAL SCIENCES

selected_countries <- c("CH", "US", "UK", "DE", "AT", "DK", "FR", "IT", "ES", "PT", "CA",
                        "KR", "FI", "SE", "NO", "NL", "BE", "PL", "CZ", "HU", "SK", "IE",
                        "IL", "JP")
plot_dat <- female_inv_shares %>%
  filter(Ctry_code %in% selected_countries,
         FIELD == "F05", ISC11_LEVEL == "L7",
         p_year >= 2010,
         total_graduates > 30,
         total_inventors > 30) %>%
  group_by(Ctry_code) %>% summarize(total_graduates = sum(total_graduates),
                                    total_inventors = sum(total_inventors),
                                    female_inventors = sum(female_inventors),
                                    female_graduates = sum(female_graduates)) %>%
  mutate(female_share_inventors = female_inventors / total_inventors,
         female_share_graduates = female_graduates / total_graduates)

write.csv(plot_data, "Data/female_inventors_graduates.csv")


# plot with 45degree line (all countries are bad)
ggplot(plot_dat, aes(x = female_share_graduates, 
                      y = female_share_inventors))+
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
ggplot(plot_dat, aes(x = female_share_graduates, 
                      y = female_share_inventors))+
  geom_point(aes(size = total_graduates), alpha = 0.5, color = "steelblue")+
  geom_text(aes(label = Ctry_code), size = 3, nudge_y = 0.005, nudge_x = -0.003)+
  xlim(0.4, 0.75)+
  ylim(0, 0.4)+
  labs(x = "Female Graduate Share in Natural Sciences", y = "Female Inventor Share")+
  geom_vline(xintercept = mean(plot_dat$female_share_graduates), 
             linetype = "dotted")+
  geom_hline(yintercept = mean(plot_dat$female_share_inventors), 
             linetype = "dotted")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))





