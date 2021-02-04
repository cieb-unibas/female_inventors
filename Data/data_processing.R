#######################################################################
# Description:    This script processes patent data from the USPTO    #
#                 and the EPO to investigate female inventor shares   #
#                 across countries and over time.                     #
# Authors:        Matthias Niggli/CIEB UniBasel                       #
#                 Christian Rutzer/CIEB UniBasel                      #
# Last Revised:   04.02.2021                                          #
#######################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("countrycode")
library("viridis")
library("zoo")
library("data.table")

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
grad_dat <- rename(grad_dat, inv_ctry = COUNTRY, p_year = YEAR, 
                   female_graduates = Value)
grad_dat <- select(grad_dat, - SEX)
print("Data on university graduates ready for analysis")

#### USPTO patent inventor data with gender information
pat_dat <- readRDS(paste0(mainDir1, "/created data/us_inv_gender.rds"))
pat_dat <- filter(pat_dat, !(inv_ctry %in% c("unknown", "Canada", "NA")))
pat_dat <- mutate(pat_dat, inv_ctry = substr(inv_ctry, 1, 2))
pat_dat <- dplyr::select(pat_dat, -p_year) %>% dplyr::rename(p_year = pub_year)
gender_path <- paste0(mainDir1,"/raw data/inventor_gender.tsv")
gender <- read.table(gender_path, sep = "\t", header = TRUE)#, quote = "")
gender$male <- as.character(gender$male)
gender <- gender %>%
  select(disambig_inventor_id_20200630, male) %>%
  distinct(disambig_inventor_id_20200630, .keep_all = TRUE) %>%
  dplyr::rename(id = disambig_inventor_id_20200630,
         gender = male)
gender <- left_join(pat_dat, gender, by = "id")

#### PCT inventor data with gender information
pct_dat <- read.csv("Data/female_pct.csv")
names(pct_dat) <- c("Country", as.character(seq(2000, 2019)))
pct_dat<- pct_dat %>% gather("p_year","female_share_inventors", -Country)
pct_dat$inv_ctry <- countrycode(pct_dat$Country, origin = "country.name.en", "iso2c")
write.csv(pct_dat, "Report/female_inventor_share_PCT.csv")
print("All data ready for analysis")

################################
######## INSEPCT DATA  #########
################################

# "female patenting share"
female_pat <- nrow(gender %>% filter(gender == "0"))
total_pat <- nrow(gender)
paste("Overall female share is:", round(female_pat / total_pat, 4) * 100, "%")

# female inventor share
female_inv <- nrow(gender %>% distinct(id, .keep_all = TRUE) %>% filter(gender == "0"))
total_inv <- nrow(gender %>% distinct(id))
paste("Female inventor share is:", round(female_inv / total_inv, 4) * 100, "%")


####################################
######## CREATE PLOT DATA  #########
####################################

# calculate the number and share of female inventors on patents per country and p_year:
female_inv_shares <- gender %>% distinct(id, p_year, .keep_all = TRUE) %>%
  group_by(inv_ctry, p_year) %>%
  summarise(total_inventors = n(),
            female_inventors = sum(gender == 0, na.rm = TRUE),
            female_share_inventors = female_inventors / total_inventors)%>%
  filter(total_inventors > 30)

# add female graduate data
female_inv_shares$inv_ctry <- trimws(female_inv_shares$inv_ctry)
female_inv_shares <- merge(female_inv_shares, grad_dat, 
                           by = c("inv_ctry", "p_year"), all = TRUE)
female_inv_shares$Country <- countrycode(female_inv_shares$inv_ctry,
                                         "iso2c", "country.name.en")

## DATA FIGURE 1: FEMALE INVENTOR SHARES ACROSS COUNTRIES ----------------------
plot_dat_1 <- female_inv_shares
plot_dat_1 <- setDT(plot_dat_1)[order(p_year), .SD, by = .(inv_ctry)]

# Create 5 year averages
plot_dat_1 <- setDT(plot_dat_1)[, c("total_inventors_5", "female_inventors_5") := list(rollsum(total_inventors, 5, fill = NA, align = "right"), rollsum(female_inventors, 5, fill = NA, align = "right")), by = .(inv_ctry)]
plot_dat_1 <- mutate(plot_dat_1, female_share_inventors_5 = female_inventors_5 / total_inventors_5)

plot_dat_1 <- plot_dat_1 %>%
  filter(p_year >= 1980, p_year <= 2019,
         total_inventors_5 > 30) %>% 
  dplyr::select(p_year, Country, inv_ctry, female_share_inventors_5, total_inventors) %>%
  distinct(p_year, inv_ctry, .keep_all = TRUE)

# Create observation for missings and set them to zero / important for dynamic plot
temp <- data.table::dcast(plot_dat_1, p_year ~ inv_ctry, value.var = c("female_share_inventors_5"))
temp <- melt(temp, id.vars = c("p_year"))
temp <- dplyr::mutate(temp, inv_ctry = variable) 
temp <- dplyr::select(temp, -variable, -value)

plot_dat_1 <- left_join(temp, plot_dat_1, by = c("inv_ctry", "p_year"))
plot_dat_1 <- filter(plot_dat_1, inv_ctry != "NA")
plot_dat_1 <- setDF(plot_dat_1)
plot_dat_1 <- data.frame(plot_dat_1[, colnames(plot_dat_1) %in% c("inv_ctry", "Country", "p_year")], 
                             sapply(plot_dat_1[, !(colnames(plot_dat_1) %in% c("inv_ctry", "Country", "p_year"))], 
                                    function(x) ifelse(is.na(x) == T, 0, x)))

colnames(plot_dat_1) <- c("p_year", "inv_ctry", "country", "female_share_inventors", "total_inventors")
plot_dat_1 <- mutate(plot_dat_1, country = ifelse(is.na(country), countrycode(inv_ctry, "iso2c", "country.name.en"), country))

# Keep only countries having the most inventors
sum_inv <- aggregate(total_inventors ~ inv_ctry, data =  female_inv_shares, FUN = sum, na.rm = T) %>% arrange(-total_inventors)
plot_dat_1 <- filter(plot_dat_1, inv_ctry %in% sum_inv[1:40, "inv_ctry"])

write.csv(plot_dat_1, "Report/graph_gender_time/female_inventor_share_USPTO.csv")
print("Data for animated plot saved")

## FIGURE 2 & 3: FEMALE INVENTOR SHARES AND FEMALE UNIVERSITY GRADUATES IN NATURAL SCIENCES
plot_dat <- female_inv_shares %>%
  filter(FIELD == "F05", ISC11_LEVEL == "L7",
         p_year >= 2010,
         total_graduates > 30,
         total_inventors > 30) %>%
  group_by(inv_ctry) %>% summarize(total_graduates = sum(total_graduates),
                                    total_inventors = sum(total_inventors),
                                    female_inventors = sum(female_inventors),
                                    female_graduates = sum(female_graduates)) %>%
  mutate(female_share_inventors = female_inventors / total_inventors,
         female_share_graduates = female_graduates / total_graduates)

write.csv(plot_dat, "Report/female_inventors_graduates_USPTO.csv", row.names = FALSE)
print("Data for static plots saved.")

# plot with 45degree line (all countries are bad)
ggplot(plot_dat, aes(x = female_share_graduates, 
                      y = female_share_inventors))+
  geom_point(aes(size = total_graduates), alpha = 0.5, color = "steelblue")+
  geom_text(aes(label = inv_ctry), size = 3, nudge_y = 0.005, nudge_x = -0.003)+
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
  geom_text(aes(label = inv_ctry), size = 3, nudge_y = 0.005, nudge_x = -0.003)+
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



