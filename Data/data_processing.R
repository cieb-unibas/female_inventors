#######################################################################
# Description:    This script processes patent data from the USPTO    #
#                 and the EPO to investigate female inventor shares   #
#                 across countries and over time.                     #
# Authors:        Matthias Niggli/CIEB UniBasel                       #
#                 Christian Rutzer/CIEB UniBasel                      #
# Last Revised:   23.02.2021                                          #
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
grad_dat <- read.csv("Data/oecd_graduates_STEMFields.csv")
names(grad_dat)[1] <- "COUNTRY"
grad_dat <- grad_dat %>% #select(-SEX, -Country.of.origin, -COUNTRY_ORIGIN, -Level.of.education,
                                # -YEAR, -Unit.Code, -Unit, -PowerCode, -PowerCode.Code, -Flag.Codes,
                                # -Flags, -Reference.Period.Code, -Reference.Period, -ISC11P_CAT) %>%
  filter(Category.of.education == "All educational programmes")

total_year <- grad_dat %>% filter(Sex == "Total") %>% 
  select(COUNTRY, FIELD, Field, Year, Value) %>%
  rename(total_graduates = Value)
grad_dat <- grad_dat %>% filter(Sex == "Women") %>% 
  select(COUNTRY, FIELD, Year, Value)
grad_dat <- merge(grad_dat, total_year, 
                  by = c("COUNTRY", "FIELD", "Year"), all = TRUE)
total_year <- NULL
grad_dat <- mutate(grad_dat, female_share_graduates = Value / total_graduates)
grad_dat <- grad_dat[is.nan(grad_dat$female_share_graduates) == FALSE &
                       is.na(grad_dat$female_share_graduates) == FALSE, ]
grad_dat$COUNTRY <- countrycode(grad_dat$COUNTRY, "iso3c", "iso2c")
grad_dat <- rename(grad_dat, inv_ctry = COUNTRY, p_year = Year, 
                   female_graduates = Value)

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

###########################################
######## CREATE DATA FOR PLOTTING #########
###########################################

#### DATA FIGURE 1: FEMALE INVENTOR SHARES ACROSS COUNTRIES
# (1) calculate the number and share of female inventors on patents per country and p_year:
female_inv_shares <- gender %>% distinct(id, p_year, .keep_all = TRUE) %>%
  group_by(inv_ctry, p_year) %>%
  summarise(total_inventors = n(),
            female_inventors = sum(gender == 0, na.rm = TRUE))

female_inv_shares$Country <- countrycode(female_inv_shares$inv_ctry, "iso2c", "country.name.en")

plot_dat_1 <- female_inv_shares
plot_dat_1 <- setDT(plot_dat_1)[order(p_year), .SD, by = .(inv_ctry)]

# (2) Create 5 year averages
plot_dat_1 <- setDT(plot_dat_1)[, c("total_inventors_5", "female_inventors_5") := list(rollsum(total_inventors, 5, fill = NA, align = "right"), rollsum(female_inventors, 5, fill = NA, align = "right")), by = .(inv_ctry)]
plot_dat_1 <- mutate(plot_dat_1, female_share_inventors_5 = female_inventors_5 / total_inventors_5)

plot_dat_1 <- plot_dat_1 %>%
  filter(p_year >= 1980, p_year <= 2019,
         total_inventors_5 > 60) %>% 
  dplyr::select(p_year, Country, inv_ctry, female_share_inventors_5, total_inventors_5) %>%
  distinct(p_year, inv_ctry, .keep_all = TRUE)

# (3) Create observation for missings and set them to zero / important for dynamic plot
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

# (4) Keep only countries having the most inventors
sum_inv <- aggregate(total_inventors ~ inv_ctry, data =  female_inv_shares, FUN = sum, na.rm = T) %>% arrange(-total_inventors)
plot_dat_1 <- filter(plot_dat_1, inv_ctry %in% sum_inv[1:40, "inv_ctry"])
plot_dat_1 <- filter(plot_dat_1, !(inv_ctry %in% c("HU")))
plot_dat_1 <- dplyr::select(plot_dat_1, -X)

# (5) Save the data for plotting
write.csv(plot_dat_1, "/scicore/home/weder/rutzer/innoscape/female_inventors/Report/graph_gender_time/female_inventor_share_USPTO.csv", row.names = FALSE)
print("Data for animated plot saved")




#### FIGURE 2 & 4: FEMALE INVENTOR SHARES AND FEMALE UNIVERSITY GRADUATES IN STEM FIELDS
# (1) assign ipc classes to technology groups as in Schmoch (2008):
# https://www.wipo.int/export/sites/www/ipstats/en/statistics/patents/pdf/wipo_ipc_technology.pdf
ELECTRICAL_ENGINEERING <- seq(1, 8)
INSTRUMENTS <- seq(9, 13)
CHEMISTRY <- seq(14, 24)
MECHANICAL_ENGINEERING <- seq(25, 32)
OTHER_FIELDS <- seq(33, 35)

gender <- gender %>% mutate(
  tech_group = case_when(tech_field %in% ELECTRICAL_ENGINEERING ~ "Electrical Engineering",
                         tech_field %in% INSTRUMENTS ~ "Instruments",
                         tech_field %in% CHEMISTRY ~ "Chemistry",
                         tech_field %in% MECHANICAL_ENGINEERING ~ "Mechanical Engineering",
                         tech_field %in% OTHER_FIELDS ~ "Other Fields")
  )

# (2) calculate female inventor shares per tech_group:
female_inv_shares <- gender %>%
  filter(inv_ctry %in% unique(grad_dat$inv_ctry) & 
           p_year >= 2010 & p_year <= 2015) %>% 
  group_by(inv_ctry, tech_group) %>% 
  distinct(id, p_year, .keep_all = TRUE) %>% # only unique inventors per tech_group
  summarise(total_inventors = n(),
            female_inventors = sum(gender == 0, na.rm = TRUE),
            female_share_inventors = female_inventors / total_inventors) %>%
  filter(total_inventors >= 60)

# (3) calculate female graduate shares in STEM fields
STEM_FIELDS <- c("F05", "F06", "F07") # all STEM

grad_shares_STEM <- function(stem_field){
  grad_shares <- grad_dat %>%
    filter(FIELD %in% stem_field,
           p_year >= 2010, p_year <= 2015) %>%
    group_by(inv_ctry) %>% summarize(total_graduates = sum(total_graduates, na.rm = TRUE),
                                   female_graduates = sum(female_graduates, na.rm = TRUE)) %>%
    mutate(female_share_graduates = female_graduates / total_graduates) %>% 
    filter(total_graduates >= 60) %>%
    mutate(stem_fields = paste(stem_field, collapse = ", "))
  
  return(grad_shares)
}
grad_shares <- grad_shares_STEM(stem_field = STEM_FIELDS)

# (4) combine female inventor and STEM graduate shares:
plot_dat <- merge(female_inv_shares, grad_shares, by = "inv_ctry", all = TRUE)

# (5) calculate country-level female inventor shares for the overall economy:
tmp <- gender %>% distinct(id, p_year, .keep_all = TRUE) %>%
  group_by(inv_ctry) %>%
  filter(p_year >= 2010 & p_year <= 2015) %>% 
  summarise(total_inventors = n(),
            female_inventors = sum(gender == 0, na.rm = TRUE)) %>%
  mutate(female_share_inventors = female_inventors / total_inventors,
         tech_group = "Overall")

# (6) add graduates information to female shares in the overall economy
MERGE_VARS <- c("inv_ctry", names(plot_dat)[!names(plot_dat) %in% names(tmp)])
tmp <- merge(tmp, plot_dat[!duplicated(plot_dat$inv_ctry), MERGE_VARS], by = "inv_ctry", all.x = TRUE)

# (7) create the same column structure and combine datasets
tmp <- tmp[, names(plot_dat)]
plot_dat <- rbind(plot_dat, tmp)
plot_dat <- plot_dat[complete.cases(plot_dat), ]
plot_dat <- mutate(plot_dat, country = countrycode(inv_ctry, "iso2c", "country.name.en"))
plot_dat <- filter(plot_dat, inv_ctry != "JP") # exclude Japan because of missing information

# (8) save data for plotting
write.csv(plot_dat, "Report/graph_gender_techgroup/female_inventors_graduates_techgroup_USPTO.csv", 
          row.names = FALSE)
print("Data for tech_group plot saved.")
write.csv(plot_dat[plot_dat$tech_group == "Overall", ], 
          "Report/female_inventors_graduates_USPTO.csv", row.names = FALSE)
print("Data for overall-economy plot saved.")




#### FIGURE 3: CONVERSION RATE OF FEMALE GRADUATES TO PATENT INVENTORS
# (1) calculate conversion rate at the country-level:
plot_dat_3 <- plot_dat[plot_dat$tech_group == "Overall", ]
plot_dat_3 <- plot_dat_3 %>% 
  mutate(conv_rate = female_share_inventors / female_share_graduates) %>%
  select(country, conv_rate, total_inventors) %>% arrange(-total_inventors)
paste("Overall mean conversion rate is:", round(100 * mean(plot_dat_3$conv_rate), 2), "%") # 30.67%

# only keep 20 largest inventor countries for plotting:
plot_dat_3 <- plot_dat_3[1:20, ] %>% select(-total_inventors)
plot_dat_3 <- plot_dat_3 %>% arrange(-conv_rate)
paste("Top20 country mean conversion rate is:", round(100 * mean(plot_dat_3$conv_rate), 2), "%") # 31.84%

# (2) save for plotting:
write.csv(plot_dat_3, "Report/converstion_rate_plot.csv", row.names = FALSE)
print("Data for conversion-rate plot saved.")




#### GROWTH RATES OF FEMALE STEM GRADUATES IN SWITZERLAND
COUNTRY <- "CH"
female_stem <- grad_dat %>% 
  filter(inv_ctry == COUNTRY & FIELD %in% c("F05", "F06", "F07") & p_year != "2005") %>%
  group_by(p_year) %>% summarise(female_graduates = sum(female_graduates),
                              total_graduates = sum(total_graduates),
                              female_share_graduates = female_graduates / total_graduates)
share_2010 <- round(100 * female_stem[female_stem$p_year == "2010", "female_share_graduates"], digits = 2)
share_2018 <- round(100 * female_stem[female_stem$p_year == "2018", "female_share_graduates"], digits = 2)

# summarize:
paste0("Female STEM graduate share in ", COUNTRY, " in 2010: ", share_2010, "%")# 28.05%
paste0("Female STEM graduate share in ", COUNTRY, " in 2018: ", share_2018, "%") # 29.44%
paste0("Female STEM graduate share has increased in ", COUNTRY, " by: ", share_2018 - share_2010, " percentage points") # 1.39pp
paste0("Female STEM graduate share has increased in ", COUNTRY, " by: ", 
       round(((share_2018 / share_2010) -1) * 100, 2), "%") # 4.96%


