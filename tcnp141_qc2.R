
###############################################################################################################

# Filename    : tcnp141_qc.R 

# Developer   : Amal Anandan (ANANDAM3) 

# Date        : 30OCT2019 

# Description : QC program for Demographic table 

# Assumptions	: 

###############################################################################################################

# Modified by : 

# Date        : 

# Description : 
###############################################################################################################

# To see which folders are defined for R packages 
.libPaths() 

# Load "haven", "tidyverse", "dplyr" packages for further use
packages <-  c("haven", "tidyverse", "dplyr", "knitr")
lapply(packages, library, character.only = TRUE)

# To know the working directory environment 
setwd("~/CNP520A2202J")
getwd() 

# Read ADSL dataset
adsl <- read_sas("analysis_data/adsl.sas7bdat")

# Consider only those variables which are required for report and apply needed formats
adsl1 <- filter(adsl, SAFFL == "Y") %>%   # Consider only Safety patients
  select(SAFFL, TRT01A, AGEGR1, AAGE, SEX, RACE, ETHNIC, WEIGHT, HEIGHT, BMI) %>%
  mutate(TRTA = factor(TRT01A, levels = c("D01", "D02", "D03"),
                       labels = c("Dummy T01", "Dummy T02", "Dummy T03")),
         AGEGRL = factor(AGEGR1, levels = c("60-64", "65-69", "70-75", ">=76"), 
                         labels = c("60-64", "65-69", "70-75", ">=76")),
         SEXL = factor(SEX, levels = c("M", "F"),
                       labels = c("Male", "Female")),
         RACEL = factor(RACE, levels = c("CAUCASIAN", "BLACK", "ASIAN", "NATIVE AMERICAN", "PACIFIC ISLANDER", 
                                         "UNKNOWN", "OTHER"),
                        labels = c("Caucasian", "Black", "Asian", "Native American", "Pacific Islander", 
                                   "Unknown", "Other")),
         ETHCL = factor(ETHNIC, levels = c("HISPANIC OR LATINO", "OTHER EAST ASIAN", "SOUTHEAST ASIAN", "SOUTH ASIAN",
                                           "WEST ASIAN", "RUSSIAN", "JAPANESE", "CHINESE", "NOT REPORTED", "MIXED ETHNICITY", 
                                           "UNKNOWN", "OTHER"),
                        labels = c("Hispanic/Latino", "Other East Asian", "Southeast Asian", "South Asian", "West Asian", "Russian", 
                                   "Japanese", "Chinese", "Not reported", "Mixed Ethnicity", "Unknown", "Other"))) 


# Create function to handle Frequency count and calculate percentage
r_freq <- function(dsin, # Provide input dataset
                   trt,  # Provide treatment variable
                   cat,  # Provide category variables
                   by,   # For merging datasets, provide key variable
                   ord,  # For ordering purpose
                   txt)  # Provide text to be retained in dataset
{
      big_n <- count(dsin, !!trt)   # Get population N count for each treatment
      cat1 <- count(dsin, !!trt, !!cat)   # Get population n count for each category
      result1 <- merge(big_n, cat1, by = by)   # Merge datasets
      dso <- mutate(result1,                    # Populate percentage
                    PCT = format((n.y / n.x) * 100, digits = 1),
                    PCT_N = str_c(n.y, ' (', PCT, ')'),
                    CAT = !!cat,
                    ord = ord,
                    txt = txt) %>%
             select(!!trt, ord, txt, CAT, PCT_N) %>%     # Keep only required variables
             spread(key = !!trt, value = PCT_N)            # Transpose dataset
              
}

# Create Frequency for AGE Group   
agegrp <- r_freq(dsin=adsl1, trt = quo(TRTA), cat = quo(AGEGRL), by = "TRTA", ord = 1, txt = "Age category")

# Create frequency for SEX
sex <- r_freq(dsin=adsl1, trt = quo(TRTA), cat = quo(SEXL), by = "TRTA", ord = 3, txt = "Sex")

# Create Frequency for RACE  
race <- r_freq(dsin=adsl1, trt = quo(TRTA), cat = quo(RACEL), by = "TRTA", ord = 4, txt = "Race")

# Create Frequency for EHINICTY  
ethcl <- r_freq(dsin=adsl1, trt = quo(TRTA), cat = quo(ETHCL), by = "TRTA", ord = 5, txt = "Ethnicity")


# Create function to handle summary statistics
r_stats_summary <- function(dsin, # Provide input dataset
                            trt,  # Provide treatment variable
                            cat,  # Provide category variables
                            by,   # For merging datasets, provide key variable
                            ord,  # For ordering purpose
                            txt)  # Provide text to be retained in dataset
{
  dso <- select(dsin, !!trt, !!cat) %>%
         na.omit() %>%
         group_by(!!trt) %>%
         summarise(n = n(),
                   mean = format(mean(!!cat), digits = 5),
                   SD = format(sd(!!cat), digits = 4),
                   Min = format(min(!!cat), digits = 4),
                   Median = format(median(!!cat), digits = 5),
                   Max = format(max(!!cat), digits = 4)) %>% 
         gather(key = "CAT", value = "STAT", -!!trt) %>% 
         mutate(ord = ord,
                txt = txt,
                sort =  ifelse(CAT == "n", 1, 
                              ifelse(CAT == "mean", 2,
                                   ifelse(CAT == "SD", 3,
                                          ifelse(CAT == "Min", 4,
                                                ifelse(CAT == "Median", 5,
                                                       ifelse(CAT == "Max", 6, 0))))))) %>% 
    spread(key = !!trt, value = STAT) %>% 
    arrange(sort)
}

# AGE in Years summary statistics
ageyr <- r_stats_summary(dsin=adsl1, trt = quo(TRTA), cat = quo(AAGE), by = "TRTA", ord = 2, txt = "Age in years")

# WEIGHT summary statistics
weight <- r_stats_summary(dsin=adsl1, trt = quo(TRTA), cat = quo(WEIGHT), by = "TRTA", ord = 6, txt = "Weight")

# HEIGHT summary statistics
height <- r_stats_summary(dsin=adsl1, trt = quo(TRTA), cat = quo(HEIGHT), by = "TRTA", ord = 7, txt = "Height")

# BMI summary statistics
bmi <- r_stats_summary(dsin=adsl1, trt = quo(TRTA), cat = quo(BMI), by = "TRTA", ord = 8, txt = "BMI")



# Append all data frames
sec <- bind_rows(agegrp, sex, race, ethcl, ageyr, weight, height, bmi)

# Handle Missing Text & Values
sec$CAT <- ifelse(is.na(sec$CAT), "Missing", sec$CAT)
sec$`Dummy T01` <- ifelse(is.na(sec$`Dummy T01`), "0", sec$`Dummy T01`)
sec$`Dummy T02` <- ifelse(is.na(sec$`Dummy T02`), "0", sec$`Dummy T02`)
sec$`Dummy T03` <- ifelse(is.na(sec$`Dummy T03`), "0", sec$`Dummy T03`)

# Create final dataset
final <- arrange(sec, ord, sort)
View(final)

kable(final)

