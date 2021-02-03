

#install.packages("foreign")
#install.packages("car")
#install.packages("haven")
#install.packages("magrittr")
#install.packages("tidyverse")

#loading packages
library(foreign)
library(car)
library(haven)
library(magrittr)
library(tidyverse)

#importing the data that has been reviewed for completion of PT task
wide_data <- Import("data_downloaded_January_20_2021.csv") 

# Checked if there are any repeat IP addresses that need removed
#bot_check  <- wide_data %>% 
#select(sub_id, IPAddress)

#identifying the repeat IP addresses
#repeat_ips <- bot_check$IPAddress[duplicated(bot_check$IPAddress)]

# There are none - as expected using Prolific - hashed out code above since unnecessary

###################################
### Cleaning Overall PT Dataset ###
###################################

long_format <- wide_data %>% 
  # reverse coding items that need reverse coded
  mutate_at(c("selfBFI_1", "selfBFI_7", "selfBFI_3", "selfBFI_8",
              "selfBFI_14", "selfBFI_10", "selfBFI_17", "selfBFI_18",
              "SterBFIAnti_1", "SterBFIAnti_7", "SterBFIAnti_3",
              "SterBFIAnti_8", "SterBFIAnti_14", "SterBFIAnti_10",
              "SterBFIAnti_17", "SterBFIAnti_18", "SterBFITrump_1",
              "SterBFITrump_7", "SterBFITrump_3", "SterBFITrump_8",
              "SterBFITrump_14", "SterBFITrump_10", "SterBFITrump_17",
              "SterBFITrump_18", "TargBFITrump_1", "TargBFITrump_7",
              "TargBFITrump_3", "TargBFITrump_8", "TargBFITrump_14",
              "TargBFITrump_10", "TargBFITrump_17", "TargBFITrump_18",
              "TargBFIAnti_1", "TargBFIAnti_7", "TargBFIAnti_3", 
              "TargBFIAnti_8", "TargBFIAnti_14", "TargBFIAnti_10",
              "TargBFIAnti_17", "TargBFIAnti_18"), 
            list(~dplyr::recode(., `1`= 5, 
                                `2`= 4, 
                                `3` = 3, 
                                `4` = 2, 
                                `5` = 1))) %>% 
  # get their two chances to answer about their opinion in the same column
  pivot_longer(c(opin_pol, opin_pol2)) %>% 
  mutate(opin_pol_combined = value) %>% 
  dplyr::select(-value, -name) %>% 
  filter(!is.na(opin_pol_combined)) %>% #remove people who did not respond about voting preference
  # get the BFI scores used for projection in long-format
  pivot_longer(c(SterBFIAnti_1:SterBFIAnti_19,
                 SterBFITrump_1:SterBFITrump_19,
                 TargBFITrump_1:TargBFITrump_19,
                 TargBFIAnti_1:TargBFIAnti_19,
                 selfBFI_1:selfBFI_19),
               names_sep = "_",
               names_to = c("bfi_type", "BFI_number")) %>% 
  pivot_wider(names_from = bfi_type, values_from = value) %>% 
  # wrangle to remove NA's from target, since participants only got one target
  pivot_longer(c(TargBFITrump, TargBFIAnti),
               names_to = "BFI_target_group") %>% 
  mutate_at(vars(value), ~replace(., is.na(.), 99)) %>% 
  filter(value != "99") %>% #remove NA's that formed when we made BFI_target_group into long format
  rename(TargBFI = value) %>% 
  # create column for target group for BFI
  mutate(BFI_target_group = (dplyr::recode(BFI_target_group, 
                                           `TargBFITrump`= "Trump",
                                           `TargBFIAnti`= "Anti"))) %>% 
  # get everyday responses in long format
  pivot_longer(c(SterEDAnti_1:SterEDAnti_5,
                 SterEDTrump_1:SterEDTrump_5,
                 TargEDTrump_1:TargEDTrump_5,
                 TargEDAnti_1:TargEDAnti_5,
                 selfED_1:selfED_5),
               names_sep = "_",
               names_to = c("ed_type", "ED_number")) %>% 
  pivot_wider(names_from = ed_type, values_from = value) %>% 
  # also need to wrangle to remove NA's due to one target per participant
  pivot_longer(c(TargEDTrump, TargEDAnti),
               names_to = c("ED_targ_group")) %>% 
  mutate_at(vars(value), ~replace(., is.na(.), 99)) %>% #to remove NA's using filtering, making them 99's first, since filter wasn't letting me filter "NA"
  filter(value != "99") %>% # remove NA's that occurred by making ED into long format
  rename(TargED = value) %>% #renaming "value" to keep the variable
  # target per everyday responses and making variables factors or numeric
  mutate(ED_targ_group = (dplyr::recode(ED_targ_group, 
                                        `TargEDTrump`= "Trump",
                                        `TargEDAnti`= "Anti")),
         group_cond = as.factor(ifelse(opin_pol_combined == 1 #1 means "yes" they supported Trump's 2020 bid for re-relection
                                       & BFI_target_group == "Trump", 
                                       "In-group",
                                       ifelse(opin_pol_combined == 2 #2 means "no" they did not support Trump's re-election
                                              & BFI_target_group == "Trump", 
                                              "Out-group",
                                              ifelse(opin_pol_combined == 1 
                                                     & BFI_target_group == "Anti", 
                                                     "Out-group",
                                                     ifelse(opin_pol_combined == 2 
                                                            & BFI_target_group == "Anti", 
                                                            "In-group", NA))))),
         ED_number = as.numeric(ED_number),
         BFI_number = as.numeric(BFI_number),
         ED_targ_group = as.factor(ED_targ_group),
         BFI_target_group = as.factor(BFI_target_group),
         opin_pol_combined = as.factor(opin_pol_combined),
         race = as.factor(race),
         gender = as.factor(gender),
         education = as.factor(education),
         parent_education = as.factor(parent_education),
         country_birth = as.factor(country_birth),
         native_language = as.factor(native_language),
         citizen = as.factor(citizen),
         us_location = as.factor(us_location)) %>% 
  # dropping unnecessary text variables
  select(-c(day_nosupport_targ, day_support_trump_targ, gender_3_TEXT,
            race_8_TEXT, country_birth_2_TEXT, country_raised_2_TEXT,
            native_language_2_TEXT, IPAddress)) %>% 
  # making one variable for the following variables depending on which target (supported Trump vs did not) participants saw for both BFI and ED scales
  pivot_longer(c(lib_pol_trump_8, cons_pol_trump_8, 
                 lib_pol_antitrump_8, cons_pol_antitrump_8),
               names_sep = "_",
               names_to = c("polar_group", "drop1", "drop2", "drop3")) %>% 
  pivot_wider(names_from = polar_group, values_from = value) %>% #will need to filter NA's in this dataset when we use it later & filter  datasets for confirmatory analyses to make sure they are unique for their analysis; not doing it now to avoid removing data that could be used in the overall analysis for power
  mutate(stereoBFI = ifelse(BFI_target_group == "Trump", SterBFITrump,
                            ifelse(BFI_target_group == "Anti", SterBFIAnti, 
                                   NA)),
         stereoED = ifelse(ED_targ_group == "Trump", SterEDTrump,
                           ifelse(ED_targ_group == "Anti", SterEDAnti, NA)),
         # making one for each of these individual differences collapses across who the participant supported in the election
         contentiousness = ifelse(opin_pol_combined == 1, 
                                  cont_pol_trump,
                                  ifelse(opin_pol_combined == 2, 
                                         cont_pol_antitrump, NA)),
         fundaMoral = ifelse(opin_pol_combined == 1, 
                             fund_pol_trump,
                             ifelse(opin_pol_combined == 2, 
                                    fund_pol_antitrump, NA)),
         threat = ifelse(opin_pol_combined == 1, 
                         threat_pol_trump,
                         ifelse(opin_pol_combined == 2, 
                                threat_pol_antitrump, NA)),
         identification = ifelse(opin_pol_combined == 1, 
                                 ident_pol_trump,
                                 ifelse(opin_pol_combined == 2, 
                                        ident_pol_antitrump, NA)),
         # making manipulation checks all have the same levels so they can be combined to avoid NA's and used to filter out participants; keeping participants who got either qcheck 1 or 2 for the same question right
         qcheck_bfi_anti_total = ifelse(qcheck_bfi_notsupport == 2, 1,
                                        ifelse(qcheck2_bfi_notsupport == 2, 1,
                                               ifelse(qcheck_bfi_notsupport == 1, 2, #recoding so 1 = include and 2 = exclude
                                                      ifelse(qcheck_bfi_notsupport == 3, 2, NA)))),
         qcheck_bfi_trump_total = ifelse(qcheck_bfi_trump == 1, 1,
                                         ifelse(qcheck2_bfi_trump == 1, 1,
                                                ifelse(qcheck_bfi_trump == 2, 2,
                                                       ifelse(qcheck_bfi_trump == 3, 2, NA)))),
         qcheck_targ_trump_total = ifelse(qcheck_targ_trump == 1, 1,
                                          ifelse(qcheck2_targ_trump == 1, 1,
                                                 ifelse(qcheck_targ_trump == 2, 2, NA))),
         qcheck_targ_anti_total = ifelse(qcheck_targ_nosupport == 2, 1, #recoding so 1 = include, 2 = exclude
                                         ifelse(qcheck2_targ_nosupport == 2, 1,
                                                ifelse(qcheck_targ_nosupport == 1, 2, NA)))) %>% 
  # dropping variables no longer needed after creating new variables above
  select(-c(cont_pol_trump, cont_pol_antitrump, fund_pol_trump, 
            fund_pol_antitrump, threat_pol_trump,
            threat_pol_antitrump, ident_pol_trump, ident_pol_antitrump, consent,
            qcheck_bfi_notsupport, qcheck2_bfi_notsupport, qcheck2_bfi_trump,
            qcheck_bfi_trump, qcheck_targ_trump, qcheck2_targ_trump,
            qcheck_targ_nosupport, qcheck2_targ_nosupport,
            opin_pol_lastchance, drop1, drop2, drop3)) %>% 
  filter(qcheck_bfi_anti_total == 1) %>% # only keeping participants who got manipulation check for the bfi anti trump right
  filter(qcheck_bfi_trump_total == 1) %>% # same as above but for bfi supporting target
  # putting the qcheck for targets together to avoid NAs, removing NAs, then only keeping correct responses
  pivot_longer(c(qcheck_targ_trump_total, qcheck_targ_anti_total),
               names_to = c("qcheck_question")) %>% 
  mutate_at(vars(value), ~replace(., is.na(.), 99)) %>% #same process as above for filtering NA's
  filter(value != "99") %>% 
  filter(value == 1) %>% 
  rename(qcheck_targ = value) %>% 
  filter(citizen == 1)  %>% # only keeping people who are US citizens because study on US politics
  # centering predictors - will wait to center exploratory predictors until after EFA
  mutate(selfBFI_c = selfBFI - mean(selfBFI, na.rm = TRUE),
         selfED_c = selfED - mean(selfED, na.rm = TRUE),
         stereoBFI_c = stereoBFI - mean(stereoBFI, na.rm = TRUE),
         stereoED_c = stereoED - mean(stereoED, na.rm = TRUE),
         identification_c = identification - mean(identification, na.rm = TRUE))

#opin_pol: Do you support Trump? 1 = yes; 2 = no; same in follow-up question
# for qcheck not support bfi, 2 is correct; originally
# for qcheck for trump bfi, 1 is correct; originally
# for qcheck for trump targ, 1 is correct; originally
# for qcheck for anti targ, 2 is correct; originally

long_format <- long_format %>% 
  filter(questionable_pt == 1) # only keeping people who completed the PT task correctly

write.csv(long_format, 'long_format.csv')
