####################################
### Cleaning Overall PT Data-set ###
####################################

##### Notes about original data #####

# 652 participants originally collected from Prolific Academic
# 3 had empty data lines = 649 in imported data-set

#### Packages needed #####

#install.packages("magrittr")
#install.packages("tidyverse")
#install.packages("rio")

# Loading packages
library(magrittr)
library(tidyverse)
library(rio)

##### Importing the data #####

# This data has been manually reviewed for completion of PT task; filter will be applied at end of this .R document
wide_data <- import("data_downloaded_January_20_2021.csv") 

##### Code to check for bots #####

# There are none - as expected using Prolific - hashed out code above since unnecessary
## Checked if there are any repeat IP addresses that need removed
#bot_check  <- wide_data %>% 
#select(sub_id, IPAddress)

## Identified the repeat IP addresses
#repeat_ips <- bot_check$IPAddress[duplicated(bot_check$IPAddress)]

##### Wrangling data to long format #####

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
  # get their two chances to answer about their opinion in the same column. They were asked twice in case they accidentally didn't answer the first time
  pivot_longer(c(opin_pol, opin_pol2)) %>% 
  # Giving 'value' created by pivot a more meaningful name
  rename(opin_pol_combined = value) %>% 
  #remove people who did not respond about voting preference
  filter(!is.na(opin_pol_combined)) %>% 
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
  # to remove NA's using filtering, making them 99's first, since filter wasn't letting me filter "NA"
  mutate_at(vars(value), ~replace(., is.na(.), 99)) %>% 
  #remove NA's that formed when we made BFI_target_group into long format
  filter(value != "99") %>% 
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
  # Same issue with needing to remove NA's that occurred by making ED into long format and making them 99 first
  mutate_at(vars(value), ~replace(., is.na(.), 99)) %>% 
  filter(value != "99") %>% 
  rename(TargED = value) %>%
  # target per everyday responses and making variables factors or numeric
  mutate(ED_targ_group = (dplyr::recode(ED_targ_group, 
                                        `TargEDTrump`= "Trump",
                                        `TargEDAnti`= "Anti")),
         group_cond = as.factor( # used case_when instead of ifelse for more complex changes (more than 2 variables in one line) after Murat's suggestions bc it is cleaner and does the same thing
           case_when(opin_pol_combined == 1 & BFI_target_group == "Trump" ~ "In-group", #1 means "yes" they supported Trump's 2020 bid for re-election
             opin_pol_combined == 2 & BFI_target_group == "Trump" ~ "Out-group",
             opin_pol_combined == 1 & BFI_target_group == "Anti" ~ "Out-group", #2 means "no" they did not support Trump's re-election
             opin_pol_combined == 2 & BFI_target_group == "Anti" ~ "In-group"))) %>% 
  # dropping unnecessary text variables & the variable "name" that came up from wrangling earlier
  select(-c(name, day_nosupport_targ, day_support_trump_targ, gender_3_TEXT,
            race_8_TEXT, country_birth_2_TEXT, country_raised_2_TEXT,
            native_language_2_TEXT, IPAddress)) %>% 
  # making one variable perceived polarization depending on which target (supported Trump vs did not) participants saw for both BFI and ED scales
  pivot_longer(c(lib_pol_trump_8, cons_pol_trump_8, 
                 lib_pol_antitrump_8, cons_pol_antitrump_8),
               names_sep = "_",
               names_to = c("polar_group", "drop1", "drop2", "drop3")) %>% 
  # will need to filter NA's in this data-set when we use it later & filter data-sets for confirmatory analyses to make sure they are unique for their analysis; not doing it now to avoid removing data that could be used in the overall analysis for power
  pivot_wider(names_from = polar_group, values_from = value) %>% 
  mutate(stereoBFI = ifelse(BFI_target_group == "Trump", # if the target was a trump supporter
                            SterBFITrump, # Use stereotype about Trump on BFI
                            ifelse(BFI_target_group == "Anti", # if the target was anti Trump
                                   SterBFIAnti, # Use stereotypes about anti-trump group on BFI
                                   NA)), # Otherwise NA
         stereoED = ifelse(ED_targ_group == "Trump",  # Same as with BFI above, but with ED
                           SterEDTrump,
                           ifelse(ED_targ_group == "Anti", 
                                  SterEDAnti, 
                                  NA)),
         # making one variable in long format for each of these individual differences collapsed across who the participant supported in the election based on structure of survey
         contentiousness = ifelse(opin_pol_combined == 1, # if participants supported Trump
                                  cont_pol_trump, # Use contentiousness item for Trump supporters
                                  ifelse(opin_pol_combined == 2, # if participants were anti-Trump
                                         cont_pol_antitrump, # Use contentiousness item for anti-Trump supporters
                                         NA)), # Otherwise NA
         fundaMoral = ifelse(opin_pol_combined == 1, # same format as contentiousness
                             fund_pol_trump,
                             ifelse(opin_pol_combined == 2, 
                                    fund_pol_antitrump, 
                                    NA)),
         threat = ifelse(opin_pol_combined == 1, # same format as contentiousness
                         threat_pol_trump,
                         ifelse(opin_pol_combined == 2, 
                                threat_pol_antitrump, 
                                NA)),
         identification = ifelse(opin_pol_combined == 1, # same format as contentiousness
                                 ident_pol_trump,
                                 ifelse(opin_pol_combined == 2, 
                                        ident_pol_antitrump, 
                                        NA)),
         # making manipulation checks all have the same levels so they can be combined to avoid NA's and used to filter out participants; keeping participants who got either qcheck for the same question correct (asked twice in case the first time was an error) 
         qcheck_bfi_anti_total = ifelse(qcheck_bfi_notsupport == 2, 1, # recoding so 1 = include and 2 = exclude; 2 was originally the correct answer to this question, these participants were correct
                                        ifelse(qcheck2_bfi_notsupport == 2, 1, # same as above here; these participants were correct
                                               ifelse(qcheck_bfi_notsupport == 1, 2, # recoding so 1 = include and 2 = exclude; these participants got the answer wrong, so we are excluding them
                                                      ifelse(qcheck_bfi_notsupport == 3, 2, NA)))), # same as the line above, we are excluding these participants
         qcheck_bfi_trump_total = ifelse(qcheck_bfi_trump == 1, 1, # 1 was correct for this question and 2 or 3 was incorrect; coding 1 as include and 2 as exclude again
                                         ifelse(qcheck2_bfi_trump == 1, 1,
                                                ifelse(qcheck_bfi_trump == 2, 2,
                                                       ifelse(qcheck_bfi_trump == 3, 2, NA)))),
         qcheck_targ_trump_total = ifelse(qcheck_targ_trump == 1, 1, # 1 was correct for this question and 2 was incorrect; coding 1 as include and 2 as exclude again
                                          ifelse(qcheck2_targ_trump == 1, 1,
                                                 ifelse(qcheck_targ_trump == 2, 2, NA))),
         qcheck_targ_anti_total = ifelse(qcheck_targ_nosupport == 2, 1, # 2 was correct for this question and 1 was incorrect; recoding so 1 = include, 2 = exclude
                                         ifelse(qcheck2_targ_nosupport == 2, 1,
                                                ifelse(qcheck_targ_nosupport == 1, 2, NA)))) %>% 
  # dropping variables no longer needed after creating new variables above or were created from pivot
  select(-c(cont_pol_trump, cont_pol_antitrump, fund_pol_trump, 
            fund_pol_antitrump, threat_pol_trump,
            threat_pol_antitrump, ident_pol_trump, ident_pol_antitrump, consent,
            qcheck_bfi_notsupport, qcheck2_bfi_notsupport, qcheck2_bfi_trump,
            qcheck_bfi_trump, qcheck_targ_trump, qcheck2_targ_trump,
            qcheck_targ_nosupport, qcheck2_targ_nosupport,
            opin_pol_lastchance, drop1, drop2, drop3)) %>% 
  # only keeping participants who got manipulation check for the bfi anti trump right
  filter(qcheck_bfi_anti_total == 1) %>% 
  # same as above but for bfi supporting trump target
  filter(qcheck_bfi_trump_total == 1) %>%
  # putting the qcheck for targets together to avoid NAs, removing NAs, then only keeping correct responses
  pivot_longer(c(qcheck_targ_trump_total, qcheck_targ_anti_total),
               names_to = c("qcheck_question")) %>% 
  mutate_at(vars(value), ~replace(., is.na(.), 99)) %>% #same process as previously for filtering NA's
  filter(value != "99") %>% 
  filter(value == 1) %>% 
  rename(qcheck_targ = value) %>% 
  # only keeping people who are US citizens because study on US politics
  filter(citizen == 1)  %>% 
  # centering predictors - will wait to center exploratory predictors until after EFA
  mutate(selfBFI_c = selfBFI - mean(selfBFI, na.rm = TRUE),
         selfED_c = selfED - mean(selfED, na.rm = TRUE),
         stereoBFI_c = stereoBFI - mean(stereoBFI, na.rm = TRUE),
         stereoED_c = stereoED - mean(stereoED, na.rm = TRUE),
         identification_c = identification - mean(identification, na.rm = TRUE))

##### Notes on opin_pol variable #####

# opin_pol: Do you support Trump? 1 = yes; 2 = no; same in follow-up question
# for qcheck not support bfi, 2 is correct; originally
# for qcheck for trump bfi, 1 is correct; originally
# for qcheck for trump targ, 1 is correct; originally
# for qcheck for anti targ, 2 is correct; originally

##### Filtering for incorrect PT responses #####
long_format <- long_format %>% 
  filter(questionable_pt == 1) # only keeping people who completed the PT task correctly

##### Saving to .csv format #####

write.csv(long_format, 'long_format.csv')
