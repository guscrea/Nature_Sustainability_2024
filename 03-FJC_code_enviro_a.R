# Script to Code Appellate Cases
# Written by Chris Rea
# Last modified April 24, 2024

# Notes ####
# Matching appellate cases to district cases (i.e., using district court party
# names to link a district court case and parties to a later appellate case) is
# not straightforward. Simple name matches do not work, since the names of the
# parties in the appellate data are often recorded differently than in the
# district case data. We can use the CDD_ID as a starting place for matching,
# but this is not simple either, since it's often the case that there are
# multiple district cases for a single appellate CDD_ID, and sometimes multiple
# appellate CDD_IDs for multiple district cases.

# NOTE: the CDD_ID is a constructed variable generated in the 01-FJC_build.R
# script. For district court-level data, it combines circuit (CIRC), district
# (DIST), and docket (DOCKET) numbers. For appellate data, it combines the
# district-level circuit number (DCIRC), the district level district number
# (DDIST), and the district-level docket number (DDOCKET).

# Load packages ####
library(tidyverse)
library(stringdist)

# Build simple dfs linking district and appellate data ####

# First make a simple df of the district data and of the appellate data, for
# manipulation and joining.

# !! NOTE: below, we retain the district-level disposition (DISP) and outcome
# (JUDGMENT) and include but rename the appellate disposition (also coded DISP,
# updated to ADISP). Later, we use the district level disposition to calculate
# if the plaintiff or defendant won, which is then used to determine the result
# (OUTCOME) of the appellate case. We need the district-level outcome because
# the appellate case outcome is defined in terms of the district-level outcome.

# district cases to join with appellate data
fjc_e_d_match <- fjc_e %>%
  select(
    CDD_ID, DISTRICT, OFFICE, FILEDATE, TERMDATE, JURIS, DISP, JUDGMENT, PLT, PLT_typ, DEF, DEF_typ
  ) %>%
  ungroup() %>%
  rename(
    "DJURIS" = "JURIS",
    #"DDISP" = "DISP",
    "CDD_ID_D" = "CDD_ID"
  ) %>%
  mutate(
    CDD_ID_DATE = str_c(CDD_ID_D, FILEDATE, sep = "-")
  )

# appellate cases to match with appellate data
fjc_e_a_match <- fjc_e_a %>%
  select(
    CDD_ID, DOFFICE, DDKTDATE, DKTDATE, JUDGDATE, JURIS, APPELLAN, APPELLEE, DISP,OUTCOME
  ) %>%
  rename(
    "ADISP" = "DISP"
  ) %>%
  group_by(
    CDD_ID
  ) %>%
  mutate(
    CDD_ID_DATE = str_c(CDD_ID, DDKTDATE, sep = "-"),# append date to CDD_ID
    CDD_ID_row = n() # count number of rows for each CDD_ID
  ) %>%
  group_by(
    CDD_ID_DATE
  ) %>%
  mutate(
    CDD_ID_DATE_row = n() # count number of rows for each CDD_ID_DATE
  ) %>%
  rename(
    "CDD_ID_A" = "CDD_ID"
  ) %>%
  ungroup() %>%
  mutate(
    ROW_ID = row_number() # create basic row number to identify original appellate cases after join with district data
  ) %>%
  ungroup()

# join district data to appellate data by CDD_ID_DATE
# Note <- where there is more than one appellate case for a given CDD_ID_DATE,
# duplicate rows will be created. Pre-join, appellate data includes 4458 cases.
# This is what we should end up with once clean-up is done. Post-join, appellate
# data includes 4509 cases. That's 51 "extra cases" each of which turns out to
# be a duplicate (no triplicates).
fjc_e_da <- left_join(fjc_e_a_match, fjc_e_d_match, by = "CDD_ID_DATE") %>%
  select(
    CDD_ID_A,
    CDD_ID_D,
    CDD_ID_DATE,
    CDD_ID_row,
    CDD_ID_DATE_row,
    ROW_ID,
    PLT, PLT_typ,
    DEF, DEF_typ,
    APPELLAN, APPELLEE,
    OFFICE, DISTRICT, DOFFICE,
    FILEDATE, TERMDATE,DKTDATE, DDKTDATE, JUDGDATE,
    JURIS, DJURIS, DISP, JUDGMENT,
    ADISP, OUTCOME
  ) %>%
  group_by(
    ROW_ID
  ) %>%
  mutate(
    num_dup = n() # count number of times extra rows are added by joining district data
  ) %>%
  ungroup() %>%
  mutate(
    # add row number for IDing cases in pivot (below)
    row_num = row_number()
  )

# Build word similarity matrices for matching district and appellate litigant types ####

# The next step is to break plaintiff and defendant (at district-level) and
# appellant and appellee (at appellate-level) names into lists of words and
# compute word similarity scores, as a means of estimating where the
# plaintiff/defendant is the appellant/appellee. This requires making a matrix
# of scores, since litigant names may have multiple words and we have to check
# each word against all other words. We use the stringsim() function to compute
# these similarities. Then, we can use the level of similarity to help us guess
# who is who: whether the plaintiff or defendant becomes the appellant or
# appellee.

# list of USA-related derivatives used in cleaning up data
USA_list = c("USA|USA |U.S.A.|U.S.|US " )

# turn PLT, DEF, Appellant and Appellee into lists of words; clean up some of
# the US abbreviations and agency names.
fjc_e_da_pivot <- fjc_e_da %>%
  ungroup() %>%
  mutate(
    # FOR ORGINAL LITIGANTS
    PLT_sim = PLT,
    DEF_sim = DEF,
    # FOR APPELLANT
    APPELLAN_sim = APPELLAN,
    APPELLEE_sim = APPELLEE
  ) %>%
  # pivot longer to recode all names at once
  pivot_longer(
    cols = PLT_sim:APPELLEE_sim,
    names_to = "litigant_type",
    values_to = "litigant_name"
  ) %>%
  # clean up names
  mutate(
    # remove et al
    litigant_name = str_remove_all(litigant_name, "ET"),
    litigant_name = str_remove_all(litigant_name, "ET "),
    litigant_name = str_remove_all(litigant_name, "AL"),
    # remove parentheses and punctuation
    litigant_name = str_remove_all(litigant_name, "\\("),
    litigant_name = str_remove_all(litigant_name, "\\)"),
    litigant_name = str_replace_all(litigant_name,"[:punct:]", ""), # remove punctuation
    litigant_name = str_remove_all(litigant_name, "  "), #remove two spaces
    litigant_name = trimws(litigant_name), # remove trailing white space
    # replace iterations of USA with UNITED STATES (see USA_list above)
    litigant_name = str_replace_all(litigant_name, USA_list, "UNITED STATES "),
    litigant_name = case_when(
      litigant_name == "US" | litigant_name == "USA" ~ "UNITED STATES",
      TRUE ~ litigant_name
    ),
    litigant_name = str_remove_all(litigant_name, "  "), #remove two spaces,
    # add full agency names for known agency abbreviations
    litigant_name = case_when(
      litigant_name == "USFS" ~ "UNITED STATES FOREST SERVICE",
      litigant_name == "USEPA" ~ "UNITED STATES ENVIRONMENTAL PROTECTION AGENCY",
      litigant_name == "USIA" ~ "UNITED STATES INFORMATION AGENCY",
      litigant_name == "USPS" ~ "UNITED STATES POSTAL SERVICE",
      litigant_name == "USDA" ~ "UNITED STATES DEPARTMENT OF AGRICULTURE",
      litigant_name == "USFWS" ~ "UNITED STATES FISH AND WILDLIFE SERVICE",
      TRUE ~ litigant_name
      )
    ) %>%
  pivot_wider(
    id_cols = row_num,
    names_from = "litigant_type",
    values_from = "litigant_name"
  )
  
# join lists of words from litigant names back to primary df
fjc_e_da <- left_join(
  fjc_e_da,
  fjc_e_da_pivot,
  by = "row_num",
  relationship = "many-to-many"
  ) %>%
  mutate(
    PLT_list = str_split(PLT_sim, " "),
    DEF_list = str_split(DEF_sim, " "),
    appellan_list = str_split(APPELLAN_sim, " "),
    appellee_list = str_split(APPELLEE_sim, " ")
  )
rm(fjc_e_da_pivot)

# call word similarity matrix function
source("Functions/word_sim_matrix.R")

# Compute name similarity matrices ####

# Put the similarity function to work: compute similarity scores for plaintiff
# and defendants with appellants and appellees.
fjc_e_da <- fjc_e_da %>%
  rowwise %>%
  mutate(
    # drop single letter entries
    appellan_list = list(appellan_list[str_length(appellan_list) >= 2]),
    appellee_list = list(appellee_list[str_length(appellee_list) >= 2]),
    # compute similarity matrix for each word in appellant and plaintiff lists;
    # identify perfect matches and also return mean similarity score.
    appellan_PLT_sim = list(sim_mat_f(appellan_list,PLT_list)), # full matrix
    appellan_PLT_diag = list(diag(appellan_PLT_sim)), # matrix diagonals (all 1s if names are perfect match *in same order*)
    appellan_PLT_pft = sum(appellan_PLT_sim == 1), # number of perfect matches in matrix
    appellan_PLT_mean = mean(unlist(appellan_PLT_sim)), # average value of all matrix elements
    # compute similarity matrix for each word in appellant and defendant lists;
    # identify perfect matches and also return mean similarity score.
    appellan_DEF_sim = list(sim_mat_f(appellan_list,DEF_list)),
    appellan_DEF_diag = list(diag(appellan_DEF_sim)), # matrix diagonals (all 1s if names are perfect match *in same order*)
    appellan_DEF_pft = sum(appellan_DEF_sim == 1),
    appellan_DEF_mean = mean(unlist(appellan_DEF_sim)),
    # compute similarity matrix for each word in appellee and plaintiff lists;
    # identify perfect matches and also return mean similarity score.
    appellee_PLT_sim = list(sim_mat_f(appellee_list,PLT_list)),
    appellee_PLT_diag = list(diag(appellee_PLT_sim)),
    appellee_PLT_pft = sum(appellee_PLT_sim == 1),
    appellee_PLT_mean = mean(unlist(appellee_PLT_sim)),
    # compute similarity matrix for each word in appellee and defendant lists;
    # identify perfect matches and also return mean similarity score.
    appellee_DEF_sim = list(sim_mat_f(appellee_list,DEF_list)),
    appellee_DEF_diag = list(diag(appellee_DEF_sim)),
    appellee_DEF_pft = sum(appellee_DEF_sim == 1),
    appellee_DEF_max = mean(unlist(appellee_DEF_sim))
  )

# examine duplicates and uncodable names

# look at cases where there are duplicate rows created (num_dup > 1)
fjc_e_da_dups <- fjc_e_da %>%
  filter(
    num_dup > 1
  )
# --> for these duplicates, there is no simple, highly reliable way to select
# which district case is the "right" match with the appellate case. So, for now,
# we drop all 102 duplicate rows (1.1% of data).
fjc_e_da <- fjc_e_da %>%
  filter(
    num_dup == 1
  )

# look at appellate cases where there is no district case to match appellate
# data perfectly
fjc_e_da_nas <- fjc_e_da %>%
  filter(
    is.na(CDD_ID_D)
  )
# --> there are 412 non-matched cases (9% of data) --> too many to drop
# Note also that there are fewer unique values. 
 
# Make list of the unique CDD_ID_A IDs for these cases. We'll use this to build a
# dataframe just of these appellate cases that do not match district cases with
# both circuit, district, docket AND docket/file date. We'll also use it to
# identify district cases with the same CDD_ID_D number that could be matches.
fjc_e_da_nas_list <- unique(fjc_e_da_nas$CDD_ID_A)

# Create a list of all the appellate cases that contain the CDD_ID's associated
# with NAs. Note that there will be more than 530 cases here because a) there
# are some repeats and b) some of those repeats have distinctive dates that *do*
# yield matches above.
fjc_e_a_nomatch <- fjc_e_da%>%
  filter(
    CDD_ID_A %in% fjc_e_da_nas_list
  )

# Create a list of all the district cases that could be matches (that have the
# same CDD_IDs) This only yields 172 matches. This means that many of the
# appellate cases that cannot be matched have CDD_ID numbers different from any
# district case, and thus that other means may be necessary to match them.
# (There can also be unusual circumstances where appellate courts have original
# jurisdiction.)
fjc_e_d_nomatch <- fjc_e_d_match %>%
  filter(
    CDD_ID_D %in% fjc_e_da_nas_list
  ) %>% # count the number of cases with each CDD_ID
  group_by(
    CDD_ID_D
  ) %>%
  mutate(
    CDD_ID_D_count = n()
  ) %>%
  ungroup()

# Create a list of the CDD_ID_D codes that are also preset in the list of CDD_ID_A,
# i.e. the codes that overlap between appellate and district data. This yields
# 131 matches.
CDD_ID_na_overlap <- fjc_e_d_nomatch %>%
  filter(
    CDD_ID_D %in% fjc_e_da_nas_list
  )
CDD_ID_na_overlap <- unique(CDD_ID_na_overlap$CDD_ID_D)

# look at the number of cases in district and appellate data that overlap CDD_IDs
fjc_e_d_match_overlap <- fjc_e_d_match %>%
  filter(
    CDD_ID_D %in% CDD_ID_na_overlap
  ) %>% # count the number of cases with each CDD_ID
  group_by(
    CDD_ID_D
  ) %>%
  mutate(
    CDD_ID_D_count = n()
  ) %>%
  ungroup()

fjc_e_a_match_overlap <- fjc_e_a_match %>%
  filter(
    CDD_ID_A %in% CDD_ID_na_overlap
  ) %>% # count the number of cases with each CDD_ID
  group_by(
    CDD_ID_A
  ) %>%
  mutate(
    CDD_ID_A_count = n()
  ) %>%
  ungroup()

# !! after examining non-matches closely, I think they will simply have to be coded
# manually. We will drop them here and move forward, code the NAs manually, and then
# add them back in.
fjc_e_da <- fjc_e_da %>%
  filter(
    !is.na(CDD_ID_D)
  )
# Write out, manually code, and re-read in NA appellate cases ####

# write out non-matches for manual coding
write_csv(
  fjc_e_da_nas %>%
  select(
    CDD_ID_A,
    CDD_ID_D,
    CDD_ID_DATE,
    APPELLAN,
    APPELLEE
    ),
  "Data/FJC_postprocessed/Appellate/appellate_cases_for_manual_coding.csv"
  )

# re-load manually CODED appellate cases that were no-matches
# Note: these coded values number 530 - more than the 412 NAs produced
# above.
fjc_e_da_nas_CODED <- read_csv("Data/FJC_postprocessed/Appellate/appellate_cases_for_manual_coding_CODED.csv")

# create unique ID with appellant and appellee name for join
# note: this creates 513 unique IDs; the remaining 17 cases of the orginal 530
# cases have NAs in appellant/ee names and are dropped.
fjc_e_da_nas_CODED <- fjc_e_da_nas_CODED %>%
  ungroup() %>%
  arrange(CDD_ID_DATE, APPELLAN, APPELLEE) %>%
  mutate(
    row_num = row_number(),
    join_id = str_c(CDD_ID_DATE, APPELLAN, APPELLEE, row_num, sep = "-")
  ) %>%
  filter(
    !is.na(join_id)
  )
  

fjc_e_da_nas <- fjc_e_da_nas %>%
  ungroup() %>%
  arrange(CDD_ID_DATE, APPELLAN, APPELLEE) %>%
  mutate(
    row_num = row_number(),
    join_id = str_c(CDD_ID_DATE, APPELLAN, APPELLEE, row_num, sep = "-")
  ) %>%
  filter(
    !is.na(join_id)
  )


# join coded cases to nas
fjc_e_da_nas_CODED <- fjc_e_da_nas_CODED %>%
  select(
    join_id, `APPELANT TYPE`, `APPELEE TYPE`
  ) %>%
  rename(
    "APPELLEE_typ" = `APPELEE TYPE`,
    "APPELLAN_typ" = `APPELANT TYPE`
  )
# join!
fjc_e_da_nas <- left_join(fjc_e_da_nas, fjc_e_da_nas_CODED, by = "join_id")

# add new DISP code for these unknown cases (99)
fjc_e_da_nas <- fjc_e_da_nas %>%
  mutate(
    DISP = 99
  ) %>%
  # rename DISTRICT-LEVEL PLT and DEF for later reference
  rename(
      "DPLT" = "PLT",
      "DDEF" = "DEF",
      "DPLT_typ" = "PLT_typ",
      "DDEF_typ" = "DEF_typ"
  )

# clean up now-coded NAs for row_bind.
fjc_e_da_nas <- fjc_e_da_nas %>%
  select(
    CDD_ID_A,
    CDD_ID_D,
    CDD_ID_row,
    DISTRICT,
    OFFICE,
    DOFFICE,
    DKTDATE,# from appeals
    DDKTDATE, # from appeals
    JUDGDATE, # from appeals
    FILEDATE,# from district
    TERMDATE,# from district
    JURIS,
    JUDGMENT,
    OUTCOME,
    DJURIS,
    DISP,
    ADISP,
    DPLT,
    DPLT_typ,
    DDEF,
    DDEF_typ,
    APPELLAN,
    APPELLAN_typ,
    APPELLEE,
    APPELLEE_typ
    ) %>%
  mutate(
    DKTDATE = mdy(DKTDATE),
    JUDGDATE =mdy(JUDGDATE),
    yr_file = year(DKTDATE),
    yr_term = year(JUDGDATE)
  ) %>%
  rename(
    "PLT" = "APPELLAN",
    "PLT_typ" = "APPELLAN_typ",
    "DEF" = "APPELLEE",
    "DEF_typ" = "APPELLEE_typ"
  )



# !!NOTE: WE JOIN THESE MANUALLY CODED DATA (fjc_e_da_nas) to the main dataframe
# (fjc_e_da) below

# A [crude] attempt at automated coding of appellate cases ####

# NOW, continue with coding non-na appellant/ee types
# look for appellant/ee words in PLT and DEF
fjc_e_da <- fjc_e_da %>%
  rowwise %>%
  mutate(
    # drop single letter entries
    appellan_list = list(appellan_list[str_length(appellan_list) >= 2]),
    appellee_list = list(appellee_list[str_length(appellee_list) >= 2]),
    # compute similarity matrix for each word in appellant and plaintiff lists;
    # identify perfect matches and also return max similarity score.
    appellan_PLT_sim = list(sim_mat_f(appellan_list,PLT_list)),
    appellan_PLT_pft = case_when(
      1 %in% appellan_PLT_sim == TRUE ~ "YES",
      TRUE ~ "NO"
    ),
    appellan_PLT_max = mean(unlist(appellan_PLT_sim)),
    # compute similarity matrix for each word in appellant and defendant lists;
    # identify perfect matches and also return max similarity score.
    appellan_DEF_sim = list(sim_mat_f(appellan_list,DEF_list)),
    appellan_DEF_pft = case_when(
      1 %in% appellan_DEF_sim == TRUE ~ "YES",
      TRUE ~ "NO"
    ),
    appellan_DEF_max = mean(unlist(appellan_DEF_sim)),
    # compute similarity matrix for each word in appellee and plaintiff lists;
    # identify perfect matches and also return max similarity score.
    appellee_PLT_sim = list(sim_mat_f(appellee_list,PLT_list)),
    appellee_PLT_pft = case_when(
      1 %in% appellee_PLT_sim == TRUE ~ "YES",
      TRUE ~ "NO"
    ),
    appellee_PLT_max = mean(unlist(appellee_PLT_sim)),
    # compute similarity matrix for each word in appellee and defendant lists;
    # identify perfect matches and also return max similarity score.
    appellee_DEF_sim = list(sim_mat_f(appellee_list,DEF_list)),
    appellee_DEF_pft = case_when(
      1 %in% appellee_DEF_sim == TRUE ~ "YES",
      TRUE ~ "NO"
    ),
    appellee_DEF_max = mean(unlist(appellee_DEF_sim))
  )

fjc_e_da <- fjc_e_da %>%
  mutate(
    # identify if PLT or DEF has highest match with appellant
    appellant_PLT_DEF_list = list(c(appellan_PLT_max,appellan_DEF_max)),
    appellant_PLT_DEF_max = max(unlist(appellant_PLT_DEF_list)),
    appellant_PLT_DEF = case_when(
      appellant_PLT_DEF_max == appellan_PLT_max & appellant_PLT_DEF_max != appellan_DEF_max ~ "PLT",
      appellant_PLT_DEF_max == appellan_DEF_max & appellant_PLT_DEF_max != appellan_PLT_max ~ "DEF",
      appellant_PLT_DEF_max == appellan_DEF_max & appellant_PLT_DEF_max == appellan_DEF_max ~ "BOTH",
      TRUE ~ "NEITHER"
    ),
    # identify if PLT or DEF has highest match with appellee
    appellee_PLT_DEF_list = list(c(appellee_PLT_max,appellee_DEF_max)),
    appellee_PLT_DEF_max = max(unlist(appellee_PLT_DEF_list)),
    appellee_PLT_DEF = case_when(
      appellee_PLT_DEF_max == appellee_PLT_max & appellee_PLT_DEF_max != appellee_DEF_max ~ "PLT",
      appellee_PLT_DEF_max == appellee_DEF_max & appellee_PLT_DEF_max != appellee_PLT_max ~ "DEF",
      appellee_PLT_DEF_max == appellee_DEF_max & appellee_PLT_DEF_max == appellee_DEF_max ~ "BOTH",
      TRUE ~ "NEITHER"
    ),
    # now differentiate between strength of appellant and appellee inferences
    # where they are divergent. In particular, when appellant/ee similarity
    # score is much higher than its appellant/ee compliment, keep originally
    # applied code for high scoring part and recode low scoring part to NEITHER.
    appellant_PLT_DEF = case_when(
      appellant_PLT_DEF_max > .32 & appellee_PLT_DEF_max < .2 ~ appellant_PLT_DEF,
      appellant_PLT_DEF_max < .2 & appellee_PLT_DEF_max > .32 ~ "NEITHER",
      TRUE ~ appellant_PLT_DEF
    ),
    appellee_PLT_DEF = case_when(
      appellee_PLT_DEF_max > .32 & appellant_PLT_DEF_max < .2 ~ appellee_PLT_DEF,
      appellee_PLT_DEF_max < .2 & appellant_PLT_DEF_max > .32 ~ "NEITHER",
      TRUE ~ appellee_PLT_DEF
    ),
    # code difference combinations of match between appellant/ee and PLT/DEF
    match_type = case_when(
      # when both appellant and appellee match in opposite ways with PLT and DEF
      (appellant_PLT_DEF == "PLT" & appellee_PLT_DEF == "DEF") |
        (appellant_PLT_DEF == "DEF" & appellee_PLT_DEF == "PLT") ~ "STRONG",
      ((appellant_PLT_DEF == "PLT" | appellant_PLT_DEF == "DEF") & appellee_PLT_DEF == "NEITHER") |
        ((appellee_PLT_DEF == "PLT" | appellee_PLT_DEF == "DEF") & appellant_PLT_DEF == "NEITHER") ~ "MOD",
      ((appellant_PLT_DEF == "PLT" | appellant_PLT_DEF == "DEF") & appellee_PLT_DEF == "BOTH") |
        ((appellee_PLT_DEF == "PLT" | appellee_PLT_DEF == "DEF") & appellant_PLT_DEF == "BOTH") ~ "WEAK",
      TRUE ~ "UNKNOWN"
    ),
    # make sure that 1s (where both are not coded as 1) are coded as STRONG
    match_type = case_when(
      (appellan_PLT_max == 1 & appellan_DEF_max != 1) & appellee_PLT_DEF_max != 1 ~ "STRONG",
      (appellan_DEF_max == 1 & appellan_PLT_max != 1) & appellee_PLT_DEF_max != 1 ~ "STRONG",
      (appellee_PLT_max == 1 & appellee_DEF_max != 1) & appellant_PLT_DEF_max != 1 ~ "STRONG",
      (appellee_DEF_max == 1 & appellee_PLT_max != 1) & appellant_PLT_DEF_max != 1 ~ "STRONG",
      TRUE ~ match_type
    )
  )

# look at matching criteria more closely
fjc_e_a_closeup <- fjc_e_da %>%
  select(
    CDD_ID_A,
    CDD_ID_D,
    PLT_list,
    DEF_list,
    appellan_list,
    appellee_list,
    match_type,
    appellant_PLT_DEF,
    #appellant_PLT_DEF_list,
    appellant_PLT_DEF_max,
    appellee_PLT_DEF,
    #appellee_PLT_DEF_list,
    appellee_PLT_DEF_max,
    #appellan_PLT_pft, appellan_PLT_max,
    #appellan_DEF_pft, appellan_DEF_max,
    #appellee_PLT_pft, appellee_PLT_max,
    #appellee_DEF_pft, appellee_DEF_max,
  )

# After examining, it seems that in the vast majority of UNKNOWNs, whichever
# similarity score is higher among appellant_PLT_DEF_max and
# appellee_PLT_DEF_max correctly predicts the coded party for that group. Use
# this to automate coding of UKNOWN to MOD
fjc_e_da <- fjc_e_da %>%
  mutate(
    max_diff = abs(appellant_PLT_DEF_max - appellee_PLT_DEF_max),
    # recode UNKNOWN instances where appellant_PLT_DEF_max and
    # appellee_PLT_DEF_max are more different than threshold (e.g. 0.05) as "MOD"
    # in updated match_type
     match_type_update = case_when(
      match_type == "UNKNOWN" & max_diff >= .05 ~ "MOD",
      TRUE ~ match_type
      ),
    # for these newly coded "MOD" types, keep PLT/DEF code for whichever
    # appellant_PLT_DEF_max/appellee_PLT_DEF_max is larger; code the smaller one
    # as the opposite.
    appellee_PLT_DEF = case_when(
      match_type == "UNKNOWN" & match_type_update == "MOD" ~ case_when(
        appellant_PLT_DEF_max > appellee_PLT_DEF_max ~ case_when(
          appellant_PLT_DEF == "PLT" ~ "DEF",
          appellant_PLT_DEF == "DEF" ~ "PLT",
          TRUE ~ appellee_PLT_DEF
          ),
        TRUE ~  appellee_PLT_DEF
        ),
      TRUE ~ appellee_PLT_DEF
      ),
    appellant_PLT_DEF = case_when(
      match_type == "UNKNOWN" & match_type_update == "MOD" ~ case_when(
        appellee_PLT_DEF_max > appellant_PLT_DEF_max ~ case_when(
          appellee_PLT_DEF == "PLT" ~ "DEF",
          appellee_PLT_DEF == "DEF" ~ "PLT",
          TRUE ~ appellant_PLT_DEF
          ),
        TRUE ~  appellant_PLT_DEF
        ),
      TRUE ~ appellant_PLT_DEF
      )
    ) %>%
  ungroup()
    

# look at matching criteria more closely (again)
fjc_e_a_closeup <- fjc_e_da %>%
  select(
    CDD_ID_A,
    CDD_ID_D,
    PLT_list,
    DEF_list,
    appellan_list,
    appellee_list,
    match_type,
    match_type_update,
    appellant_PLT_DEF,
    #appellant_PLT_DEF_list,
    appellant_PLT_DEF_max,
    appellee_PLT_DEF,
    #appellee_PLT_DEF_list,
    appellee_PLT_DEF_max,
    max_diff
  )

# recode the recoded unknowns to moderate
fjc_e_da <- fjc_e_da %>%
  mutate(
    match_type = case_when(
      match_type == "UNKNOWN" & match_type_update == "MOD" ~ "MOD",
      TRUE ~ match_type
    )
  )

# there are 118 unknown codes at this point

# Plot distributions of appellant/ee max similarity scores ####

# --> this plot is used to check thresholds (above) for discrepancies between
# high and low similarity scores
fjc_e_da %>%
  ungroup() %>%
  pivot_longer(
    cols = c(appellant_PLT_DEF_max,appellee_PLT_DEF_max),
    names_to = "category",
    values_to = "values"
  ) %>%
  ggplot()+
  geom_density(
    aes(
      x = values,
      fill = category,
      group = category
    ),
    alpha = .4
  ) +
  scale_fill_viridis_d() +
  labs(
    title = "Distribution of highest mean similarity scores between PLT/DEF and Appellant/ee"
  ) +
  theme_linedraw()

# same things as above, faceted by match_type
fjc_e_da %>%
  ungroup() %>%
  pivot_longer(
    cols = c(appellant_PLT_DEF_max,appellee_PLT_DEF_max),
    names_to = "category",
    values_to = "values"
  ) %>%
  ggplot()+
  geom_density(
    aes(
      x = values,
      fill = category,
      group = category
    ),
    alpha = .4
  ) +
  scale_fill_viridis_d() +
  labs(
    title = "Distribution of highest mean similarity scores between PLT/DEF and Appellant/ee",
    subtitle = "by match type"
  ) +
  facet_wrap(
    vars(match_type)
  ) +
  theme_linedraw()

# Clean up appellate dataframe ####
fjc_e_da <- fjc_e_da %>%
  select(
    CDD_ID_A,
    CDD_ID_D,
    CDD_ID_row,
    DISTRICT,
    OFFICE,
    DOFFICE,
    DKTDATE,# from appeals
    DDKTDATE, # from appeals
    JUDGDATE, # from appeals
    FILEDATE,# from district
    TERMDATE,# from district
    JURIS,
    DJURIS,
    DISP,
    JUDGMENT,
    ADISP,
    OUTCOME,
    PLT,
    PLT_typ,
    DEF,
    DEF_typ,
    APPELLAN,
    APPELLEE,
    appellant_PLT_DEF,
    appellee_PLT_DEF,
    match_type,
    match_type_update
    ) %>%
    mutate(
      DKTDATE = mdy(DKTDATE),
      JUDGDATE =mdy(JUDGDATE),
      yr_file = year(DKTDATE),
      yr_term = year(JUDGDATE)
    )

# Code appellate cases that can be coded by type thus far ####


fjc_e_da <- fjc_e_da %>%
  ungroup() %>%
  mutate(
    appellant_PLT_DEF = case_when(
      match_type != "UNKNOWN" ~ case_when(
        appellant_PLT_DEF == "NEITHER" & appellee_PLT_DEF == "PLT" ~ "DEF",
        appellant_PLT_DEF == "NEITHER" & appellee_PLT_DEF == "DEF" ~ "PLT",
        appellant_PLT_DEF == "NEITHER" & appellee_PLT_DEF == "NEITHER" ~ "NEITHER",
        appellant_PLT_DEF == "BOTH" & appellee_PLT_DEF == "PLT" ~ "DEF",
        appellant_PLT_DEF == "BOTH" & appellee_PLT_DEF == "DEF" ~ "PLT",
        appellant_PLT_DEF == "BOTH" & appellee_PLT_DEF == "BOTH" ~ "BOTH",
        TRUE ~ appellant_PLT_DEF
        ),
      TRUE ~ appellant_PLT_DEF
      ),
    appellee_PLT_DEF = case_when(
      match_type != "UNKNOWN" ~ case_when(
        appellee_PLT_DEF == "NEITHER" & appellant_PLT_DEF == "PLT" ~ "DEF",
        appellee_PLT_DEF == "NEITHER" & appellant_PLT_DEF == "DEF" ~ "PLT",
        appellee_PLT_DEF == "NEITHER" & appellant_PLT_DEF == "NEITHER" ~ "NEITHER",
        appellee_PLT_DEF == "BOTH" & appellant_PLT_DEF == "PLT" ~ "DEF",
        appellee_PLT_DEF == "BOTH" & appellant_PLT_DEF == "DEF" ~ "PLT",
        appellee_PLT_DEF == "BOTH" & appellant_PLT_DEF == "BOTH" ~ "BOTH",
        TRUE ~ appellee_PLT_DEF
        ),
      TRUE ~ appellee_PLT_DEF
      ),
    APPELLAN_typ = case_when(
      match_type != "UNKNOWN" ~ case_when(
        appellant_PLT_DEF == "PLT" ~ PLT_typ,
        appellant_PLT_DEF == "DEF" ~ DEF_typ,
        TRUE ~ "UNKNOWN"
      ),
      TRUE ~ "UNKNOWN"
    ),
    APPELLEE_typ = case_when(
      match_type != "UNKNOWN" ~ case_when(
        appellee_PLT_DEF == "PLT" ~ PLT_typ,
        appellee_PLT_DEF == "DEF" ~ DEF_typ,
        TRUE ~ "UNKNOWN"
      ),
      TRUE ~ "UNKNOWN"
    )
  )


# drop unknowns; drop PLT and DEF types; rename appellant/ee types so that they
# align with district coding
fjc_e_da <- fjc_e_da %>%
  filter(
    match_type != "UNKNOWN"
  ) %>%
  # rename DISTRICT-LEVEL PLT and DEF for reference
  rename(
      "DPLT" = "PLT",
      "DDEF" = "DEF",
      "DPLT_typ" = "PLT_typ",
      "DDEF_typ" = "DEF_typ"
  ) %>%
  # rename appellant/ee to PLT/DEF so codes align with district data and, e.g., 
  # heatmap plotting function
  rename(
    "PLT" = "APPELLAN",
    "DEF" = "APPELLEE",
    "PLT_typ" = "APPELLAN_typ",
    "DEF_typ" = "APPELLEE_typ"
  ) %>%
  # take care of above
  # mutate(
  #   DKTDATE = mdy(DKTDATE),
  #   yr_file = year(DKTDATE),
  #   #DISP = 1 # fake disposition for heatmap function
  # ) %>%
  ungroup()



# Join manually coded appellate cases to computationally coded ones ####

# bind manually coded appellate cases to automatically coded df.
fjc_e_da <- bind_rows(fjc_e_da, fjc_e_da_nas)

# Create lists of federal agency heads for coding fed agency heads listed by name ####

# load agency head data to correct for any names that should be coded as FED
agency_heads <- read_csv("Data/Agency_Heads/Fed_heads_and_administrators_1988-2020.csv")

# create single list of names
agency_heads <- unique(str_to_upper(trimws(agency_heads$`Last Name`)))

## add variants of et al to agency head names
agency_heads_et_al <- unlist(lapply(agency_heads,str_c,"ET AL", sep = " "))
agency_heads_etal <- unlist(lapply(agency_heads,str_c,"ETAL", sep = " "))
agency_heads_eta <- unlist(lapply(agency_heads,str_c,"ETA", sep = " "))
agency_heads_et_a <- unlist(lapply(agency_heads,str_c,"ET A", sep = " "))
agency_heads_et <- unlist(lapply(agency_heads,str_c,"ET", sep = " "))
agency_heads_et_al_c <- unlist(lapply(agency_heads,str_c,", ET AL", sep = ""))
agency_heads_etal_c <- unlist(lapply(agency_heads,str_c,", ETAL", sep = ""))
agency_heads_eta_c <- unlist(lapply(agency_heads,str_c,", ETA", sep = ""))
agency_heads_et_a_c <- unlist(lapply(agency_heads,str_c,", ET A", sep = ""))
agency_heads_et_c <- unlist(lapply(agency_heads,str_c,", ET", sep = ""))

# collapse woth OR operator (except agency_heads)
#agency_heads <- paste(agency_heads, collapse='|')
agency_heads_et_al <- paste(agency_heads_et_al, collapse='|')
agency_heads_etal <- paste(agency_heads_etal, collapse='|')
agency_heads_eta <- paste(agency_heads_eta, collapse='|')
agency_heads_et_a <- paste(agency_heads_et_a, collapse='|')
agency_heads_et <- paste(agency_heads_et, collapse='|')
agency_heads_et_al_c <- paste(agency_heads_et_al_c, collapse='|')
agency_heads_etal_c <- paste(agency_heads_etal_c, collapse='|')
agency_heads_eta_c <- paste(agency_heads_eta_c, collapse='|')
agency_heads_et_a_c <- paste(agency_heads_et_a_c, collapse='|')
agency_heads_et_c <- paste(agency_heads_et_c, collapse='|')

# Create state name supporting lists ####

# list of state names
state_names <- c("Alabama", "Alaska", "American Samoa", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Minor Outlying Islands", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "U.S. Virgin Islands", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
# make all caps
state_names <- str_to_upper(state_names)

# list of state names - lower case
state_names_lower <- c("Alabama", "Alaska", "American Samoa", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Minor Outlying Islands", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "U.S. Virgin Islands", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

# list of state abbreviations
state_abbr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# append "DEP" to state names and abbreviations, for identifying state
# departments of X.
state_name_dep <- unlist(lapply(state_names,
                         str_c,
                         " DEP",
                         sep = ""))

state_abbr_dep <- unlist(lapply(state_abbr,
                         str_c,
                         " DEP",
                         sep = ""))

# state abbr with "ST" and "DEP"
state_abbr_st_dep <- unlist(lapply(state_abbr,
                         str_c,
                         "ST DEP",
                         sep = ""))

# turn vectors of values into single "OR" statements
state_name_dep <- paste(state_name_dep, collapse = "|")
state_name_lower_orlist <- paste(state_names_lower, collapse = "|")
state_abbr_dep <- paste(state_abbr_dep, collapse = "|")
state_abbr_st_dep <- paste(state_abbr_st_dep, collapse = "|")


# Make corrections to appellate coding ####

# Note: in essence, this "corrections" work recodes all the data.

fjc_e_da <- fjc_e_da %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    # first-pass type corrections
    PLT_typ = case_when(
      str_detect(PLT, "WILD") == T ~ "NGO",
      TRUE ~ PLT_typ
    ),
    # second pass type corrections
    PLT_typ = case_when(
      str_detect(PLT, "STATES FOREST") == T ~ "FED",
      str_detect(PLT, "U.S. FOREST") == T ~ "FED",
      str_detect(PLT, "NATIONAL OCEANIC") == T ~ "FED",
      str_detect(PLT, "UNITED STATES FISH") == T ~ "FED",
      str_detect(PLT, "U.S. FISH AND WILD") == T ~ "FED",
      str_detect(PLT, "MARINE FISH") == T ~ "FED",
      str_detect(PLT, "EPA ") == T ~ "FED",
      str_detect(PLT, "U.S. ENVIRO") == T ~ "FED",
      str_detect(PLT, "UNITED STATES ENV") == T ~ "FED",
      str_detect(PLT, "ARMY COR") == T ~ "FED",
      str_detect(PLT, "BUREAU") == T ~ "FED",
      str_detect(PLT, "STATE OF") == T ~ "STA",
      str_detect(PLT, "USA ET") == T ~ "FED",
      PLT == "USA" ~ "FED",
      PLT == "USA" ~ "EPA",
      PLT == "USA" ~ "E.P.A.",
      str_detect(PLT, "STATES OF") == T ~ "FED",
      str_detect(PLT, "DEPT") == T ~ "FED",
      str_detect(PLT, "SIERRA C") == T ~ "NGO",
      str_detect(PLT, "SIERRA ET") == T ~ "NGO",
      str_detect(PLT, "DEFENDERS") == T ~ "NGO",
      str_detect(PLT, "NATURAL R") == T ~ "NGO",
      str_detect(PLT, "NATURAL ET") == T ~ "NGO",
      str_detect(PLT, "OREGON NATURAL D") == T ~ "NGO",
      TRUE ~ PLT_typ
    ),
    # agency heads check
    PLT_typ = case_when(
      PLT %in% agency_heads == TRUE ~ "FED",
      str_detect(PLT, agency_heads_et_al) == TRUE ~ "FED",
      str_detect(PLT, agency_heads_etal) == TRUE ~ "FED",
      str_detect(PLT, agency_heads_eta) == TRUE ~ "FED",
      str_detect(PLT, agency_heads_et_a) == TRUE ~ "FED",
      str_detect(PLT, agency_heads_et) == TRUE ~ "FED",
      str_detect(PLT, agency_heads_et_al_c) == TRUE ~ "FED",
      str_detect(PLT, agency_heads_etal_c) == TRUE ~ "FED",
      str_detect(PLT, agency_heads_eta_c) == TRUE ~ "FED",
      str_detect(PLT, agency_heads_et_a_c) == TRUE ~ "FED",
      str_detect(PLT, agency_heads_et_c) == TRUE ~ "FED",
      TRUE ~ PLT_typ
    ),
    # third pass (taken from district-level coding process)
    PLT_typ = case_when(
      PLT == "ALON USA ENERGY" ~ "BIZ",
      PLT == "CHEVRON USA IN" ~ "BIZ",
      PLT == "GLENN-COLUSA IRRIGAT" ~ "LOC",
      PLT == "KOOCANUSA INTER COALITION" ~ "CIVIC",
      PLT == "NESTLE USA BEVERAGE, ET AL" ~ "BIZ",
      PLT == "NEWMONT USA LIMITED" ~ "BIZ",
      PLT == "TEHAMA COLUSA CANAL AUT, ET AL" ~ "LOC",
      PLT == "USA ENVIRONMENTAL PROTECTION A" ~ "FED",
      PLT == "CHEVRON U.S.A." ~ "BIZ",
      PLT == "DRYCLEAN U.S.A." ~ "BIZ",
      PLT == "GREENPEACE U.S.A., ET AL" ~ "NGO",
      PLT == "SUNCOR ENERGY (U.S.A.) , ET AL" ~ "BIZ",
      PLT == "U.S.A. ENVIRONMENTAL PROTECTIO" ~ "FED",
      PLT == "U.S. POSTAL SERVICE, ET AL" ~ "PUB_ORG",
      # specific type corrections
      PLT_typ == "LOCAL" ~ "LOC",
      # specific name corrections
      # these come from plaintiffs
      PLT == "AMUSSEN  JOAN AND GREGORY ETL" ~ "IND",
      PLT == "BARGEN" ~ "IND",
      PLT == "CALIF  STATE" ~ "STA",
      PLT == "COALITION TO LIMIT  UNIVERSITY" ~ "CIVIC",
      PLT == "HARLEY" ~ "IND",
      PLT == "MCMULLEN  ETL" ~ "IND",
      PLT == "NEW MANCHESTER RESORT  & GOLF," ~ "BIZ",
      PLT == "TOLEDO, CITY  OF" ~ "LOC",
      PLT == "UNITED STATES POSTAL SERVICE" ~ "PUB_ORG",
      PLT == "UNITED STATES POSTAL" ~ "PUB_ORG",
      PLT == "UNITED STATES POS" ~ "PUB_ORG",
      PLT == "UNITED STATES P. S." ~ "PUB_ORG",
      PLT == "UNKNOWN PLAINTIFF" ~ "OTHER",
      PLT == "SEE ATT PAGES" ~ "OTHER",
      # these come from defendant corrections
      PLT == "SEALED" ~ "OTHER",
      PLT == "DC" ~ "OTHER",
      PLT == "RICAHARD OGLE, ET AL, ET AL" ~ "IND",
      PLT == "UNION SCRAP IRON&META  ETAL" ~ "BIZ",
      PLT == "ALL-OUT  SEWER AND DRAIN SERVI" ~ "BIZ",
      # random fixes
      PLT == "BIOKYOWA,INC" ~ "BIZ",
      PLT == "ATLANTIC  STATES  LEGAL" ~ "NGO",
      PLT == "ATLANTIC  RICHFIELD" ~ "BIZ",
      PLT == "STATE  OF TEXAS" ~ "STA",
      TRUE ~ PLT_typ
    ),
    # fourth stage fixes (taken from district-level coding process)
    PLT_typ = case_when(
      PLT_typ == "FED" & str_detect(PLT," INC") == TRUE ~ "BIZ",
      PLT_typ == "FED" & str_detect(PLT,"CORP") == TRUE 
      & str_detect(PLT,"ARMY") == FALSE & str_detect(PLT,"CORPS") == FALSE
      & str_detect(PLT,"CORP OF") == FALSE ~ "BIZ",
      str_detect(PLT, state_name_dep)  == TRUE ~ "STA",
      str_detect(PLT, state_abbr_dep)  == TRUE ~ "STA",
      str_detect(PLT, state_abbr_st_dep)  == TRUE ~ "STA",
      TRUE ~ PLT_typ
    ),
    # fifth stage (taken from district-level coding process)
    PLT_typ = case_when(
      # plaintiff types coded as FED that should be coded as other things
      str_detect(PLT, "-8")  == TRUE ~ "UNKNOWN",
      str_detect(PLT, "HOME DEPOT")  == TRUE ~ "BIZ", # this gets categorized as STA because contains "ME DEP"
      str_detect(PLT, "ALASKA, STATES OF, DEPT OF TRA") == TRUE ~ "STA",
      str_detect(PLT, "BAY INSTITUTE") == TRUE ~ "NGO",
      str_detect(PLT, "CALIFORNIA NATIVE PLANT SOCIET") == TRUE ~ "NGO",
      str_detect(PLT, "CYPRUS WESTERN COAL") == TRUE ~ "NGO",
      str_detect(PLT, "DAMASCUS CITIZENS FOR S, ET AL") == TRUE ~ "NGO",
      str_detect(PLT, "DEPT OF TOXIC SUBS") == TRUE ~ "STA",
      str_detect(PLT, "E.I. DU PONT DE NEMOURS") == TRUE ~ "BIZ",
      str_detect(PLT, "E I DUPONT DENEMOURS") == TRUE ~ "BIZ",
      str_detect(PLT, "EI DUPONT DE NEMOURS") == TRUE ~ "BIZ",
      str_detect(PLT, "GREENPEACE U.S.A.") == TRUE ~ "NGO",
      str_detect(PLT, "HIHIWAI STREAM RESTO") == TRUE ~ "NGO",
      str_detect(PLT, "HONEY IS SWAMP TOURS") == TRUE ~ "BIZ",
      PLT_typ == "FED" & str_detect(PLT, "LOUISIANA") == TRUE ~ "NGO",
      str_detect(PLT, "LOUISIANA CRAWFISH PROD") == TRUE ~ "BIZ",
      str_detect(PLT, "HLOUISIANA ENVIRONMEN") == TRUE ~ "NGO",
      str_detect(PLT, "MERISOL USA LLC") == TRUE ~ "BIZ",
      str_detect(PLT, "NATIONAL PARKS") == TRUE ~ "NGO",
      str_detect(PLT, "NATURAL RES. D.C.,") == TRUE ~ "NGO",
      str_detect(PLT, "NEPA COALITION,") == TRUE ~ "NGO",
      str_detect(PLT, "NO PARTY") == TRUE ~ "UNKNOWN",
      str_detect(PLT, "NORTH CASCADES GRIZZ") == TRUE ~ "NGO",
      str_detect(PLT, "OPTIMUS STEEL, LLC") == TRUE ~ "BIZ",
      str_detect(PLT, "ROBERT W. HALL VS U.S. DEPARTM") == TRUE ~ "IND",
      str_detect(PLT, "RUSSO DEVELOPMENT") == TRUE ~ "BIZ",
      str_detect(PLT, "VOYAGEURS NATL PARK") == TRUE ~ "NGO",
      TRUE ~ PLT_typ
    ),
    # sixth stage (taken from district-level coding process)
    PLT_typ = case_when(
      # defendant types coded as FED that should be coded as other things
      str_detect(PLT, "-8")  == TRUE ~ "UNKNOWN",
      str_detect(PLT, "ACQUEST TRANSIT LLC, ET AL")  == TRUE ~ "BIZ",
      str_detect(PLT, "ACQUEST WEHRLE, LLC, ET AL") == TRUE ~ "BIZ",
      str_detect(PLT, "ARCELORMITTAL USA LLC, ET AL") == TRUE ~ "BIZ",
      str_detect(PLT, "AZ TRANS DEPT OF, ET AL") == TRUE ~ "STA",
      str_detect(PLT, "AZUSA PIPE AND TUBE BENDING CO") == TRUE ~ "BIZ",
      str_detect(PLT, "CHEVRON USA PROD CO") == TRUE ~ "BIZ",
      str_detect(PLT, "CHURCHILL DOWNS LOUISIANA HORS") == TRUE ~ "BIZ",
      str_detect(PLT, "COLUMBUS MCKINNON CO") == TRUE ~ "BIZ",
      str_detect(PLT, "CRAIN POWER TUCUMCARTI") == TRUE ~ "BIZ",
      str_detect(PLT, "CYPRUS AMAX MINERALS") == TRUE ~ "BIZ",
      str_detect(PLT, "CYPRUS BAGDAD COPPER") == TRUE ~ "BIZ",
      str_detect(PLT, "CYPRUS MIAMI MINING") == TRUE ~ "BIZ",
      str_detect(PLT, "DEGUSSA INITIATORS") == TRUE ~ "BIZ",
      str_detect(PLT, "E I DUPONT DENEMOURS, ET AL") == TRUE ~ "BIZ",
      str_detect(PLT, "E. I. DU PONT DE NEMOURS AND C") == TRUE ~ "BIZ",
      str_detect(PLT, "E.I. DU PONT DE NEMOURS, ET AL") == TRUE ~ "BIZ",
      str_detect(PLT, "E.I. DU PONT DE NEMOURS AND CO") == TRUE ~ "BIZ",
      str_detect(PLT, "E.I. DUPONT DE NEMOURS AND COM") == TRUE ~ "BIZ",
      str_detect(PLT, "EI DUPONT DE NEMOURS, ET AL") == TRUE ~ "BIZ",
      str_detect(PLT, "ELSA SKINNER MORGAN, ET AL") == TRUE ~ "BIZ",
      str_detect(PLT, "QUISTAR CHEMICALS, LP.") == TRUE ~ "BIZ",
      str_detect(PLT, "EXXON COMPANY U S A") == TRUE ~ "BIZ",
      str_detect(PLT, "GERDAU SPECIALTY STEEL, N.A.,") == TRUE ~ "BIZ",
      str_detect(PLT, "GLOUCESTER ENV MA") == TRUE ~ "LOC",
      str_detect(PLT, "GNB INDUS BATTERY CO") == TRUE ~ "BIZ",
      str_detect(PLT, "HAILI CHRISTIAN SCHOOL") == TRUE ~ "NGO",
      str_detect(PLT, "IDAHO TRANSPORT DEPT, ET AL") == TRUE ~ "BIZ",
      str_detect(PLT, "INEOS USA LLC") == TRUE ~ "BIZ",
      str_detect(PLT, "INTERSTATE NON, ET AL") == TRUE ~ "BIZ",
      str_detect(PLT, "JOINT MEETING OF ESSEX AND") == TRUE ~ "STA",
      str_detect(PLT, "KELCOURSE ET AL") == TRUE ~ "IND",
      str_detect(PLT, "LOUISIANA GENERATING, L, ET AL") == TRUE ~ "BIZ",
      str_detect(PLT, "LOUISIANA MIDLAND TRANSPORT CO") == TRUE ~ "BIZ",
      str_detect(PLT, "LOUISIANA ONSHORE PROPERTIES L") == TRUE ~ "BIZ",
      str_detect(PLT, "LOUISIANA-PACIFIC") == TRUE ~ "BIZ",
      str_detect(PLT, "MT ST DEPT ENVIRON, ET AL") == TRUE ~ "STA",
      str_detect(PLT, "N Y STATE DEPT OF TRANS") == TRUE ~ "STA",
      str_detect(PLT, "QUESTAR GAS MANAGEMENT, ET AL") == TRUE ~ "BIZ",
      str_detect(PLT, "SHINTECH LOUISIANA LLC") == TRUE ~ "BIZ",
      str_detect(PLT, "SOUTHEASTERN UTAH OHV CLUB") == TRUE ~ "NGO",
      str_detect(PLT, "U S CERAMIC TILE CO") == TRUE ~ "BIZ",
      str_detect(PLT, "VERTELLUS AGRICULTURE & NUTRIT") == TRUE ~ "BIZ",
      str_detect(PLT, "WASHINGTON STATE DEPARTMENT OF") == TRUE ~ "STA",
      TRUE ~ PLT_typ
    ),
    # seventh stage corrections (taken from district-level coding process)
    PLT_typ = case_when(
      PLT == "SMITH" ~ "IND",
      PLT == "TRUMP, ET AL" ~ "FED",
      str_detect(PLT, "CONSER")  == TRUE ~ "NGO",
      str_detect(PLT, "NATURAL RESOURCES DEF")  == TRUE ~ "NGO",
      str_detect(PLT, "NATURAL RES DEF COU")  == TRUE ~ "NGO",
      str_detect(PLT, "NATURAL RESOURCES COUN")  == TRUE ~ "NGO",
      str_detect(PLT, "POLLUTION COALITION")  == TRUE ~ "NGO",
      str_detect(PLT, "COALI")  == TRUE ~ "NGO",
      str_detect(PLT, "BLASKE MARINE")  == TRUE ~ "BIZ",
      str_detect(PLT, "LEGAL")  == TRUE ~ "NGO",
      str_detect(PLT, "KEEPER")  == TRUE ~ "NGO",
      str_detect(PLT, "LEAGU")  == TRUE ~ "NGO",
      str_detect(PLT, "MINE CO")  == TRUE ~ "BIZ",
      str_detect(PLT, "COMMUNITIES FOR")  == TRUE ~ "CIVC",
      str_detect(PLT, "LOUISIANA, ET AL")  == TRUE ~ "FED",
      str_detect(PLT, " BANK")  == TRUE ~ "BIZ",
      str_detect(PLT, "COUNCIL ON")  == TRUE ~ "FED",
      str_detect(PLT, "COUNCIL")  == TRUE & str_detect(PLT, "ENV")  == TRUE ~ "NGO",
      str_detect(PLT, "IRRITAT")  == TRUE ~ "CIVIC",
      str_detect(PLT, "UNITED STATES  OF AMERICA")  == TRUE ~ "FED",
      str_detect(PLT, "RIVERS UNLIMITED")  == TRUE ~ "NGO",
      str_detect(PLT, "PORTNEUF ENVIRONMENT")  == TRUE ~ "NGO",
      str_detect(PLT, "VUI")  == TRUE ~ "BIZ",
      str_detect(PLT, "ANGLERS CONSERVATION")  == TRUE ~ "NGO",
      str_detect(PLT, "ANGLERS OF THE")  == TRUE ~ "NGO",
      str_detect(PLT, "WATERSHED")  == TRUE ~ "NGO",
      str_detect(PLT, "LAW FO")  == TRUE ~ "NGO",
      str_detect(PLT, "EAST MEETS WEST EXCURSI")  == TRUE ~ "BIZ",
      str_detect(PLT, "LAW FO")  == TRUE ~ "NGO",
      str_detect(PLT, "GAUTIER FAMILY SPORTS")  == TRUE ~ "BIZ",
      str_detect(PLT, "COLLIER, ET AL")  == TRUE ~ "BIZ",
      str_detect(PLT, "PROTECTION COMM")  == TRUE ~ "NGO",
      TRUE ~ PLT_typ
      ),
    # first-pass type corrections foe DEF
    DEF_typ = case_when(
      str_detect(DEF, "WILD") == T ~ "NGO",
      TRUE ~ DEF_typ
    ),
    # second pass type corrections
    DEF_typ = case_when(
      str_detect(DEF, "STATES FOREST") == T ~ "FED",
      str_detect(DEF, "U.S. FOREST") == T ~ "FED",
      str_detect(DEF, "NATIONAL OCEANIC") == T ~ "FED",
      str_detect(DEF, "UNITED STATES FISH") == T ~ "FED",
      str_detect(DEF, "U.S. FISH AND WILD") == T ~ "FED",
      str_detect(DEF, "MARINE FISH") == T ~ "FED",
      str_detect(DEF, "EPA ") == T ~ "FED",
      str_detect(DEF, "U.S. ENVIRO") == T ~ "FED",
      str_detect(DEF, "UNITED STATES ENV") == T ~ "FED",
      str_detect(DEF, "ARMY COR") == T ~ "FED",
      str_detect(DEF, "BUREAU") == T ~ "FED",
      str_detect(DEF, "STATE OF") == T ~ "STA",
      str_detect(DEF, "USA ET") == T ~ "FED",
      DEF == "USA" ~ "FED",
      DEF == "USA" ~ "EPA",
      DEF == "USA" ~ "E.P.A.",
      str_detect(DEF, "STATES OF") == T ~ "FED",
      str_detect(DEF, "DEPT") == T ~ "FED",
      str_detect(DEF, "SIERRA C") == T ~ "NGO",
      str_detect(DEF, "SIERRA ET") == T ~ "NGO",
      str_detect(DEF, "DEFENDERS") == T ~ "NGO",
      str_detect(DEF, "NATURAL R") == T ~ "NGO",
      str_detect(DEF, "NATURAL ET") == T ~ "NGO",
      str_detect(DEF, "OREGON NATURAL D") == T ~ "NGO",
      TRUE ~ DEF_typ
    ),
    # agency heads check
    DEF_typ = case_when(
      DEF %in% agency_heads == TRUE ~ "FED",
      str_detect(DEF, agency_heads_et_al) == TRUE ~ "FED",
      str_detect(DEF, agency_heads_etal) == TRUE ~ "FED",
      str_detect(DEF, agency_heads_eta) == TRUE ~ "FED",
      str_detect(DEF, agency_heads_et_a) == TRUE ~ "FED",
      str_detect(DEF, agency_heads_et) == TRUE ~ "FED",
      str_detect(DEF, agency_heads_et_al_c) == TRUE ~ "FED",
      str_detect(DEF, agency_heads_etal_c) == TRUE ~ "FED",
      str_detect(DEF, agency_heads_eta_c) == TRUE ~ "FED",
      str_detect(DEF, agency_heads_et_a_c) == TRUE ~ "FED",
      str_detect(DEF, agency_heads_et_c) == TRUE ~ "FED",
      TRUE ~ DEF_typ
    ),
    # third pass (taken from district-level coding process)
    DEF_typ = case_when(
      DEF == "ALON USA ENERGY" ~ "BIZ",
      DEF == "CHEVRON USA IN" ~ "BIZ",
      DEF == "GLENN-COLUSA IRRIGAT" ~ "LOC",
      DEF == "KOOCANUSA INTER COALITION" ~ "CIVIC",
      DEF == "NESTLE USA BEVERAGE, ET AL" ~ "BIZ",
      DEF == "NEWMONT USA LIMITED" ~ "BIZ",
      DEF == "TEHAMA COLUSA CANAL AUT, ET AL" ~ "LOC",
      DEF == "USA ENVIRONMENTAL PROTECTION A" ~ "FED",
      DEF == "CHEVRON U.S.A." ~ "BIZ",
      DEF == "DRYCLEAN U.S.A." ~ "BIZ",
      DEF == "GREENPEACE U.S.A., ET AL" ~ "NGO",
      DEF == "SUNCOR ENERGY (U.S.A.) , ET AL" ~ "BIZ",
      DEF == "U.S.A. ENVIRONMENTAL PROTECTIO" ~ "FED",
      DEF == "U.S. POSTAL SERVICE, ET AL" ~ "PUB_ORG",
      # specific type corrections
      DEF_typ == "LOCAL" ~ "LOC",
      # specific name corrections
      # these come from plaintiffs
      DEF == "AMUSSEN  JOAN AND GREGORY ETL" ~ "IND",
      DEF == "BARGEN" ~ "IND",
      DEF == "CALIF  STATE" ~ "STA",
      DEF == "COALITION TO LIMIT  UNIVERSITY" ~ "CIVIC",
      DEF == "HARLEY" ~ "IND",
      DEF == "MCMULLEN  ETL" ~ "IND",
      DEF == "NEW MANCHESTER RESORT  & GOLF," ~ "BIZ",
      DEF == "TOLEDO, CITY  OF" ~ "LOC",
      DEF == "UNITED STATES POSTAL SERVICE" ~ "PUB_ORG",
      DEF == "UNITED STATES POSTAL" ~ "PUB_ORG",
      DEF == "UNITED STATES POS" ~ "PUB_ORG",
      DEF == "UNITED STATES P. S." ~ "PUB_ORG",
      DEF == "UNKNOWN PLAINTIFF" ~ "OTHER",
      DEF == "SEE ATT PAGES" ~ "OTHER",
      # these come from defendant corrections
      DEF == "SEALED" ~ "OTHER",
      DEF == "DC" ~ "OTHER",
      DEF == "RICAHARD OGLE, ET AL, ET AL" ~ "IND",
      DEF == "UNION SCRAP IRON&META  ETAL" ~ "BIZ",
      DEF == "ALL-OUT  SEWER AND DRAIN SERVI" ~ "BIZ",
      # random fixes
      DEF == "BIOKYOWA,INC" ~ "BIZ",
      DEF == "ATLANTIC  STATES  LEGAL" ~ "NGO",
      DEF == "ATLANTIC  RICHFIELD" ~ "BIZ",
      DEF == "STATE  OF TEXAS" ~ "STA",
      TRUE ~ DEF_typ
    ),
    # fourth stage fixes (taken from district-level coding process)
    DEF_typ = case_when(
      DEF_typ == "FED" & str_detect(DEF," INC") == TRUE ~ "BIZ",
      DEF_typ == "FED" & str_detect(DEF,"CORP") == TRUE 
      & str_detect(DEF,"ARMY") == FALSE & str_detect(DEF,"CORPS") == FALSE
      & str_detect(DEF,"CORP OF") == FALSE ~ "BIZ",
      str_detect(DEF, state_name_dep)  == TRUE ~ "STA",
      str_detect(DEF, state_abbr_dep)  == TRUE ~ "STA",
      str_detect(DEF, state_abbr_st_dep)  == TRUE ~ "STA",
      TRUE ~ DEF_typ
    ),
    # fifth stage (taken from district-level coding process)
    DEF_typ = case_when(
      # plaintiff types coded as FED that should be coded as other things
      str_detect(DEF, "-8")  == TRUE ~ "UNKNOWN",
      str_detect(DEF, "HOME DEPOT")  == TRUE ~ "BIZ", # this gets categorized as STA because contains "ME DEP"
      str_detect(DEF, "ALASKA, STATES OF, DEPT OF TRA") == TRUE ~ "STA",
      str_detect(DEF, "BAY INSTITUTE") == TRUE ~ "NGO",
      str_detect(DEF, "CALIFORNIA NATIVE PLANT SOCIET") == TRUE ~ "NGO",
      str_detect(DEF, "CYPRUS WESTERN COAL") == TRUE ~ "NGO",
      str_detect(DEF, "DAMASCUS CITIZENS FOR S, ET AL") == TRUE ~ "NGO",
      str_detect(DEF, "DEPT OF TOXIC SUBS") == TRUE ~ "STA",
      str_detect(DEF, "E.I. DU PONT DE NEMOURS") == TRUE ~ "BIZ",
      str_detect(DEF, "E I DUPONT DENEMOURS") == TRUE ~ "BIZ",
      str_detect(DEF, "EI DUPONT DE NEMOURS") == TRUE ~ "BIZ",
      str_detect(DEF, "GREENPEACE U.S.A.") == TRUE ~ "NGO",
      str_detect(DEF, "HIHIWAI STREAM RESTO") == TRUE ~ "NGO",
      str_detect(DEF, "HONEY IS SWAMP TOURS") == TRUE ~ "BIZ",
      DEF_typ == "FED" & str_detect(DEF, "LOUISIANA") == TRUE ~ "NGO",
      str_detect(DEF, "LOUISIANA CRAWFISH PROD") == TRUE ~ "BIZ",
      str_detect(DEF, "HLOUISIANA ENVIRONMEN") == TRUE ~ "NGO",
      str_detect(DEF, "MERISOL USA LLC") == TRUE ~ "BIZ",
      str_detect(DEF, "NATIONAL PARKS") == TRUE ~ "NGO",
      str_detect(DEF, "NATURAL RES. D.C.,") == TRUE ~ "NGO",
      str_detect(DEF, "NEPA COALITION,") == TRUE ~ "NGO",
      str_detect(DEF, "NO PARTY") == TRUE ~ "UNKNOWN",
      str_detect(DEF, "NORTH CASCADES GRIZZ") == TRUE ~ "NGO",
      str_detect(DEF, "OPTIMUS STEEL, LLC") == TRUE ~ "BIZ",
      str_detect(DEF, "ROBERT W. HALL VS U.S. DEPARTM") == TRUE ~ "IND",
      str_detect(DEF, "RUSSO DEVELOPMENT") == TRUE ~ "BIZ",
      str_detect(DEF, "VOYAGEURS NATL PARK") == TRUE ~ "NGO",
      TRUE ~ DEF_typ
    ),
    # sixth stage (taken from district-level coding process)
    DEF_typ = case_when(
      # defendant types coded as FED that should be coded as other things
      str_detect(DEF, "-8")  == TRUE ~ "UNKNOWN",
      str_detect(DEF, "ACQUEST TRANSIT LLC, ET AL")  == TRUE ~ "BIZ",
      str_detect(DEF, "ACQUEST WEHRLE, LLC, ET AL") == TRUE ~ "BIZ",
      str_detect(DEF, "ARCELORMITTAL USA LLC, ET AL") == TRUE ~ "BIZ",
      str_detect(DEF, "AZ TRANS DEPT OF, ET AL") == TRUE ~ "STA",
      str_detect(DEF, "AZUSA PIPE AND TUBE BENDING CO") == TRUE ~ "BIZ",
      str_detect(DEF, "CHEVRON USA PROD CO") == TRUE ~ "BIZ",
      str_detect(DEF, "CHURCHILL DOWNS LOUISIANA HORS") == TRUE ~ "BIZ",
      str_detect(DEF, "COLUMBUS MCKINNON CO") == TRUE ~ "BIZ",
      str_detect(DEF, "CRAIN POWER TUCUMCARTI") == TRUE ~ "BIZ",
      str_detect(DEF, "CYPRUS AMAX MINERALS") == TRUE ~ "BIZ",
      str_detect(DEF, "CYPRUS BAGDAD COPPER") == TRUE ~ "BIZ",
      str_detect(DEF, "CYPRUS MIAMI MINING") == TRUE ~ "BIZ",
      str_detect(DEF, "DEGUSSA INITIATORS") == TRUE ~ "BIZ",
      str_detect(DEF, "E I DUPONT DENEMOURS, ET AL") == TRUE ~ "BIZ",
      str_detect(DEF, "E. I. DU PONT DE NEMOURS AND C") == TRUE ~ "BIZ",
      str_detect(DEF, "E.I. DU PONT DE NEMOURS, ET AL") == TRUE ~ "BIZ",
      str_detect(DEF, "E.I. DU PONT DE NEMOURS AND CO") == TRUE ~ "BIZ",
      str_detect(DEF, "E.I. DUPONT DE NEMOURS AND COM") == TRUE ~ "BIZ",
      str_detect(DEF, "EI DUPONT DE NEMOURS, ET AL") == TRUE ~ "BIZ",
      str_detect(DEF, "ELSA SKINNER MORGAN, ET AL") == TRUE ~ "BIZ",
      str_detect(DEF, "QUISTAR CHEMICALS, LP.") == TRUE ~ "BIZ",
      str_detect(DEF, "EXXON COMPANY U S A") == TRUE ~ "BIZ",
      str_detect(DEF, "GERDAU SPECIALTY STEEL, N.A.,") == TRUE ~ "BIZ",
      str_detect(DEF, "GLOUCESTER ENV MA") == TRUE ~ "LOC",
      str_detect(DEF, "GNB INDUS BATTERY CO") == TRUE ~ "BIZ",
      str_detect(DEF, "HAILI CHRISTIAN SCHOOL") == TRUE ~ "NGO",
      str_detect(DEF, "IDAHO TRANSPORT DEPT, ET AL") == TRUE ~ "BIZ",
      str_detect(DEF, "INEOS USA LLC") == TRUE ~ "BIZ",
      str_detect(DEF, "INTERSTATE NON, ET AL") == TRUE ~ "BIZ",
      str_detect(DEF, "JOINT MEETING OF ESSEX AND") == TRUE ~ "STA",
      str_detect(DEF, "KELCOURSE ET AL") == TRUE ~ "IND",
      str_detect(DEF, "LOUISIANA GENERATING, L, ET AL") == TRUE ~ "BIZ",
      str_detect(DEF, "LOUISIANA MIDLAND TRANSPORT CO") == TRUE ~ "BIZ",
      str_detect(DEF, "LOUISIANA ONSHORE PROPERTIES L") == TRUE ~ "BIZ",
      str_detect(DEF, "LOUISIANA-PACIFIC") == TRUE ~ "BIZ",
      str_detect(DEF, "MT ST DEPT ENVIRON, ET AL") == TRUE ~ "STA",
      str_detect(DEF, "N Y STATE DEPT OF TRANS") == TRUE ~ "STA",
      str_detect(DEF, "QUESTAR GAS MANAGEMENT, ET AL") == TRUE ~ "BIZ",
      str_detect(DEF, "SHINTECH LOUISIANA LLC") == TRUE ~ "BIZ",
      str_detect(DEF, "SOUTHEASTERN UTAH OHV CLUB") == TRUE ~ "NGO",
      str_detect(DEF, "U S CERAMIC TILE CO") == TRUE ~ "BIZ",
      str_detect(DEF, "VERTELLUS AGRICULTURE & NUTRIT") == TRUE ~ "BIZ",
      str_detect(DEF, "WASHINGTON STATE DEPARTMENT OF") == TRUE ~ "STA",
      TRUE ~ DEF_typ
    ),
    # seventh stage corrections (taken from district-level coding process)
    DEF_typ = case_when(
      DEF == "SMITH" ~ "IND",
      DEF == "TRUMP, ET AL" ~ "FED",
      str_detect(DEF, "CONSER")  == TRUE ~ "NGO",
      str_detect(DEF, "NATURAL RESOURCES DEF")  == TRUE ~ "NGO",
      str_detect(DEF, "NATURAL RES DEF COU")  == TRUE ~ "NGO",
      str_detect(DEF, "NATURAL RESOURCES COUN")  == TRUE ~ "NGO",
      str_detect(DEF, "POLLUTION COALITION")  == TRUE ~ "NGO",
      str_detect(DEF, "COALI")  == TRUE ~ "NGO",
      str_detect(DEF, "BLASKE MARINE")  == TRUE ~ "BIZ",
      str_detect(DEF, "LEGAL")  == TRUE ~ "NGO",
      str_detect(DEF, "KEEPER")  == TRUE ~ "NGO",
      str_detect(DEF, "LEAGU")  == TRUE ~ "NGO",
      str_detect(DEF, "MINE CO")  == TRUE ~ "BIZ",
      str_detect(DEF, "COMMUNITIES FOR")  == TRUE ~ "CIVC",
      str_detect(DEF, "LOUISIANA, ET AL")  == TRUE ~ "FED",
      str_detect(DEF, " BANK")  == TRUE ~ "BIZ",
      str_detect(DEF, "COUNCIL ON")  == TRUE ~ "FED",
      str_detect(DEF, "COUNCIL")  == TRUE & str_detect(DEF, "ENV")  == TRUE ~ "NGO",
      str_detect(DEF, "IRRITAT")  == TRUE ~ "CIVIC",
      str_detect(DEF, "UNITED STATES  OF AMERICA")  == TRUE ~ "FED",
      str_detect(DEF, "RIVERS UNLIMITED")  == TRUE ~ "NGO",
      str_detect(DEF, "PORTNEUF ENVIRONMENT")  == TRUE ~ "NGO",
      str_detect(DEF, "VUI")  == TRUE ~ "BIZ",
      str_detect(DEF, "ANGLERS CONSERVATION")  == TRUE ~ "NGO",
      str_detect(DEF, "ANGLERS OF THE")  == TRUE ~ "NGO",
      str_detect(DEF, "WATERSHED")  == TRUE ~ "NGO",
      str_detect(DEF, "LAW FO")  == TRUE ~ "NGO",
      str_detect(DEF, "EAST MEETS WEST EXCURSI")  == TRUE ~ "BIZ",
      str_detect(DEF, "LAW FO")  == TRUE ~ "NGO",
      str_detect(DEF, "GAUTIER FAMILY SPORTS")  == TRUE ~ "BIZ",
      str_detect(DEF, "COLLIER, ET AL")  == TRUE ~ "BIZ",
      str_detect(DEF, "PROTECTION COMM")  == TRUE ~ "NGO",
      TRUE ~ DEF_typ
    )
  )

# Manually recode remaining unknown plaintiffs ####

# create unique ID for appellate cases
fjc_e_da <- fjc_e_da %>%
  ungroup() %>%
  arrange(CDD_ID_A,PLT, DEF) %>%
  group_by(CDD_ID_A) %>%
  mutate(
    row_add = row_number(),
    ID = str_c(
      CDD_ID_A, row_add, sep = "-"
    )
  ) %>%
  ungroup()

# write out all cases to manually inspect and recode remaining cases manually
write_csv(
  fjc_e_da,
  "Data/FJC_postprocessed/Appellate/appellate_cases_for_manual_coding_of_unknown_litigant_types.csv"
)

# read manually recoded cases; keep only ID and plt and def type.
fjc_e_da_coded <- read_csv(
  "Data/FJC_postprocessed/Appellate/appellate_cases_for_manual_coding_of_unknown_litigant_types_CODED.csv"
  ) %>%
  select(
    ID, PLT_typ, DEF_typ
  )

# rename PLT_typ and DEF_typ in orginal data; join re-coded data
fjc_e_da <- left_join(
  fjc_e_da %>%
    rename(
      "PLT_typ_old" = "PLT_typ",
      "DEF_typ_old" = "DEF_typ"
      ),
  fjc_e_da_coded,
  by = "ID"
  )


# look at cases with unknown appellant and appellee types
fjc_e_da_nas2 <- fjc_e_da %>%
  filter(
    is.na(PLT_typ) |
      is.na(DEF_typ)
  )

# try to match to PD_typ_dic - first plaintiffs...
fjc_e_da_nas2 <- left_join(
  fjc_e_da_nas2,
  PD_type_dic %>% rename(
    "PLT" = "Litigant",
    "PLT_typ_new" = "l_typ"
    ),
  by = "PLT"
)

# ... now defendants
fjc_e_da_nas2 <- left_join(
  fjc_e_da_nas2,
  PD_type_dic %>% rename(
    "DEF" = "Litigant",
    "DEF_typ_new" = "l_typ"
    ),
  by = "DEF"
)

# sub-in new codes from dictionary; drop added codes variable
fjc_e_da_nas2 <- fjc_e_da_nas2 %>%
  mutate(
    PLT_typ = case_when(
      is.na(PLT_typ_new) == F & is.na(PLT_typ) == F ~ PLT_typ_new,
      is.na(PLT_typ_new) == F & is.na(PLT_typ) == T ~ PLT_typ_new,
      is.na(PLT_typ_new) == T & is.na(PLT_typ) == F ~ PLT_typ,
      is.na(PLT_typ_new) == T & is.na(PLT_typ) == T ~ PLT_typ,
      TRUE ~ PLT_typ
    ),
    DEF_typ = case_when(
      is.na(DEF_typ_new) == F & is.na(DEF_typ) == F ~ DEF_typ_new,
      is.na(DEF_typ_new) == F & is.na(DEF_typ) == T ~ DEF_typ_new,
      is.na(DEF_typ_new) == T & is.na(DEF_typ) == F ~ DEF_typ,
      is.na(DEF_typ_new) == T & is.na(DEF_typ) == T ~ DEF_typ,
      TRUE ~ DEF_typ
    )
  ) %>%
  select(
    -c(PLT_typ_new, DEF_typ_new)
  )

# keep only formerly NA cases with both PLT and DEF; drop the rest. The NA cases
# have docket numbers that do not match to the district-level data; this calls
# into question whether they were originally litigated in the set of district
# cases. Thus, we drop them.
fjc_e_da_nas2 <- fjc_e_da_nas2 %>%
  filter(
    is.na(PLT_typ) == F & is.na(DEF_typ) == F
  )

# drop NAs from original df; bind handful of newly coded cases
fjc_e_da <- fjc_e_da %>%
  filter(
    is.na(PLT_typ) == F & is.na(DEF_typ) == F
  ) %>%
  bind_rows(
    fjc_e_da_nas2
  )


# Get district crosswalk linking court districts to census regions ####

# this crosswalk also offers test-names for districts

# read back in coded crosswalk; join to shape file so that shape file can be
# joined to FJC data
dist_crosswalk <- read_csv("Data/Crosswalks/district_crosswalk_coded.csv") %>%
  mutate(
    FJC_dist_code = str_pad(FJC_dist_code,2,side = "left", pad = 0)
  ) %>%
  rename(
    "DISTRICT" = "FJC_dist_code"
  ) %>%
  mutate(
    DISTRICT = str_pad(DISTRICT,2,side = "left", pad = 0)
  )

# Final df formatting ####



fjc_e_da <- fjc_e_da %>%
#test <- fjc_e_da %>%
  mutate(
    jud_or_set = case_when(
      # first, restrict set of cases to those DISTRICT cases that are settled or reach judgement
      DISP %in% c(2,3,12,13,14,4,5,6,7,8,9,15,16,17,18,19) ~ 1, #&
      # then, keep only APPEAKS cases that also terminate 
      #ADISP %in% c(1,2,3) ~ 1, # based on appeals codebook
      TRUE ~ 0
    )
  ) %>%
  # join geographic (district, region) info
  left_join(
    dist_crosswalk, by = "DISTRICT"
  ) %>%
  # now we need to define the appellate outcomes in terms of the district-level
  # outcomes.
  mutate(
    # PLT (appellant) wins include:
    # - all appellate cases where district-level decision in reversed OR affirmed
    #  in part and reversed in part.
    # PLT (appellant) loses include:
    # - all appellate cases where the district-level decision is affirmed OR
    #   appellate-level cases is dismissed
    PLT_wl = case_when(
      OUTCOME %in% c(2,3,6) ~ "w",
      OUTCOME %in% c(1,5) ~ "l"
      ),
  # DEF (appellee) wins include:
  # - all appellate cases where the district-level decision is affirmed OR
  #   appellate-level cases is dismissed OR affirmed in part and reversed in
  #   part.
  # DEF (appellee) loses include:
  # - all appellate cases where district-level decision in reversed 
  DEF_wl = case_when(
    OUTCOME %in% c(1,3,5) ~ "w",
    OUTCOME %in% c(2,6) ~ "l"
    )
  ) %>%
  # rename date-related variables to allow use of dist_df function to build data frame
  rename(
    "DFILEDATE" = "FILEDATE",
    "DTERMDATE" = "TERMDATE",
    # in appellate date, make docket date the file date; make the judgement date the term date.
    "FILEDATE" = "DKTDATE",
    "TERMDATE" = "JUDGDATE",
    # rename district-level JUDGMENT as DJUDGMENT;
    # OUTCOME (from appellate) as JUDGMENT to match coding in district-level data
    "DJUDGMENT" = "JUDGMENT",
    "JUDGMENT" = "OUTCOME"
  )

# remove unnecessary data frames
rm(fjc_e_da_dups,
   fjc_e_da_nas,
   fjc_e_da_nas_CODED,
   fjc_e_da_coded,
   fjc_e_a_closeup,
   fjc_e_a_match,
   fjc_e_a_match_overlap,
   fjc_e_a_nomatch,
   fjc_e_d_match,
   fjc_e_d_match_overlap,
   fjc_e_d_nomatch,
   fjc_e_da_na,
   fjc_e_da_nas2
   )


# the end.
