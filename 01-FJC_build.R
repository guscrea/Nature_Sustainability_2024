# Compile FJC Data
# Script by Chris Rea
# Late Updated April 16, 2024

# Notes ####

# This script reads in and compiles the raw Federal Judicial Center Integrated
# Database civil and appellate cases and also includes code to plot overall
# trends in fjc data.

# These data come the the Federal Judaical Center Integrated Database (FJC IDB).
# You can find the data and supporting materials here:
# https://www.fjc.gov/research/idb. We're using the civil data, which include
# federal district court cases but not appellate cases.

# For information about variables mean (e.g. what "NOS = 893" means), see the
# "Civil Codebook" in the github repository. We'll focus on 1988 forward, so you
# only need to look at that codebook for now.

# Load packages #####

library(tidyverse)
library(patchwork)

# Read in data ######

# !! NOTE: the specific data used to run this script can all be downloaded from
# the Harvard Dataverse:
# https://dataverse.harvard.edu/dataverse/rea_merten_rife_2024_NS
# The script won't run unmodified until these data have been added to the paths
# below. 

# load civil data (1970-1987)
f = "Data/FJC_raw/Civil 1970 to 1987.txt"
fjc_old <- read_delim(f, delim = "\t", col_names = T)

# load civil data (1988-2023)
f = "Data/FJC_raw/cv88_23.txt"
fjc <- read_delim(f, delim = "\t", col_names = T)

# examine test row to make sure read-in was clean
test <- fjc %>%
  filter(
    DISTRICT == "48",
    DOCKET == "0000256"
  )

rm(test)

# !! NOTE: same as above. These data can all be downloaded from the Harvard
# Dataverse:
# https://dataverse.harvard.edu/dataverse/rea_merten_rife_2024_NS
# The script won't run unmodified until these data have been added to the paths
# below. 

# load appellate data (1971-2007)
f = "Data/FJC_raw/ap71to07.txt"
fjc_a_old <- read_delim(f, delim = "\t", col_names = T)

# load appellate data (2008-2022)
f = "Data/FJC_raw/ap08on.txt"
fjc_a <- read_delim(f, delim = "\t", col_names = T)

# Basic formatting to ensure consistency across data sources ####

# examine names of variables in dfs
names(fjc)
names(fjc_old)
names(fjc_a_old)
names(fjc_a)

# make variable formats compatible across newer and older data; add variable
# indicating old or new data
fjc <- fjc %>%
  mutate(
    FDATEUSE = mdy(FDATEUSE),
    FILEDATE = mdy(FILEDATE),
    TERMDATE = mdy(TERMDATE),
    TDATEUSE = mdy(TDATEUSE),
    yr_term = year(TERMDATE),
    yr_file = year(FILEDATE),
    # variable indicating 1988 and later
    new = 1
  )

fjc_old <- fjc_old %>%
  mutate(
    FDATEUSE =  ym(FDATEUSE),
    FILEDATE = mdy(FILEDATE),
    TERMDATE = mdy(TERMDATE),
    TDATEUSE = ym(TDATEUSE),
    OFFICE = as.character(OFFICE),
    yr_term = year(TERMDATE),
    yr_file = year(FILEDATE),
    # variable indicating 1988 and later
    new = 0
  )

fjc_a <- fjc_a %>%
  mutate(
    FILEDATE = mdy(DKTDATE),
    TERMDATE = mdy(JUDGDATE),
    REOPEN = as.character(REOPEN),
    OFFENSE = as.character(OFFENSE),
    DOFFICE = as.character(DOFFICE),
    PROSEFLE = as.character(PROSEFLE),
    PROSETRM = as.character(PROSETRM),
    DDKTDATE = mdy(DDKTDATE),
    yr_term = year(TERMDATE),
    yr_file = year(FILEDATE),
    # variable indicating 2008 and later
    new = 1
  )

fjc_a_old <- fjc_a_old %>%
  mutate(
    FILEDATE = mdy(DKTDATE),
    TERMDATE = mdy(JUDGDATE),
    REOPEN = as.character(REOPEN),
    OFFENSE = as.character(OFFENSE),
    DOFFICE = as.character(DOFFICE),
    PROSEFLE = as.character(PROSEFLE),
    PROSETRM = as.character(PROSETRM),
    DDKTDATE = mdy(DDKTDATE),
    yr_term = year(TERMDATE),
    yr_file = year(FILEDATE),
    # variable indicating 2008 and later
    new = 0
  )

# filter appeals data to only include civil and administrative appeals
# administrative appeals: APPTYPE == 1 or 2
# civil appeals: APPTYPE == 3, 4, or 7

fjc_a <- fjc_a %>%
  filter(
    APPTYPE %in% c(1,2,3,7)
  )

fjc_a_old <- fjc_a_old %>%
  filter(
    APPTYPE %in% c(1,2,3,7)
  )


# examine NOS code (old and new)
# NOS = "Nature of Suit". Note that there is one NOS codes of special
# importance: "Environmental Matters" (NOS = 893) See "Civil Nature of Suit Code
# Descriptions" issued by federal judicial center for more details.

NOS <- fjc %>%
  group_by(
    NOS
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    NOS
  ) %>%
  arrange(NOS) %>%
  mutate(
    new = "new"
  ) %>%
  ungroup()

NOS_old <- fjc_old %>%
  group_by(
    NOS
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    NOS
  ) %>%
  arrange(NOS) %>%
  mutate(
    old = "old"
  ) %>%
  ungroup()

# full join - to look for missing codes
NOS <- full_join(NOS,NOS_old, by = "NOS")
# make df of just codes present in one data source, but not other:
NOS_mismatch <- NOS %>%
  filter(
    is.na(new) == T | is.na(old) == T
  )
# Results notes: some mismatches across old and new data sets. For example, 100
# is present in old, but not in new. 196 is present in new, but not in old. And
# so on. Most NOS codes are present in both data sets. 893 (environmental
# matters) is present in new and old. In total, 21 NOS codes are in the new data
# but not the old; 30 NOS codes are in old but not new. Could work on mapping
# old-to-new to combine data [not completed as of 2024-04-16].

# after examination remove NOS
rm(NOS, NOS_old, NOS_mismatch)

# Parallel examination for appellate data
NOS <- fjc_a %>%
  group_by(
    NOS
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    NOS
  ) %>%
  arrange(NOS) %>%
  mutate(
    new = "new"
  )

NOS_old <- fjc_a_old %>%
  group_by(
    NOS
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    NOS
  ) %>%
  arrange(NOS) %>%
  mutate(
    old = "old"
  )

# full join - to look for missing codes
NOS <- full_join(NOS,NOS_old, by = "NOS")
NOS_mismatch <- NOS %>%
  filter(
    is.na(new) == T | is.na(old) == T
  )
# Results after inspection: some mismatches across old and new data sets. For
# example, 100 is present in old, but not in new. 196 is present in new, but not
# in old. And so on. Most NOS codes are present in both data sets. 893
# (environmental matters) is present in new and old. In total, 11 NOS codes are
# in the new data but not the old; 50 NOS codes are in old but not new. Could
# work on mapping old-to-new to combine data [not completed as of 2024-04-16].

# remove NOS
rm(NOS, NOS_old, NOS_mismatch)

# bind old and new civil data together
fjc <- bind_rows(fjc,fjc_old)

# remove old only
rm(fjc_old)

# bind old and new appellate data together
fjc_a <- bind_rows(fjc_a,fjc_a_old)

# remove old only
rm(fjc_a_old)

# make unique ID for fjc cases
# WARNING: this is a slow call
fjc <- fjc %>%
  mutate(
    fjc_ID = str_c(
      CIRCUIT,DISTRICT,DOCKET,FILEDATE,sep = "-"
    ),
    CDD_ID = str_c(
      CIRCUIT,DISTRICT,DOCKET,sep = "-"
      ),
    #resl_ID = NA_character_ # add resl_ID var (blank for now)
  ) %>%
  group_by(
    fjc_ID
  ) %>%
  mutate(
    # add row numbers for any non-uniue IDs
    fjc_ID = str_c(fjc_ID,row_number(),sep = "-")
  ) %>%
  ungroup()

# make unique ID for fjc appellate cases
fjc_a <- fjc_a %>%
  mutate(
    fjc_ID = str_c(
      DCIRC,DDIST,DDOCKET,FILEDATE,sep = "-"
    ),
    CDD_ID = str_c(
      DCIRC,DDIST,DDOCKET,sep = "-"
      ),
    #resl_ID = NA_character_ # add resl_ID var (blank for now)
  ) %>%
  group_by(
    fjc_ID
  ) %>%
  mutate(
    # add row numbers for any non-uniue IDs
    fjc_ID = str_c(fjc_ID,row_number(),sep = "-")
  ) %>%
  ungroup()

# get a sense of CDD_IDs in fjc appellate data (sample 25 rows to)
fjc_CDD_IDs <- fjc_a %>%
  select(
    CDD_ID,DDOCKET
  ) %>%
  filter(
    str_length(DDOCKET) >= 6
  ) %>%
  sample_n(25)

fjc_CDD_IDs <- as.vector(fjc_CDD_IDs$CDD_ID)


ID = "3-12-1917580"

test_a <- fjc_a %>%
  filter(
    CDD_ID == ID
  )
test <- fjc %>%
  filter(
    CDD_ID == ID
  )
  
# make sure the number of rows here = number of rows in fjc
# test <- fjc %>%
#   group_by(
#     fjc_ID
#   ) %>%
#   select(
#     fjc_ID
#   ) %>%
#   filter(
#     row_number() == 1
#   )

# remove tests
rm(test, test_a)


# Add counts of cases and human-readable codes for NOS and DISP ######

# Note: this entails adjusting for changes in disposition codes pre-1988. See
# FJC documentation.

# district data
fjc <- fjc %>%
  # create count of total cases per year (by filing date)
  group_by(
    yr_file
  ) %>%
  mutate(
    cases_tot_yr = n()
  ) %>%
  # create count by cases per year by NOS type (by filing date)
  group_by(
    yr_file, NOS
  ) %>%
  mutate(
    cases_tot_typ_yr = n(),
    # express cases as fraction of total
    cases_pct_tot_yr = round((cases_tot_typ_yr/cases_tot_yr)*100,3)
  ) %>%
  # create count of total cases per year (by termination date)
  group_by(
    yr_term
  ) %>%
  mutate(
    cases_tot_yr_term = n()
  ) %>%
  ungroup() %>%
  # apply meaningful "nature of suit" codes according to 1988-later data
  # --> note: this presents challenges for codes that were not present
  #     prior to 1988.
  mutate(
    NOS_t = case_when(
      NOS == 110 ~ "INSURANCE",
      NOS == 120 ~ "MARINE CONTRACT ACTIONS",
      NOS == 130 ~ "MILLER ACT",
      NOS == 140 ~ "NEGOTIABLE INSTRUMENTS",
      NOS == 150 ~ "OVERPAYMENTS & ENFORCEMENT OF JUDGMENTS",
      NOS == 151 ~ "OVERPAYMENTS UNDER THE MEDICARE ACT",
      NOS == 152 ~ "RECOVERY OF DEFAULTED STUDENT LOANS",
      NOS == 153 ~ "RECOVERY OF OVERPAYMENTS OF VET BENEFITS",
      NOS == 160 ~ "STOCKHOLDER'S SUITS",
      NOS == 190 ~ "OTHER CONTRACT ACTIONS",
      NOS == 195 ~ "CONTRACT PRODUCT LIABILITY",
      NOS == 196 ~ "CONTRACT FRANCHISE",
      NOS == 210 ~ "LAND CONDEMNATION",
      NOS == 220 ~ "FORECLOSURE",
      NOS == 230 ~ "RENT, LEASE, EJECTMENT",
      NOS == 240 ~ "TORTS TO LAND",
      NOS == 245 ~ "TORT PRODUCT LIABILITY",
      NOS == 290 ~ "OTHER REAL PROPERTY ACTIONS",
      NOS == 310 ~ "AIRPLANE PERSONAL INJURY",
      NOS == 315 ~ "AIRPLANE PRODUCT LIABILITY",
      NOS == 320 ~ "ASSAULT, LIBEL, AND SLANDER",
      NOS == 330 ~ "FEDERAL EMPLOYERS' LIABILITY",
      NOS == 340 ~ "MARINE PERSONAL INJURY",
      NOS == 345 ~ "MARINE - PRODUCT LIABILITY",
      NOS == 350 ~ "MOTOR VEHICLE PERSONAL INJURY",
      NOS == 355 ~ "MOTOR VEHICLE PRODUCT LIABILITY",
      NOS == 360 ~ "OTHER PERSONAL INJURY",
      NOS == 362 ~ "MEDICAL MALPRACTICE",
      NOS == 365 ~ "PERSONAL INJURY -PRODUCT LIABILITY",
      NOS == 367 ~ "HEALTH CARE / PHARM",
      NOS == 368 ~ "ASBESTOS PERSONAL INJURY - PROD.LIAB.",
      NOS == 370 ~ "OTHER FRAUD",
      NOS == 371 ~ "TRUTH IN LENDING",
      NOS == 375 ~ "FALSE CLAIMS ACT",
      NOS == 380 ~ "OTHER PERSONAL PROPERTY DAMAGE",
      NOS == 385 ~ "PROPERTY DAMAGE - PRODUCT LIABILTY",
      NOS == 400 ~ "STATE RE-APPORTIONMENT",
      NOS == 410 ~ "ANTITRUST",
      NOS == 422 ~ "BANKRUPTCY APPEALS RULE 28 USC 158",
      NOS == 423 ~ "BANKRUPTCY WITHDRAWAL 28 USC 157",
      NOS == 430 ~ "BANKS AND BANKING",
      NOS == 440 ~ "OTHER CIVIL RIGHTS",
      NOS == 441 ~ "CIVIL RIGHTS VOTING",
      NOS == 442 ~ "CIVIL RIGHTS JOBS",
      NOS == 443 ~ "CIVIL RIGHTS ACCOMMODATIONS",
      NOS == 444 ~ "CIVIL RIGHTS WELFARE",
      NOS == 445 ~ "CIVIL RIGHTS ADA EMPLOYMENT",
      NOS == 446 ~ "CIVIL RIGHTS ADA OTHER",
      NOS == 448 ~ "EDUCATION",
      NOS == 450 ~ "INTERSTATE COMMERCE",
      NOS == 460 ~ "DEPORTATION",
      NOS == 462 ~ "NATURALIZATION, PETITION FOR HEARING OF DENIAL",
      NOS == 463 ~ "HABEAS CORPUS – ALIEN DETAINEE",
      NOS == 465 ~ "OTHER IMMIGRATION ACTIONS",
      NOS == 470 ~ "CIVIL (RICO)",
      NOS == 480 ~ "CONSUMER CREDIT",
      NOS == 490 ~ "CABLE/SATELLITE TV",
      NOS == 510 ~ "PRISONER PETITIONS -VACATE SENTENCE",
      NOS == 530 ~ "PRISONER PETITIONS -HABEAS CORPUS",
      NOS == 535 ~ "HABEAS CORPUS: DEATH PENALTY",
      NOS == 540 ~ "PRISONER PETITIONS -MANDAMUS AND OTHER",
      NOS == 550 ~ "PRISONER -CIVIL RIGHTS",
      NOS == 555 ~ "PRISONER - PRISON CONDITION",
      NOS == 560 ~ "CIVIL DETAINEE",
      NOS == 610 ~ "AGRICULTURAL ACTS",
      NOS == 620 ~ "FOOD AND DRUG ACTS",
      NOS == 625 ~ "DRUG RELATED SEIZURE OF PROPERTY",
      NOS == 630 ~ "LIQUOR LAWS",
      NOS == 640 ~ "RAILROAD AND TRUCKS",
      NOS == 650 ~ "AIRLINE REGULATIONS",
      NOS == 660 ~ "OCCUPATIONAL SAFETY/HEALTH",
      NOS == 690 ~ "OTHER FORFEITURE AND PENALTY SUITS",
      NOS == 710 ~ "FAIR LABOR STANDARDS ACT",
      NOS == 720 ~ "LABOR/MANAGEMENT RELATIONS ACT",
      NOS == 730 ~ "LABOR/MANAGEMENT REPORT & DISCLOSURE",
      NOS == 740 ~ "RAILWAY LABOR ACT",
      NOS == 751 ~ "FAMILY AND MEDICAL LEAVE ACT",
      NOS == 790 ~ "OTHER LABOR LITIGATION",
      NOS == 791 ~ "EMPLOYEE RETIREMENT INCOME SECURITY ACT",
      NOS == 810 ~ "SELECTIVE SERVICE",
      NOS == 820 ~ "COPYRIGHT",
      NOS == 830 ~ "PATENT",
      NOS == 840 ~ "TRADEMARK",
      NOS == 850 ~ "SECURITIES, COMMODITIES, EXCHANGE",
      NOS == 860 ~ "SOCIAL SECURITY",
      NOS == 861 ~ "HIA (1395 FF)/ MEDICARE",
      NOS == 862 ~ "BLACK LUNG",
      NOS == 863 ~ "D.I.W.C./D.I.W.W.",
      NOS == 864 ~ "S.S.I.D.",
      NOS == 865 ~ "R.S.I.",
      NOS == 870 ~ "TAX SUITS",
      NOS == 871 ~ "IRS 3RD PARTY SUITS 26 USC 7609",
      NOS == 875 ~ "CUSTOMER CHALLENGE 12 USC 3410",
      NOS == 890 ~ "OTHER STATUTORY ACTIONS",
      NOS == 891 ~ "AGRICULTURAL ACTS",
      NOS == 892 ~ "ECONOMIC STABILIZATION ACT",
      NOS == 893 ~ "ENVIRONMENTAL MATTERS",
      NOS == 894 ~ "ENERGY ALLOCATION ACT",
      NOS == 895 ~ "FREEDOM OF INFORMATION ACT OF 1974",
      NOS == 896 ~ "ARBITRATION",
      NOS == 899 ~ "ADMINISTRATIVE PROCEDURE ACT/REVIEW OR APPEAL OF AGENCY DECISION",
      NOS == 900 ~ "APPEAL OF FEE -EQUAL ACCESS TO JUSTICE",
      NOS == 910 ~ "DOMESTIC RELATIONS",
      NOS == 920 ~ "INSANITY",
      NOS == 930 ~ "PROBATE",
      NOS == 940 ~ "SUBSTITUTE TRUSTEE",
      NOS == 950 ~ "CONSTITUTIONALITY OF STATE STATUTES",
      NOS == 990 ~ "OTHER",
      NOS == 992 ~ "LOCAL JURISDICTIONAL APPEAL",
      NOS == 999 ~ "MISCELLANEOUS",
      TRUE ~ "UNKNOWN"
    ),
    # code disposition
    # Note: these are stable codes from 1988 onward. But the coding of case disposition varies in earlier years.
    # From 1970-1978:
    DISP_t = case_when(
        yr_term <= 1978 ~ case_when(
          DISP == 0 ~ "Transferd or Remanded",
          DISP == 1 ~ "Judgement Issued", #"Default Judgement"
          DISP == 2 ~ "Settled (Consent Judgement)",
          DISP == 3 ~ "Dismissed (want of pros. or lack of juris.)",
          DISP == 4 ~ "Settled", # "Dismissed by action of parties" --> includes voluntary dismissals
          DISP == 5 ~ "Transferd or Remanded", #"Remanded to state court"
          DISP == 6 | DISP == 7 | DISP == 8 ~ "Judgement Issued",
          DISP == 9 | DISP == -9 | DISP == -8 | DISP == 10 ~ "Unknown"
        ),
        # from 1979 -1986 (except four pilot districts below)
        yr_term >=1979 & yr_term <= 1986 ~ case_when(
          DISP == 0 | DISP == 1 ~ "Transferd or Remanded",
          DISP == 2 ~ "Dismissed (want of pros. or lack of juris.)",
          DISP == 3 ~ "Settled", # "Dismissed, discontinued, settled, withdrawn, etc." --> includes voluntary dismissals
          DISP == 4 ~ "Judgement Issued", #"Default Judgement"
          DISP == 5 ~ "Settled (Consent Judgement)",
          DISP == 11 ~ "Unknown", # "Statistical closing"
          TRUE ~ "Judgement Issued"
        ),
        # for 
        ((yr_term == 1986 ) & (DISTRICT == "13" | DISTRICT == "39" | DISTRICT == "3A" | DISTRICT == "66")) |
          yr_term >= 1987 ~ case_when(
            DISP == 0 | DISP == 1 | DISP == 10  | DISP == 11 ~ "Transferd or Remanded",
            DISP == 2 | DISP == 3  | DISP == 14 ~ "Dismissed (want of pros. or lack of juris.)",
            DISP == 5 ~ "Settled (Consent Judgement)",
            DISP == 12 ~ "Dismissed Voluntarily",
            DISP == 13 ~ "Settled",
            DISP == 18 ~ "Unknown", # "#Statistical closing"
            DISP == -8 ~ "Unknown",
            TRUE ~ "Judgement Issued"
            )
        )
    )

# appellate data
fjc_a <- fjc_a %>%
  # create count of total cases per year (by filing date)
  group_by(
    yr_file
  ) %>%
  mutate(
    cases_tot_yr = n()
  ) %>%
  # create count by cases per year by NOS type (by filing date)
  group_by(
    yr_file, NOS
  ) %>%
  mutate(
    cases_tot_typ_yr = n(),
    # express cases as fraction of total
    cases_pct_tot_yr = round((cases_tot_typ_yr/cases_tot_yr)*100,3)
  ) %>%
  # create count of total cases per year (by termination date)
  group_by(
    yr_term
  ) %>%
  mutate(
    cases_tot_yr_term = n()
  ) %>%
  ungroup() %>%
  # apply meaningful "nature of suit" codes according to 1988-later data
  # --> note: this presents challenges for codes that were not present
  #     prior to 1988.
  mutate(
    NOS_t = case_when(
      NOS == 110 ~ "INSURANCE",
      NOS == 120 ~ "MARINE CONTRACT ACTIONS",
      NOS == 130 ~ "MILLER ACT",
      NOS == 140 ~ "NEGOTIABLE INSTRUMENTS",
      NOS == 150 ~ "OVERPAYMENTS & ENFORCEMENT OF JUDGMENTS",
      NOS == 151 ~ "OVERPAYMENTS UNDER THE MEDICARE ACT",
      NOS == 152 ~ "RECOVERY OF DEFAULTED STUDENT LOANS",
      NOS == 153 ~ "RECOVERY OF OVERPAYMENTS OF VET BENEFITS",
      NOS == 160 ~ "STOCKHOLDER'S SUITS",
      NOS == 190 ~ "OTHER CONTRACT ACTIONS",
      NOS == 195 ~ "CONTRACT PRODUCT LIABILITY",
      NOS == 196 ~ "CONTRACT FRANCHISE",
      NOS == 210 ~ "LAND CONDEMNATION",
      NOS == 220 ~ "FORECLOSURE",
      NOS == 230 ~ "RENT, LEASE, EJECTMENT",
      NOS == 240 ~ "TORTS TO LAND",
      NOS == 245 ~ "TORT PRODUCT LIABILITY",
      NOS == 290 ~ "OTHER REAL PROPERTY ACTIONS",
      NOS == 310 ~ "AIRPLANE PERSONAL INJURY",
      NOS == 315 ~ "AIRPLANE PRODUCT LIABILITY",
      NOS == 320 ~ "ASSAULT, LIBEL, AND SLANDER",
      NOS == 330 ~ "FEDERAL EMPLOYERS' LIABILITY",
      NOS == 340 ~ "MARINE PERSONAL INJURY",
      NOS == 345 ~ "MARINE - PRODUCT LIABILITY",
      NOS == 350 ~ "MOTOR VEHICLE PERSONAL INJURY",
      NOS == 355 ~ "MOTOR VEHICLE PRODUCT LIABILITY",
      NOS == 360 ~ "OTHER PERSONAL INJURY",
      NOS == 362 ~ "MEDICAL MALPRACTICE",
      NOS == 365 ~ "PERSONAL INJURY -PRODUCT LIABILITY",
      NOS == 367 ~ "HEALTH CARE / PHARM",
      NOS == 368 ~ "ASBESTOS PERSONAL INJURY - PROD.LIAB.",
      NOS == 370 ~ "OTHER FRAUD",
      NOS == 371 ~ "TRUTH IN LENDING",
      NOS == 375 ~ "FALSE CLAIMS ACT",
      NOS == 380 ~ "OTHER PERSONAL PROPERTY DAMAGE",
      NOS == 385 ~ "PROPERTY DAMAGE - PRODUCT LIABILTY",
      NOS == 400 ~ "STATE RE-APPORTIONMENT",
      NOS == 410 ~ "ANTITRUST",
      NOS == 422 ~ "BANKRUPTCY APPEALS RULE 28 USC 158",
      NOS == 423 ~ "BANKRUPTCY WITHDRAWAL 28 USC 157",
      NOS == 430 ~ "BANKS AND BANKING",
      NOS == 440 ~ "OTHER CIVIL RIGHTS",
      NOS == 441 ~ "CIVIL RIGHTS VOTING",
      NOS == 442 ~ "CIVIL RIGHTS JOBS",
      NOS == 443 ~ "CIVIL RIGHTS ACCOMMODATIONS",
      NOS == 444 ~ "CIVIL RIGHTS WELFARE",
      NOS == 445 ~ "CIVIL RIGHTS ADA EMPLOYMENT",
      NOS == 446 ~ "CIVIL RIGHTS ADA OTHER",
      NOS == 448 ~ "EDUCATION",
      NOS == 450 ~ "INTERSTATE COMMERCE",
      NOS == 460 ~ "DEPORTATION",
      NOS == 462 ~ "NATURALIZATION, PETITION FOR HEARING OF DENIAL",
      NOS == 463 ~ "HABEAS CORPUS – ALIEN DETAINEE",
      NOS == 465 ~ "OTHER IMMIGRATION ACTIONS",
      NOS == 470 ~ "CIVIL (RICO)",
      NOS == 480 ~ "CONSUMER CREDIT",
      NOS == 490 ~ "CABLE/SATELLITE TV",
      NOS == 510 ~ "PRISONER PETITIONS -VACATE SENTENCE",
      NOS == 530 ~ "PRISONER PETITIONS -HABEAS CORPUS",
      NOS == 535 ~ "HABEAS CORPUS: DEATH PENALTY",
      NOS == 540 ~ "PRISONER PETITIONS -MANDAMUS AND OTHER",
      NOS == 550 ~ "PRISONER -CIVIL RIGHTS",
      NOS == 555 ~ "PRISONER - PRISON CONDITION",
      NOS == 560 ~ "CIVIL DETAINEE",
      NOS == 610 ~ "AGRICULTURAL ACTS",
      NOS == 620 ~ "FOOD AND DRUG ACTS",
      NOS == 625 ~ "DRUG RELATED SEIZURE OF PROPERTY",
      NOS == 630 ~ "LIQUOR LAWS",
      NOS == 640 ~ "RAILROAD AND TRUCKS",
      NOS == 650 ~ "AIRLINE REGULATIONS",
      NOS == 660 ~ "OCCUPATIONAL SAFETY/HEALTH",
      NOS == 690 ~ "OTHER FORFEITURE AND PENALTY SUITS",
      NOS == 710 ~ "FAIR LABOR STANDARDS ACT",
      NOS == 720 ~ "LABOR/MANAGEMENT RELATIONS ACT",
      NOS == 730 ~ "LABOR/MANAGEMENT REPORT & DISCLOSURE",
      NOS == 740 ~ "RAILWAY LABOR ACT",
      NOS == 751 ~ "FAMILY AND MEDICAL LEAVE ACT",
      NOS == 790 ~ "OTHER LABOR LITIGATION",
      NOS == 791 ~ "EMPLOYEE RETIREMENT INCOME SECURITY ACT",
      NOS == 810 ~ "SELECTIVE SERVICE",
      NOS == 820 ~ "COPYRIGHT",
      NOS == 830 ~ "PATENT",
      NOS == 840 ~ "TRADEMARK",
      NOS == 850 ~ "SECURITIES, COMMODITIES, EXCHANGE",
      NOS == 860 ~ "SOCIAL SECURITY",
      NOS == 861 ~ "HIA (1395 FF)/ MEDICARE",
      NOS == 862 ~ "BLACK LUNG",
      NOS == 863 ~ "D.I.W.C./D.I.W.W.",
      NOS == 864 ~ "S.S.I.D.",
      NOS == 865 ~ "R.S.I.",
      NOS == 870 ~ "TAX SUITS",
      NOS == 871 ~ "IRS 3RD PARTY SUITS 26 USC 7609",
      NOS == 875 ~ "CUSTOMER CHALLENGE 12 USC 3410",
      NOS == 890 ~ "OTHER STATUTORY ACTIONS",
      NOS == 891 ~ "AGRICULTURAL ACTS",
      NOS == 892 ~ "ECONOMIC STABILIZATION ACT",
      NOS == 893 ~ "ENVIRONMENTAL MATTERS",
      NOS == 894 ~ "ENERGY ALLOCATION ACT",
      NOS == 895 ~ "FREEDOM OF INFORMATION ACT OF 1974",
      NOS == 896 ~ "ARBITRATION",
      NOS == 899 ~ "ADMINISTRATIVE PROCEDURE ACT/REVIEW OR APPEAL OF AGENCY DECISION",
      NOS == 900 ~ "APPEAL OF FEE -EQUAL ACCESS TO JUSTICE",
      NOS == 910 ~ "DOMESTIC RELATIONS",
      NOS == 920 ~ "INSANITY",
      NOS == 930 ~ "PROBATE",
      NOS == 940 ~ "SUBSTITUTE TRUSTEE",
      NOS == 950 ~ "CONSTITUTIONALITY OF STATE STATUTES",
      NOS == 990 ~ "OTHER",
      NOS == 992 ~ "LOCAL JURISDICTIONAL APPEAL",
      NOS == 999 ~ "MISCELLANEOUS",
      TRUE ~ "UNKNOWN"
    ),
    # code nature of judgement
    # Note: these are stable codes from 1985 onward. But the coding of case disposition varies in earlier years.
    # From 1970-1980:
    DISP_t = case_when(
        yr_term <= 1980 ~ case_when(
          DISP == 1 ~ "After Oral Hearing",
          DISP == 2 ~ "After Submission Without Hearing",
          DISP == -8 ~ "Unknown",
          TRUE ~ "Unknown"
        ),
        # from 1981-1984
        yr_term >=1979 & yr_term <= 1984 ~ case_when(
          DISP == 1 ~ "After Oral Hearing",
          DISP == 2 ~ "After Submission Without Hearing",
          DISP == 3 ~ "After Other Judicial Action",
          DISP == 4 ~ "Without Judicial Action",
          TRUE ~ "Unknown"
        ),
        # from 1985-forward
        yr_term >= 1985 ~ case_when(
            DISP == 1 ~ "After Oral Hearing",
            DISP == 2 | DISP == 3 ~ "After Submission Without Hearing",
            DISP == 4 ~ "After Other Judicial Action",
            DISP == 5 ~ "Without Judicial Action",
            TRUE ~ "Unknown"
            )
        )
    )

# test: look at data pre-1980
test <- fjc %>%
  filter(
    yr_term <= 1980,
    NOS == 893
  ) %>%
  select(
    NOS, DISP, DISP_t, yr_term, yr_file
  )

# look for number of unknown cases dispositions
test <- fjc %>%
  filter(
    NOS == 893 # environmental cases
  ) %>%
  group_by(DISP_t,yr_file) %>%
  select(DISP_t, yr_file) %>%
  mutate(
    n = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup()
# --> not too many - only 340 cases
rm(test)

# make a simple fjc df with counts and other basic info - district data
fjc_tots <- fjc %>%
  select(
    fjc_ID,
    yr_file,
    yr_term,
    PLT,
    DEF,
    NOS,
    NOS_t,
    DISP,
    DISP_t,
    DISTRICT,
    cases_tot_yr,
    cases_tot_typ_yr,
    cases_tot_yr_term
    ) %>%
  # keep only cases filed in 1988 to 2023. We filter data down to 2022 in the
  # next script
  filter(
    yr_file <= 2023 & yr_file >= 1988
  ) %>%
  ungroup()

# Write out cleaned and organized data for future use ######

# write out fjc (district data)
write_csv(
  fjc,
  "Data/FJC_preprocessed/fjc_district.csv"  
)

# write out fjc (appellate data)
write_csv(
  fjc_a,
  "Data/FJC_preprocessed/fjc_appellate.csv"  
)

# write out fjc_tots
write_csv(
  fjc_tots,
  "Data/FJC_preprocessed/fjc_tots.csv"  
)


# Plot: NOS distributions by litigant type #####

# call NOS distribution plotting function
source("Functions/NOS_dist_for_litigant.R")

# plot EPA NOS codes
in_terms <- "US EPA|USEPA|E\\.P\\.A\\.|ENVIRONMENTAL PRO|U\\.S\\. ENV|U\\.S\\.ENV|U S ENV|EPA"
out_terms <- "INC|CO\\.|COMPANY|PROF|PROD|PROP|PROC|CORP|EPA[A-Z]|[A-R]EPA|[T-Z]EPA"
facet_title = "U.S. Environmental Protection Agency"
epa_nos <- NOS_dis_plot(in_terms,out_terms,facet_title)


# plot FOREST SERVICE NOS codes
in_terms <- "US FOREST|FOREST SER|U\\.S\\.F\\.S\\.|USFS|U\\.S\\. FS|U\\.S\\.FS|US FS"
out_terms <- "INC|CO\\.|COMPANY|PROF|PROD|PROP|PROC|CORP|EPA[A-Z]|[A-R]EPA|[T-Z]EPA"
facet_title = "U.S. Forest Service"
fs_nos <- NOS_dis_plot(in_terms,out_terms,facet_title)


# plot ARMY CORPS NOS codes
in_terms <- "ARMY CORPS|USACE|U\\.S\\.A\\.C\\.E\\.|CORPS OF ENG"
out_terms <- "INC|CO\\.|COMPANY|PROF|PROD|PROP|PROC|EPA[A-Z]|[A-R]EPA|[T-Z]EPA"
facet_title = "U.S. Army Corps"
army_corps_nos <- NOS_dis_plot(in_terms,out_terms,facet_title)


# plot USFWS NOS codes
in_terms <- "US FISH|U\\.S\\. FISH|FISH AND WILDLIFE|U\\.S\\.F\\.W\\.S\\.|USFWS|U\\.S\\. FWS|U\\.S\\.FWS|FISH \\& WILD|FISHANDWILD|FWS"
out_terms <- "INC|CO\\.|COMPANY|PROF|PROD|PROP|PROC|CORP|EPA[A-Z]|[A-R]EPA|[T-Z]EPA"
facet_title = "U.S. Fish and Wildlife Service"
fws_nos <- NOS_dis_plot(in_terms,out_terms,facet_title)


# plot SIERRA CLUB NOS codes
in_terms <- "SIERRA C|SIERRA\\, ET"
out_terms <- "INC|CO\\.|COMPANY|PROF|PROD|PROP|PROC|CORP|EPA[A-Z]|[A-R]EPA|[T-Z]EPA|SIERRA C[A-K]|SIERRA C[M-Z]"
facet_title = "The Sierra Club"
sierra_nos <- NOS_dis_plot(in_terms,out_terms,facet_title)


# plot Natural Resources Defense Counsil NOS codes
in_terms <- "RESOURCES D|RES DEF"
out_terms <- "COMPANY|PROF|PROD|PROP|PROC|CORP|EPA[A-Z]|[A-R]EPA|[T-Z]EPA|SIERRA C[A-K]|SIERRA C[M-Z]|ARES"
facet_title = "Natural Resources Defense Council"
nrdc_nos <- NOS_dis_plot(in_terms,out_terms,facet_title)


# plot combo plot
(epa_nos | army_corps_nos) /
  (fs_nos | fws_nos) /
  (sierra_nos | nrdc_nos) +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save combo plot
ggsave(
  filename = "Figures/LITIGNAT_NOS_Distributions.png",
  width =16,
  height = 16
)


# remove plots dataframes 
rm(
  army_corps_nos,
  epa_nos,
  fs_nos,
  fws_nos,
  nrdc_nos,
  sierra_nos
)






# the end. 































# Plot: total number of cases by filing date ######

# NOTE: this is pre-cleaning! BP oil spil and other cases later excluded from
# analysis are still included

# plot just environmental (NOS = 893) district cases and total cases
fjc %>%
  filter(
    NOS == 893,
    (yr_file > 1970 & yr_file <= 2023)
  ) %>%
  group_by(
    yr_file
    ) %>%
  filter(
    row_number() == 1
  ) %>%
  pivot_longer(
    cols = c(cases_tot_yr,cases_tot_typ_yr,cases_pct_tot_yr),
    names_to = "category",
    values_to = "amt"
  ) %>%
  mutate(
    category =  case_when(
      category == "cases_tot_yr" ~ "Total Federal Cases\n* by filing date *",
      category == "cases_tot_typ_yr" ~ "Envrionmental Cases (annual count)\n* by filing date *",
      category == "cases_pct_tot_yr" ~ "Environmental Cases (percent of total cases)\n* by filing date *",
    )
  ) %>%
  ggplot(
    aes(
      x = yr_file,
      y = amt
      )
  ) +
  geom_line(
    alpha = 0.6
  ) +
  geom_smooth(
    method = "loess"
  ) +
  labs(
    y = NULL,
    x = NULL
  ) +
  facet_wrap(
    vars(category),
    scales = "free_y"
  ) +
  theme_linedraw()

ggsave(
  "Figures/FJC_environmental_federal_case_counts.png",
  width = 20,
  height = 5
)



# Plot: just environmental cases and total cases - FED AS DEFENDANT ######

# NOTE: this is pre-cleaning! BP oil spill and other cases later excluded from
# analysis are still included

fjc %>%
  filter(
    JURIS == 2,
    (yr_file > 1970 & yr_file <= 2021)
  ) %>%
  group_by(
    yr_file
    ) %>%
  mutate(
    cases_tot_yr = n()
  ) %>%
  filter(
    NOS == 893
  ) %>%
  mutate(
    cases_tot_typ_yr = n(),
    cases_pct_tot_yr = round(cases_tot_typ_yr/cases_tot_yr*100,3)
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  pivot_longer(
    cols = c(cases_tot_yr,cases_tot_typ_yr,cases_pct_tot_yr),
    names_to = "category",
    values_to = "amt"
  ) %>%
  mutate(
    category =  case_when(
      category == "cases_tot_yr" ~ "Total Federal Cases\nfederal gov. as defendant\n* by filing date *",
      category == "cases_tot_typ_yr" ~ "Envrionmental Cases (annual count)\nfederal gov. as defendant\n* by filing date *",
      category == "cases_pct_tot_yr" ~ "Environmental Cases (percent of total cases)\nfederal gov. as defendant\n* by filing date *",
    )
  ) %>%
  ggplot(
    aes(
      x = yr_file,
      y = amt
      )
  ) +
  geom_line(
    alpha = 0.6
  ) +
  geom_smooth(
    method = "loess"
  ) +
  labs(
    y = NULL,
    x = NULL
  ) +
  facet_wrap(
    vars(category),
    scales = "free_y"
  ) +
  theme_linedraw()

ggsave(
  "Figures/FJC_environmental_federal_case_counts_fed_defendant.png",
  width = 10,
  height = 5
)

# Plot: total number of cases by termination date ######

# NOTE: this is pre-cleaning! BP oil spill and other cases later excluded from
# analysis are still included

# directly compare environmental and all other cases, aggregated. 
fjc %>%
   filter(
     # NOS == "893",
     (yr_term > 1970 & yr_term <= 2021)
   ) %>%
  mutate(
    enviro_case = case_when(
      NOS == 893 ~ "Envieronmental Cases",
      TRUE ~ "All Other Cases"
    )
  ) %>%
  group_by(
    yr_term, enviro_case
    ) %>%
  mutate(
    no_cases = n()
    ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      x = yr_term,
      y = no_cases,
      group = enviro_case,
      color = enviro_case
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  #scale_y_continuous(
  #  breaks = seq(0,10000,100)
  #) +
  labs(
    y = NULL,
    x = NULL,
    color = "Case Type",
    title = "Annual number of environemntal and all other case types, by termination date"
  ) +
  facet_wrap(
    vars(enviro_case),
    scales = "free_y"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,NA)
  )

ggsave(
  "Figures/No_cases_environmental_and_all_other.png",
  width = 10,
  height = 5
)


# Compare environmental and all other cases by disposition 
fjc %>%
   filter(
     # NOS == "893",
     (yr_term > 1988 & yr_term < 2021)
   ) %>%
  mutate(
    enviro_case = case_when(
      NOS == 893 ~ "Envieronmental Cases",
      TRUE ~ "All Other Cases"
    )
  ) %>%
  group_by(
    yr_term, enviro_case, DISP_t
    ) %>%
  mutate(
    no_cases = n()
    ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      x = yr_term,
      y = no_cases,
      group = DISP_t,
      color = DISP_t
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  labs(
    y = NULL,
    x = NULL,
    color = "Case Type",
    title = "Annual number of environemntal and all other case types, by ending disposition and termination date"
  ) +
  facet_wrap(
    vars(enviro_case),
    scales = "free_y"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,NA)
    #ylim = c(0,600)
  )

ggsave(
  "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/Environmental_v_all_other_case_duration_BY_DISPOSITION.png",
  width = 10,
  height = 5
)



# Compare environmental and all other cases by disposition - FED as DEFENDANT
fjc %>%
   filter(
     JURIS == 2,
     (yr_term > 1988 & yr_term < 2021)
   ) %>%
  mutate(
    enviro_case = case_when(
      NOS == 893 ~ "Envieronmental Cases",
      TRUE ~ "All Other Cases"
    )
  ) %>%
  group_by(
    yr_term, enviro_case, DISP_t
    ) %>%
  mutate(
    no_cases = n()
    ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      x = yr_term,
      y = no_cases,
      group = DISP_t,
      color = DISP_t
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  labs(
    y = NULL,
    x = NULL,
    color = "Case Type",
    title = "Annual number of environemntal and all other case types, by ending disposition and termination date - Fed. as Defendant"
  ) +
  facet_wrap(
    vars(enviro_case),
    scales = "free_y"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,NA)
    #ylim = c(0,600)
  )

ggsave(
  "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/Environmental_v_all_other_case_duration_BY_DISPOSITION_fed_as_defendant.png",
  width = 10,
  height = 5
)

# PLOT PERCENT OF CASES BY ENDING DISPOSITION ######

# Compare environmental and all other cases by disposition 
fjc %>%
   filter(
     # NOS == "893",
     (yr_term > 1988 & yr_term < 2021)
   ) %>%
  mutate(
    enviro_case = case_when(
      NOS == 893 ~ "Envieronmental Cases",
      TRUE ~ "All Other Cases"
    )
  ) %>%
  group_by(
    yr_term, enviro_case
  ) %>%
  mutate(
    tot_cases = n()
  ) %>%
  group_by(
    yr_term, enviro_case, DISP_t
    ) %>%
  mutate(
    no_cases = n(),
    pct_cases = no_cases/tot_cases*100
    ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      x = yr_term,
      y = pct_cases,
      group = DISP_t,
      color = DISP_t
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  labs(
    y = NULL,
    x = NULL,
    color = "Case Type",
    title = "Percent of environemntal and all other case types, by ending disposition and termination date"
  ) +
  facet_wrap(
    vars(enviro_case)#,
    #scales = "free_y"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,50)
    #ylim = c(0,600)
  )

ggsave(
  "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/Environmental_v_all_other_case_duration_PERCENT_BY_DISPOSITION.png",
  width = 10,
  height = 5
)


# Compare environmental and all other cases by disposition - FED DEFENDNAT
fjc %>%
   filter(
     JURIS == 2,
     (yr_term > 1988 & yr_term < 2021)
   ) %>%
  mutate(
    enviro_case = case_when(
      NOS == 893 ~ "Envieronmental Cases",
      TRUE ~ "All Other Cases"
    )
  ) %>%
  group_by(
    yr_term, enviro_case
  ) %>%
  mutate(
    tot_cases = n()
  ) %>%
  group_by(
    yr_term, enviro_case, DISP_t
    ) %>%
  mutate(
    no_cases = n(),
    pct_cases = no_cases/tot_cases*100
    ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      x = yr_term,
      y = pct_cases,
      group = DISP_t,
      color = DISP_t
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  labs(
    y = NULL,
    x = NULL,
    color = "Case Type",
    title = "Percent of environemntal and all other case types, by ending disposition and termination date - Fed. as Defendant"
  ) +
  facet_wrap(
    vars(enviro_case)#,
    #scales = "free_y"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,50)
    #ylim = c(0,600)
  )

ggsave(
  "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/Environmental_v_all_other_case_duration_PERCENT_BY_DISPOSITION_fed_defendant.png",
  width = 10,
  height = 5
)



# PLOT ENVIRONMENTAL CASES BY ENDING DISPOSITION ######

# plot environmental cases by disposition
fjc %>%
  filter(
    NOS == 893,
    (yr_term > 1988 & yr_term < 2021)
  ) %>%
  group_by(
    yr_term
  ) %>%
  mutate(
    cases_tot_typ_yr_term = n()
  ) %>%
  group_by(
    yr_term, DISP_t
    ) %>%
  mutate(
    cases_by_DIS = n(),
    cases_by_DIS_pct = round(cases_by_DIS/cases_tot_typ_yr_term*100,3)
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  pivot_longer(
    cols = c(cases_by_DIS,cases_by_DIS_pct),
    names_to = "category",
    values_to = "amt"
  ) %>%
  mutate(
    category =  case_when(
      category == "cases_by_DIS" ~ "Environmental Federal Cases by Disposition (n)\nall defendant types\n* by termination date *",
      category == "cases_by_DIS_pct" ~ "Environmental Federal Cases by Disposition (%)\nall defendant types\n* by termination date *",
    )
  ) %>%
  ggplot(
    aes(
      x = yr_term,
      y = amt,
      group = DISP_t,
      color = DISP_t
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  labs(
    y = NULL,
    x = NULL,
    color = "Final Case Disposition"
  ) +
  facet_wrap(
    vars(category),
    scales = "free_y"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,NA)
  )

ggsave(
  "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/FJC_environmental_federal_cases_by_disposition.png",
  width = 15,
  height = 5
)


# plot environmental cases by disposition - cases with fed as defendant
fjc %>%
  filter(
    NOS == 893,
    JURIS == 2,
    (yr_term > 1988 & yr_term < 2021)
  ) %>%
  group_by(
    yr_term
  ) %>%
  mutate(
    cases_tot_typ_yr_term = n()
  ) %>%
  group_by(
    yr_term, DISP_t
    ) %>%
  mutate(
    cases_by_DIS = n(),
    cases_by_DIS_pct = round(cases_by_DIS/cases_tot_typ_yr_term*100,3)
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  pivot_longer(
    cols = c(cases_by_DIS,cases_by_DIS_pct),
    names_to = "category",
    values_to = "amt"
  ) %>%
  mutate(
    category =  case_when(
      category == "cases_by_DIS" ~ "Environmental Federal Cases by Disposition (n)\nfederal gov. as defendant\n* by termination date *",
      category == "cases_by_DIS_pct" ~ "Environmental Federal Cases by Disposition (%)\nfederal gov. as defendant\n* by termination date *",
    )
  ) %>%
  ggplot(
    aes(
      x = yr_term,
      y = amt,
      group = DISP_t,
      color = DISP_t
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  labs(
    y = NULL,
    x = NULL,
    color = "Final Case Disposition"
  ) +
  facet_wrap(
    vars(category),
    scales = "free_y"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,NA)
  )

ggsave(
  "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/FJC_environmental_federal_cases_by_disposition_fed_defend.png",
  width = 10,
  height = 5
)



# # plot environmental cases by disposition - cases with fed as defendant - BY FILING YEAR
# fjc %>%
#   filter(
#     NOS == 893,
#     JURIS == 2,
#     (yr_file > 1988 & yr_file < 2021)
#   ) %>%
#   group_by(
#     yr_file
#   ) %>%
#   mutate(
#     cases_tot_typ_yr_file = n()
#   ) %>%
#   group_by(
#     yr_file, DISP_t
#     ) %>%
#   mutate(
#     cases_by_DIS = n(),
#     cases_by_DIS_pct = round(cases_by_DIS/cases_tot_typ_yr_file*100,3)
#   ) %>%
#   filter(
#     row_number() == 1
#   ) %>%
#   pivot_longer(
#     cols = c(cases_by_DIS,cases_by_DIS_pct),
#     names_to = "category",
#     values_to = "amt"
#   ) %>%
#   mutate(
#     category =  case_when(
#       category == "cases_by_DIS" ~ "Environmental Federal Cases by Disposition (n)\nfederal gov. as defendant\n* by filing date *",
#       category == "cases_by_DIS_pct" ~ "Environmental Federal Cases by Disposition (%)\nfederal gov. as defendant\n* by filing date *",
#     )
#   ) %>%
#   ggplot(
#     aes(
#       x = yr_file,
#       y = amt,
#       group = DISP_t,
#       color = DISP_t
#       )
#   ) +
#   geom_line(
#     alpha = .5
#   ) +
#   geom_smooth(
#     method = "loess"
#   ) +
#   labs(
#     y = NULL,
#     x = NULL,
#     color = "Final Case Disposition"
#   ) +
#   facet_wrap(
#     vars(category),
#     scales = "free_y"
#   ) +
#   theme_linedraw() +
#   coord_cartesian(
#     ylim = c(0,NA)
#   )
# 
# ggsave(
#   "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/FJC_environmental_federal_cases_by_disposition_fed_defend_by_filing_date.png",
#   width = 10,
#   height = 5
# )


# PLOT ALL CASES BY ENDING DISPOSITION ######
# plot ALL cases by disposition
fjc %>%
   filter(
     # NOS == "893",
     (yr_term > 1988 & yr_term < 2021)
   ) %>%
  group_by(
    yr_term
  ) %>%
  mutate(
    cases_tot_yr_term = n()
  ) %>%
  group_by(
    yr_term, DISP_t
    ) %>%
  mutate(
    cases_by_DIS = n(),
    cases_by_DIS_pct = round(cases_by_DIS/cases_tot_yr_term*100,3)
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(cases_by_DIS,cases_by_DIS_pct),
    names_to = "category",
    values_to = "amt"
  ) %>%
  mutate(
    category =  case_when(
      category == "cases_by_DIS" ~ "All Federal Cases by Disposition (n)\nall defendant types\n* by termination date *",
      category == "cases_by_DIS_pct" ~ "All Federal Cases by Disposition (%)\nall defendant types\n* by termination date *",
    )
  ) %>%
  ggplot(
    aes(
      x = yr_term,
      y = amt,
      group = DISP_t,
      color = DISP_t
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  labs(
    y = NULL,
    x = NULL,
    color = "Final Case Disposition"
  ) +
  facet_wrap(
    vars(category),
    scales = "free_y"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,NA)
  )

ggsave(
  "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/FJC_all_federal_cases_by_disposition.png",
  width = 10,
  height = 5
)



# plot ALL cases by disposition - federal defendant
fjc %>%
   filter(
     # NOS == "893",
     JURIS == 2,
     (yr_term > 1988 & yr_term < 2021)
   ) %>%
  group_by(
    yr_term
  ) %>%
  mutate(
    cases_tot_yr_term = n()
  ) %>%
  group_by(
    yr_term, DISP_t
    ) %>%
  mutate(
    cases_by_DIS = n(),
    cases_by_DIS_pct = round(cases_by_DIS/cases_tot_yr_term*100,3)
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(cases_by_DIS,cases_by_DIS_pct),
    names_to = "category",
    values_to = "amt"
  ) %>%
  mutate(
    category =  case_when(
      category == "cases_by_DIS" ~ "All Federal Cases by Disposition (n)\nfederal gov. as defendant\n* by termination date *",
      category == "cases_by_DIS_pct" ~ "All Federal Cases by Disposition (%)\nfederal gov. as defendant\n* by termination date *",
    )
  ) %>%
  ggplot(
    aes(
      x = yr_term,
      y = amt,
      group = DISP_t,
      color = DISP_t
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  labs(
    y = NULL,
    x = NULL,
    color = "Final Case Disposition"
  ) +
  facet_wrap(
    vars(category),
    scales = "free_y"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,NA)
  )

ggsave(
  "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/FJC_all_federal_cases_by_disposition_fed_defend.png",
  width = 10,
  height = 5
)


# # plot cases by disposition - cases with fed as defendant - BY FILING YEAR
# fjc %>%
#   filter(
#     #NOS == 893,
#     JURIS == 2,
#     (yr_file > 1988 & yr_file < 2021)
#   ) %>%
#   group_by(
#     yr_file
#   ) %>%
#   mutate(
#     cases_tot_typ_yr_file = n()
#   ) %>%
#   group_by(
#     yr_file, DISP_t
#     ) %>%
#   mutate(
#     cases_by_DIS = n(),
#     cases_by_DIS_pct = round(cases_by_DIS/cases_tot_typ_yr_file*100,3)
#   ) %>%
#   filter(
#     row_number() == 1
#   ) %>%
#   pivot_longer(
#     cols = c(cases_by_DIS,cases_by_DIS_pct),
#     names_to = "category",
#     values_to = "amt"
#   ) %>%
#   mutate(
#     category =  case_when(
#       category == "cases_by_DIS" ~ "Federal Cases by Disposition (n)\nfederal gov. as defendant\n* by filing date *",
#       category == "cases_by_DIS_pct" ~ "Federal Cases by Disposition (%)\nfederal gov. as defendant\n* by filing date *",
#     )
#   ) %>%
#   ggplot(
#     aes(
#       x = yr_file,
#       y = amt,
#       group = DISP_t,
#       color = DISP_t
#       )
#   ) +
#   geom_line(
#     alpha = .5
#   ) +
#   geom_smooth(
#     method = "loess"
#   ) +
#   labs(
#     y = NULL,
#     x = NULL,
#     color = "Final Case Disposition"
#   ) +
#   facet_wrap(
#     vars(category),
#     scales = "free_y"
#   ) +
#   theme_linedraw() +
#   coord_cartesian(
#     ylim = c(0,NA)
#   )
# 
# ggsave(
#   "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/FJC_all_federal_cases_by_disposition_fed_defend_by_filing_date.png",
#   width = 10,
#   height = 5
# )




test <- fjc %>%
  filter(
    yr_file <= 2020,
    yr_file >= 1970
  )


# PLOT CASES BY TIME FROM FILING TO TERMINATION ######

# directly compare environmental and all other cases, aggregated. 
fjc %>%
   filter(
     # NOS == "893",
     (yr_term > 1970 & yr_term < 2021)
   ) %>%
  mutate(
    duration = TERMDATE - FILEDATE,
    enviro_case = case_when(
      NOS == 893 ~ "Envieronmental Case",
      TRUE ~ "All Other Cases"
    )
  ) %>%
  group_by(
    yr_term
  ) %>%
  mutate(
    dur_avg = median(duration, na.rm = T)
  ) %>%
  group_by(
    yr_term, enviro_case
    ) %>%
  mutate(
    dur_avg_by_enviro = median(duration, na.rm = T)
    ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      x = yr_term,
      y = dur_avg_by_enviro,
      group = enviro_case,
      color = enviro_case
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  scale_y_continuous(
    breaks = seq(0,10000,100)
  ) +
  labs(
    y = "Days",
    x = NULL,
    color = "Case Type",
    title = "Median duration for environemntal and all other case types"
  ) +
  # facet_wrap(
  #   vars(enviro_case)#,
  #   #scales = "free_y"
  # ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,NA)
  )

ggsave(
  "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/Environmental_v_all_other_case_duration.png",
  width = 10,
  height = 5
)


# Compare environmental and all other cases by disposition 
fjc %>%
   filter(
     # NOS == "893",
     (yr_term > 1988 & yr_term < 2021)
   ) %>%
  mutate(
    duration = TERMDATE - FILEDATE,
    enviro_case = case_when(
      NOS == 893 ~ "Envieronmental Cases",
      TRUE ~ "All Other Cases"
    )
  ) %>%
  group_by(
    yr_term, enviro_case, DISP_t
    ) %>%
  mutate(
    dur_avg_by_enviro = median(duration, na.rm = T)
    ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      x = yr_term,
      y = dur_avg_by_enviro,
      group = DISP_t,
      color = DISP_t
      )
  ) +
  geom_line(
    alpha = .5
  ) +
  geom_smooth(
    method = "loess"
  ) +
  scale_y_continuous(
    breaks = seq(0,10000,100)
  ) +
  labs(
    y = "Days",
    x = NULL,
    color = "Case Type",
    title = "Median duration for environemntal and all other case types, by ending disposition"
  ) +
  facet_wrap(
    vars(enviro_case)#,
    #scales = "free_y"
  ) +
  theme_linedraw() +
  coord_cartesian(
    #ylim = c(0,NA)
    ylim = c(0,600)
  )

ggsave(
  "/Users/chrisreaborn/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Figures/Environmental_v_all_other_case_duration_BY_DISPOSITION.png",
  width = 10,
  height = 5
)



# WRITE OUT ENVIRONMENTAL CASES ONLY ######

# write out district cases
fjc_enviro <- fjc %>%
  filter(
    NOS == 893
  )

write_csv(
  fjc_enviro,
  "fjc_enviro.csv"
  )

# write out appellate cases
fjc_enviro_a <- fjc_a %>%
  filter(
    NOS == 893
  )

write_csv(
  fjc_enviro_a,
  "fjc_enviro_A.csv"
  )

# get a sense of CDD_IDs in fjc appellate data (sample 25 rows to)
fjc_CDD_IDs <- fjc_enviro_a %>%
  select(
    CDD_ID,DDOCKET
  ) %>%
  filter(
    str_length(DDOCKET) >= 6
  ) %>%
  sample_n(25)

fjc_CDD_IDs <- as.vector(fjc_CDD_IDs$CDD_ID)


ID = "9-77-1200027"

test_a <- fjc_a %>%
  filter(
    CDD_ID == ID
  )
test <- fjc %>%
  filter(
    CDD_ID == ID
  )
  





