# Code FJC Plt and Def type
# Environmental cases only
# (NOS == 893)
# By Chris Rea with support from Casey Rife
# Last modified April 22, 2024

# Notes ####
# This script begins with the pre-processed FJC data parsed and assembled by the
# 01-FJC_build.R file and begins to processes it for analysis.

# Load packages ####
library(tidyverse)

# Load FJC district and appellate cases; filter to environmental (NOS == 893) ####

# Filtering by NOS == 893 leaves 42.0k district cases
fjc_e <- read_csv(
  "Data/FJC_preprocessed/fjc_district.csv"
  ) %>%
  filter(
    NOS == 893
  )

gc()

# Filtering by NOS == 893 leaves 6.1k appellate cases
fjc_e_a <- read_csv(
  "Data/FJC_preprocessed/fjc_appellate.csv"
  ) %>%
  filter(
    NOS == 893
  )

gc()

# Drop years with inconsistent outcome coding ######

# Note that for district data, prior to 1988, plaintiffs and defendants are not
# listed individually - just the case name is. Note also that for district case
# data, prior to 1988, case outcomes are coded inconsistently. Thus, we focus on
# 1988 forward (still almost 35 years of litigation) for district cases.
# Specifically, for the district data, we retain cases that stem from the fjc
# 1988-forward civil data, which contain the variable new == 1. For the
# appellate data, we retain cases filed in or before 1988. (The "new" variable
# in the appellate data signals 2008-forward.) We also only retain cases that
# were filed in or before 2022. This drops the total number of district cases
# from 42.0k to 34.9k, and the number of appellate cases from 6.1k to 4,487.

# for district cases
fjc_e <- fjc_e %>%
  filter(
    new == 1,
    yr_file <= 2022
  )

# for appellate cases
fjc_e_a <- fjc_e_a %>%
  filter(
    yr_file >= 1988,
    yr_file <= 2022
  )


# Recode the JURIS variable in human-readable terms ####
fjc_e <- fjc_e %>%
  mutate(
    JURIS = as.character(JURIS),
    JURIS = case_when(
      str_detect(JURIS,"1") ~ "US govt plaintiff",
      str_detect(JURIS,"2") ~ "US govt defendant",
      str_detect(JURIS,"3") ~ "federal question",
      str_detect(JURIS,"4") ~ "diversity of citizenship",
      str_detect(JURIS,"5") ~ "local question",
      TRUE ~ JURIS
    )
  )
### District cases - Begin coding of plaintiff and defendant types ####

# In the following code, we create very rough, preliminary lists of terms that
# signal different types of plaintiffs and defendants. For example, where a
# plaintiff is listed as USEPA, we know that we want to code the plaintiff as a
# "federal" (fed) defendant. And so on. We then use these lists to automate
# coding of plaintiffs and defendants in cases: any plaintiff or defendant with
# "USEPA" in the PLT or DEF variable will be coded as having a "fed" defendant
# or plaintiff type. We do this coding work in several iterations: applying
# these list-based codes to automatically code plaintiffs and defendants,
# inspecting the results to ensure accuracy; conducting further automated
# coding. etc. Eventually, when automated coding begins to achieve diminishing
# returns, we write-out remaining un-coded plaintiff and defendant types;
# hand-coding unknown variables; re-read in the hand-coded variables, and add
# them to the automated ones.

# Much of this work involves creating dictionaries - lists, essentially - to
# apply particular type codes to plaintiffs and defendants with particular words
# in their recorded names.

# There is one exception to this method: after the preliminary lists are
# applied, we use the "jurisdiction" code to code plaintiff as fed if the reason
# for jurisdiction is that fed is plaintiff/defendant AND if another code has
# not been applied. This is important to be aware of given the structure of the
# FJC data. The FJC data contain only the lead plaintiff and lead defendant; we
# use these lead litigant types to code the plaintiff types. The jurisdiction
# code, however, may make reference to a non-first (e.g. a second or third or
# tenth) defendant. For example, if an environmental advocacy group sues a
# private firm for polluting in violation of federal law and also names a
# federal agency in the suit as a second defendant, we would code that federal
# defendant as BIZ (a private firm) since the lead defendant is a firm. But if
# that lead defendant is not coded in the automated approach below - if it is
# coded as "unknown" in the automated process below - AND the jurisdictional
# reasons for the case being filed in federal court is listed as "US govt
# defendant", then that case will be coded as having a "fed" defendant even
# though the federal defendant is listed second (or later) and thus not directly
# visible in the FJC data. As we show below, the result of this is primarily to
# recode unknown codes to fed codes; it does not substantially shift other types
# of codes, providing reassurances that this move is not shifting codes that
# would have be coded as non-federal by the first plaintiff/def to FED codes.
# See details below.

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


# Build preliminary lists of plaintiff and defendant types ####

# In this 

#make lists

FED  <- c("DEPT|DEPARTMENT |U S |US |U.S. |U.S.A.|USA |CORPS|EPA |E.P.A. |U.S.E.P.A. |USEPA |
             |INDIAN HEALTH|U S|SECRETARY OF INTERIOR|UNITED STATES ENVIRO|BUREA OF|SECRETARY|SECRETAR|
             |SKINNER|PEOPLE OF THE UNITED|FEDERAL HIGHWAY ADM|FISH AND WILDLIFE SV, ET AL")

STA <- c("STATE OF |ST OF |COMMON|OR NATURAL|OR. NAT.|OR NAT|OREGON NAT|OF NY|OF NE|
            |NY STATE|NEW YORK SATE|PENNA. DEPT.|PA. DEPT.|COMMOW OF KY| STATE OF|W.VA. DIV.|
            |COMMOWELTH|COMMOWEALTH OF|COMMO OF|PEOPLE OF|PEOPLE CALIF|PEOPLE STATE|YORK STATE|
         |ATLANTIC STATES|WASHINGTON DEPT OF WILDLIFE")

LOC <-c("COUNTY |CITY |TOWN |MUNICIPAL|COUNTY|PARISH|MUNI|CITY OF|BOROUGH|BORO|CITIES| CITY|
           |TWN. |METROPOLITAN| SCHOOL DIST|VILLAGE OF|CTY OF|PORT OF| CNTY| COUN|TOWNSHIP|PORT AUTH|
        |SCHOOL BOARD|CITY COUNCIL")

BIZ <-c(" INC|INC.| CORP|CORP| CO |CO.| COMPANY| REAL| GROUP|DEVEL| MFG|MANUFACTUR|
           |PROD|CHEM|IND|SHIP|OIL|PETRO|MOTOR|ELEC|PARTNERS|SERVIC|BROTHERS|SONS|
           |AUTO|METAL|WASTE|DISPOSAL|ASSOC|ASSO|SIERRA PACIFIC|BANK|BK|STEEL|FARM|
           |INSUR|PROSPER| ENTERPRISES| LTD| LLC| L.L.C.| LLP|MINING|EXCAVATING|HOTEL|
           |TECHNOLOGIES|GOODYEAR TIRE|REFINING|PRODUCTS|NAMUFACTURING|DU PONT|DUPONT|
           |LANDFILL|CABLE|TRUCKING|PIPLINE|PIPELINE|FREIGHT|TELEPHONE|HARDWARE|MINES|
           |JOHNSON & JOHNSON|MILLS|POWER & LIGHT|POWER&LIGHT| WORKS|MOBIL|CHROMALLOY|
           |PLATING|ALUMINUM|PRODUCTION|PLASTICS|SPARK PLUG|TRUCKING|CEMENT|AGRICULTURAL|
           |SOLVENT|EXXON|DREDGING|SUPPLY|INSULATION|SYSTEMS|PAVEMENT|POWER AND LIGHT|
           |INVESTMENTS|LOGISTICS|WAREHOUSING|DISTRIBUTION|PERFORMANCE|PROCESSING|
           |SUPPLY|PAPER|FABRICATION|BP|B.P.|COLUMBIA GAS|TOWING AND TRANS|OPERATING|
           |RAILROAD|SAND AND GRAVEL|TOWING| AUTO|AUTO |LUMBER|VEHICLES|PLASTIC|SEMICONDUCTOR|
           |HOMEBUILDERS|REFINERY|PNEUMATIC TUBE|ETHANOL|AGGREGATES|NUCLEAR|STONE|ENGINEERING|
           |FRANCHISES|RECYCLING|RECYCL|ENTERPRISE|EQUIPMENT|TECHNOLOGY|PROCTOR AND GAMB|MILL|
           |CXS|TEXACO|AUTOMOTIVE|BATTERY|TIRE|ACME|ALLOYS|MANUFACT|TECHNOLOGIES|BROS|DRILLING|
           |BUILDERS|BATTERIES|& SON|AND SON|BOTTLING|LANDFILL|AVIS|PROPERTIES|EQUITIES|AIRLINES|
        |RAIL|INVEST|PROP|POWER/LIGHT|GASOLINE|COAL |CSX | ELECT| VENTURE|ACCENT CLEANERS|VOLKSWAGEN")

NGO <-c("SIERRA CLUB|DEFENSE|FUND|RESOURCE|AUDUBO|HUMANE|WILD|HIGH SIERRA HIKERS|
           |KEEP|WATC|DEF|BIO|CENTER|LEAGUE|ALLIANCE|CLEAN|SAVE|EARTH|CONSERV|CITIZENS|
           |ADVOCA|PUBLIC INT|PUB.|PIRG|PUB INT|RIVER|FOUND|GREENPE|GUARD|ATLANTIC ST|
           |ATLA S|ATLAN ST|FRIENDS|JUST|AUDOBON|PROTECT|SUPPORTERS|PROTEC|PEOPLE FOR THE ETHICAL|
           |ANIMAL WELFARE|PRESERVATION SOC|PARKS ARE FOR PEOPLE|LEGAL ACTION FOR ANIMALS|
           |STEWARDS OF|ACTION NET|PEOPLE ETH ANIMALS|PEOPLE FOR TREATMENT|FOREST ALLIA|
        |ENVIRONMENTAL ACTION|PLASTIC POLLUTION COALITION|CALIFORNIA SPORTFISHING|LANDS COUNCIL")

NGO_O <-c("FRIENDS OF LIBERTY")

IND <-c("MCLEAN|CLAIMANT ID")

# Note: we later re-categorize PUB_ORG variables largely as LOC; the major exceptions are
# institutions of higher educaiton, which we ultimately code as OTHER.
PUB_ORG <-c("UNIV|AIRPORT|P R AQUEDUCT AND SEWER|SEWER AUTH|SEWERAGE|SEWAGE AUTH| AUTHORITY|
            | AUTH|SOUTH COAST AIR QUALITY, ET AL|PUBLIC SERVICE CO CO|MIT")

TRIBE <-c("CHEROKEE|NAVAJO|TRIBE|TRIBAL|NATIVE AMERI|CHIPPEWA")

CIVIC <-c("NEIGHBORS FOR|NEIGHBORS|CITZENS FOR HEALTHY ENVIRON|ASSOCIATION OF IRRITATED RESID")

#sewage authorities? housing authorities? port authorities? transportation?

# Build lists of unique plaintiffs and defendants ####

# this code creates a unique list of plaintiffs
# 15,654 unique plaintiffs in district fjc data
env_plt <- fjc_e %>%
  filter(
    #JURIS == 1,
    NOS == 893
  ) %>%
  select(
    PLT, JURIS
  ) %>%
  group_by(
    PLT
  ) %>%
  filter(
    row_number() == 1
  )


# this code generates a unique list of defendants
# 18,576 unique defendants in district fjc data
env_def <- fjc_e %>%
  filter(
    #JURIS == 2,
    #NOS == 893
  ) %>%
  select(
    DEF, JURIS
  ) %>%
  group_by(
    DEF
  ) %>%
  filter(
    row_number() == 1
  )

# Preliminary plaintiff type coding ####
env_plt <- env_plt %>%
  # start by coding all plt types as "Unknown"
  mutate(
    PLT_typ = "Unknown",
    # get rid of extra spaces
    PLT_typ = case_when(
      str_detect(PLT, "  ") == TRUE ~ " ", #double space replace to single
      str_detect(PLT, "   ") == TRUE ~ " ",#triple space replace to single
      str_detect(PLT, "    ") == TRUE ~ " ",#quad space replace to single
      str_detect(PLT, "     ") == TRUE ~ " ",#quint space replace to single
      TRUE ~ PLT_typ
    ),
    # apply preliminary lists to code defendant types
    # Note: it's faster to run this in one call, but case_when() does not treat cases
    # sequentially: it treats them simultaneously, and applies the first match
    # if there are multiple. So, to "code over" earlier codes, we have to apply
    # multiple case_when calls through mutate. Mutate() does treat variables
    # sequentially. In other words, ORDER MATTERS here: 
    PLT_typ = case_when(
      str_detect(PLT,NGO) ~ "NGO",
      TRUE ~ PLT_typ
    ),
    PLT_typ = case_when(
      str_detect(PLT,NGO_O) ~ "NGO_O",
      TRUE ~ PLT_typ
    ),
    PLT_typ = case_when(
      str_detect(PLT,IND) ~ "IND",
      TRUE ~ PLT_typ
    ),
    PLT_typ = case_when(
      str_detect(PLT,PUB_ORG) ~ "PUB_ORG",
      TRUE ~ PLT_typ
    ),
    PLT_typ = case_when(
      str_detect(PLT,TRIBE) ~ "TRIBE",
      TRUE ~ PLT_typ
    ),
    PLT_typ = case_when(
      str_detect(PLT,CIVIC) ~ "CIVIC",
      TRUE ~ PLT_typ
    ),
    PLT_typ = case_when(
      str_detect(PLT,BIZ) ~ "BIZ",
      TRUE ~ PLT_typ
    ),
    PLT_typ = case_when(
      str_detect(PLT,FED) ~ "FED",
      TRUE ~ PLT_typ
    ),
    PLT_typ = case_when(
      str_detect(PLT,STA) ~ "STA",
      TRUE ~ PLT_typ
    ),
    PLT_typ = case_when(
      str_detect(PLT,LOC) ~ "LOC",
      TRUE ~ PLT_typ
    ),
    PLT_typ = case_when(
      # last but not least, code plaintiff as fed if reason for jurisdiction is
      # that fed is plaintiff AND another code has not been applied.
      JURIS == "US govt plaintiff" & PLT_typ == "Unknown"  ~ "FED",
      TRUE ~ PLT_typ
    )
  )

# check to make sure there are only unique values of plaintiff types
env_plt <- env_plt %>%
  ungroup() %>%
  unique()

# keep only unique values; drop jurisdiction variable
env_plt <- env_plt %>%
  ungroup() %>%
select(-JURIS) %>%
  group_by(
    PLT
  ) %>%
  filter(
    row_number() == 1
  )

# Result: 5,881 plaintiff types remain unknown (62.4% are coded)

# Preliminary defendant type coding ####
env_def <- env_def %>%
  # start by coding all def types as "Unknown"
  mutate(
    DEF_typ = "Unknown",
    # get rid of extra spaces
    DEF_typ = case_when(
      str_detect(DEF, "  ") == TRUE ~ " ", #double space replace to single
      str_detect(DEF, "   ") == TRUE ~ " ",#triple space replace to single
      str_detect(DEF, "    ") == TRUE ~ " ",#quad space replace to single
      TRUE ~ DEF_typ
    ),
    # apply preliminary lists to code defendant types. Note: as with plaintiffs
    # above, ORDER MATTERS.
    DEF_typ = case_when(
      str_detect(DEF,NGO) ~ "NGO",
      TRUE ~ DEF_typ
    ),
    DEF_typ = case_when(
      str_detect(DEF,NGO_O) ~ "NGO_O",
      TRUE ~ DEF_typ
    ),
    DEF_typ = case_when(
      str_detect(DEF,IND) ~ "IND",
      TRUE ~ DEF_typ
    ),
    DEF_typ = case_when(
      str_detect(DEF,PUB_ORG) ~ "PUB_ORG",
      TRUE ~ DEF_typ
    ),
    DEF_typ = case_when(
      str_detect(DEF,TRIBE) ~ "TRIBE",
      TRUE ~ DEF_typ
    ),
    DEF_typ = case_when(
      str_detect(DEF,CIVIC) ~ "CIVIC",
      TRUE ~ DEF_typ
    ),
    DEF_typ = case_when(
      str_detect(DEF,BIZ) ~ "BIZ",
      TRUE ~ DEF_typ
    ),
    DEF_typ = case_when(
      str_detect(DEF,FED) ~ "FED",
      TRUE ~ DEF_typ
    ),
    DEF_typ = case_when(
      str_detect(DEF,STA) ~ "STA",
      TRUE ~ DEF_typ
    ),
    DEF_typ = case_when(
      str_detect(DEF,LOC) ~ "LOC",
      TRUE ~ DEF_typ
    ),
    DEF_typ = case_when(
      # last but not least, code defendant as fed if reason for jurisdiction is
      # that fed is defendant AND another code has not been applied.
      JURIS == "US govt defendant" & DEF_typ == "Unknown" ~ "FED",  
      TRUE ~ DEF_typ
    )
  )

# check to make sure there are only unique defendant types
env_def <- env_def %>%
  ungroup() %>%
  unique()
  
# keep only unique values and drop JURIS variable.
env_def <- env_def %>%
  ungroup() %>%
  select(-JURIS) %>%
  group_by(
    DEF
    ) %>%
  filter(
    row_number() == 1
  )

# Result: 4,330 defendant types remain unknown (76.7% are coded)

# Join preliminary coded plt/def type codes to full fjc_e df ####
fjc_e <- left_join(fjc_e,env_def,by="DEF")
fjc_e <- left_join(fjc_e,env_plt,by="PLT")

# Recode using JURIS variable for FED litigants ####

# For now, re-code all examples where the federal government is litigant as FED,
# using the JURIS variable information. WHY? Very often, cases where the federal
# government is sued list by name as the defendant the secretary of a given
# department. JACKSON, for example, is listed as the defendant in cases where
# the reason for federal jurisdiction is a federal government defendant 102
# times (see "test" data frame below). That's because JACKSON is referring to
# Lisa Jackson, in her capacity as EPA Administrator. In 61 of those entries,
# the defendant is coded as "Unknown" despite the fact that, in
# all-to-nearly-all of these cases, the name Jackson does, indeed, refer to Lisa
# Jackson, not, e.g., a private plaintiff with that name. So, we re-code all
# cases in the fjc_e data frame with federal plaintiff or defendants as the
# reason for federal jurisdiction as FED. We return to address errors caused by
# this process below.

test <- fjc_e %>%
  select(
    JURIS, PLT, DEF, PLT_typ, DEF_typ
  ) %>%
  filter(
    str_detect(DEF, "JACKSON") == T,
    JURIS == "US govt defendant"
  )

rm(test)

# We also preserve a data frame (test_pre) of all the observations coded by type
# before recoding by JURIS, to see how things change.
test_pre_raw <- fjc_e

# recode full fjc_e dataframe using JURIS data
fjc_e <- fjc_e %>%
  ungroup() %>%
  mutate(
    PLT_typ = case_when(
      JURIS == "US govt plaintiff" ~ "FED",
      TRUE ~ PLT_typ
    ),
    DEF_typ = case_when(
      JURIS == "US govt defendant" ~ "FED",
      TRUE ~ DEF_typ
    )
  )

# Examine how JURIS variable coding alters code applications ####

# first, look as small subset where DEF contains "JACKSON"

# gather pre DEF contains "JACKSON" observations
test_pre <- test_pre_raw %>%
  filter(
    str_detect(DEF, "JACKSON") == T
  ) %>%
  group_by(
    DEF_typ
  ) %>%
  mutate(
    n = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  mutate(
    pct = round(n/sum(n)*100,2)
  ) %>%
  select(
    DEF_typ, n, pct
  ) %>%
  arrange(
    DEF_typ
  )

test_post <- fjc_e %>%
  select(
    JURIS, PLT, DEF, PLT_typ, DEF_typ
  ) %>%
  filter(
    str_detect(DEF, "JACKSON") == T
  ) %>%
  group_by(
    DEF_typ
  ) %>%
  mutate(
    n_new = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  mutate(
    pct_new = round(n_new/sum(n_new)*100,2)
  ) %>%
  select(
    DEF_typ, n_new, pct_new
  ) %>%
  arrange(
    DEF_typ
  )

# compare pre-and post categorizations
test <- left_join(
  test_pre,
  test_post,
  by = "DEF_typ") %>%
  mutate(
    pp_cng = pct_new-pct
  ) %>%
  select(
    DEF_typ, n, n_new, pct, pct_new, pp_cng
  )

# RESULT: In cases where JACKSON is listed anywhere in the plaintiff name, the
# above recoding shifts 10.8 percentage points of "Unknown" defendant types to
# "FED" defendant types, with NO CHANGE on LOC and BIZ codes.

rm(test_pre, test_post, test)

# Now, compare shifts is litigant coding across entire data frame, not just for
# defendants called JACKSON.

# start with plaintiff codings
test_pre <- test_pre_raw %>%
  group_by(
    PLT_typ
  ) %>%
  mutate(
    n = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  mutate(
    pct = round(n/sum(n)*100,2)
  ) %>%
  select(
    PLT_typ, n, pct
  ) %>%
  arrange(
    PLT_typ
  )

# gather codings post-recoding with JURIS variable info
test_post <- fjc_e %>%
  select(
    PLT, PLT_typ
  ) %>%
  group_by(
    PLT_typ
  ) %>%
  mutate(
    n_new = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  mutate(
    pct_new = round(n_new/sum(n_new)*100,2)
  ) %>%
  select(
    PLT_typ, n_new, pct_new
  )

# compare pre-and post categorizations
test_plt <- left_join(
  test_pre,
  test_post,
  by = "PLT_typ") %>%
  mutate(
    pp_cng = pct_new-pct
  ) %>%
  select(
    PLT_typ, n, n_new, pct, pct_new, pp_cng
  ) %>%
  arrange(pp_cng)

rm(test_pre, test_post)

# RESULT - PLAINTIFFS:
# Very minor changes
# BIZ declines by 0.12%, blank by 0.09%,  etc.
# IND does not change.
# FED grows by 0.41%.


# now look at defednant codings
test_pre <- test_pre_raw %>%
  group_by(
    DEF_typ
  ) %>%
  mutate(
    n = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  mutate(
    pct = round(n/sum(n)*100,2)
  ) %>%
  select(
    DEF_typ, n, pct
  ) %>%
  arrange(
    DEF_typ
  )

# gather codings post-recoding with JURIS variable info
test_post <- fjc_e %>%
  select(
    DEF, DEF_typ
  ) %>%
  group_by(
    DEF_typ
  ) %>%
  mutate(
    n_new = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  mutate(
    pct_new = round(n_new/sum(n_new)*100,2)
  ) %>%
  select(
    DEF_typ, n_new, pct_new
  )

# compare pre-and post categorizations
test_def <- left_join(
  test_pre,
  test_post,
  by = "DEF_typ") %>%
  mutate(
    pp_cng = pct_new-pct
  ) %>%
  select(
    DEF_typ, n, n_new, pct, pct_new, pp_cng
  ) %>%
  arrange(pp_cng)


rm(test_pre, test_post)

# RESULT:
# Slightly more substantial changes:
# BIZ declines 1.66%
# Unknown declines 1.47%
# NGO declines 0.34%
# IND does not change
# FED grows 3.73%.


# OVERALL RESULT: recoding using JURIS variable has no substantial impact on
# coding of plaintiffs. It leads to  substantial  (3.72 percentage points)
# growth in the identification of federal defendants, mostly drawn from entities
# that had previously been coded as BIZ and Unknown, and smaller fraction from
# NGOs (0.34 percentage points)

# examine those BIZ and NGO recoding more carefully to assess possible biases
# introduced:

test_pre <- test_pre_raw %>%
  select(
    PLT_typ, DEF_typ, fjc_ID
  )

test_post <- fjc_e %>%
  select(
    PLT, PLT_typ, DEF, DEF_typ, JURIS, fjc_ID
  ) %>%
  rename(
    "PLT_typ_new" = "PLT_typ",
    "DEF_typ_new" = "DEF_typ",
  ) %>%
  left_join(
    test_pre, by = "fjc_ID"
  ) %>%
  select(
    PLT, PLT_typ, PLT_typ_new, DEF, DEF_typ, DEF_typ_new, JURIS, fjc_ID
  ) %>%
  mutate(
    PLT_typ_cng = case_when(
      PLT_typ != PLT_typ_new ~ 1,
      TRUE ~ 0
    ),
    DEF_typ_cng = case_when(
      DEF_typ != DEF_typ_new ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(
    (PLT_typ_cng == 1 |
      DEF_typ_cng == 1)
  )

# RESULT: 
# 145 plaintiff type codes change
# 1,303 defendant type codes change
    # Of those, 580 were originally coded as BIZ; manual inspection suggests a
    # mixed bag of new errors introduced and errors corrected. For example, 34
    # rows originally coded as at BIZ with the letters "INC" in them are now
    # coded as FED, wrongly, judging by the FIRST defendant name. But 146 rows
    # with the string "FOREST SER" that were originally coded as "BIZ" are now
    # correctly coded as FED. Of the 508 rows originally coded as "Unknown"
    # and now coded as "FED" The large majority seem to be secretaries (e.g. "HODEL")
    # or instances where the defendant is missing (-8). Net change is improvement
    # in accuracy. We return to address errors below. 


rm(test_def, test_plt, test_post, test_pre)

# remove pre-JURIS coding df
rm(test_pre_raw)


# Write out "Unknown" litigant codes for hand-coding ####

# export unknown plaintiffs
# !! CREATES DF WITH 9,474 obs. INSTEAD OF ~6,000 "Unknown" present in env_plt
# this is b/c of non-unique values
fjc_plt <- fjc_e %>%
  select(fjc_ID, FILEDATE, JURIS, PLT, PLT_typ, DEF, DEF_typ)%>%
  filter(
    PLT_typ == "Unknown"
  ) %>%
  group_by(PLT) %>%
  mutate(
    PLT_repeat_count = n()
  ) %>%
  ungroup() %>%
  arrange(-PLT_repeat_count, PLT)

# write .csv for unknown plaintiff
#write_csv(fjc_plt, "Data/FJC_enviro_litigant_coding/fjc_unknown_plaintiffs.csv")


# export unknown defendants
fjc_def <- fjc_e %>%
  select(fjc_ID, FILEDATE, JURIS, PLT, PLT_typ, DEF, DEF_typ)%>%
  filter(
    DEF_typ == "Unknown"
  ) %>%
  group_by(DEF) %>%
  mutate(
    DEF_repeat_count = n()
  ) %>%
  ungroup() %>%
  arrange(-DEF_repeat_count, DEF)

# write .csv for unknown defendants
#write_csv(fjc_def, "Data/FJC_enviro_litigant_coding/fjc_unknown_defendants.csv")


rm(fjc_plt, fjc_def)
### PAUSE HERE: RESL RESEARCHERS HAND CODE UNKNOWN LITIGANTS ####
# Read in hand-coded litigants; join to data ######

# read in coded unknown plaintiffs
# this file has 9809 non-unique rows; we keep only unique values (6077 rows)
env_plt_ukn <- read_csv("Data/FJC_enviro_litigant_coding/fjc_unknown_plaintiffs_coded.csv") %>%
  rename(
    "PLT_typ_new" = "PLT_typ"
  ) %>%
  group_by(
    PLT
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    PLT, PLT_typ_new
  )

# read in coded unknown defendants
# this file has 5471 non-unique rows; we keep only unique values (4257 rows)
env_def_ukn <- read_csv("Data/FJC_enviro_litigant_coding/fjc_unknown_defendants_coded.csv") %>%
    rename(
    "DEF_typ_new" = "DEF_typ"
  ) %>%
  group_by(
    DEF
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    DEF, DEF_typ_new
  )

# join manually coded values to computationally coded values
env_plt <- left_join(env_plt, env_plt_ukn, by = "PLT")
env_def <- left_join(env_def, env_def_ukn, by = "DEF")

# replace unknown values with manually coded ones where original values are
# "Unknown"

# first plaintiffs...
env_plt <- env_plt %>%
  ungroup() %>%
  mutate(
    PLT_typ = case_when(
      PLT_typ == "Unknown" ~ PLT_typ_new,
      TRUE ~ PLT_typ
    )
  ) %>%
  select(
    PLT, PLT_typ
  )

# ... now defendants
env_def <- env_def %>%
  ungroup() %>%
  mutate(
    DEF_typ = case_when(
      DEF_typ == "Unknown" ~ DEF_typ_new,
      TRUE ~ DEF_typ
    )
  )%>%
  select(
    DEF, DEF_typ
  )

rm(env_plt_ukn, env_def_ukn)

# Begin to build master plaintiff-defendant name-type dictionary ####

# Before beginning, note that there are possibilities for errors or
# discrepancies here that conflict with the JURIS code, as well as "random"
# error. Specifically, sometimes the lead plaintiff or defendant is NOT a
# federal entity, but the reason for federal jurisdiction is coded as "federal
# plaintiff" or "federal defendant". In such cases, the name of the litigant
# listed may indicate one "type" code, while the jurisdiction indicates a
# different one. As a rule, we code litigation type by names first and follow-up
# with coding cases by federal jurisdiction. In other words, we prioritize type
# codes by the name of the lead litigant, and use the reason for federal
# jurisdiction as a secondary source of information. The logic is that when a
# federal entity is not named as the lead plaintiff, the case is more centrally
# driven by other actors who are named, and should be categorized accordingly.
# There may also be other "random" sources of error that we want to correct. We
# address these in the "manual" coding below.

# Build master dictionary.

# plaintiff dic
p_type_dic <- env_plt %>%
  select(
    PLT, PLT_typ
  ) %>%
  rename(
    "Litigant" = "PLT",
    "l_typ" = "PLT_typ" 
  )

# defendant dic
d_type_dic <- env_def %>%
  select(
    DEF, DEF_typ
  ) %>%
  rename(
    "Litigant" = "DEF",
    "l_typ" = "DEF_typ" 
  )

# start dictionary
PD_type_dic <- bind_rows(
  p_type_dic,
  d_type_dic
  ) %>%
  group_by(
    Litigant, l_typ
  ) %>%
  mutate(
    n = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup()

# remove components
rm(p_type_dic, d_type_dic)

# Begin recoding and correcting litigant types
PD_type_dic <- PD_type_dic %>%
  ungroup() %>%
  mutate(
    l_typ = case_when(
      Litigant == "AMERICAN FOREST, ET AL" ~ "BIZ",
      Litigant == "OARD OF SUPERVISORS, ET AL" ~ "LOC",
      Litigant == "CALIFORNIA ENVIRONMENTAL PROTEL" ~ "STA",
      Litigant == "CENTRAL MAINE POWER" ~ "BIZ",
      Litigant == "DEPE, ET AL" ~ "STA",
      Litigant == "ENVIRONMENTAL, ET AL" ~ "FED",
      Litigant == "GROENENDYK" ~ "IND",
      Litigant == "GROUP AGAINST SMOG AND POLLUTI" ~ "CIVIC",
      Litigant == "JEA" ~ "BIZ",
      Litigant == "KIRKPATRICK" ~ "STA",
      Litigant == "METHOW VALLEY" ~ "STA",
      Litigant == "NIAGARA MOHAWK" ~ "BIZ",
      Litigant == "PARIS, ET AL" ~ "IND",
      Litigant == "PRASA" ~ "PUB_ORG",
      Litigant == "RICE" ~ "PUB_ORG",
      Litigant == "SOMERSWORTH, ET AL" ~ "LOC",
      Litigant == "SOUTHERN PACIFIC TRA, ET AL" ~ "BIZ",
      Litigant == "TAHOE REGIONAL PLANNING AGENCY" ~ "LOC",
      Litigant == "WASHINGTON, ET ALL" ~ "STA",
      TRUE ~ l_typ
    )
  )

# make further corrections to dictionary
PD_type_dic <- PD_type_dic %>%
  mutate(
    l_typ = case_when(
      str_detect(Litigant, "ARMY CORP") == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      # general first-passs corrections
      Litigant == "ALON USA ENERGY" ~ "BIZ",
      str_detect(Litigant, "CHEVRON") == T ~ "BIZ",
      Litigant == "GLENN-COLUSA IRRIGAT" ~ "LOC",
      Litigant == "KOOCANUSA INTER COALITION" ~ "CIVIC",
      str_detect(Litigant, "NESTLE") == T ~ "BIZ",
      Litigant == "NEWMONT USA LIMITED" ~ "BIZ",
      Litigant == "TEHAMA COLUSA CANAL AUT, ET AL" ~ "LOC",
      str_detect(Litigant, "DRYCLEAN") == T ~ "BIZ",
      str_detect(Litigant, "GREENPEACE") == T ~ "NGO",
      str_detect(Litigant, "SUNCOR") == T ~ "BIZ",
      Litigant == "U.S. POSTAL SERVICE, ET AL" ~ "PUB_ORG",
      # local union corrections
      str_detect(Litigant, "OPERATING ENGINEERS LOCAL") == T ~ "OTHER",
      str_detect(Litigant, "LOCAL NO. 682 HEALTH AND") == T ~ "OTHER",
      # specific name corrections
      # these come from plaintiffs
      Litigant == "AMUSSEN  JOAN AND GREGORY ETL" ~ "IND",
      Litigant == "BARGEN" ~ "IND",
      Litigant == "CALIF  STATE" ~ "STA",
      Litigant == "COALITION TO LIMIT  UNIVERSITY" ~ "CIVIC",
      Litigant == "HARLEY" ~ "IND",
      Litigant == "MCMULLEN  ETL" ~ "IND",
      Litigant == "NEW MANCHESTER RESORT  & GOLF," ~ "BIZ",
      str_detect(Litigant, "UNITED STATES POS") == T ~ "PUB_ORG",
      Litigant == "UNITED STATES P. S." ~ "PUB_ORG",
      Litigant == "UNKNOWN PLAINTIFF" ~ "OTHER",
      Litigant == "SEE ATT PAGES" ~ "OTHER",
      # these come from defendant corrections
      Litigant == "SEALED" & l_typ != "FED" ~ "OTHER",
      Litigant == "DC" ~ "OTHER",
      Litigant == "RICAHARD OGLE, ET AL, ET AL" ~ "IND",
      Litigant == "UNION SCRAP IRON&META  ETAL" ~ "BIZ",
      Litigant == "ALL-OUT  SEWER AND DRAIN SERVI" ~ "BIZ",
      # random fixes
      Litigant == "BIOKYOWA,INC" ~ "BIZ",
      Litigant == "ATLANTIC  STATES  LEGAL" ~ "NGO",
      Litigant == "ATLANTIC  RICHFIELD" ~ "BIZ",
      Litigant == "STATE  OF TEXAS" ~ "STA",
      TRUE ~ l_typ
    ),
    # second stage fixes
    l_typ = case_when(
      # for cases initially coded as FED type but where litigant name includes INC
      l_typ == "FED" & str_detect(Litigant," INC") == TRUE ~ "BIZ",
      l_typ == "FED" & str_detect(Litigant,"CORP") == TRUE &
        str_detect(Litigant,"ARMY") == FALSE & str_detect(Litigant,"CORPS") == FALSE &
        str_detect(Litigant,"CORP OF") == FALSE ~ "BIZ",
      str_detect(Litigant, state_name_dep)  == TRUE ~ "STA",
      str_detect(Litigant, state_abbr_dep)  == TRUE ~ "STA",
      str_detect(Litigant, state_abbr_st_dep)  == TRUE ~ "STA",
      TRUE ~ l_typ
    ),
    # third stage
    l_typ = case_when(
      # plaintiff types coded as FED that should be coded as other things
       str_detect(Litigant, "-8")  == TRUE ~ "UNKNOWN",
       str_detect(Litigant, "HOME DEPOT")  == TRUE ~ "BIZ", # this gets categorized as STA because contains "ME DEP"
       str_detect(Litigant, "ALASKA, STATES OF, DEPT OF TRA") == TRUE ~ "STA",
       str_detect(Litigant, "BAY INSTITUTE") == TRUE ~ "NGO",
       str_detect(Litigant, "CALIFORNIA NATIVE PLANT SOCIET") == TRUE ~ "NGO",
       str_detect(Litigant, "CYPRUS WESTERN COAL") == TRUE ~ "BIZ",
       str_detect(Litigant, "DAMASCUS CITIZENS FOR S, ET AL") == TRUE ~ "CIVIC",
       str_detect(Litigant, "DEPT OF TOXIC SUBS") == TRUE ~ "STA",
       str_detect(Litigant, "DU PONT") == TRUE ~ "BIZ",
       str_detect(Litigant, "DUPONT") == TRUE &
         str_detect(Litigant, "CITY OF") == FALSE ~ "BIZ",
       #str_detect(Litigant, "GREENPEACE U.S.A.") == TRUE ~ "NGO",
       str_detect(Litigant, "HIHIWAI STREAM RESTO") == TRUE ~ "NGO",
       str_detect(Litigant, "HONEY IS SWAMP TOURS") == TRUE ~ "BIZ",
       l_typ == "FED" & str_detect(Litigant, "LOUISIANA") == TRUE &
         str_detect(Litigant, "GIRL SCOUTS") != TRUE &
         str_detect(Litigant, "STATE") != TRUE &
         str_detect(Litigant, "ENVIRONM") != TRUE ~ "BIZ",
       str_detect(Litigant, "LOUISIANA ENVIRONMEN") == TRUE ~ "NGO",
       str_detect(Litigant, "LOUISIANA STATE") == TRUE ~ "STA",
       str_detect(Litigant, "MERISOL USA LLC") == TRUE ~ "BIZ",
       str_detect(Litigant, "NATIONAL PARKS") == TRUE ~ "NGO",
       str_detect(Litigant, "NATURAL RES. D.C.,") == TRUE ~ "NGO",
       str_detect(Litigant, "NEPA COALITION,") == TRUE ~ "NGO",
       str_detect(Litigant, "NO PARTY") == TRUE ~ "UNKNOWN",
       str_detect(Litigant, "NORTH CASCADES GRIZZ") == TRUE ~ "NGO",
       str_detect(Litigant, "OPTIMUS STEEL, LLC") == TRUE ~ "BIZ",
       str_detect(Litigant, "ROBERT W. HALL VS U.S. DEPARTM") == TRUE ~ "IND",
       str_detect(Litigant, "RUSSO DEVELOPMENT") == TRUE ~ "BIZ",
       str_detect(Litigant, "VOYAGEURS NATL PARK") == TRUE ~ "NGO",
       TRUE ~ l_typ
    ),
    # fourth stage
    l_typ = case_when(
      # defendant types coded as FED that should be coded as other things
      str_detect(Litigant, "-8")  == TRUE ~ "UNKNOWN",
       str_detect(Litigant, "ACQUEST")  == TRUE ~ "BIZ",
       str_detect(Litigant, "ARCELORMITTAL") == TRUE ~ "BIZ",
       str_detect(Litigant, "AZ TRANS DEPT OF, ET AL") == TRUE ~ "STA",
       str_detect(Litigant, "AZUSA PIPE AND TUBE BENDING CO") == TRUE ~ "BIZ",
       #str_detect(Litigant, "CHEVRON USA PROD CO") == TRUE ~ "BIZ",
       #str_detect(Litigant, "CHURCHILL DOWNS LOUISIANA HORS") == TRUE ~ "BIZ",
       str_detect(Litigant, "COLUMBUS MCKINNON CO") == TRUE ~ "BIZ",
       str_detect(Litigant, "CRAIN POWER TUCUMCARTI") == TRUE ~ "BIZ",
       str_detect(Litigant, "CYPRUS") == TRUE ~ "BIZ",
       str_detect(Litigant, "DEGUSSA INITIATORS") == TRUE ~ "BIZ",
       #str_detect(Litigant, "E I DUPONT DENEMOURS, ET AL") == TRUE ~ "BIZ",
       #str_detect(Litigant, "E. I. DU PONT DE NEMOURS AND C") == TRUE ~ "BIZ",
       #str_detect(Litigant, "E.I. DU PONT DE NEMOURS, ET AL") == TRUE ~ "BIZ",
       #str_detect(Litigant, "E.I. DU PONT DE NEMOURS AND CO") == TRUE ~ "BIZ",
       #str_detect(Litigant, "E.I. DUPONT DE NEMOURS AND COM") == TRUE ~ "BIZ",
       #str_detect(Litigant, "EI DUPONT DE NEMOURS, ET AL") == TRUE ~ "BIZ",
       str_detect(Litigant, "ELSA SKINNER MORGAN, ET AL") == TRUE ~ "BIZ",
       str_detect(Litigant, "CHEMICAL") == TRUE &
        str_detect(Litigant, "U.S.") == FALSE ~ "BIZ",
       str_detect(Litigant, "EXXON") == TRUE ~ "BIZ",
       str_detect(Litigant, "STEEL") == TRUE ~ "BIZ",
       str_detect(Litigant, "GLOUCESTER ENV MA") == TRUE ~ "LOC",
       str_detect(Litigant, "INDUS") == TRUE ~ "BIZ",
       str_detect(Litigant, "HAILI CHRISTIAN SCHOOL") == TRUE ~ "NGO",
       str_detect(Litigant, "IDAHO TRANSPORT DEPT, ET AL") == TRUE ~ "STA",
       str_detect(Litigant, "INEOS USA LLC") == TRUE ~ "BIZ",
       str_detect(Litigant, "INTERSTATE NON, ET AL") == TRUE ~ "BIZ",
       str_detect(Litigant, "JOINT MEETING OF ESSEX AND") == TRUE ~ "STA",
       str_detect(Litigant, "KELCOURSE ET AL") == TRUE ~ "IND",
       #str_detect(Litigant, "LOUISIANA GENERATING, L, ET AL") == TRUE ~ "BIZ",
       #str_detect(Litigant, "LOUISIANA MIDLAND TRANSPORT CO") == TRUE ~ "BIZ",
       #str_detect(Litigant, "LOUISIANA ONSHORE PROPERTIES L") == TRUE ~ "BIZ",
       #str_detect(Litigant, "LOUISIANA-PACIFIC") == TRUE ~ "BIZ",
       str_detect(Litigant, "MT ST DEPT ENVIRON, ET AL") == TRUE ~ "STA",
       str_detect(Litigant, "N Y STATE DEPT OF TRANS") == TRUE ~ "STA",
       str_detect(Litigant, "QUESTAR GAS MANAGEMENT, ET AL") == TRUE ~ "BIZ",
      str_detect(Litigant, "STAUFFER MANAGEMENT") == TRUE ~ "BIZ",
      str_detect(Litigant, "RESOURCE MANAGEMENT, ET AL") == TRUE ~ "BIZ",
      str_detect(Litigant, "MANAGEMENT RECRUITERS OF NEW O") == TRUE ~ "BIZ",
      str_detect(Litigant, "WASTE MANAGEMENT OF NEW, ET AL") == TRUE ~ "BIZ",
       #str_detect(Litigant, "SHINTECH LOUISIANA LLC") == TRUE ~ "BIZ",
       str_detect(Litigant, "SOUTHEASTERN UTAH OHV CLUB") == TRUE ~ "NGO_O",
       str_detect(Litigant, "U S CERAMIC TILE CO") == TRUE ~ "BIZ",
       str_detect(Litigant, "VERTELLUS AGRICULTURE & NUTRIT") == TRUE ~ "BIZ",
       str_detect(Litigant, "WASHINGTON STATE DEPARTMENT OF") == TRUE ~ "STA",
       TRUE ~ l_typ
    ),
    # fifth stage corrections
    l_typ = case_when(
      Litigant == "BRIGGS MORRIS-SMITH" ~ "IND",
      Litigant == "DALMIDA SMITH" ~ "IND",
      Litigant == "DOUGLAS D. SMITH, ET AL" ~ "IND",
      Litigant == "ERING SMITH" ~ "IND",
      Litigant == "ESTATE OF JAMES M. SMITH" ~ "IND",
      Litigant == "F SMITH" ~ "IND",
      Litigant == "GOLDSMITH, ET AL" ~ "IND",
      Litigant == "HAROLD T. SMITH" ~ "IND",
      Litigant == "HOWARD C SMITH, ET AL" ~ "IND",
      Litigant == "J. RICHARD SMITH" ~ "IND",
      Litigant == "JERRY SMITH" ~ "IND",
      Litigant == "KELLY SMITH" ~ "IND",
      Litigant == "REUBEN SMITH RUBBISH, ET AL" ~ "BIZ",
      Litigant == "SMITH" ~ "IND",
      Litigant == "SMITH ETAL" ~ "IND",
      Litigant == "SMITH FROZEN FOODS" ~ "BIZ",
      Litigant == "SMITH INTERNATL" ~ "BIZ",
      Litigant == "SMITH LAND & IMPROV" ~ "BIZ",
      Litigant == "SMITH LAND IMPROVEMENT" ~ "BIZ",
      Litigant == "SMITH, ET AL" ~ "IND",
      Litigant == "SMITH, MAX R." ~ "IND",
      Litigant == "SMITH, SHERMAN C." ~ "IND",
      Litigant == "SMITH'S FOOD AND" ~ "BIZ",
      str_detect(Litigant, "SMITHFIELD FOODS")  == TRUE ~ "BIZ",
      Litigant == "SMITHSON, WILFRED E., ET AL" ~ "IND",
      Litigant == "TRUMP, ET AL" ~ "FED",
      str_detect(Litigant, "CONSER")  == TRUE ~ "NGO",
      str_detect(Litigant, "RESOURCES DEF")  == TRUE ~ "NGO",
      str_detect(Litigant, "NATURAL RES DEF COU")  == TRUE ~ "NGO",
      str_detect(Litigant, "RESOURCES COUN")  == TRUE ~ "NGO",
      str_detect(Litigant, "POLLUTION COALITION")  == TRUE ~ "NGO",
      str_detect(Litigant, "COALI")  == TRUE ~ "NGO",
      str_detect(Litigant, "BLASKE MARINE")  == TRUE ~ "BIZ",
      str_detect(Litigant, "LEGAL")  == TRUE ~ "NGO",
      str_detect(Litigant, "KEEPER")  == TRUE ~ "NGO",
      str_detect(Litigant, "SAVE THE")  == TRUE ~ "NGO",
      str_detect(Litigant, "LEAGU")  == TRUE ~ "NGO",
      str_detect(Litigant, "MINE CO")  == TRUE ~ "BIZ",
      str_detect(Litigant, "COMMUNITIES FOR")  == TRUE ~ "CIVIC",
      #str_detect(Litigant, "LOUISIANA, ET AL")  == TRUE ~ "FED",
      str_detect(Litigant, " BANK")  == TRUE ~ "BIZ",
      str_detect(Litigant, "COUNCIL ON ENVIRONMENTA, ET AL")  == TRUE ~ "NGO",
      #str_detect(Litigant, "COUNCIL")  == TRUE & str_detect(Litigant, "ENV")  == TRUE ~ "NGO",
      str_detect(Litigant, "IRRITAT")  == TRUE ~ "CIVIC",
      str_detect(Litigant, "UNITED STATES  OF AMERICA")  == TRUE ~ "FED",
      str_detect(Litigant, "RIVERS UNLIMITED")  == TRUE ~ "NGO",
       str_detect(Litigant, "TROUT UNLIMITED")  == TRUE ~ "NGO",
      str_detect(Litigant, "PORTNEUF ENVIRONMENT")  == TRUE ~ "NGO",
      str_detect(Litigant, "VUI")  == TRUE ~ "BIZ",
      str_detect(Litigant, "ANGLERS CONSERVATION")  == TRUE ~ "NGO",
      str_detect(Litigant, "ANGLERS OF THE")  == TRUE ~ "NGO",
      str_detect(Litigant, "WATERSHED")  == TRUE ~ "NGO",
      str_detect(Litigant, "LAW FO")  == TRUE ~ "NGO",
      str_detect(Litigant, "EAST MEETS WEST EXCURSI")  == TRUE ~ "BIZ",
      #str_detect(Litigant, "LAW FO")  == TRUE ~ "NGO",
      str_detect(Litigant, "GAUTIER FAMILY SPORTS")  == TRUE ~ "BIZ",
      str_detect(Litigant, "COLLIER, ET AL")  == TRUE ~ "BIZ",
      str_detect(Litigant, "PROTECTION COMM")  == TRUE ~ "NGO",
      str_detect(Litigant, "FRIENDS OF THE EARTH, INC.")  == TRUE ~ "NGO",
      str_detect(Litigant, "EARTH PROTECTOR INC")  == TRUE ~ "NGO",
      str_detect(Litigant, "AUDUBON")  == TRUE ~ "NGO",
      str_detect(Litigant, "PRAIRIE ISLAND INDIAN COMMUNIT")  == TRUE ~ "TRIBE",
      str_detect(Litigant, "BAND") == TRUE &
        (str_detect(Litigant, "COAL") == FALSE | str_detect(Litigant, "INDUSTRIES")) == FALSE  ~ "TRIBE",
      str_detect(Litigant, "IRRIGATION DI")  == TRUE ~ "PUB_ORG",
      str_detect(Litigant, "GOVT OF V I")  == TRUE ~ "STA",
      str_detect(Litigant, "COMMITTEE FOR PRESERV ETAL")  == TRUE ~ "FED",
      TRUE ~ l_typ
    )
  )

# Make still further corrections, related to USA parties
PD_type_dic <- PD_type_dic %>%
  mutate(
    l_typ = case_when(
      Litigant == "UNITED STATES OF  AMERICA" ~ "FED",
      Litigant == "UNITED  STATES" ~ "FED",
      Litigant == "UNITED  STATES OF AMERICA" ~ "FED",
      Litigant == "UNITED STATES OF  AMERI, ET AL" ~ "FED",
      Litigant == "UNITED STATES  OF AMERI, ET AL" ~ "FED",
      TRUE ~ l_typ
    )
  )

# Handle litigants that start with a number, virtually all of which we will
# classify as BIZ
PD_type_dic <- PD_type_dic %>%
  mutate(
    l_typ = case_when(
      str_detect(Litigant, "^[0-9]") == TRUE ~ "BIZ",
      TRUE ~ l_typ
    )
  )

# Unify coding of utilities, authorities, ports, USPS, civic groups, schools ####

# Utilities are a challenging entity to classify: they are sometimes private firms,
# sometimes public orgs, and sometimes local cooperatives. Further, "utility
# districts" or "authorities" are different from utility companies. Here, we
# examine utilities in the dictionary and make any coding corrections necessary.
# Generally, we consider utility districts and authorities as LOC (local
# governmental bodies) and utility companies as businesses (BIZ), including
# cooperatives. We also label ports as LOC.
PD_type_dic <- PD_type_dic %>%
  mutate(
    l_typ = case_when(
      str_detect(Litigant, "DISTRICT") == TRUE ~ "LOC",
      TRUE ~ l_typ
    ),
    #code all authorities "LOCAL"
    l_typ = case_when(
      str_detect(Litigant, "AUTH") == TRUE ~ "LOC",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      Litigant == "CHESAPEAKE UTILITIES" ~ "BIZ",
      Litigant == "PUBLIC UTILITY DIST" ~ "LOC",
      Litigant == "CHISHOLM CREEK UTILITY AUTHORI" ~ "LOC",
      Litigant == "YOSEMITE SPRINGS PARK UTILITY" ~ "LOC",
      Litigant == "PUBLIC UTILITY DISTRICT NO. 1" ~ "LOC",
      Litigant == "EMERALD COAST UTILITIES AUTHOR" ~ "LOC",
      Litigant == "KA CTY BD PUBLIC UTIL" ~ "LOC",
      Litigant == "HENDERSONVILLE UTIL TENN" ~ "LOC",
      Litigant == "PB WASTE WATER UTIL COMM, ETAL" ~ "LOC",
      Litigant == "NATIONAL UTILITIES" ~ "BIZ",
      Litigant == "CITIZENS UTILITIES CO" ~ "BIZ",
      Litigant == "SANLANDO UTILITIES" ~ "BIZ",
      Litigant == "CITIZENS UTILITIES" ~ "BIZ",
      Litigant == "DUNCAN PUB UTILITIES" ~ "LOC",
      Litigant == "PUBLIC UTILITIES" ~ "LOC",
      Litigant == "DALTON UTILITIES, ET AL" ~ "BIZ",
      Litigant == "PUBLIC UTILITY COMM." ~ "LOC",
      Litigant == "CARROLLTON UTILITIES, ET AL" ~ "BIZ",
      Litigant == "PUBLIC UTILITY" ~ "LOC",
      Litigant == "UTILITIES BD OF SYLA" ~ "LOC",
      Litigant == "COLORADO SPRINGS UTILITIES" ~ "BIZ",
      Litigant == "RURAL UTILITIES SERVICE, ET AL" ~ "FED",
      Litigant == "TUOLUMNE UTILITIES DISTRICT" ~ "LOC",
      Litigant == "LAMAR UTILITIES BOARD, ET AL" ~ "LOC",
      Litigant == "MARIPOSA PUBLIC UTILITY DISTRI" ~ "LOC",
      Litigant == "FIREBAUGH CANAL WATER DISTRICT" ~ "LOC",
      Litigant == "BI-STATE MO-IL DIST" ~ "LOC",
      Litigant == "RURAL WATER DIST 8" ~ "LOC",
      Litigant == "RECLAMATION DISTRICT, ET AL" ~ "LOC",
      Litigant == "WY ASSOC CONSR DIST, ET AL" ~ "LOC",
      Litigant == "PORT WASHINGTON WATER DISTRICT" ~ "LOC",
      Litigant == "AMA DISTRICT, ET AL" ~ "NGO_O",
      Litigant == "AMA DISTRICT 37, ET AL" ~ "NGO_O",
      Litigant == "U.S. DISTRICT COURT, ET AL" ~ "FED",
      Litigant == "DISTRICT OF COLUMBIA" ~ "LOC",
      Litigant == "TURNPIKE DIST CTR" ~ "LOC",
      Litigant == "CAPITAL DIST. YOUTH" ~ "NGO_O",
      Litigant == "HOLIDAY SHORES SANITARY DISTRI" ~ "LOC",
      Litigant == "WESTLANDS WATER DIST, ET AL" ~ "LOC",
      Litigant == "WALDO MINING DISTRICT" ~ "LOC",
      Litigant == "SOUTH FARMINGDALE WATER DISTRI" ~ "LOC",
      Litigant == "OAKDALE IRRIGATION DIST, ET AL" ~ "LOC",
      Litigant == "PUBLIC UTILITY DISTRICT NO. 1" ~ "LOC",
      Litigant == "CARLE PLACE WATER DISTRICT" ~ "LOC",
      Litigant == "RICHARDSON BAY SANITARY DISTRI" ~ "LOC",
      Litigant == "THE COOPERATIVE DISTRICT OF TH" ~ "LOC",
      Litigant == "NORTHSHORE HARBOR CENTER DISTR" ~ "LOC",
      Litigant == "SAN DIEGO UNIFIED PORT DISTRIC" ~ "PUB_ORG",
      Litigant == "LAFOURCHE FIRE PROTECTION DIST" ~ "LOC",
      Litigant == "LAFOURCHE PARISH DRAINAGE DIST" ~ "LOC",
      Litigant == "HANCOCK WATER AND SEWER DISTRI" ~ "LOC",
      Litigant == "BEACH MOSQUITO CONTROL DISTRIC" ~ "LOC",
      Litigant == "COOPERATIVE DISTRICT OF THE CI" ~ "LOC",
      Litigant == "WAUKEGAN PORT DISTRICT" ~ "PUB_ORG",
      Litigant == "KLAMATH IRRIGATION DISTRICT" ~ "LOC",
      Litigant == "PORLAND WATER DISTRICT" ~ "LOC",
      Litigant == "MT. VIEW SANITARY DISTRIC" ~ "LOC",
      Litigant == "VASHON SEWER DIST" ~ "LOC",
      Litigant == "MADISON METRO. SEWERAGE DISTRI" ~ "LOC",
      Litigant == "VASHON SEWER DISTRIC" ~ "LOC",
      Litigant == "RENS.CO.SEWER DIST.1" ~ "LOC",
      Litigant == "TALENT IRRIGATION DISTRICT" ~ "LOC",
      Litigant == "ORCHARD MESA DIST, ET AL" ~ "LOC",
      Litigant == "FOREST PRESERVE DISTRICT OF DU" ~ "LOC",
      Litigant == "FOREST PRESERVE DISTRIC, ET AL" ~ "LOC",
      Litigant == "BEAUREGARD WATER WORKS DISTRIC" ~ "LOC",
      Litigant == "MARIPOSA PUBLIC UTILITY DISTRI" ~ "LOC",
      Litigant == "22ND DISTRICT AGRICULTURAL ASS" ~ "NGO_O",
      Litigant == "WINCHESTER WATER CONTROL DISTR" ~ "LOC",
      Litigant == "AUTH COMPANY, ET AL" ~ "BIZ",
      Litigant == "THE M/V AUTHOR" ~ "BIZ",
      Litigant == "CHAUTHANI, ET AL" ~ "IND",
      Litigant == "METRO DENVER SEWAGE DISPOSAL" ~ "LOC",
      Litigant == "DELCORA SEWAGE, ET AL" ~ "LOC",
      Litigant == "CLEAN TREATMENT SEWAGE CO" ~ "BIZ",
      Litigant == "AIRPORT IMPACT, ET AL" ~ "BIZ",
      Litigant == "AIRPORT WKG GRP O/C, ET AL" ~ "NGO_O",
      Litigant == "AIRPORT WORKING GRP, ET AL" ~ "NGO_O",
      Litigant == "CITIZENS FOR AIRPORT, ET AL" ~ "BIZ",
      Litigant == "CARIBBEAN AIRPORT, ET AL" ~ "BIZ",
      Litigant == "COMMITTEE TO STOP AIRPORT EXPA" ~ "CIVIC",
      Litigant == "FRIENDS OF RIVERSIDE AIRPORT L" ~ "CIVIC",
      Litigant == "FRIENDS OF RIVERSIDE AIRPORT," ~ "CIVIC",
      Litigant == "JOHNSON CTY AIRPORT" ~ "LOC",
      Litigant == "METROPLITAN AIRPORTS" ~ "LOC",
      Litigant == "RICHLAND-LEXINGTON AIRPORT DIS" ~ "LOC",
      Litigant == "SANFORD AIRPORT" ~ "LOC",
      Litigant == "ST PAUL AIRPORT NOISE ETAL" ~ "CIVIC",
      TRUE ~ l_typ
    )
  )

# convert all PUB_ORG codes to LOC when not colleges or universities or
# hospitals or schools; then correct errors
PD_type_dic <- PD_type_dic %>%
  mutate(
    l_typ = case_when(
      l_typ == "PUB_ORG" &
          (str_detect(Litigant, "UNIV") == F |
             str_detect(Litigant, "HOS") == F |
             str_detect(Litigant, "SCHOOL") == F) ~ "LOC",
      l_typ == "PUB_ORG" &
        str_detect(Litigant, "LIM") == T ~ "BIZ",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      Litigant == "ABLE TERMITE" ~ "BIZ",
      Litigant == "AIRPORT SHOPPING" ~ "BIZ",
      Litigant == "ALAMITOS BAY PARTN" ~ "BIZ",
      Litigant == "AMERICAN FAST PRINT LIMITED" ~ "BIZ",
      Litigant == "ARMITAGE, ET AL" ~ "IND",
      Litigant == "BLUMENTHAL POWER" ~ "BIZ",
      Litigant == "CALIFORNIA STATE UNI, ET AL" ~ "PUB_ORG",
      Litigant == "CALIFORNIA-AMERICAN" ~ "BIZ",
      Litigant == "ECUA, ET AL" ~ "UNKNOWN",
      Litigant == "EDMOND WOLF SANCTUARY" ~ "BIZ",
      Litigant == "EDMONDS INSTITUTE, ET AL" ~ "NGO",
      Litigant == "FEDERAL PACIFIC" ~ "BIZ",
      Litigant == "FIFTH AND MITCHELL, ET AL" ~ "BIZ",
      Litigant == "FORT ORD TOXICS PROJ, ET AL" ~ "NGO",
      Litigant == "FORT WALTON BEACH MEDICAL CENT" ~ "PUB_ORG",
      Litigant == "FRIENDS OF YOSEMITE VALLEY" ~ "NGO",
      Litigant == "FRIENDS OF YOSEMITE, ET AL" ~ "NGO",
      Litigant == "HOLY CROSS" ~ "PUB_ORG",
      Litigant == "MAGNOLIA VALLEY PLANTAT, ET AL" ~ "BIZ",
      Litigant == "MINNESOTA POWER" ~ "BIZ",
      Litigant == "MIT" ~ "PUB_ORG",
      Litigant == "MIT, ET AL" ~ "PUB_ORG",
      Litigant == "MITCHELL, LARRY D." ~ "IND",
      Litigant == "MITCHELL, ALBERT, JR." ~ "IND",
      Litigant == "MITCHELL, ET AL" ~ "IND",
      Litigant == "MITCHESON, ET AL" ~ "IND",
      Litigant == "MITEV" ~ "BIZ",
      Litigant == "MITRENGA, ET AL" ~ "IND",
      Litigant == "MITTELBACH, ET AL" ~ "IND",
      Litigant == "MITTENTHAL, SUZANNE" ~ "IND",
      Litigant == "PHX UNION HIGH, ET AL" ~ "PUB_ORG",
      Litigant == "PLEASANTON GARBAGE SERV, ET AL" ~ "BIZ",
      Litigant == "PRESTOLITE, ET AL" ~ "BIZ",
      Litigant == "RICE" ~ "PUB_ORG",
      Litigant == "RICHARD MITZELFELT ETAL" ~ "IND",
      Litigant == "ROYAL  MITCHELL" ~ "IND",
      Litigant == "RUTGERS" ~ "PUB_ORG",
      Litigant == "SCHMITT, ET AL" ~ "IND",
      Litigant == "SCHMITTEN" ~ "IND",
      Litigant == "SCHMITTEN, ET AL" ~ "IND",
      Litigant == "SOMITEX PRINTS" ~ "BIZ",
      Litigant == "SUMMIT MACHINE TOOL" ~ "BIZ",
      Litigant == "SUMMIT MIDSTREAM PARTNE, ET AL" ~ "BIZ",
      Litigant == "SUMMIT NATL FAC TRST" ~ "BIZ",
      Litigant == "SUMMIT RESOURCES" ~ "BIZ",
      Litigant == "SUMMITT" ~ "BIZ",
      Litigant == "TAPLF" ~ "STA",
      Litigant == "TAPLF ETL" ~ "STA",
      Litigant == "TENNESSEE VALLEY AUT, ET AL" ~ "FED",
      Litigant == "TRANSOCEAN LIMITED, ET AL" ~ "BIZ",
      Litigant == "TRAVERSE BAY AREA INTERMEDIATE" ~ "PUB_ORG",
      Litigant == "TVA, ET AL" ~ "FED",
      Litigant == "U.S. POSTAL SERVICE, ET AL" ~ "PUB_ORG",
      Litigant == "UA, ET AL" ~ "PUB_ORG",
      Litigant == "UNITED STATES P. S." ~ "PUB_ORG",
      Litigant == "UNITED STATES POS" ~ "PUB_ORG",
      Litigant == "UNITED STATES POSTAL" ~ "PUB_ORG",
      Litigant == "UNITED STATES POSTAL SERVICE" ~ "PUB_ORG",
      Litigant == "UNIVERAL SCIENTIFIC" ~ "BIZ",
      Litigant == "UNIVERSAL CROP PROTECTION ALLI" ~ "BIZ",
      Litigant == "UNIVERSAL PRINTING AND , ET AL" ~ "BIZ",
      Litigant == "UNIVERSAL WELDING & FABRICATIO" ~ "BIZ",
      Litigant == "UNIVERSALY DRY CLEANING, ET AL" ~ "BIZ",
      Litigant == "UNIVERSITY EXCHANGE, ET AL" ~ "BIZ",
      Litigant == "URI" ~ "PUB_ORG",
      Litigant == "WHARTON" ~ "PUB_ORG",
      Litigant == "WOODLAND PARK ZOOLOGICA, ET AL" ~ "NGO",
      TRUE ~ l_typ
    )
  )

# Unify Postal Service Coding
# We treat the postal service as a unit of the federal government
PD_type_dic <- PD_type_dic %>%
  mutate(
    l_typ = case_when(
      Litigant == "US POSTAL SVC, ET AL" ~ "FED",
      Litigant == "U.S. POSTAL SERVICE" ~ "FED",
      Litigant == "U.S. POSTAL SERVICE, ET AL" ~ "FED",
      Litigant == "U.S.POSTAL SERVICE" ~ "FED",
      Litigant == "US POSTAL SERVICE" ~ "FED",
      Litigant == "UNITED STATES POSTAL SERVICE" ~ "FED",
      Litigant == "UNITED STATES POSTAL" ~ "FED",
      Litigant == "UNITED STATES P. S." ~ "FED",
      Litigant == "UNITED STATES POS" ~ "FED",
      Litigant == "USPS" ~ "FED",
      TRUE ~ l_typ
    )
  )

# all remaining pub_org codes are schools/universities. re-classify as OTHER
# (This includes "TRAVERSE BAY AREA INTERMEDIATE", which was a school/sch dist.)
PD_type_dic <- PD_type_dic %>%
  mutate(
    l_typ = case_when(
      l_typ == "PUB_ORG" ~ "OTHER",
      TRUE ~ l_typ
    )
  )


# Code federal agency heads who are listed by name ####
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

# look for names in PLT; recode PLT_typ as FED if any of PLT words match agency heads list
PD_type_dic <- PD_type_dic %>%
  mutate(
    l_typ = case_when(
      Litigant %in% agency_heads ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      str_detect(Litigant, agency_heads_et_al) == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      str_detect(Litigant, agency_heads_etal) == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      str_detect(Litigant, agency_heads_eta) == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      str_detect(Litigant, agency_heads_et_a) == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      str_detect(Litigant, agency_heads_et) == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      str_detect(Litigant, agency_heads_et_al_c) == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      str_detect(Litigant, agency_heads_etal_c) == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      str_detect(Litigant, agency_heads_eta_c) == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
     str_detect(Litigant, agency_heads_et_a_c) == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      str_detect(Litigant, agency_heads_et_c) == TRUE ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      Litigant %in% agency_heads ~ "FED",
      TRUE ~ l_typ
    ),
    l_typ = case_when(
      Litigant %in% agency_heads ~ "FED",
      TRUE ~ l_typ
    )
  )

# Check for uncoded (blank) values in dictionary & code where necessary ####

# look for an uncoded names/types in master list
PD_type_dic_uncoded <- PD_type_dic %>%
  filter(
    str_length(l_typ) <= 2
  )

PD_type_dic <- PD_type_dic %>%
  mutate(
    l_typ = case_when(
      Litigant == "UNITED STATES  OF  AMER, ET AL" ~ "FED",
      Litigant == "BABBITT, BURCE  ETAL" ~ "FED",
      TRUE ~ l_typ
    )
  )

# remove inspection df after corrections
rm(PD_type_dic_uncoded)

# Check for NA values in dictionary & code where necessary ####

# make df of NAs
PD_type_dic_NA <- PD_type_dic %>%
  filter(
    is.na(l_typ)
  )

# drop NAs from primary dictionary
PD_type_dic <- PD_type_dic %>%
  filter(
    !is.na(l_typ)
  )
# Many NAs are companies that end in " CO", as in, "Pepsi Co".
# Code " CO" as BIZ in NAs df
PD_type_dic_NA <- PD_type_dic_NA %>%
  rowwise() %>%
  mutate(
    l_typ = case_when(
      str_detect(Litigant, " CO") == TRUE ~ "BIZ",
      TRUE ~ l_typ
    )
  )
# filter just newly coded BIZ codes
PD_type_dic_co <- PD_type_dic_NA %>%
  filter(
   !is.na(l_typ)
  )
# append non-NAs back to primary dictionary
PD_type_dic <- bind_rows(
  PD_type_dic,
  PD_type_dic_co
  )
# remove " CO" values from NAs df (keep only remianing NAs); drop unneeded df
PD_type_dic_NA <- PD_type_dic_NA %>%
  filter(
    is.na(l_typ)
  )
rm(PD_type_dic_co)

# manually code non-names
PD_type_dic_NA <- PD_type_dic_NA %>%
  mutate(
    l_typ = case_when(
      Litigant == "EAST HAVEN" ~ "LOC",
      Litigant == "AL TECH SPECIALTY" ~ "BIZ",
      Litigant == "CHESAPEAKE BAY ETAL" ~ "NGO",
      Litigant == "SEABOARD SYS RR" ~ "BIZ",
      Litigant == "STATE TEXAS" ~ "STA",
      Litigant == "TAHOE" ~ "UNKNOWN",
      Litigant == "SACRA REGI CTY SANITA" ~ "LOC",
      Litigant == "PEAGEMOVES AND ALL OTHERS" ~ "UNKNOWN",
      Litigant == "IKUNI TRAD" ~ "TRIBE",
      Litigant == "PYRAMID LAKE PAIUTE" ~ "TRIBE",
      Litigant == "DOZIER & MCGOUGH" ~ "BIZ",
      Litigant == "SARASOTA" ~ "LOC",
      Litigant == "NAT AUDURON SOC" ~ "NGO",
      Litigant == "AMERICAN EXPLORATION & , ET AL" ~ "BIZ",
      Litigant == "DAKOTA RURAL ACTION, ET AL" ~ "NGO",
      Litigant == "HEALTHY GULF, ET AL" ~ "NGO",
      Litigant == "BUFFALO FIELD CAMPAIGN, ET AL" ~ "NGO",
      Litigant == "TEXAS ENVIRONMENTAL JUS, ET AL" ~ "NGO",
      Litigant == "RED LAKE BAND OF CHIPPE, ET AL" ~ "TRIBE",
      Litigant == "SEAFREEZE SHORESIDE, IN, ET AL" ~ "BIZ",
      Litigant == "ATLANTIC SALMON FEDERAT, ET AL" ~ "NGO",
      Litigant == "NANTUCKET RESIDENTS AGA, ET AL" ~ "CIVIC",
      Litigant == "PETERSON - PURITAN SITE OU 2 R" ~ "BIZ",
      Litigant == "TEXAS CAMPAIGN FOR THE , ET AL" ~ "NGO",
      Litigant == "ENVIRONMENTAL LAW & POL, ET AL" ~ "NGO",
      Litigant == "NATURAL LAND INSTITUTE" ~ "NGO",
      Litigant == "1500 5TH STREET PARTNER, ET AL" ~ "BIZ",
      Litigant == "RED CLIFF BAND OF LAKE , ET AL" ~ "TRIBE",
      Litigant == "GALLAGHER & KENNEDY PA" ~ "BIZ",
      Litigant == "THE SUCCESSOR AGENCY TO THE FO" ~ "LOC",
      Litigant == "THE BOARD OF TRUSTEES OF THE L" ~ "OTHER",
      Litigant == "CALIFORNIA OPEN LANDS" ~ "NGO",
      Litigant == "1431 HUNTINGTON DRIVE L, ET AL" ~ "BIZ",
      Litigant == "ENDANGERED HABITATS LEA, ET AL" ~ "NGO",
      Litigant == "HOMEFED VILLAGE III MASTER, LL" ~ "BIZ",
      Litigant == "EHUI MALAMA AINA O LA'IE, ET AL" ~ "NGO",
      Litigant == "SAWTOOTH MOUNTAIN RANCH, ET AL" ~ "BIZ",
      Litigant == "THE ASSINIBOINE AND SIOUX TRIB" ~ "TRIBE",
      Litigant == "WESTERN ORGANIZATION OF, ET AL" ~ "NGO",
      Litigant == "FLATHEAD-LOLO-BITTERROOT CITIZ" ~ "NGO",
      Litigant == "KLAMATH IRRIGATION DISTRICT" ~ "LOC",
      Litigant == "NO MORE FREEWAYS, ET AL" ~ "NGO",
      Litigant == "OKANOGAN HIGHLANDS ALLI, ET AL" ~ "NGO",
      Litigant == "HANFORD CHALLENGE, ET AL" ~ "NGO",
      Litigant == "FISH NORTHWEST" ~ "NGO",
      Litigant == "LIVABLE LAKE STEVENS" ~ "NGO",
      Litigant == "ATTORNEY GENERAL OF THE, ET AL" ~ "FED",
      Litigant == "1999 CAMPBELLTON ROAD, , ET AL" ~ "BIZ",
      Litigant == "CRYSTAL LAKE GARDEN CLU, ET AL" ~ "NGO",
      Litigant == "ENVIRONMENTAL PROT AGCY" ~ "FED",
      Litigant == "PORTECCION TECNICA" ~ "BIZ",
      Litigant == "---------------------------" ~ "UNKNOWN",
      Litigant == "BEAVER SMELTING ETTAL" ~ "BIZ",
      Litigant == "JOINT MEETING-RUTHERFORD" ~ "LOC",
      Litigant == "RAPID AMER" ~ "BIZ",
      Litigant == "DONEGAL MUTUAL INS" ~ "BIZ",
      Litigant == "BETHLEHEM STEE" ~ "BIZ",
      Litigant == "UNMOVIN UTILITIES" ~ "BIZ",
      Litigant == "DALLAS DEMOLITION EXCAVAT" ~ "BIZ",
      Litigant == "PERFORACIONES MARINAS" ~ "BIZ",
      Litigant == "TX MUN POWER AGNEY" ~ "BIZ",
      Litigant == "KURDZIEL IRON ETAL" ~ "BIZ",
      Litigant == "BD OF TRUSTEES JACKSON" ~ "LOC",
      Litigant == "MIDWEST ASBESTOS ETAL" ~ "BIZ",
      Litigant == "ASBESTOS SAFETY" ~ "BIZ",
      Litigant == "UNITED TECH IN C" ~ "BIZ",
      Litigant == "PROVIMI VEAL ETAL" ~ "BIZ",
      Litigant == "AMERICAN BRASS LP" ~ "BIZ",
      Litigant == "BETHLEHEM STE" ~ "BIZ",
      Litigant == "INTERNATIONAL JENSEN" ~ "BIZ",
      Litigant == "LEE M THOMAS, EPA" ~ "FED",
      Litigant == "TAHOE REGIONAL" ~ "LOC",
      Litigant == "TAHOE REI" ~ "LOC",
      Litigant == "ATLAS MINERALS DIVISION" ~ "BIZ",
      Litigant == "BROOKFIELD RENEWABLE PA, ET AL" ~ "BIZ",
      Litigant == "PAUL REVERE TRANSPORTAT, ET AL" ~ "BIZ",
      Litigant == "GREATER NEW HAVEN WATER POLLUT" ~ "NGO",
      Litigant == "ALL-STAR TRANSPORTATION, ET AL" ~ "BIZ",
      Litigant == "MIDDLETOWN" ~ "LOC",
      Litigant == "BURLINGTON" ~ "LOC",
      Litigant == "REDDING" ~ "LOC",
      Litigant == "RIDGEFIELD" ~ "LOC",
      Litigant == "STILLWELL READY MIX & B, ET AL" ~ "BIZ",
      Litigant == "NYS OFFICE OF PARKS, RE, ET AL" ~ "STA",
      Litigant == "UNITED STATES ATOMIC EN, ET AL" ~ "FED",
      Litigant == "KURT WEISS GREENHOUSES, ET AL" ~ "BIZ",
      Litigant == "EZ LYNK, SEZC, ET AL" ~ "BIZ",
      Litigant == "ROCKLAND TRANSIT MIX, I, ET AL" ~ "BIZ",
      Litigant == "DIMES ENERGY, LP, ET AL" ~ "BIZ",
      Litigant == "STATE EMERGENCY RESPONS, ET AL" ~ "STA",
      Litigant == "21ST CENTURY FOX AMERIC, ET AL" ~ "BIZ",
      Litigant == "NRG POWER MIDWEST LP" ~ "BIZ",
      Litigant == "ACTIVE ENERGY RENEWABLE, ET AL" ~ "BIZ",
      Litigant == "NATIONAL GRID NE HOLDIN, ET AL" ~ "BIZ",
      Litigant == "KIRBY INLAND MARINE, LP" ~ "BIZ",
      Litigant == "LOCKWOOD ANDREWS & NEWN, ET AL" ~ "BIZ",
      Litigant == "BEST IN NEIGHBORHOOD LL, ET AL" ~ "BIZ",
      Litigant == "CAHOKIA HEIGHTS, ET AL" ~ "LOC",
      Litigant == "AEROJET ROCKETDYNE HOLDINGS, I" ~ "BIZ",
      Litigant == "SUNNYVALE, ET AL" ~ "LOC",
      Litigant == "CALIFORNIA CASCADE BUIL, ET AL" ~ "BIZ",
      Litigant == "SUNPOL RESINS AND POLYM, ET AL" ~ "BIZ",
      Litigant == "1433 HUNTINGTON DRIVE L, ET AL" ~ "BIZ",
      Litigant == "NISSAN NORTH AMERICA, I, ET AL" ~ "BIZ",
      Litigant == "AMERICAN RECLAMATION, I, ET AL" ~ "BIZ",
      Litigant == "ATKEMIX THIRTY-SEVEN IN, ET AL" ~ "BIZ",
      Litigant == "FOASBERG LAUNDRY AND CL, ET AL" ~ "BIZ",
      Litigant == "THE INTERNATIONAL BOUNDARY AND" ~ "FED",
      Litigant == "MARYLAND SQUARE SHOPPIN, ET AL" ~ "BIZ",
      Litigant == "MEDFORD" ~ "LOC",
      Litigant == "PERENNIAL POWER HOLDING, ET AL" ~ "BIZ",
      Litigant == "WASHINGTON DAIRY HOLDIN, ET AL" ~ "BIZ",
      Litigant == "UNITED STATES BAKERY" ~ "BIZ",
      Litigant == "JANUARY ENVIRONMENTAL S, ET AL" ~ "BIZ",
      Litigant == "TAP WORLDWIDE" ~ "BIZ",
      Litigant == "JACKSONVILLE HOSPITALIT, ET AL" ~ "BIZ",
      Litigant == "GOVERNOR RON DESANTIS, ET AL" ~ "STA",
      Litigant == "LEATHERBROOK HOLSTEINS , ET AL" ~ "BIZ",
      Litigant == "MORTAGE OF AMERICA LENDERS, LL" ~ "BIZ",
      Litigant == "CENTRAL VALLEY EDEN ENVIRONMEN" ~ "NGO",
      Litigant == "RANCHO VISTA DEL MAR" ~ "BIZ",
      Litigant == "SAN ANTONIO BAY ESTUARI, ET AL" ~ "NGO",
      Litigant == "FOR HEALTHY ENVIRONMENT, ET AL" ~ "NGO",
      Litigant == "MID-NEW YORK ENVIRONMEN, ET AL" ~ "NGO",
      Litigant == "GALLAGHER & KENNEDY PA, ET AL" ~ "BIZ",
      Litigant == "INTERNATIONAL SOCIETY FOR THE" ~ "NGO",
      Litigant == "ENVIRONMENTAL DEMOCRACY PROJEC" ~ "NGO",
      Litigant == "ONE HUNDRED MILES, ET AL" ~ "NGO",
      Litigant == "STATEWIDE ORGANIZING FO, ET AL" ~ "NGO",
      Litigant == "MARYLAND CHAPTER OF THE, ET AL" ~ "NGO",
      Litigant == "FOND DU LAC BAND OF LAKE SUPER" ~ "TRIBE",
      Litigant == "NORTHERN ALASKA ENVIRON, ET AL" ~ "NGO",
      Litigant == "THE TWO HUNDRED FOR HOM, ET AL" ~ "BIZ",
      Litigant == "CENTRAL CALIFORNIA ENVI, ET AL" ~ "NGO",
      Litigant == "DON'T CAGE OUR OCEANS, ET AL" ~ "NGO",
      Litigant == "MASSACHUSETTS DIVISION , ET AL" ~ "STA",
      Litigant == "NORTHROP GRUMMAN SYSTEM, ET AL" ~ "BIZ",
      Litigant == "KAYDON ACQUISITION XI, , ET AL" ~ "BIZ",
      Litigant == "NRG POWER MIDWEST LP, ET AL" ~ "BIZ",
      Litigant == "DETROIT BULK STORAGE, I, ET AL" ~ "BIZ",
      Litigant == "LAS GALLINAS VALLEY SANITARY D" ~ "LOC",
      Litigant == "NEW HAMPSHIRE FISH AND , ET AL" ~ "STA",
      Litigant == "TOTALENERGIES MARKETING PUERTO" ~ "BIZ",
      Litigant == "HUDSON WHOLESALERS RESTAURANT" ~ "BIZ",
      Litigant == "LAWRENCEBURG UTILITY SY, ET AL" ~ "BIZ",
      Litigant == "THUNDER DIESEL & PERFOR, ET AL" ~ "BIZ",
      Litigant == "NATIVE LANDSCAPE SOLUTIONS, IN" ~ "BIZ",
      Litigant == "MOSS LANDING MARINE, LL, ET AL" ~ "BIZ",
      Litigant == "GLENBROOK HOMEOWNERS AS, ET AL" ~ "CIVIC",
      Litigant == "VIEW POINT DAIRY" ~ "BIZ",
      Litigant == "MORTGAGE OF AMERICA LEN, ET AL" ~ "BIZ",
      Litigant == "ISP ENVIRONMENTAL SERVI, ET AL" ~ "BIZ",
      Litigant == "GRAPHIC PACKAGING INTERNATIONA" ~ "BIZ",
      Litigant == "CHUCK CALLAHAN FORD, IN, ET AL" ~ "BIZ",
      Litigant == "BUILDING MATERIAL DISTRIBUTORS" ~ "BIZ",
      Litigant == "Q BAR X RANCH, ET AL" ~ "BIZ",
      Litigant == "PACIFIC PILE & MARINE LP" ~ "BIZ",
      Litigant == "HARLEY-DAVIDSON OF SALT, ET AL" ~ "BIZ",
      Litigant == "GROWTH ENERGY" ~ "BIZ",
      Litigant == "BLUE AND GOLD FLEET" ~ "BIZ",
      Litigant == "J-8 EQUIPMENT SVC CO" ~ "BIZ",
      Litigant == "CARNEGIE HILL-87TH" ~ "BIZ",
      TRUE ~ l_typ
    )
  )

# remove just-labeled values to separate df
PD_type_dic_NA_lab <- PD_type_dic_NA %>%
  filter(
    !is.na(l_typ)
  )
# append non-NAs back to primary dictionary
PD_type_dic <- bind_rows(
  PD_type_dic,
  PD_type_dic_NA_lab
  )
# remove labeled values from NA df
PD_type_dic_NA <- PD_type_dic_NA %>%
  filter(
    is.na(l_typ)
  )
rm(PD_type_dic_NA_lab)

# label all the rest of the NAs as individuals; re-append to dictionary
PD_type_dic_NA <- PD_type_dic_NA %>%
  mutate(
    l_typ = "IND"
  )
# append non-NAs back to primary dictionary
PD_type_dic <- bind_rows(
  PD_type_dic,
  PD_type_dic_NA
  )
rm(PD_type_dic_NA)


# Look for and remove duplicates in dictionary ####
PD_type_dic <- PD_type_dic %>%
  group_by(
    Litigant
  ) %>%
  mutate(
    n = n()
  ) %>%
  ungroup()


# make df of duplicates; drop duplicate rows from dictionary
PD_type_dic_dups <- PD_type_dic %>%
  filter(
    n > 1
    ) %>%
  arrange(Litigant,n)

PD_type_dic <- PD_type_dic %>%
  filter(
    n == 1
    )

# in df of duplicates, where all duplicates are the same, code "same" 1; 0 otherwise.
PD_type_dic_dups <- PD_type_dic_dups %>%
  group_by(
    Litigant
  ) %>%
  mutate(
    same = +(n_distinct(l_typ) == 1)
    )

# make df of duplicate litigants with the same type classificaiton. In these instances,
# drop duplicate and then re-bind to dictionary.
PD_type_dic_dups_resolved <- PD_type_dic_dups %>%
  filter(
    same == 1
  ) %>%
  group_by(
    Litigant
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  select(-same)
# join back to dictionary
PD_type_dic <- bind_rows(
  PD_type_dic,
  PD_type_dic_dups_resolved
  )
rm(PD_type_dic_dups_resolved)
# drop duplicates with the same classification from the duplicates df
PD_type_dic_dups <- PD_type_dic_dups %>%
  filter(
    same != 1
  ) %>%
  ungroup()

# Sort through duplicates with different classifications

# Some duplicates will be coded as FED because of earlier work attaching a FED
# code to cases based on the JURIS variable. Thus, for duplicates where one is
# FED and the other is something else, code as FED (i.e., defer to information
# gained throgh JURIS variabe)

PD_type_dic_dups <- PD_type_dic_dups %>%
  group_by(
    Litigant
  ) %>%
  mutate(
    fed = case_when(
      any(l_typ == "FED") ~ 1,
      TRUE ~ 0
    )
  )

# create separate df of pairs with FED; keep fed code; rejoin to dic
PD_type_dic_dups_fed <- PD_type_dic_dups %>%
  filter(
    fed == 1
  ) %>%
  group_by(
    Litigant
  ) %>%
  filter(
    l_typ == "FED"
  ) %>%
  ungroup() %>%
  select(-same, -fed)
# join back to dictionary
PD_type_dic <- bind_rows(
  PD_type_dic,
  PD_type_dic_dups_fed
  )
rm(PD_type_dic_dups_fed)
# drop duplicates with fed as one of the values from the duplicates df
PD_type_dic_dups <- PD_type_dic_dups %>%
  filter(
    fed != 1
  ) %>%
  ungroup() %>%
  select(-same, -fed)

# manually recode remaining duplicates; drop duplicates
PD_type_dic_dups <- PD_type_dic_dups %>%
  mutate(
    l_typ = case_when(
      Litigant == "ABBOTT LABORATORIES, ET AL" ~ "BIZ",
      Litigant == "ATLANTIC" ~ "BIZ",
      Litigant == "BLASCH PRECISION" ~ "BIZ",
      Litigant == "BOARD OF SUPERVISORS, ET AL" ~ "LOC",
      Litigant == "CALIFORNIA ENVIRONMENTAL PROTE" ~ "STA",
      Litigant == "GRECO" ~ "IND",
      Litigant == "KENNARD" ~ "IND",
      Litigant == "KIRKPATRICK, ET AL" ~ "IND",
      Litigant == "KLEIN" ~ "IND",
      Litigant == "KRAMER" ~ "IND",
      Litigant == "LAMBERT, ET AL" ~ "IND",
      Litigant == "LESLIE SALTL" ~ "BIZ",
      Litigant == "PATTERSON, ET AL" ~ "IND",
      Litigant == "PEARCE, ET AL" ~ "IND",
      Litigant == "PHAN" ~ "IND",
      Litigant == "POWELL DUFFRYN" ~ "BIZ",
      Litigant == "RAMSEY" ~ "IND",
      Litigant == "WASHINGTON, ET AL" ~ "STA",
      Litigant == "WESTINGHOUSE, ET AL" ~ "BIZ",
      TRUE ~ l_typ
    )
  ) %>%
  group_by(
    Litigant
  ) %>%
  filter(
    row_number() == 1
  )

# join back to dictionary
PD_type_dic <- bind_rows(
  PD_type_dic,
  PD_type_dic_dups
  )
rm(PD_type_dic_dups)
  
# recheck for duplicates in dictionary
PD_type_dic <- PD_type_dic %>%
  group_by(
    Litigant
  ) %>%
  mutate(
    n = n()
  ) %>%
  ungroup()

# drop count column (n)
PD_type_dic <- PD_type_dic %>%
  select(
    -(n)
  )

# Further corrections ####

# code for looking for miscodes based on two (or three or more) strings
test <- PD_type_dic %>%
  filter(
    str_detect(Litigant, "CITIZ") == T &
      (str_detect(Litigant, "CONCER") == T |
      str_detect(Litigant, " FOR") == T |
      str_detect(Litigant, " COMM") == T |
        str_detect(Litigant, " GROU") == T |
      str_detect(Litigant, " AGAI") == T)
      #&
      #str_detect(Litigant, " DE") == T
  )

# make further corrections to dictionary
PD_type_dic <- PD_type_dic %>%
  ungroup() %>%
  mutate(
    # make sure all forest service entries are coded as FED
    l_typ = case_when(
      str_detect(Litigant, "FOR") == T &
        str_detect(Litigant, " SER") == T ~ "FED",
      TRUE ~ l_typ
    ),
    # for all litigants with "LIMITED" and where l_typ is currently LOC,
    # recode as BIZ
    l_typ = case_when(
      str_detect(Litigant, "LIMITED") == T &
        l_typ == "LOC" ~ "BIZ",
      TRUE ~ l_typ
    ),
    # for all litigants with "FRIENDS OF" and where l_typ is currently BIZ,
    # recode as NGO
    l_typ = case_when(
      str_detect(Litigant, "FRIENDS OF") == T &
        l_typ == "BIZ" ~ "NGO",
      TRUE ~ l_typ
    ),
    # code all "Concerned Citizens" and similar cases as CIVIC
    l_typ = case_when(
      str_detect(Litigant, "CITIZ") == T &
      (str_detect(Litigant, "CONCER") == T |
         str_detect(Litigant, " FOR") == T |
        str_detect(Litigant, " COMM") == T |
        str_detect(Litigant, " GROU") == T |
        str_detect(Litigant, " AGAI") == T) ~ "CIVIC",
      TRUE ~ l_typ
    ),
    # re-code Citizens for a better env. as NGO
    l_typ = case_when(
      str_detect(Litigant, "CITIZ") == T &
        str_detect(Litigant, "BET") == T ~ "NGO",
      TRUE ~ l_typ
    ),
    # code all "committee" entries as NGO
    l_typ = case_when(
      str_detect(Litigant, "COMMITTEE") == T ~ "NGO",
      TRUE ~ l_typ,
    ),
    # code all "Amer Lung Assoc" entries as OTHER
    l_typ = case_when(
      str_detect(Litigant, "LUNG") == T &
        str_detect(Litigant, "CORPORATION") == F ~ "OTHER",
      TRUE ~ l_typ,
    ),
    # code all couty "farm bureaus" as BIZ
    l_typ = case_when(
      str_detect(Litigant, "FARM BUREAU") == T &
        (str_detect(Litigant, "COUNTY") == T |
        str_detect(Litigant, "FED") == T) ~ "BIZ",
      TRUE ~ l_typ,
    ),
    l_typ = case_when(
      Litigant == "A1 PUBLIC ADJUSTERS & E, ET AL" ~ "BIZ",
      Litigant == "ABLE TERMITE" ~ "BIZ",
      Litigant == "FOREST SERV EMPL" ~ "FED",
      Litigant == "FOREST SERVICE EMPL" ~ "FED",
      Litigant == "FOREST SERVICE EMPLOYEE, ET AL" ~ "FED",
      Litigant == "FISH & WILDLIFE SVC" ~ "FED",
      Litigant == "FISH & WILDLIFE SVC, ET AL" ~ "FED",
      Litigant == "WA FISH & WILDLIFE" ~ "STA",
      Litigant == "FISH AND WILDLIFE, ET AL" ~ "FED",
      Litigant == "FISH & WILDLIFE WA S, ET AL" ~ "STA",
      Litigant == "FISH & WILDLIFE SVCS, ET AL" ~ "FED",
      Litigant == "UNITED STATES FISH AND WILDLIF" ~ "FED",
      Litigant == "UNITED STATES FISH & WILDLIFE" ~ "FED",
      Litigant == "UNTIED STATES FISH AND WILDLIF" ~ "FED",
      Litigant == "USDA APHIS WILDLIFE SER, ET AL" ~ "FED",
      Litigant == "USDA APHIS WILDLIFE SERVICES" ~ "FED",
      Litigant == "ANIMAL & PLANT HEALTH" ~ "FED",
      Litigant == "BUREAU OF PRISONS, ET AL" ~ "FED",
      Litigant == "INTERIOR BOARD OF LAND , ET AL" ~ "FED",
      Litigant == "NATURAL RES CONSV" ~ "FED",
      Litigant == "AFFORDABLE MARINE SERVI, ET AL" ~ "BIZ",
      Litigant == "TROY LINEN & UNIFORM SERVIC" ~ "BIZ",
      Litigant == "TRANSFORMER SERVICE, INC." ~ "BIZ",
      Litigant == "CALIFORNIA METAL SERVICES, INC" ~ "BIZ",
      Litigant == "WASTE ACTION PROJECT" ~ "NGO",
      Litigant == "DON'T WASTE ARIZONA" ~ "NGO",
      Litigant == "EDEN ENVIRONMENTAL CITIZEN'S G" ~ "NGO",
      Litigant == "NATIVE ECOSYSTEMS COUNC, ET AL" ~ "NGO",
      Litigant == "CALIFORNIA SPORT" ~ "NGO",
      Litigant == "OCEANA, INC." ~ "NGO",
      Litigant == "ECOLOGICAL RIGHTS FOUNDATION" ~ "NGO",
      Litigant == "CONNECTICUT" ~ "STA",
      Litigant == "CALIFORNIA COMMUNITIES AGAINST" ~ "CIVIC",
      Litigant == "OREGON NATURAL DESERT ASSOCIAT" ~ "NGO",
      Litigant == "GREATER YELLOWSTONE, ET AL" ~ "NGO",
      Litigant == "SAN FRANCISCO BAY, ET AL" ~ "NGO",
      Litigant == "CENTER FOR COMMUNITY ACTION AN" ~ "NGO",
      Litigant == "COASTAL ENVIRONMENTAL RIGHTS F" ~ "NGO",
      Litigant == "FOREST SERVICE EMPLOYEES FOR E" ~ "NGO",
      Litigant == "UT ENVIRONMENTAL CON" ~ "NGO",
      Litigant == "COMMUNITIES FOR A BETTER ENVIR" ~ "NGO",
      Litigant == "OREGON NATURAL DESERT A, ET AL" ~ "NGO",
      Litigant == "IDAHO SPORTING, ET AL" ~ "NGO",
      Litigant == "IDAHO SPORTING" ~ "NGO",
      Litigant == "SKYLINE SPORTMENS" ~ "NGO",
      Litigant == "IDAHO SPORTMEN'S COA" ~ "NGO",
      Litigant == "IDAHO SPORTMEN'S COA, ET AL" ~ "NGO",
      Litigant == "AMERICAN CANOE ASSOC" ~ "NGO",
      Litigant == "SANTA MONICA BAY" ~ "NGO",
      Litigant == "COMMUNITY ASSOCIATION F, ET AL" ~ "CIVIC",
      Litigant == "DEPARTMENT OF PLANNING AND NAT" ~ "STA",
      Litigant == "PLANNING COMM ELPASO, ET AL" ~ "LOC",
      Litigant == "SIERRA CLUB, INC., ET AL" ~ "NGO",
      Litigant == "CALIFORNIANS FOR ALTERNATIVES" ~ "NGO",
      Litigant == "MICCOSUKEE TRIBE" ~ "TRIBE",
      Litigant == "NATIVE ECOSYSTEMS, ET AL" ~ "NGO",
      Litigant == "HELLS CANYON PRESERVATION COUN" ~ "NGO",
      Litigant == "OREGON NATURAL DESERT ASS'N" ~ "NGO",
      Litigant == "HELLS CANYON COUNCIL, ET AL" ~ "NGO",
      Litigant == "OR NATURAL DESERT, ET AL" ~ "NGO",
      Litigant == "SAN FRANCISCO BAY" ~ "NGO",
      Litigant == "AMERICAN CANOE ASSOC, ET AL" ~ "NGO",
      Litigant == "RE SOURCES FOR SUSTAINABLE COM" ~ "NGO",
      Litigant == "ATLANTIC STATES ETAL" ~ "NGO",
      Litigant == "ATLANTIC STATES" ~ "NGO",
      Litigant == "ATLANTIC STATES, ET AL" ~ "NGO",
      Litigant == "OR NATURAL RESOURCES" ~ "NGO",
      Litigant == "WY OUTDOOR COUNCIL, ET AL" ~ "NGO",
      Litigant == "APPALACHIAN VOICES, ET AL" ~ "NGO",
      Litigant == "CALIFORNIA SPORTFISH" ~ "NGO",
      Litigant == "LANDS COUNCIL, THE, ET AL" ~ "NGO",
      Litigant == "CENTER FOR NATIVE ECOSYSTEMS" ~ "NGO",
      Litigant == "NATIVE VILLAGE OF POINT, ET AL" ~ "TRIBE",
      Litigant == "AN LUIS & DELTA-MENDOT, ET AL" ~ "LOC",
      Litigant == "COTTONWOOD ENVIRONMENTA, ET AL" ~ "NGO",
      Litigant == "ORANGE COUNTY COASTKEEP, ET AL" ~ "NGO",
      Litigant == "PUBLIC CITIZEN" ~ "NGO",
      Litigant == "OREGON NATURAL RESOURCE COUNCI" ~ "NGO",
      Litigant == "CENTER FOR NATIVE ECOSY, ET AL" ~ "NGO",
      Litigant == "MICCOSUKEE TRIBE OF INDIANS OF" ~ "NGO",
      Litigant == "MONTANA ENVIRONMENTAL I, ET AL" ~ "NGO",
      Litigant == "UNKNOWN PERSONS" ~ "UNKNOWN",
      Litigant == "COLORADO ENVIRONMENTAL , ET AL" ~ "NGO",
      Litigant == "CONNECTICUT FUND FOR TH, ET AL" ~ "NGO",
      Litigant == "ECOLOGICAL RIGHTS FOUND, ET AL" ~ "NGO",
      str_detect(Litigant, "OREGON NATURAL") == T ~ "NGO",
      str_detect(Litigant, "BAYKEE") == T ~ "NGO",
      str_detect(Litigant, "TRUST FOR HIST") == T ~ "NGO",
      Litigant == "PENN. ENVIRONMENTAL" ~ "NGO",
      Litigant == "HAWKSBILL SEA TURTLE, ET AL" ~ "NGO",
      Litigant == "PRESCOTT NAT'L FORES, ET AL" ~ "NGO",
      Litigant == "DELAWARE VALLEY TOX., ET AL" ~ "NGO",
      Litigant == "TULARE COUNTY AUDUB, ET AL" ~ "NGO",
      Litigant == "BIODIVERSITY ASSOC, ET AL" ~ "NGO",
      Litigant == "CO ENVIRONMENT COAL, ET AL" ~ "NGO",
      Litigant == "CO ENVIRONMENT COAL, ET AL" ~ "NGO",
      Litigant == "CA NATIVE PLANT SOC, ET AL" ~ "NGO",
      Litigant == "CA NATIVE PLANT, ET AL" ~ "NGO",
      Litigant == "MARYLAND NATIVE PLANT S, ET AL" ~ "NGO",
      str_detect(Litigant, "TRIBE") == T ~ "TRIBE",
      Litigant == "DEPT ENVIRO. PROTECT" ~ "STA",
      Litigant == "ENVIRONMENTAL PROTECTION AGENC" ~ "FED",
      Litigant == "NAT'L TRUST FOR HIST, ET AL" ~ "FED",
      Litigant == "ENVIRONMENTAL PRO" ~ "FED",
      Litigant == "DEPT OF ENV PROT" ~ "STA",
      Litigant == "ENVIRONMENTAL PROTECTIO, ET AL" ~ "FED",
      Litigant == "ENVIRONMENTAL PROTECTION" ~ "FED",
      Litigant == "ENVIR. PROTECTION AGENCY" ~ "FED",
      Litigant == "ENVIRONMENTAL PROTECTION AG" ~ "FED",
      Litigant == "ENVIRON PROTECT AGENCY" ~ "FED",
      Litigant == "OHIO ENVIROMENTAL PROTECTION A" ~ "STA",
      Litigant == "ENVIROMENTAL PROTECT" ~ "FED",
      Litigant == "ENVIORNMENTAL PROTECTION AGENC" ~ "FED",
      Litigant == "ENVIRON PROTECTION, ET AL" ~ "FED",
      Litigant == "IL ENV PROTECTION, ET AL" ~ "STA",
      Litigant == "ENVIR PROTECTION" ~ "FED",
      Litigant == "NATL PARKS & CONSV, ET AL" ~ "NGO",
      Litigant == "NATL. PARKS & CONS." ~ "NGO",
      Litigant == "NATL. PARKS & CONS., ET AL" ~ "NGO",
      Litigant == "MONTROSE PARKWAY ALTERNATIVES" ~ "CIVIC",
      Litigant == "NATIONAL PARK SERVICE" ~ "FED",
      Litigant == "NATIONAL PARK SERVICE, ET AL" ~ "FED",
      Litigant == "MARBLE MOUNTAIN AUDU, ET AL" ~ "NGO",
      Litigant == "MINERALS MANAGEMENT SERVICE" ~ "FED",
      Litigant == "COMM SOCIAL SECURITY" ~ "FED",
      Litigant == "COMMISSIONER, SOCIAL SECURITY" ~ "FED",
      Litigant == "SIERRA CLUB INC" ~ "NGO",
      Litigant == "CLUB ETAL" ~ "NGO",
      Litigant == "CLUB, ET AL" ~ "NGO",
      Litigant == "CLUB" ~ "NGO",
      Litigant == "SIERRA CLUB, INC., ET AL" ~ "NGO",
      Litigant == "SIERRA CLUB INC, ET AL" ~ "NGO",
      Litigant == "THE SIERRA CLUB, INC, ET AL" ~ "NGO",
      Litigant == "SIERRA CLUB, INC." ~ "NGO",
      Litigant == "NAT RES DEFENSE COUNCIL INC" ~ "NGO",
      Litigant == "NAT RES DEF COUNCIL INC" ~ "NGO",
      Litigant == "NAT RES DEF COUNC" ~ "NGO",
      Litigant == "NAT RES DEF COUNCIL" ~ "NGO",
      Litigant == "NAT'L RESOURSES DEFENSE COUNC" ~ "NGO",
      Litigant == "NATURAL RESOURCE DEFENSE COUNC" ~ "NGO",
      Litigant == "CENTER FOR COALFIELD JUSTICE" ~ "NGO",
      Litigant == "KEEP YELLOWSTONE NUCLEAR FREE" ~ "NGO",
      Litigant == "ORANGE COUNTY COASTKEEP, ET AL" ~ "NGO",
      Litigant == "APALACHICOLA RIVERKEEPE, ET AL" ~ "NGO",
      Litigant == "KEEP AMERICA BEAUTIFUL, LLC" ~ "NGO",
      Litigant == "MISSOURI CONFLUENCE WATERKEEPE" ~ "NGO",
      Litigant == "NATL MARINE SERVICE, ET AL" ~ "NGO",
      Litigant == "TENNESSEE VALLEY AUTHORITY" ~ "FED",
      Litigant == "TENNESSEE VALLEY AUTHOR, ET AL" ~ "FED",
      Litigant == "MANASOTA-88, INC." ~ "BIZ",
      Litigant == "TAHOE" ~ "LOC",
      Litigant == "ENVIRONMENTAL PROT" ~ "FED",
      Litigant == "ENVIRONMENTAL PROTEC, ET AL" ~ "FED",
      Litigant == "NEVADA ENVIRONMENTAL PROTECTIO" ~ "STA",
      str_detect(Litigant, "ADVANCED ENV") ~ "BIZ",
      str_detect(Litigant, "DEPARTMENT OF TOXIC") ~ "STA",
      Litigant == "DEPT OF TOXIC SUB" ~ "STA",
      Litigant == "CALIFORNIA ENVIRON., ET AL" ~ "STA",
      Litigant == "OR NATURAL RESOURCES, ET AL" ~ "NGO",
      Litigant == "OREGON NATURAL RESOU, ET AL" ~ "NGO",
      Litigant == "ORE NATURAL RESOURCE" ~ "NGO",
      Litigant == "KANSAS NATURAL RES" ~ "NGO",
      Litigant == "KANSAS NATURAL RESOURCE COUNCI" ~ "NGO",
      Litigant == "DEER CREEK VALLEY NATURAL RESO" ~ "NGO",
      Litigant == "KANSAS NATURAL RESOURCE COUNCI" ~ "NGO",
      Litigant == "U.S. ALUMINUM COMPANY" ~ "BIZ",
      Litigant == "FED RESERVE BANK" ~ "FED",
      Litigant == "FEDERAL RESERVE BANK OF, ET AL" ~ "FED",
      Litigant == "FEDERAL RESERVE BANK OF SAN FR" ~ "FED",
      Litigant == "SHERWIN-WILLIAMS, ET AL" ~ "BIZ",
      Litigant == "CROWN CITY PLATING COMPANY" ~ "BIZ",
      Litigant == "CLEVELAND-CLIFFS BURNS , ET AL" ~ "BIZ",
      Litigant == "MIDDLETOWN COKE COMPANY, ET AL" ~ "BIZ",
      Litigant == "PAULSBORO REFINING COMPANY LLC" ~ "BIZ",
      Litigant == "ROSBORO LUMBER COMPANY" ~ "BIZ",
      Litigant == "UNITED PARK CITY MINES COMPANY" ~ "BIZ",
      Litigant == "UNITED PARK CITY MINES, ET AL" ~ "BIZ",
      Litigant == "COMMONWEALTH EDISON COMPANY" ~ "BIZ",
      Litigant == "UBLIC SERVICE COMPANY OF NEW" ~ "BIZ",
      Litigant == "WASTE ACTION PROJECT, ET AL" ~ "NGO",
      Litigant == "HORSEHEAD RESOURCES, ET AL" ~ "BIZ",
      Litigant == "CABINET RESOURCES, ET AL" ~ "BIZ",
      Litigant == "NATIONAL RESOURCES, ET AL" ~ "BIZ",
      Litigant == "PACIFIC SOUND RESOURCES, ET AL" ~ "BIZ",
      Litigant == "EL PASO ENERGY, ET AL" ~ "BIZ",
      Litigant == "U.S. ENERGY PARTNERS, LLC" ~ "BIZ",
      Litigant == "ALLEGHENY ENERGY SUP" ~ "BIZ",
      Litigant == "INLAND EMPIRE ENERGY CENTER LL" ~ "BIZ",
      Litigant == "FEDERAL ENERGY REGULATORY COMM" ~ "FED",
      Litigant == "NEXTERA ENERGY RESOURCE, ET AL" ~ "BIZ",
      Litigant == "GULF RESOURCES, ET AL" ~ "BIZ",
      Litigant == "CASMALIA RESOURCES, ET AL" ~ "BIZ",
      Litigant == "AIR RESOURCES, ET AL" ~ "STA",
      Litigant == "NEEDHAM RESOURCES, ET AL" ~ "BIZ",
      Litigant == "ANDALEX RESOURCES, ET AL" ~ "BIZ",
      Litigant == "WATER RESOURCES, ET AL" ~ "STA",
      Litigant == "CANYON RESOURCES, ET AL" ~ "BIZ",
      Litigant == "AMEREN ENERGY RESOURCES, ET AL" ~ "BIZ",
      Litigant == "ALPHA NATURAL RESOURCES, ET AL" ~ "BIZ",
      Litigant == "ENERGY FUELS RESOURCES, ET AL" ~ "BIZ",
      Litigant == "CALIFORNIA AIR RESOURCES CONTR" ~ "STA",
      Litigant == "WISCONSIN RESOURCES PRO, ET AL" ~ "NGO",
      Litigant == "CALIFORNIA RESOURCES CO, ET AL" ~ "STA",
      Litigant == "RESOURCES LIMITED, ET AL" ~ "BIZ",
      Litigant == "FIBRE RESOURCES UNLIMIT, ET AL" ~ "BIZ",
      Litigant == "VT NAT RESOURCES" ~ "ST",
      Litigant == "NESHAMINY WATER RESOURCES" ~ "LOC",
      Litigant == "VT NATURAL RESOURCES" ~ "NGO",
      Litigant == "PENNTEX RESOURCES ILLINOIS, IN" ~ "BIZ",
      Litigant == "CALIFORNIA AIR RESOURCES BOARD" ~ "STA",
      Litigant == "FLINT HILLS RESOURCES PORT ART" ~ "BIZ",
      Litigant == "DOMINION RESOURCES SERV, ET AL" ~ "BIZ",
      Litigant == "SPRAGUE RESOURCES LP, ET AL" ~ "BIZ",
      Litigant == "STATE WATER RESOURCES C, ET AL" ~ "STA",
      Litigant == "OREGON WATER RESOURCES DEPARTM" ~ "STA",
      Litigant == "OREGON NATL RESOURCES" ~ "NGO",
      Litigant == "OR NATURAL RESOURCES, ET AL" ~ "BIZ",
      Litigant == "ESCAMBIA CO BOARD" ~ "LOC",
      Litigant == "U.S. OIL RECOVERY, L.P., ET AL" ~ "BIZ",
      Litigant == "U.S. OIL & REFINING CO." ~ "BIZ",
      Litigant == "COLAITION AGAINST COLUMBUS CTE" ~ "CIVIC",
      Litigant == "HAZARDOUS SUB CLEAN UP" ~ "NGO",
      Litigant == "DIVERSEY US HOLDINGS, ET AL" ~ "BIZ",
      Litigant == "GOLD RIVER BUS PARK" ~ "BIZ",
      Litigant == "CATELLUS DEVELOPMENT" ~ "BIZ",
      Litigant == "CIRCUS & ELDORADO JNT VENTURE" ~ "BIZ",
      Litigant == "FERROUS TECHNOLOGY" ~ "BIZ",
      Litigant == "CITRUS CENTER LLC" ~ "BIZ",
      Litigant == "STANISLAUS UNION" ~ "LOC",
      Litigant == "URSUS AMERICANUS" ~ "NGO",
      Litigant == "CAMPUS ST. JAMES LARKSPUR, LLC" ~ "BIZ",
      Litigant == "CAMILLUS CLEAN AIR COAL, ET AL" ~ "BIZ",
      Litigant == "INDIGENOUS PEOPLES OF T, ET AL" ~ "TRIBE",
      Litigant == "INDIGENOUS ENVIRONMENTA, ET AL" ~ "TRIBE",
      Litigant == "ADAMKUS ET" ~ "BIZ",
      Litigant == "JANUS ENTERPRISES IN" ~ "BIZ",
      Litigant == "LAZARUS BURMAN ASSOC." ~ "BIZ",
      Litigant == "DOREMUS ETAL" ~ "IND",
      Litigant == "AUTOHAUS ROCHESTER, ET AL" ~ "BIZ",
      Litigant == "SOGEM PRECIOUS METAL, ET AL" ~ "BIZ",
      Litigant == "US PUBLIC INTEREST" ~ "NGO",
      Litigant == "US WEST BUSINESS" ~ "BIZ",
      Litigant == "THE US RADIUM COMPAN, ET AL" ~ "BIZ",
      Litigant == "INTL BUS MACHINES, ET AL" ~ "BIZ",
      Litigant == "US CAN CO, ET AL" ~ "BIZ",
      Litigant == "APPARATUS UNLIMITED" ~ "BIZ",
      Litigant == "CACTUS PIPE & SUPPLY CO" ~ "BIZ",
      Litigant == "ARGUS ENERGY, LLC" ~ "BIZ",
      Litigant == "LAKESIDE NON-FERROUS METALS, I" ~ "BIZ",
      Litigant == "SUPERIOR PLUS ENERGY SERVICES," ~ "BIZ",
      Litigant == "URSINUS COLLEGE" ~ "OTHER",
      Litigant == "LOUIS DREYFUS COMMODITI, ET AL" ~ "BIZ",
      Litigant == "ARCTURUS MANUFACTURING , ET AL" ~ "BIZ",
      Litigant == "US DEVELOPMENT GROUP, L, ET AL" ~ "BIZ",
      Litigant == "TREMBLAY'S BUS CO., LLC" ~ "BIZ",
      Litigant == "ANGELUS WESTERN PAPER FIBERS," ~ "BIZ",
      Litigant == "UNITED STATES NUCLEAR" ~ "FED",
      #Litigant == "CONCERNED CITIZENS OF NEBRASKA" ~ "NGO",
      Litigant == "UNIVERSITY OF NEBRASKA" ~ "OTHER",
      Litigant == "UNITED STATES COAST" ~ "FED",
      Litigant == "UNITED STATES COAST, ET AL" ~ "FED",
      Litigant == "THE UNITED STATES COAST GUARD" ~ "FED",
      Litigant == "UNITED STATES COAST GUA, ET AL" ~ "BIZ",
      Litigant == "UNITED STATES COAST GUARD" ~ "BIZ",
      #Litigant == "THE UNITED STATES COAST, ET AL" ~ "BIZ",
      Litigant == "FRIENDS/COLLEGE PARK, ET AL" ~ "CIVIC",
      Litigant == "WMATA" ~ "LOC",
      Litigant == "MILLS COLLEGE" ~ "OTHER",
      Litigant == "SAINT FRANCIS COLLEGE" ~ "OTHER",
      Litigant == "MILLS COLLEGE" ~ "OTHER",
      Litigant == "GEORGE FOX COLLEGE, ET AL" ~ "OTHER",
      Litigant == "IMMACULATA COLLEGE" ~ "OTHER",
      Litigant == "GETTYSBURG COLLEGE, ET AL" ~ "OTHER",
      Litigant == "OHIO COLLEGE OF PODIATRIC" ~ "OTHER",
      Litigant == "STATE CENTER COMMUNITY COLLEGE" ~ "OTHER",
      Litigant == "NORTHCOAST ENVIRMNTL, ET AL" ~ "NGO",
      Litigant == "NORTHCOAST ENVIROMEN, ET AL" ~ "NGO",
      Litigant == "COASTAL ADVOCATES/MONTARA" ~ "NGO",
      Litigant == "NORTHCOAST ENVIRONMENTAL" ~ "NGO",
      Litigant == "SURFERS' COASTAL" ~ "CIVIC",
      Litigant == "COASTSIDE HABITAT" ~ "NGO",
      Litigant == "WEST COAST FOREST" ~ "NGO",
      Litigant == "COAST ALLIANCE, ET AL" ~ "NGO",
      Litigant == "CALIFORNIA COASTAL" ~ "STA",
      Litigant == "FRIENDS OF COAST" ~ "NGO",
      Litigant == "COAST GUARD" ~ "FED",
      Litigant == "NORTH COAST RIVERS ALLI, ET AL" ~ "NGO",
      Litigant == "SOUTH COAST AIR QUALITY MANAGE" ~ "STA",
      #Litigant == "CITIZENS FOR RATIONAL COASTAL" ~ "CIVIC",
      Litigant == "COASTAL HABITAT ALLIANCE" ~ "NGO",
      Litigant == "SOUTH CAROLINA COASTAL , ET AL" ~ "NGO",
      Litigant == "SAN LUIS OBISPO COASTKE, ET AL" ~ "NGO",
      Litigant == "CENTER FOR A SUSTAINABLE COAST" ~ "NGO",
      Litigant == "NORTH CAROLINA COASTAL , ET AL" ~ "NGO",
      Litigant == "SOUTH COAST AIR QUALITY, ET AL" ~ "STA",
      Litigant == "CA COASTAL COMM" ~ "STA",
      Litigant == "CALIF COASTAL" ~ "STA",
      Litigant == "SOUTH COAST AIR" ~ "STA",
      Litigant == "SOUTH COAST AIR, ET AL" ~ "NGO",
      Litigant == "SOUTH COAST AIR QUA" ~ "NGO",
      Litigant == "S COAST AIR QUALITY, ET AL" ~ "NGO",
      Litigant == "CALIFORNIA COASTAL COMMISSION" ~ "NGO",
      Litigant == "RHODE ISLAND COASTAL RESOURCE" ~ "STA",
      Litigant == "UNITED STATES COAST GUA, ET AL" ~ "FED",
      Litigant == "UNITED STATES COAST GUARD" ~ "FED",
      Litigant == "THE UNITED STATES COAST, ET AL" ~ "FED",
      Litigant == "CALIFORNIA COASTAL COMM, ET AL" ~ "STA",
      Litigant == "BORDER COAST REGIONAL A" ~ "LOC",
      Litigant == "ORANGE COUNTY COAST, ET AL" ~ "NGO",
      Litigant == "ORANGE COUNTY COAST" ~ "NGO",
      Litigant == "ORANGE COUNTY COASTKEEKPER" ~ "NGO",
      Litigant == "INDIANA FOREST ALLIANCE INC" ~ "NGO",
      Litigant == "NW ECOSYSTEM ALLIANC, ET AL" ~ "NGO",
      Litigant == "NORTHWEST ECOSYSTEM ALLIANCE" ~ "NGO",
      Litigant == "INDIANA FOREST ALLIANCE, ET AL" ~ "NGO",
      Litigant == "SAVE OUR SPRINGS ALLIANCE, INC" ~ "NGO",
      Litigant == "NORTH COUNTY COMMUNITY ALLIANC" ~ "CIVIC",
      Litigant == "GREATER EDWARDS AQUIFER ALLIAN" ~ "NGO",
      Litigant == "BABBITT, BURCE  ETAL" ~ "FED",
      Litigant == "SIERRA" ~ "NGO",
      Litigant == "VIRGINIANS FOR APPROPRIATE ROA" ~ "CIVIC",
      Litigant == "FLORIDA KEY DEER, ET AL" ~ "NGO",
      Litigant == "DALLAS SAFARI CLUB, ET AL" ~ "NGO",
      Litigant == "SAFARI CLUB INTERNATIONAL" ~ "NGO",
      Litigant == "ALL INDIAN PUEBLO COUNCIL" ~ "TRIBE",
      Litigant == "MUCKLESHOOT INDIAN T" ~ "TRIBE",
      Litigant == "SOUTHERN UTE INDIAN" ~ "TRIBE",
      Litigant == "SOUTHERN UTE INDIAN, ET AL" ~ "TRIBE",
      Litigant == "HOONAH INDIAN ASSOC" ~ "TRIBE",
      Litigant == "SWINOMISH INDIAN TRIBAL COMMUN" ~ "TRIBE",
      Litigant == "MICCOSUKEE TRIBE OF INDIANS OF" ~ "TRIBE",
      Litigant == "GILA RIVER INDIAN COMMUNITY" ~ "TRIBE",
      Litigant == "MUCKLESHOOT INDIAN TRIB, ET AL" ~ "TRIBE",
      Litigant == "CHILKAT INDIAN VILLAGE , ET AL" ~ "TRIBE",
      Litigant == "UNKECHAUG INDIAN NATION, ET AL" ~ "TRIBE",
      Litigant == "GILA RIVER INDIAN CM, ET AL" ~ "TRIBE",
      Litigant == "BUREAU OF INDIAN, ET AL" ~ "FED",
      Litigant == "BUREAU OF INDIAN" ~ "FED",
      Litigant == "BUREAU OF INDIAN AFFAIRS, ET A" ~ "FED",
      Litigant == "BUREAU OF INDIAN AFFAIRS" ~ "FED",
      Litigant == "PALA BAND OF MISSION INDIANS" ~ "TRIBE",
      Litigant == "FORT BELKNAP INDIAN COMMUNITY" ~ "TRIBE",
      Litigant == "PRAIRIE ISLAND INDIAN COMMUNIT" ~ "FED",
      Litigant == "SOUTH GATE ROD & GUN" ~ "OTHER",
      Litigant == "HOBUCKEN GUN CLUB, ET AL" ~ "OTHER",
      Litigant == "RICHMOND ROD & GUN CLUB" ~ "OTHER",
      Litigant == "S ST PAUL ROD & GUN" ~ "OTHER",
      Litigant == "HOLLADAY GUN CLUB, ET AL" ~ "OTHER",
      Litigant == "MONTPELIER GUN CLUB" ~ "OTHER",
      Litigant == "METACON GUN CLUB INC" ~ "OTHER",
      Litigant == "SAN GABRIEL VALLEY GUN , ET AL" ~ "OTHER",
      Litigant == "COMMUNITY BIBLE CHURCH, ET AL" ~ "OTHER",
      Litigant == "LATTER-DAY SAINTS" ~ "OTHER",
      TRUE ~ l_typ
    )
  )




# Apply dict to cases ####

# rename old type codes in primary df to avoid duplicate variable names
fjc_e <- fjc_e %>%
  rename(
    "PLT_typ_old" = "PLT_typ",
    "DEF_typ_old" = "DEF_typ",
  )

# append PLAINTIFF names to district case data
fjc_e <- left_join(
  fjc_e,
  PD_type_dic %>%
    rename(
      "PLT" = "Litigant",
      "PLT_typ" = "l_typ"
      ),
  by = "PLT"
  )

# append DEFENDANT names to district case data
fjc_e <- left_join(
  fjc_e,
  PD_type_dic %>%
    rename(
      "DEF" = "Litigant",
      "DEF_typ" = "l_typ"
      ),
  by = "DEF"
  )

#In cases where PLT or DEF is -8, but PLT_typ_old/DEF_typ_old == FED (b/c JURIS
#coded it as FED), recode as FED.
fjc_e <- fjc_e %>%
  ungroup() %>%
  mutate(
    PLT_typ = case_when(
      PLT == "-8" & PLT_typ_old == "FED" ~ "FED",
      TRUE ~ PLT_typ
    ),
    DEF_typ = case_when(
      DEF == "-8" & DEF_typ_old == "FED" ~ "FED",
      TRUE ~ DEF_typ
    )
  )

# examine most common PLT and DEF and types

# plaintiffs by number of instances
plt_common <- fjc_e %>%
  group_by(
    PLT
    ) %>%
  mutate(
    n = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    PLT, PLT_typ, n
  ) %>%
  ungroup() %>%
  arrange(
    desc(n)
  )

# defendants by number of instances
def_common <- fjc_e %>%
  group_by(
    DEF
    ) %>%
  mutate(
    n = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    DEF, DEF_typ, n
  ) %>%
  ungroup() %>%
  arrange(
    desc(n)
  )
  
# look at plaintiff and defendants and types; check for NAs and blank values
fjc_e_check <- fjc_e %>%
  select(
    PLT, PLT_typ, PLT_typ_old,
    DEF, DEF_typ, DEF_typ_old
  )

# if all looks good, drop check df
rm(fjc_e_check)

# remove unneeded dfs
rm(
  def_common,
  env_def,
  env_plt,
  plt_common,
  test
)


# remove unneeded lists
rm(
  agency_heads,
  agency_heads_et,
  agency_heads_et_a,
  agency_heads_et_a_c,
  agency_heads_et_al,
  agency_heads_et_al_c,
  agency_heads_et_c,
  agency_heads_eta,
  agency_heads_eta_c,
  agency_heads_etal,
  agency_heads_etal_c,
  BIZ,
  CIVIC,
  FED,
  IND,
  LOC,
  NGO,
  NGO_O,
  PUB_ORG,
  STA,
  state_abbr,
  state_abbr_dep,
  state_abbr_st_dep,
  state_name_lower_orlist,
  state_names,
  state_names_lower,
  state_name_dep,
  TRIBE
  )

# write out dictionary
write_csv(
  PD_type_dic,
  "Data/FJC_enviro_litigant_coding/enviro_litigant_type_dict.csv"
)

# Write random samples for data validation ######

set.seed(12345)

fjc_e_rand <- sample_n(fjc_e,500)

fjc_e_rand <- fjc_e_rand %>%
  select(
    fjc_ID, FILEDATE, JURIS, PLT, PLT_typ, PLT_typ_old, DEF, DEF_typ, DEF_typ_old
  )

write_csv(fjc_e_rand, "Data/For_reliability/fjc_e_random.csv")


# write out random sample of cases that are judgements on consent (disp == 5)
set.seed(12345)
fjc_e_consent <- fjc_e %>%
  filter(
    DISP == 5
  ) %>%
  select(
    fjc_ID, CIRCUIT, DISTRICT, OFFICE, DOCKET, FILEDATE, TERMDATE, PLT, DEF, DISP
  ) %>%
  sample_n(
    100
  )

# write out consent cases for manual inspection
#write_csv(fjc_e_consent, "Data/For_reliability/fjc_e_consent.csv")

# write out a random sample of cases where NGOs are plaintiffs
set.seed(12345)
fjc_e_plt_ngo <- fjc_e %>%
  filter(
    PLT_typ == "NGO"
  ) %>%
  select(
    fjc_ID, CIRCUIT, DISTRICT, OFFICE, DOCKET, FILEDATE, TERMDATE, PLT, PLT_typ, DEF, DEF_typ, DISP
  ) %>%
  sample_n(
    150
  )

# write out NGO plaintiff cases for manual inspection
#write_csv(fjc_e_plt_ngo, "Data/For_reliability/fjc_e_plt_NGO.csv")


# write out a random sample of cases where the federal government is a plaintiffs
set.seed(12345)
fjc_e_plt_fed <- fjc_e %>%
  filter(
    PLT_typ == "FED"
  ) %>%
  select(
    fjc_ID, CIRCUIT, DISTRICT, OFFICE, DOCKET, FILEDATE, TERMDATE, PLT, PLT_typ, DEF, DEF_typ, DISP
  ) %>%
  sample_n(
    150
  )

# write out FED plaintiff cases for manual inspection
#write_csv(fjc_e_plt_fed, "Data/For_reliability/fjc_e_plt_FED.csv")


# write out a random sample of cases where firms and trade associations are plaintiffs
set.seed(12345)
#set.seed(54321)
fjc_e_plt_biz <- fjc_e %>%
  filter(
    PLT_typ == "BIZ"
  ) %>%
  select(
    fjc_ID, CIRCUIT, DISTRICT, OFFICE, DOCKET, FILEDATE, TERMDATE, PLT, PLT_typ, DEF, DEF_typ, DISP
  ) %>%
  sample_n(
    150
  )

# write out BIZ plaintiff cases for manual inspection
#write_csv(fjc_e_plt_biz, "Data/For_reliability/fjc_e_plt_BIZ.csv")


# remove dfs written out
rm(
  fjc_e_consent, fjc_e_plt_fed, fjc_e_plt_biz, fjc_e_plt_ngo, fjc_e_rand
)


# write out post-processed data (but which still includes BP and IMC Global
# cases - See 05-clean_enviro.R script.)
write_csv(fjc_e, "Data/FJC_postprocessed/District/fjc_e_post_processed_pre_clean.csv")


# the end.