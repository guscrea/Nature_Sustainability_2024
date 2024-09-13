# Inter-Coder Reliability
# By Chris Rea
# Last Modified May 08, 2024

# Notes ####

# !! In the sequence of scripts for generating the figures and other analyses
# for the Nature Sustainability paper, this script can be skipped. It does not
# do any data-prep work required for the next script.

# The following script computes inter-coder reliability scores via
# Krippendorff's alpha for the data used in the study. This includes the FJC IDB
# data, for which plaintiff and defendant type are the most critical, and RESL
# derived from LexisNexis, which tests a range of variables used in the analysis
# to assess judicial decision focus.

# Reliability scores for RESL data are computed in two stages. Reliability scores
# for meta-categories to classify the substantive focus of cases are computed for
# pre-validation data, since (a) these meta categories were not validated through
# a second round of coding because (b) the meta-categorizations themselves were
# assembled triangulated through several different codes (the "object of contention"
# principally, but also the "aim" of the case, the "type of nature" at issue, and
# the statutes mentioned in the judicial decision).

# Reliability scores for all other variables used in this study are computed for
# validated data, i.e., data that were coded twice by RESL researchers.


# Load packages ####
library(tidyverse)
library(krippendorffsalpha)


# Reliability for FJC and plt and def type categories ####

# read in data for ICR
fjc_icr <- read_csv(
  "Data/For_reliability/fjc_e_random_coded_for_ICR.csv"
  ) %>%
  #make all variables all lowercase
  mutate(
    PLT_typ = str_to_lower(PLT_typ),
    PLT_typ_human = str_to_lower(PLT_typ_human),
    DEF_typ = str_to_lower(DEF_typ),
    DEF_typ_human = str_to_lower(DEF_typ_human),
  )

# check for unique values
unique(fjc_icr$PLT_typ)
unique(fjc_icr$PLT_typ_human)
unique(fjc_icr$DEF_typ)
unique(fjc_icr$DEF_typ_human)


plt_values <- unique(c(unique(fjc_icr$PLT_typ),unique(fjc_icr$PLT_typ_human)))
def_values <- unique(c(unique(fjc_icr$DEF_typ),unique(fjc_icr$DEF_typ_human)))

# build numerical matrix for ICR - PLT typ 
fjc_icr_plt_mat <- fjc_icr %>%
  select(
    PLT_typ,
    PLT_typ_human
  ) %>%
  mutate(
    PLT_typ = factor(
      x = PLT_typ,
      levels = plt_values,
      labels = seq(from  = 1, to = length(levels), by = 1)
    ),
    PLT_typ = as.numeric(PLT_typ),
    PLT_typ_human = factor(
      x = PLT_typ_human,
      levels = plt_values,
      labels = seq(1,length(levels),1)
    ),
    PLT_typ_human = as.numeric(PLT_typ_human),
  ) %>%
  as.matrix()

# calculate ICR
icr_ptyp <- 
  krippendorffs.alpha(
    fjc_icr_plt_mat,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_ptyp)

# Results
#       Estimate  Lower  Upper
# alpha   0.9401 0.9111 0.9599



# build numerical matrix for ICR - DEF typ 
fjc_icr_def_mat <- fjc_icr %>%
  select(
    DEF_typ,
    DEF_typ_human
  ) %>%
  mutate(
    DEF_typ = factor(
      x = DEF_typ,
      levels = def_values,
      labels = seq(from  = 1, to = length(levels), by = 1)
    ),
    DEF_typ = as.numeric(DEF_typ),
    DEF_typ_human = factor(
      x = DEF_typ_human,
      levels = def_values,
      labels = seq(1,length(levels),1)
    ),
    DEF_typ_human = as.numeric(DEF_typ_human)
  ) %>%
  as.matrix()

# calculate ICR
icr_dtyp <- 
  krippendorffs.alpha(
    fjc_icr_def_mat,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_dtyp)

# Results
#       Estimate  Lower  Upper
# alpha   0.9382 0.9034 0.9608


# compile ICR results from FJC plt and def
source("Functions/icr_df.R")

# start with FJC IDB plaintiff type ICR
df_icr <- build_icr_df(
  icr_ptyp,
  "FJC IDB",
  "Plaintiff Type",
  NULL,
  "Computed from 500 random observations of FJC IDB data, comparing quasi-automated and human-inspected code applications."
  )

# add FJC IDB defendant type ICR
df_icr <- build_icr_df(
  icr_dtyp,
  "FJC IDB",
  "Defendant Type",
  df_icr,
  "Computed from 500 random observations of FJC IDB data, comparing quasi-automated and human-inspected code applications."
  )


# remove ICR matrices and df
rm(fjc_icr, fjc_icr_plt_mat, fjc_icr_def_mat, icr_ptyp, icr_dtyp)

# Reliability for RESL primary variables - post-validation ####

# load RESL data
resl <- read_csv("Data/RESL/RESL-Val_Cor_Clean_Meta.csv")

# Make statutes and agencies lists (keep only unique)
resl <- resl %>%
  rowwise() %>%
  mutate(
    plt_typ = unique(str_split(plt_typ, "%")),
    def_typ = unique(str_split(def_typ, "%")),
    agy = unique(str_split(agy, "%")),
    statute = unique(str_split(statute, "%"))
  ) %>%
  # recode any NGO codes in Agency column as "none" (ngos are not agencies!)
  mutate(
    agy = list(case_when(
      str_detect(unlist(agy), "ngo") ~ "none",
      TRUE ~ agy
      )
    ),
      yr_term = year(case_date)
  ) %>%
  # identify repeat cases for inter-coder reliablity assessments
  group_by(
    ID
  ) %>%
  mutate(
    ID_count = n()
  ) %>%
  ungroup()

# create separate df of tripple-coded cases for ICR
resl_ICR <- resl %>%
  filter(
    ID_count > 1
  )

# prep resl data for ICR
resl_ICR <- resl_ICR %>%
  rowwise() %>%
  mutate(
    plt_typ = list(unique(plt_typ)),
    def_typ = list(unique(def_typ)),
    agy = list(unique(agy)),
    statute = list(unique(statute)),
    plt_typ = list(sort(plt_typ)),
    def_typ = list(sort(def_typ)),
    agy = list(sort(agy)),
    statute = list(sort(statute)),
  ) %>%
  select(
    CHECKED_By, ID,plt_typ,def_typ,statute,agy, climate_count,ej_count
  ) %>%
  group_by(
    ID,CHECKED_By
  ) %>%
  mutate(
    coder = str_c(CHECKED_By,row_number(),sep = "")
  ) %>%
  ungroup()

# # check plaintiff type for unique values
# 
# plt_check <- resl_ICR %>%
#   select(
#     ID, plt_typ
#   ) %>%
#   unnest_wider(
#     plt_typ,
#     names_sep = "_"
#   ) %>%
#   pivot_longer(
#     cols = plt_typ_1:last_col(),
#     names_to = "names_val",
#     values_to = "plt_typ",
#     names_prefix = "plt_typ_"
#   ) %>%
#   mutate(
#     ID = str_c(ID,names_val,sep = "-")
#   ) %>%
#   select(
#     -names_val
#   ) %>%
#   filter(
#     !is.na(plt_typ)
#   ) %>%
#   # recode ngo_other as ngo_o
#   mutate(
#     plt_typ = case_when(
#       plt_typ == "ngo_other" ~ "ngo_o",
#       TRUE ~ plt_typ
#     )
#   ) %>%
#   pivot_wider(
#     names_from = 
#   )





#unique(plt_check$plt_typ)

### RESL plt_typ ####

# build matrix for plaintiff type
resl_ICR_plt <- resl_ICR %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,plt_typ
  ) %>%
  unnest_wider(
    plt_typ,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = plt_typ_1:last_col(),
    names_to = "names_val",
    values_to = "plt_typ",
    names_prefix = "plt_typ_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(plt_typ)
  ) %>%
  mutate(
    plt_typ = trimws(plt_typ)
  ) %>%
  ungroup() %>%
  mutate(
    plt_typ = factor(
      x = plt_typ,
      levels = unique(plt_typ)
    ),
    plt_typ = as.numeric(plt_typ)
  ) %>%
  # looking for disagreement
  # group_by(
  #   ID
  # ) %>%
  # mutate(
  #   avg_PLT_typ_diff = mean(PLT_typ)
  # ) %>%
  # ungroup() %>%
  # mutate(
  #   avg_PLT_typ_diff = PLT_typ-avg_PLT_typ_diff
  # ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "plt_typ"
  ) %>%
  select(-ID) %>%
  as.matrix()

# calculate ICR
icr_resl_ptyp <- 
  krippendorffs.alpha(
    resl_ICR_plt,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_ptyp)

# add RESL plaintiff type ICR
df_icr <- build_icr_df(
  icr_resl_ptyp,
  "RESL",
  "Plaintiff Type",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. Unique decision-plaintiff-type units are treated as independent observations, since a single decision can have multiple plaintiff type codes assigned to it."
  )

rm(icr_resl_ptyp, resl_ICR_plt)

### RESL plt_typ - FED binary ####

# build matrix for plaintiff type
resl_ICR_plt <- resl_ICR %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,plt_typ
  ) %>%
  unnest_wider(
    plt_typ,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = plt_typ_1:last_col(),
    names_to = "names_val",
    values_to = "plt_typ",
    names_prefix = "plt_typ_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(plt_typ)
  ) %>%
  mutate(
    plt_typ = case_when(
      plt_typ == "fed" ~ "fed",
      TRUE ~ "not"
    ),
    plt_typ = trimws(plt_typ)
  ) %>%
  ungroup() %>%
  mutate(
    plt_typ = factor(
      x = plt_typ,
      levels = unique(plt_typ)
    ),
    plt_typ = as.numeric(plt_typ)
  ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "plt_typ"
  ) %>%
  select(-ID) %>%
  as.matrix()

# calculate ICR
icr_resl_ptyp_fed <- 
  krippendorffs.alpha(
    resl_ICR_plt,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_ptyp_fed)

# add RESL plaintiff type ICR - FED BINARY 
df_icr <- build_icr_df(
  icr_resl_ptyp_fed,
  "RESL",
  "Federal Government Plaintiff (binary)",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. The presence or absence of a federal government plaintiff is treated as a binary variable."
  )

rm(icr_resl_ptyp_fed, resl_ICR_plt)

### RESL plt_typ - BIZ binary ####

# build matrix for plaintiff type
resl_ICR_biz <- resl_ICR %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,plt_typ
  ) %>%
  unnest_wider(
    plt_typ,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = plt_typ_1:last_col(),
    names_to = "names_val",
    values_to = "plt_typ",
    names_prefix = "plt_typ_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(plt_typ)
  ) %>%
  mutate(
    plt_typ = case_when(
      plt_typ == "industry" ~ "industry",
      TRUE ~ "not"
    ),
    plt_typ = trimws(plt_typ),
    # intentionally introducing error (for testing)
    # plt_typ = case_when(
    #   coder == "sie1" ~ "biz",
    #   TRUE ~ plt_typ
    # )
  ) %>%
  ungroup() %>%
  mutate(
    plt_typ = factor(
      x = plt_typ,
      levels = unique(plt_typ)
    ),
    plt_typ = as.numeric(plt_typ)
  ) %>%
  #looking for disagreement
  # group_by(
  #   ID
  # ) %>%
  # mutate(
  #   avg_plt_typ_diff = mean(plt_typ)
  # ) %>%
  # ungroup() %>%
  # mutate(
  #   avg_plt_typ_diff = plt_typ-avg_plt_typ_diff
  # ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "plt_typ"
  ) %>%
  select(-ID) %>%
  as.matrix()

# calculate ICR
icr_resl_ptyp_biz <- 
  krippendorffs.alpha(
    resl_ICR_biz,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_ptyp_biz)

# add RESL plaintiff type ICR - BIZ BINARY 
df_icr <- build_icr_df(
  icr_resl_ptyp_biz,
  "RESL",
  "Firm or Trade Association Plaintiff (binary)",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. The presence or absence of a firm or trade association plaintiff is treated as a binary variable."
  )

rm(resl_ICR_biz, icr_resl_ptyp_biz)

### RESL plt_typ - NGO binary ####

# build matrix for plaintiff type
resl_ICR_plt <- resl_ICR %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,plt_typ
  ) %>%
  unnest_wider(
    plt_typ,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = plt_typ_1:last_col(),
    names_to = "names_val",
    values_to = "plt_typ",
    names_prefix = "plt_typ_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(plt_typ)
  ) %>%
  mutate(
    plt_typ = case_when(
      plt_typ == "ngo" ~ "ngo",
      TRUE ~ "not"
    ),
    plt_typ = trimws(plt_typ)
  ) %>%
  ungroup() %>%
  mutate(
    plt_typ = factor(
      x = plt_typ,
      levels = unique(plt_typ)
    ),
    plt_typ = as.numeric(plt_typ)
  ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "plt_typ"
  ) %>%
  select(-ID) %>%
  as.matrix()

# calculate ICR
icr_resl_ptyp_ngo <- 
  krippendorffs.alpha(
    resl_ICR_plt,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_ptyp_ngo)

# add RESL plaintiff type ICR - ENGO BINARY 
df_icr <- build_icr_df(
  icr_resl_ptyp_ngo,
  "RESL",
  "ENGO Plaintiff (binary)",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. The presence or absence of an ENGO plaintiff is treated as a binary variable."
  )

rm(resl_ICR_plt, icr_resl_ptyp_ngo)


### RESL def_typ ####

# build matrix for defendant type
resl_ICR_def <- resl_ICR %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,def_typ
  ) %>%
  unnest_wider(
    def_typ,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = def_typ_1:last_col(),
    names_to = "names_val",
    values_to = "def_typ",
    names_prefix = "def_typ_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(def_typ)
  ) %>%
  mutate(
    def_typ = trimws(def_typ)
  ) %>%
  ungroup() %>%
  mutate(
    def_typ = factor(
      x = def_typ,
      levels = unique(def_typ)
    ),
    def_typ = as.numeric(def_typ)
  ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "def_typ"
  ) %>%
  select(-ID) %>%
  as.matrix()

# calculate ICR
icr_resl_dtyp <- 
  krippendorffs.alpha(
    resl_ICR_def,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_dtyp)

# add RESL defendant type ICR 
df_icr <- build_icr_df(
  icr_resl_dtyp,
  "RESL",
  "Defendant Type",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. Unique decision-defendant-type units are treated as independent observations, since a single decision can have multiple defendant type codes assigned to it."
  )

rm(resl_ICR_def, icr_resl_dtyp)


### RESL agy ####

# build matrix for agency type
resl_ICR_agy <- resl_ICR %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,agy
  ) %>%
  unnest_wider(
    agy,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = agy_1:last_col(),
    names_to = "names_val",
    values_to = "agy",
    names_prefix = "agy_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(agy)
  ) %>%
  mutate(
    agy = trimws(agy),
    # intentionally introducing error
    agy = case_when(
      coder == "ETD1" ~ "epa",
      TRUE ~ agy
    )
  ) %>%
  ungroup() %>%
  mutate(
    agy = factor(
      x = agy,
      levels = unique(agy)
    ),
    agy = as.numeric(agy),
  ) %>%
  # looking for disagreement
  # group_by(
  #   ID
  # ) %>%
  # mutate(
  #   avg_agy_diff = mean(agy)
  # ) %>%
  # ungroup() %>%
  # mutate(
  #   avg_agy_diff = agy-avg_agy_diff
  # )
  pivot_wider(
    names_from = "coder",
    values_from = "agy"
  ) %>%
  select(-ID) %>%
  as.matrix()

# calculate ICR
icr_resl_agy <- 
  krippendorffs.alpha(
    resl_ICR_agy,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_agy)

# add RESL agency type ICR 
df_icr <- build_icr_df(
  icr_resl_agy,
  "RESL",
  "Agency",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. Unique decision-agency units are treated as independent observations, since a single decision can have multiple agency codes assigned to it."
  )

rm(resl_ICR_agy, icr_resl_agy)

### RESL statute ####


# build matrix for plaintiff type
resl_ICR_stat <- resl_ICR %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,statute
  ) %>%
  unnest_wider(
    statute,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = statute_1:last_col(),
    names_to = "names_val",
    values_to = "statute",
    names_prefix = "statute_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(statute)
  ) %>%
  mutate(
    statute = trimws(statute)
  ) %>%
  ungroup() %>%
  mutate(
    statute = factor(
      x = statute,
      levels = unique(statute)
    ),
    statute = as.numeric(statute)
  ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "statute"
  ) %>%
  select(-ID) %>%
  as.matrix()

# calculate ICR
icr_resl_stat <- 
  krippendorffs.alpha(
    resl_ICR_stat,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_stat)

# add RESL statute ICR 
df_icr <- build_icr_df(
  icr_resl_stat,
  "RESL",
  "Statute",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. Unique decision-statute units are treated as independent observations, since a single decision can have multiple statute codes assigned to it."
  )

rm(resl_ICR_stat, icr_resl_stat)

### RESL climate ####

# NUMERIC DISTANCE
# build matrix for plaintiff type
resl_ICR_climate <- resl_ICR %>%
  rename(
    "climate" = "climate_count"
  ) %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,climate
  ) %>%
  mutate(
    climate = as.numeric(climate)
  ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "climate"
  ) %>%
  select(-ID) %>%
  as.matrix()

# calculate ICR - interval/numeric
icr_resl_climate <- 
  krippendorffs.alpha(
    resl_ICR_climate,
    level = "interval",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_climate)

# add RESL climate ICR - interval/numeric
df_icr <- build_icr_df(
  icr_resl_climate,
  "RESL",
  "Climate (numeric interval)",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. Computed as numeric distance between number of climate keywords coded across observations."
  )


# NOMINAL
# build matrix for plaintiff type
resl_ICR_climate <- resl_ICR %>%
  rename(
    "climate" = "climate_count"
  ) %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,climate
  ) %>%
  unnest_wider(
    climate,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = climate_1:last_col(),
    names_to = "names_val",
    values_to = "climate",
    names_prefix = "climate_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(climate)
  ) %>%
  mutate(
    climate = trimws(climate)
  ) %>%
  ungroup() %>%
  mutate(
    climate = factor(
      x = climate,
      levels = unique(climate)
    ),
    climate = as.numeric(climate)
  ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "climate"
  ) %>%
  select(-ID) %>%
  as.matrix()


# calculate ICR - nominal
icr_resl_climate <- 
  krippendorffs.alpha(
    resl_ICR_climate,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_climate)

# add RESL climate ICR - nominal
df_icr <- build_icr_df(
  icr_resl_climate,
  "RESL",
  "Climate (nominal)",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. Computed by treating coded number of climate keywords as nominal values that either match across coders or do not."
  )


# NOMINAL - BINARY
# build matrix for plaintiff type
resl_ICR_climate <- resl_ICR %>%
  rename(
    "climate" = "climate_count"
  ) %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,climate
  ) %>%
  unnest_wider(
    climate,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = climate_1:last_col(),
    names_to = "names_val",
    values_to = "climate",
    names_prefix = "climate_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(climate)
  ) %>%
  mutate(
    climate = trimws(climate)
  ) %>%
  ungroup() %>%
  mutate(
    climate = case_when(
      climate == "unknown" ~ "0",
      TRUE ~ climate
    ),
    climate = case_when(
      climate == "0" ~ "none",
      TRUE ~ "some"
    ),
    climate = factor(
      x = climate,
      levels = unique(climate)
    ),
    climate = as.numeric(climate)
  ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "climate"
  ) %>%
  select(-ID) %>%
  as.matrix()


# calculate ICR - nominal - binary
icr_resl_climate <- 
  krippendorffs.alpha(
    resl_ICR_climate,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_climate)

# add RESL climate ICR - nominal - binary
df_icr <- build_icr_df(
  icr_resl_climate,
  "RESL",
  "Climate (nominal binary)",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. Computed by treating the presence or absence of climate keywords as binary nominal values."
  )

rm(resl_ICR_climate, icr_resl_climate)

### RESL ej ######

# NUMERIC 
resl_ICR_ej <- resl_ICR %>%
  rename(
    "ej" = "ej_count"
  ) %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,ej
  ) %>%
  mutate(
    ej = as.numeric(ej)
  ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "ej"
  ) %>%
  select(-ID) %>%
  as.matrix()

# calculate ICR
icr_resl_ej <- 
  krippendorffs.alpha(
    resl_ICR_ej,
    level = "interval",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_ej)


# add RESL climate ICR - interval/numeric
df_icr <- build_icr_df(
  icr_resl_ej,
  "RESL",
  "Environmental Justice (numeric interval)",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. Computed as numeric distance between number of environmental justice keywords coded across observations."
  )


# NOMINAL
# build matrix for plaintiff type
resl_ICR_ej <- resl_ICR %>%
  rename(
    "ej" = "ej_count"
  ) %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,ej
  ) %>%
  unnest_wider(
    ej,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = ej_1:last_col(),
    names_to = "names_val",
    values_to = "ej",
    names_prefix = "ej_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(ej)
  ) %>%
  mutate(
    ej = trimws(ej)
  ) %>%
  ungroup() %>%
  mutate(
    ej = factor(
      x = ej,
      levels = unique(ej)
    ),
    ej = as.numeric(ej)
  ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "ej"
  ) %>%
  select(-ID) %>%
  as.matrix()

# calculate ICR
icr_resl_ej <- 
  krippendorffs.alpha(
    resl_ICR_ej,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_ej)

# add RESL climate ICR - nominal
df_icr <- build_icr_df(
  icr_resl_ej,
  "RESL",
  "Environmental Justice (nominal)",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. Computed by treating coded number of environmental justice keywords as nominal values that either match across coders or do not."
  )

# NOMINAL - BINARY
# build matrix for plaintiff type
resl_ICR_ej <- resl_ICR %>%
  rename(
    "ej" = "ej_count"
  ) %>%
  group_by(
    coder
  ) %>%
  select(
    ID,coder,ej
  ) %>%
  unnest_wider(
    ej,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = ej_1:last_col(),
    names_to = "names_val",
    values_to = "ej",
    names_prefix = "ej_"
  ) %>%
  mutate(
    ID = str_c(ID,names_val,sep = "-")
  ) %>%
  select(
    -names_val
  ) %>%
  filter(
    !is.na(ej)
  ) %>%
  mutate(
    ej = trimws(ej)
  ) %>%
  ungroup() %>%
  mutate(
    ej = case_when(
      ej == "unknown" ~ "0",
      TRUE ~ ej
    ),
    ej = case_when(
      ej == "0" ~ "none",
      TRUE ~ "some"
    ),
    ej = factor(
      x = ej,
      levels = unique(ej)
    ),
    ej = as.numeric(ej)
  ) %>%
  pivot_wider(
    names_from = "coder",
    values_from = "ej"
  ) %>%
  select(-ID) %>%
  as.matrix()


# calculate ICR - nominal - binary
icr_resl_ej <- 
  krippendorffs.alpha(
    resl_ICR_ej,
    level = "nominal",
    control = list(parallel = FALSE),
    verbose = TRUE)

# summarize ICR
summary(icr_resl_ej)

# add RESL ej ICR - nominal - binary
df_icr <- build_icr_df(
  icr_resl_ej,
  "RESL",
  "Environmental Justice (nominal binary)",
  df_icr,
  "Computed from 200 random observations of RESL data coded three times each. Computed by treating the presence or absence of environmental justice keywords as binary nominal values."
  )

rm(resl_ICR_ej, icr_resl_ej)








# Reliability for RESL meta-categories - pre-validation ####

resl_icr_pv <- read_csv("Data/For_reliability/resl_icr_meta.csv")

# count total number of unique cases coded
length(unique(resl_icr_pv$ID))

test <- resl_icr_pv %>%
  select(ID) %>%
  group_by(ID) %>%
  mutate(
    n = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  mutate(
    avg = mean(n, na.rm = T),
    med = median(n, na.rm = T),
    min = min(n),
    max = max(n)
  )

# build ICR matrix 
resl_icr_pv_ooc_mc1 <- resl_icr_pv %>%
  select(
    assigned, ID, ooc_mc1
  ) %>%
  # take the first ooc_mc1, not every ooc_mc1
  filter(
    str_detect(ooc_mc1,"%", negate = TRUE)
  ) %>%
  mutate(
    ooc_mc1 = as_factor(ooc_mc1),
    ooc_mc1 = as.numeric(ooc_mc1)
  ) %>%
  pivot_wider(
    names_from = assigned,
    values_from = ooc_mc1
  ) %>%
  select(
    -ID
  ) %>%
  as.matrix()

# calculate ICR
icr_ooc_mc1 <- 
  krippendorffs.alpha(
    resl_icr_pv_ooc_mc1,
    level = "nominal",
    control = list(parallel = TRUE, nodes = 10),
    verbose = TRUE)

# summarize ICR
summary(icr_ooc_mc1)

# results: 
#      Estimate  Lower  Upper
#alpha   0.8189 0.7304 0.8814

# add RESL meta-category ICR
df_icr <- build_icr_df(
  icr_ooc_mc1,
  "RESL",
  "Substantive Case Focus (Meta-Categories)",
  df_icr,
  "Computed from 101 random observations of pre-validation RESL data coded three or four times each (n = 354, µ = 3.50, M = 4) comparing substantive focus meta-category applications."
  )

# Remove unnecessary objects
rm(resl_icr_pv_ooc_mc1,icr_ooc_mc1, resl_icr_pv, resl_ICR)

# write out reliability df ####
write_csv(
  df_icr,
  "Data/For_reliability/icr_table.csv"
)


# the end.
