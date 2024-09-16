# Script to clean and further process district level enviro data; 
# compare data sources
# Written by Chris Rea
# Last Modified April 24, 2024

# Notes ####
# After pre-processing and coding litigant types, data are inspected to look
# for any anomalies or unexpected features. As is shown below, some federal
# court districts - two in particular - experience very unusual bouts of
# litigation: one related to the BP Deepwater Horizon oil spill and the other
# related to a highly unusual set of suits all targeting the same defendant,
# IMC Global, Inc. The following script investigates and documents these
# irregularities and, ultimately, purges them from the data that is used for
# further analysis given their anomalous character and distortions of
# underlying trends.

# Additionally, this script codes the data by case outcomes, and removes cases
# where no resolution is reached - a critical piece of information for an
# analysis focused on outcomes.

# Load packages ####
library(tidyverse)
library(scales)
library(stats)

# Read in and format RESL data ####

# !! NOTE: these data can be downloaded from the Harvard Dataverse:
# https://dataverse.harvard.edu/dataverse/resl_eld
# The script won't run without modification until these data have been download
# and added to the path below: 
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

# Get district linking readable court names to FJC alpha numeric district codes #####

# read in; join to RESL data
 court_crosswalk <- read_csv("Data/Crosswalks/unique_court_values_crosswalk.csv") %>%
   rename(
     "court" = "Court"
     )
 
 resl <- left_join(
   resl,
   court_crosswalk,
   by = "court"
 )
 
 rm(court_crosswalk)

# Code outcomes; add districts ####
fjc_e <- fjc_e %>%
  ungroup() %>%
  # first, identify cases that are dismissed, settled, or reach judgement.
  mutate(
    jud_or_set = case_when(
      # note -8 signals missing disposition;it is excluded
      # see FJC codebook for DISP codes
      DISP %in% c(2,3,12,13,14,4,5,6,7,8,9,15,16,17,18,19,20) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # next, code for plt/def wins and losses
  # PLT wins include:
  # - all judgements for plaintiff or both
  # - all settlements
  # PLT loses include:
  # - all judgements for defendant
  # - all dismissals except settlements
  # DEF wins include
  # - all judgements for defendant or both
  # - all dismissals except settlements
  # DEF losses include:
  # - all judgements for plaintiff
  # - all settlements
  mutate(
    PLT_wl = case_when(
      JUDGMENT %in% c(1,3) | DISP == 13 ~ "w",
      JUDGMENT == 2 | DISP %in% c(2,3,12,14) ~ "l",
      TRUE ~ "n"
    ),
    DEF_wl = case_when(
      JUDGMENT %in% c(2,3) | DISP %in% c(2,3,12,14) ~ "w",
      JUDGMENT %in% c(1) | DISP == 13 ~ "l",
      TRUE ~ "n"
    )
  ) %>%
  # finally, join geographic (district, region) info
  left_join(
    dist_crosswalk, by = "DISTRICT"
  )

# Plot total cases for year for select districts ####

# call function to build district-year dataframe
source("Functions/dist_df.R")


# Examine trends in just a few districts that have exceptionally high rates of
# litigation. The most striking of all of theses is the Louisiana Eastern
# District

# look at just:
# - Louisiana Eastern (3L): 1055 cases in 2018, 1035 in 2016, 801 in 2017, 690 in 2013, 371 in 2015, all mostly IND and BIZ
# - South Carolina (20): 947 cases in 2001, mostly IND
# - Mississippi Southern (38) 210 in 2003, 209 in 2004, mostly IND

build_dist_df(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = FALSE,
  diag = TRUE,
  yr = TRUE
  ) %>%
  filter(
    # aggregated counts are the same for w, l, n
    PLT_wl == "w"
  ) %>%
  filter(
    PLT_typ == "NGO"|
      PLT_typ == "BIZ" |
      PLT_typ == "IND" |
      PLT_typ == "FED"
  ) %>%
  mutate(
    PLT_typ = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      TRUE ~ PLT_typ
    )
  ) %>%
  filter(
    DISTRICT %in% c("3L","20","38")
  ) %>%
  ggplot() +
  geom_line(#primary lines
    aes(
      x = yr_file,
      y = d,
      group = DISTRICT,
      color = DISTRICT
    ),
    alpha = .6
  ) +
  geom_point(#primary points
    aes(
      x = yr_file,
      y = d,
      group = DISTRICT,
    ),
    shape = 3,
    alpha = .6
  ) +
  #scale_color_continuous_diverging(palette = "Green-Brown") +
  #scale_color_discrete_qualitative(palette = "Dark 3") +
  scale_color_viridis_d() +
  #scale_color_manual(values = palette) +
  labs(
    x = NULL,
    y = "No. Cases",
    #color = "σ above/below\nmean win rate (μ)",
    color = "region"
  ) +
  facet_wrap(
    vars(PLT_typ),
    ncol = 2
  ) +
  guides(shape = guide_legend(override.aes = list(size = 4))) +
  theme_linedraw() +
  theme(
    panel.grid.major = element_blank()
  )

# Manually examine cases in districts with abnormal spikes in litigation ####


# first look at cases from South Carolina in the years 2000-2003 (spike is in 2001)
# (Region 20)
fjc_e_SC20 <- fjc_e %>%
  filter(
    DISTRICT == "20",
    yr_file >= 2000 & yr_file <= 2003
  )
# --> 925 cases filed with IMC GLOBAL INC as defendant.
rm(fjc_e_SC20)


# next look at cases from Louisiana Eastern in the years 2010-2020 (spike is in 2012-2018)
# (Region 3L)
fjc_e_LA3L <- fjc_e %>%
  filter(
    DISTRICT == "3L",
    yr_file >= 2010 & yr_file <= 2020
  )
# --> 2,656 cases filed with BP EXPLORATION & PRODUC, ET AL as defendant
# --> 3,004 cases filed with "BP" of some kind in defendant name
# --> 1,127 counter-suits with "BP" in Plaintiff name

#excluding BP as plaintiff or defendant ....
fjc_e_LA3L_noBP <- fjc_e %>%
  filter(
    DISTRICT == "3L",
    yr_file >= 2010 & yr_file <= 2020,
    str_detect(DEF,"BP", negate = T),
    str_detect(PLT,"BP", negate = T)
  )
# --> yields only 144 cases over 21-year period
# --> some cases still contain BP in others forms (B.P., British Petroleum, etc.)
rm(fjc_e_LA3L, fjc_e_LA3L_noBP)


# finally look at Mississippi Southern District from 2000 to 2005 (spike is in 2003-2004)
# (Region 38)
fjc_e_MS38 <- fjc_e %>%
  filter(
    DISTRICT == "38",
    yr_file >= 2000 & yr_file <= 2005
  )
# --> 413 cases (of 429) list Jackson County as defendant
rm(fjc_e_MS38)


# Build dfs for comparing data sources, subsets, etc. ####

# resl data are judicial decisions, but we record the docket numbers assocaited
# with those decisions, which indicate ful cases. We can use this to estimate
# the number of cases those decisions correspond to.

# make simple annual counts of resl DECISIONS for plot with fjc
resl_simple_ct <- resl%>%
  group_by(yr_term) %>%
  mutate(
    yr_n = n(),
    dta = "resl"
    ) %>%
  select(
    yr_term,
    yr_n,
    dta
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup()

# using docket numbers, estimate the number of CASES per year
resl_cases <- resl %>%
  rowwise() %>%
  select(
    ID, case_date, yr_term, district, docket
    ) %>%
   mutate(
    docket = unique(str_split(docket, "%"))
    ) %>%
   unnest_wider(
    docket,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = docket_1:last_col(),
    names_to = "names_val",
    values_to = "docket",
    names_prefix = "docket_"
  ) %>%
  filter(
    !is.na(docket),
    yr_term >= 1988
  ) %>%
  # append district to docket for better estimate of unique cases (identical
  # dockets in same district are probably same case; identical dockets across
  # districts are probably different cases)
  mutate(
    dist_docket = str_c(district, docket, sep = "-")
  ) %>%
  # count number of decisions per case (i.e., per docket number)
  group_by(
    dist_docket
  ) %>%
  mutate(
    c_no = cur_group_id(), # number each case (dist_docket)
    d_per_c = n()
  ) %>%
  ungroup() %>%
  #calculate mean and median decisions per case
  mutate(
    c_no = max(c_no), # keep the max case number, which is the number of caeses
    d_per_c_avg = mean(d_per_c, na.rm = T),
    d_per_c_med = median(d_per_c, na.rm = T)
    ) %>%
  # count number of cases per decision
  group_by(
    ID
  ) %>%
  mutate(
    d_no = cur_group_id(), # number each docket number
    c_per_d = n()
  ) %>%
  ungroup() %>%
  #calculate mean and median decisions per case
  mutate(
    d_no = max(d_no), # keep the maximum docket number, which is the number of docket numbers
    c_per_d_avg = mean(c_per_d, na.rm = T),
    c_per_d_med = median(c_per_d, na.rm = T)
  )

# count the number of cases that have more than one decision
resl_cases_mult <- resl_cases %>%
  filter(
    d_per_c > 1
  ) %>%
  group_by(
    dist_docket
  ) %>% 
  filter(
    row_number() == 1
  )
length(resl_cases_mult$ID)


# count the number of decisions that have more than one case
resl_cases_mult <- resl_cases %>%
  filter(
    c_per_d > 1
  ) %>%
  group_by(
    ID
  ) %>% 
  filter(
    row_number() == 1
  )
length(resl_cases_mult$ID)

# remove these conting dfs
rm(resl_cases_mult)

# now finish filtering just by resl cases

resl_cases <- resl_cases %>%
  # keep only oldest example of each unique dist_docket (we don't want to count
  # the same case multiple times) note that errors are possible: the same docket
  # can, rarely, represent different cases in the same district. This is
  # unlikely in a subset of only environmental civil cases, though.
  group_by(
    dist_docket
  ) %>%
  filter(
    # take the oldest (last) given `arrange` call above; in cases of tie for oldest,
    # take last observation (next `filter` call)
    case_date == max(case_date)
  ) %>%
  filter(
    case_date == last(case_date)
  )



  
# build simple resl CASES estimate
resl_cases_ct <- resl_cases %>%
  group_by(
    yr_term
  ) %>%
  mutate(
    yr_n = n(),
    dta = "resl_cases"
  ) %>%
  select(
    yr_term, yr_n, dta
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup()

# calculate cases:decisions ratio
resl_c_to_d <- left_join(
  resl_cases_ct,
  resl_simple_ct %>%
    select(
      yr_term, yr_n
    ) %>%
    rename(
      "d_n" = "yr_n"
    ),
  by = "yr_term"
  ) %>%
  mutate(
    c_to_d = yr_n/d_n,
    yr_tot = sum(yr_n),
    d_tot = sum(d_n),
    c_to_d_tot = yr_tot/d_tot
  ) %>%
  select(
    yr_term, c_to_d, c_to_d_tot
  )

# plot ratio
resl_c_to_d %>%
  mutate(
    facet_lab = "Ratio of Est. Cases to Decisions in LexisNexis data"
  ) %>%
  ggplot(
    aes(
      x = yr_term,
      y = c_to_d
    )
  ) +
  geom_segment(
    aes(
      y = 1,
      x = 1985,
      xend = 2025,
      yend = 1
    ),
    color = "#dadada",
    linetype = 2
  ) +
  geom_line() +
  geom_point(
    shape = 3,
    alpha = .6
  ) +
  
  labs(
    x = NULL,
    y = "cases-to-decisions ratio"
  ) +
  facet_wrap(
    vars(facet_lab)
  ) +
  theme_linedraw() +
  theme(
    panel.grid.major = element_blank()
  ) +
  ylim(
    c(0,1.5)
  )

# save
ggsave(
  "Ratio_of_Est_Cases_to_Decisions.png",
  path = "Figures",
  height = 5,
  width =8
)

# count nubmer of cases per decision and vice versa
  
# also load ALL resl decisions (including un-coded) and make annual counts of these
# NOTE: these include "non-environmental" cases. In the 7,500 cases coded, 64.73
# percent of cases were environmental. So, at the end of the annual count
# process, reduce annual counts by factor of .64733.

# ADDITIONALLY, RESL cases do not include case terminations that do not result in written
# opinions, e.g. dismissals, including settlements. So, after reducing for non-
# environmental cases, we need to multiply by an annual "dismissal factor," 
# calculated empirically from FJC data above.

resl_all <- read_csv(
  "Data/RESL/RESL_all_unique_IDs.csv"
  ) %>%
  mutate(
   yr_file =  str_sub(ID,1,4),
   yr_file = as.numeric(yr_file)
  ) %>%
  group_by(
    yr_file
  ) %>%
  mutate(
    yr_n = round(n()*.64733,0),
    dta = "resl_all"
  ) %>%
  filter(
    row_number() == 1,
    yr_file >= 1988
  ) %>%
  ungroup() %>%
  select(
    -ID
  )

# make simple annual counts for plot with fjc and resl data together
fjc_e_simple_ct <- fjc_e %>%
  group_by(yr_file) %>%
  mutate(
    yr_n = n(),
    # calculating dismissal rate
    yr_dis_rt = case_when(
      DISP %in% c(2,3,12,13,14) ~ 1,
      TRUE ~ 0
    ),
    yr_dis_rt = sum(yr_dis_rt),
    yr_dis_rt = yr_dis_rt/yr_n
  ) %>%
  select(
    yr_file,
    yr_n,
    yr_dis_rt
  ) %>%
  mutate(
    dta = "fjc_tot"
  ) %>%
  filter(
    row_number() == 1,
    yr_file >= 1988
  ) %>%
  ungroup()

# make simple annual counts for plot with fjc and resl data together - NO DISS
fjc_e_simple_ct_NO_DISS <- fjc_e %>%
  group_by(yr_file) %>%
  mutate(
    yr_n = n(),
    yr_dis_rt = case_when(
      DISP %in% c(2,3,12,13,14) ~ 1,
      TRUE ~ 0
    ),
    yr_dis_rt = sum(yr_dis_rt),
    yr_dis_rt = yr_dis_rt/yr_n
  ) %>%
  #filter out dismissals
  filter(
   !(DISP %in% c(2,3,12,13,14))
  ) %>%
  #re-compute total number of cases
  mutate(
    yr_n = n()
  ) %>%
  select(
    yr_file,
    yr_n,
    yr_dis_rt
  ) %>%
  mutate(
    dta = "fjc_tot_no_diss"
  ) %>%
  filter(
    row_number() == 1,
    yr_file >= 1988
  ) %>%
  ungroup()


# call function to make basic filtered df
source("Functions/simple_filter.R")

# make simple annual counts for plot with fjc and resl data together
fjc_e_filtered_simple_ct <- simp_filt(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE
  ) %>%
  group_by(yr_file) %>%
  mutate(
    yr_n = n(),
    yr_dis_rt = case_when(
      DISP %in% c(2,3,12,13,14) ~ 1,
      TRUE ~ 0
    ),
    yr_dis_rt = sum(yr_dis_rt),
    yr_dis_rt = yr_dis_rt/yr_n
  ) %>%
  select(
    yr_file,
    yr_n,
    yr_dis_rt
  ) %>%
  mutate(
    dta = "fjc_fil"
  ) %>%
  filter(
    row_number() == 1,
    yr_file >= 1988
  ) %>%
  ungroup()


# make simple annual counts for plot with fjc and resl data together - NO DISS
fjc_e_filtered_simple_ct_NO_DISS <- simp_filt(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE
  ) %>%
  group_by(yr_file) %>%
  mutate(
    yr_n = n(),
    yr_dis_rt = case_when(
      DISP %in% c(2,3,12,13,14) ~ 1,
      TRUE ~ 0
    ),
    yr_dis_rt = sum(yr_dis_rt),
    yr_dis_rt = yr_dis_rt/yr_n
  ) %>%
  #filter out dismissals
  filter(
   !(DISP %in% c(2,3,12,13,14))
  ) %>%
  #re-compute total number of cases
  mutate(
    yr_n = n()
  ) %>%
  select(
    yr_file,
    yr_n,
    yr_dis_rt
  ) %>%
  mutate(
    dta = "fjc_fil_no_diss"
  ) %>%
  filter(
    row_number() == 1,
    yr_file >= 1988
  ) %>%
  ungroup()

# change yr term to yr file
resl_simple_ct <- resl_simple_ct %>%
  rename(
    "yr_file" = "yr_term"
  )

resl_cases_ct <- resl_cases_ct %>%
  rename(
    "yr_file" = "yr_term"
  )

# create stand-alone dismissal rate df
fjc_dis_rt <- fjc_e_filtered_simple_ct %>%
  select(
    yr_file, yr_dis_rt
  )

# read in fjc_tots file; format
fjc_tots <- read_csv(
  "Data/FJC_preprocessed/fjc_tots.csv"
  ) %>%
  select(
    yr_file
  ) %>%
  group_by(
    yr_file
  ) %>%
  mutate(
    yr_n = n(),
    dta = "fjc_ALL"
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup()

# gargabe collect, to reduce accumulated memory load
# (fjc_tots/csv is a large file heavily paired down in the above code)
gc()




# Adjust resl_all by annual fjc dismissal rate:
# join dis_rt to resl_all; adjust resl_counts; drop yr_dis_rt
resl_all_no_DISS <- left_join(resl_all,fjc_dis_rt, by = "yr_file") %>%
  mutate(
    yr_n = round(yr_n*(1+yr_dis_rt), 0),
    dta = "resl_all_NO_DISS"
  ) %>%
  select(
    -yr_dis_rt
  )

# bind dfs.
fjc_resl_case_cnts <- bind_rows(
  fjc_e_filtered_simple_ct,
  fjc_e_filtered_simple_ct_NO_DISS,
  fjc_e_simple_ct,
  fjc_e_simple_ct_NO_DISS,
  resl_simple_ct,
  resl_all,
  resl_all_no_DISS,
  fjc_tots,
  resl_cases_ct
  )

# remove individual dfs
rm(
  fjc_e_filtered_simple_ct,
  fjc_e_filtered_simple_ct_NO_DISS,
  fjc_e_simple_ct,
  fjc_e_simple_ct_NO_DISS,
  resl_simple_ct,
  resl_all,
  resl_all_no_DISS,
  fjc_tots,
  resl_cases_ct,
  resl_c_to_d
  )

gc()

# drop any data prior to 1988; calc enviro cases as a fraction of a all fjc
# cases
fjc_resl_case_cnts <- fjc_resl_case_cnts %>%
  #rowwise() %>%
  filter(
    yr_file >= 1988, yr_file <= 2022
  ) %>%
  group_by(
    yr_file, dta
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  mutate(
    yr_n_pct_fjc = yr_n/yr_n[dta == "fjc_ALL"]*100
  ) 

# create stand-alone df to bind to the bottom
fjc_ALL_pcts <- fjc_resl_case_cnts %>%
  mutate(
    dta = str_c(dta,"_fjc_ALL_pct",sep = "")
  ) %>%
  select(
    yr_file, dta, yr_n_pct_fjc
  ) %>%
  rename(
    "yr_n" = "yr_n_pct_fjc"
  ) 
  
# bind to existing data
fjc_resl_case_cnts <- bind_rows(fjc_resl_case_cnts,fjc_ALL_pcts)

rm(fjc_ALL_pcts)

fjc_resl_case_cnts <- fjc_resl_case_cnts %>%
  ungroup() %>%
  mutate(
    yr_n_pct_fjc = yr_n/yr_n[dta == "fjc_ALL"]*100
    ) 

# add labels for facet ; drop unlabeled
fjc_resl_case_cnts <- fjc_resl_case_cnts %>%
  mutate(
    facet_names = case_when(
      dta == "fjc_ALL" ~ "All FJC IDB Cases",
      (dta == "fjc_fil" |
         dta == "fjc_tot" |
         dta == "resl_all") ~ "\"Environmental Matters\" Cases",
      (dta == "fjc_fil_fjc_ALL_pct" |
         dta == "fjc_tot_fjc_ALL_pct" |
         dta == "resl_all_fjc_ALL_pct") ~ "Percent \"Environmental Matters\" Cases",
      TRUE ~ "other"
    )
  ) %>%
  mutate(
    shape = case_when(
      dta == "fjc_ALL" ~ "FJC IDB - all data",
      dta == "fjc_fil" ~ "FJC IDB - data used for analysis",
      dta == "fjc_tot" ~ "FJC IDB - all enviro. cases",
      dta == "resl_all" ~ "Total LexisNexis Decisions",
      dta == "resl_cases" ~ "Total LexisNexis Decisions",
      dta == "fjc_fil_fjc_ALL_pct" ~ "FJC IDB - data used for analysis",
      dta == "fjc_tot_fjc_ALL_pct" ~ "FJC IDB - all enviro. cases",
      dta == "resl_all_fjc_ALL_pct" ~ "Total LexisNexis Decisions",
      TRUE ~ "unknown"
    )
  ) %>%
   filter(
     yr_file <= 2022
   )

# Plot environmental case burden (relative to all FJC suits) ####

# plot all cases for case burden
fjc_resl_case_cnts %>%
  filter(
    shape != "Total LexisNexis Decisions",
    shape != "other",
    shape != "unknown",
  ) %>%
  ggplot(
    aes(
      x = yr_file,
      y = yr_n,
      group = dta,
      color = shape,
      fill = shape,
      shape = shape,
      #linetype = line_type
    )
  ) +
  geom_point(
    #size = 1,
    #alpha = 0.7,
    color = "grey50"
  ) +
  geom_line(
    #size = 1,
    alpha = 0.5
  ) +
  scale_y_continuous(labels = comma) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_shape_manual(
    values = c(21,22,23,24,25)
  ) +
  scale_linetype_manual(
    values = c("solid","dashed")
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = "Data Source",
    fill = "Data Source",
    shape = "Data Source",
    linetype = "Data Source",
  ) +
  guides(
    linetype = "none"
  ) +
  facet_wrap(
    vars(facet_names),
    scales = "free_y"
  ) +
  theme_linedraw()

ggsave(
  "Environmental_Case_Burden.png",
  path = "Figures",
  height = 5,
  width =14
)

# Plot examination of anomalous suits ####

# plot
fjc_resl_case_cnts %>%
  filter(
    dta %in% c("fjc_tot","fjc_fil"#,
      #"resl_all","resl"
      )
  ) %>%
  ungroup() %>%
  mutate(
    line_type = case_when(
      dta == "resl_all" ~ "A",
      TRUE ~ "B"
    ),
    dta = case_when(
      dta == "fjc_tot" ~ "FJC - all cases",
      #dta == "fjc_tot" ~ "FJC - all cases (no dismissals)",
      dta == "fjc_fil" ~ "FJC - cases used for analysis",
      dta == "resl_all" ~ "LexisNexis - all env. judicial decisions (est.)",
      dta == "resl" ~ "LexisNexis - decisions used for\nanalysis",
      TRUE ~ dta
    ),
    #plot_label = "Comparing Data Sources"
    plot_label = "Examining Anomalous Case Clusters"
  ) %>%
  ggplot(
    aes(
      x = yr_file,
      y = yr_n,
      group = dta,
      color = dta,
      fill = dta,
      shape = dta,
      linetype = line_type
    )
  ) +
  geom_point(
    #size = 1,
    #alpha = 0.7,
    color = "grey50"
  ) +
  geom_line(
    #size = 1,
    alpha = 0.5
  ) +
  annotate(
    "text",
    x = 1998.75,
    y = 1800,
    label = "IMC Global, Inc. cases\n(n = 944)",
    size = 3
    ) +
  annotate(
    "text",
    x = 2013,
    y = 1800,
    label = "BP Deepwater Horizon\noil spill cases\n(n = 4,126)",
    size = 3
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_shape_manual(
    values = c(21,22,23,24)
  ) +
  #scale_linetype_manual(
  #  values = c("dashed","solid")
  #) +
  labs(
    x = NULL,
    y = "No. Cases",
    color = "Data Source",
    fill = "Data Source",
    shape = "Data Source",
    linetype = "Data Source",
  ) +
  guides(
    linetype = "none"
  ) +
  facet_wrap(
    vars(plot_label)
  ) +
  theme_linedraw() +
  ylim(
    c(0,1900)
  )

ggsave(
  "Examining_anom.png",
  path = "Figures",
  height = 5,
  width =8
  )

# Plot RESL data with periods #####

# plot
fjc_resl_case_cnts %>%
  filter(
    dta %in% c(#"fjc_tot_no_diss",
      #"fjc_fil_no_diss",
      "resl_all","resl"#,"resl_cases"
      )
  ) %>%
  ungroup() %>%
  mutate(
    line_type = case_when(
      dta == "resl_all" ~ "A",
      TRUE ~ "B"
    ),
    dta = case_when(
      dta == "fjc_tot_no_diss" ~ "FJC - all cases (no dismissals)",
      dta == "fjc_fil_no_diss" ~ "FJC IDB - cases used for analysis\n(excl. dismissals, which include\nsettlements)",
      dta == "resl_all" ~ "LexisNexis - all enviro. decisions (est.)",
      dta == "resl" ~ "LexisNexis - decisions used for\nanalysis",
      dta == "resl_cases" ~ "LexisNexis - cases (est.)",
      TRUE ~ dta
    ),
    plot_label = "LexisNexis Data in Three Periods"
    #plot_label = "Examining Anomalous Case Clusters"
  ) %>%
  ggplot(
    aes(
      x = yr_file,
      y = yr_n,
      group = dta,
      color = dta,
      fill = dta,
      shape = dta,
      linetype = line_type
    )
  ) +
  # background rectangle
  annotate("rect",
           fill = "#bcbcbc",
           alpha = 0.5,
           xmin = 2001,
           xmax = 2010,
           ymin = -Inf,
           ymax = Inf
           ) +
  # points
  geom_point(
    #size = 1,
    #alpha = 0.7,
    color = "grey50"
  ) +
  geom_line(
    #size = 1,
    alpha = 0.5
  ) +
  # 2001 line
  geom_segment(
    aes(
      x = 2001,
      xend = 2001,
      y = -Inf,
      yend = Inf
    ),
    color = "#bcbcbc",
    linetype = 2
  ) +
  # 2010 line
  geom_segment(
    aes(
      x = 2010,
      xend = 2010,
      y = -Inf,
      yend = Inf
    ),
    color = "#bcbcbc",
    linetype = 2
  ) +
  # first label
  annotate(
    "text",
    x = 1994,
    y = 750,
    label = "Fed. Supp."
  ) +
  # second label
  annotate(
    "text",
    x = 2005.5,
    y = 750,
    label = "Expansion"
  ) +
  # third label
  annotate(
    "text",
    x = 2016,
    y = 750,
    label = "All Decisions"
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_shape_manual(
    values = c(21,22,23,24)
  ) +
  scale_linetype_manual(
    values = c("dashed","solid")
  ) +
  labs(
    x = NULL,
    y = "No. Judicial Decisions",
    color = "Data Source",
    fill = "Data Source",
    shape = "Data Source",
    linetype = "Data Source",
  ) +
  guides(
    linetype = "none"
  ) +
  facet_wrap(
    vars(plot_label)
  ) +
  theme_linedraw()

ggsave(
  "RESL_Data_Periods.png",
  path = "Figures",
  height = 5,
  width = 8
  )


# Plot comparison of FJC and RESL data sources ####

# plot
fjc_resl_case_cnts %>%
  filter(
    dta %in% c(#"fjc_tot_no_diss",
      "fjc_fil_no_diss","resl_all","resl","resl_cases")
  ) %>%
  ungroup() %>%
  mutate(
    line_type = case_when(
      dta == "resl_all" ~ "A",
      TRUE ~ "B"
    ),
    dta = case_when(
      dta == "fjc_tot_no_diss" ~ "FJC - all cases (no dismissals)",
      dta == "fjc_fil_no_diss" ~ "FJC IDB - cases used for analysis\n(excl. dismissals, which include\nsettlements)",
      dta == "resl_all" ~ "LexisNexis - all judicial decisions (est.)",
      dta == "resl" ~ "LexisNexis - decisions used for\nanalysis",
      dta == "resl_cases" ~ "LexisNexis - cases (est.)",
      TRUE ~ dta
    ),
    plot_label = "Comparing Data Sources"
    #plot_label = "Examining Anomalous Case Clusters"
  ) %>%
  ggplot(
    aes(
      x = yr_file,
      y = yr_n,
      group = dta,
      color = dta,
      fill = dta,
      shape = dta,
      linetype = line_type
    )
  ) +
  geom_point(
    #size = 1,
    #alpha = 0.7,
    color = "grey50"
  ) +
  geom_line(
    #size = 1,
    alpha = 0.5
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_shape_manual(
    values = c(21,22,23,24)
  ) +
  scale_linetype_manual(
    values = c("dashed","solid")
  ) +
  labs(
    x = NULL,
    y = "No. Cases/Decisions",
    color = "Data Source",
    fill = "Data Source",
    shape = "Data Source",
    linetype = "Data Source",
  ) +
  guides(
    linetype = "none"
  ) +
  facet_wrap(
    vars(plot_label)
  ) +
  theme_linedraw()

ggsave(
  "Comparing_FJC_and_RESL_Data_Sources.png",
  path = "Figures",
  height = 5,
  width = 8
  )


# remove extra objects ####
rm(fjc_dis_rt, fjc_resl_case_cnts, resl_cases)

 # the end.