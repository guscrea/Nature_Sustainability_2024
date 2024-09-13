# Function to build district or district-year data for analysis

# inputs
# df = data frame to use as input
# jud = if TRUE, include only cases that have been settled or reached
#     judgement
# noBP_IMC = if TRUE, drop observations assocaited with BP oil spill in 
#     LA Eastern district (District 3L) and IMC Global, Inc. in South
#     Carolina (District 20)
# diag = if FALSE, drop intra-type suit observations (e.g., FED suing FED, 
#     BIZ suing BIZ, etc.)
# yr = if TRUE, create district year observations. Otherwise, aggregate over
#     all districts.

# for testing
df = "fjc_e"
jud = TRUE
noBP_IMC = TRUE
diag = FALSE
yr = TRUE

build_dist_df <- function(df, jud, noBP_IMC, diag, yr){
  df <- get(df)
  df <- df %>%
    ungroup() %>%
    # filter by whether case is resolved (settled or judgement)
    {
      if(jud == TRUE)
        filter(., jud_or_set == 1)
      } %>%
    # filter out (or not) BP and IMC global cases
    {
      if(noBP_IMC == TRUE)
        filter(
          .,
          !(DISTRICT == "3L" & (str_detect(PLT,"BP")==T | str_detect(DEF,"BP")==T)), # drop BP cases in LA
          !(DISTRICT == "20" & str_detect(DEF,"IMC")==T)
          )# drop IMC cases in SC
      } %>%
    # filter out intra-type suits
    {
      if(diag == FALSE)
        filter(
          .,
          PLT_typ != DEF_typ
        )
      } %>%
    # drop cases where the plaintiff type or defendant type is unknown
    # based on prior coding and cleaning, there should be no NAs!
    filter(
      .,
      !is.na(PLT_typ),
      !is.na(DEF_typ)
      ) %>%
    # for dist-yr data, compute case duration (in days)
    {
      if(yr == TRUE)
        mutate(
          .,
          duration = case_when(
            TERMDATE >= "1988-01-01" ~ as.numeric(TERMDATE - FILEDATE),
            TRUE ~ NA_real_
            )
          )
      } %>%
    # set grouping for either yr or dist-yr
    {
      if(yr == TRUE)
        group_by(., yr_file)
      else
        ungroup(.)
      } %>%
    # total cases, nation wide or nation-wide by year
    mutate(
      tot = n()
      ) %>%
    # count cases by district or district-yr
    {
      if(yr == TRUE)
        group_by(., DISTRICT, yr_file)
      else
        group_by(., DISTRICT)
      } %>%
    mutate(
      d = n(),
      d_pct = round(d/tot*100,4)
      ) %>%
    # count cases by region or region-yr
    {
      if(yr == TRUE)
        group_by(., REGION, yr_file)
      else
        group_by(., REGION)
      } %>%
    mutate(
      r = n(),
      r_pct = round(r/tot*100,4)
      ) %>%
    # count cases by plaintiff type or plaintiff-type-yr
    # for typ-yr, also compute average duration by type by year
    {
      if(yr == TRUE)
        group_by(., PLT_typ, yr_file) %>%
        # compute average duration by plaintiff type by year
        mutate(
          pt_dur = mean(duration, na.rm = T)
        )
      else
        group_by(., PLT_typ)
        } %>%
    mutate(
      pt = n(),
      pt_pct = round(pt/tot*100,4),
      pt_lab = str_c("n = ",prettyNum(pt, big.mark = ","), sep = ""),
      # count number of terminated cases, too, for calculating win rates
      pt_term = case_when(
        TERMDATE >= "1988-01-01" ~ 1,
        TRUE ~ 0
        ),
      pt_term = sum(pt_term, na.rm = T)
      ) %>%
    # count cases by plaintiff type by region or region-yr
    {
      if(yr == TRUE)
        group_by(., PLT_typ, REGION, yr_file)
      else
        group_by(., PLT_typ, REGION)
      } %>%
    mutate(
      r_pt = n(),
      r_pt_pct = round(r/tot*100,4),
      # count number of terminated cases, too, for calculating win rates
      r_pt_term = case_when(
        TERMDATE >= "1988-01-01" ~ 1,
        TRUE ~ 0
        ),
      r_pt_term = sum(r_pt_term, na.rm = T),
      ) %>%
    # count by plaintiff type by outcome, nation-wide or nation-wide by year
    {
      if(yr == TRUE)
        group_by(., PLT_typ, PLT_wl, yr_file)
      else
        group_by(., PLT_typ, PLT_wl)
      } %>%
    mutate(
      pt_pwl = n(),
      pt_pwl_pct = round(pt_pwl/pt_term*100,4), # calculate with number of terminated cases, not all cases
      pt_pwl_lab = str_c("μ = ",round(pt_pwl_pct, 1),"%", sep = "")
      ) %>%
    # calculate win loss ratio by PLT_typ or PLT_typ-yr
    {
      if(yr == TRUE)
        group_by(., PLT_typ, yr_file)
      else
        group_by(., PLT_typ)
      } %>%
    mutate(
      pt_wlr = round(first(pt_pwl[PLT_wl == "w"])/first(pt_pwl[PLT_wl == "l"]),2),
      pt_wlr_lab = str_c("w:l = ",pt_wlr,":1")
      ) %>%
    # count by plaintiff type by district or by district-year
    {
      if(yr == TRUE)
        group_by(., DISTRICT,PLT_typ,yr_file)
      else
        group_by(., DISTRICT,PLT_typ)
      } %>%
    mutate(
      d_pt = n(),
      d_pt_pct = round(d_pt/d*100,4),
      #d_pt_lab = str_c("n = ",prettyNum(d_pt, big.mark = ","), sep = "")
      # count number of terminated cases, too, for calculating win rates
      d_pt_term = case_when(
        TERMDATE >= "1988-01-01" ~ 1,
        TRUE ~ 0
      ),
      d_pt_term = sum(d_pt_term, na.rm = T)
      ) %>%
    # count by plaintiff type by outcome by district or by district-year
    {
      if(yr == TRUE)
        group_by(., DISTRICT,PLT_typ,PLT_wl,yr_file)
      else
        group_by(., DISTRICT,PLT_typ,PLT_wl)
      } %>%
    mutate(
      d_pt_pwl = n(),
      d_pt_pwl_pct = round(d_pt_pwl/d_pt_term*100,4)
      ) %>%
    # if only looking across districts, not district-years, calc std.
    # deviation (across districts) of win rates
    {
      if(yr != TRUE)
        group_by(., PLT_typ,PLT_wl)
      } %>%
    mutate(
      # calculate standard deviation of win rates across districts
      d_pt_pwl_pwl_stdev = sd(d_pt_pwl_pct, na.rm = T)
      ) %>%
    # count by plaintiff type by outcome by region or by region-year
    {
      if(yr == TRUE)
        group_by(., REGION,PLT_typ,PLT_wl,yr_file)
      else
        group_by(., REGION,PLT_typ,PLT_wl)
      } %>%
    mutate(
      r_pt_pwl = n(),
      r_pt_pwl_pct = round(r_pt_pwl/r_pt_term*100,4),
      ) %>%
    # if only looking across regions, not regions-years, calculate
    # fraction of cases in each district by plaintiff type by region
    {
      if(yr != TRUE)
        group_by(., PLT_typ, REGION)
      } %>%
     mutate(
      d_pt_r_pct = round(d_pt/r_pt*100,4)
      ) %>%
    # working at the finest grouping level possible, keep only
    # first row of each variable of interest (all other values)
    # should be identical within sub-grouping. Then, select variables
    # of interest 
    {
      if(yr == TRUE)
        group_by(., DISTRICT,PLT_typ,PLT_wl,yr_file) %>%
        filter(
          row_number() == 1
        ) %>%
        select(
          REGION,
          STATE_TERR,
          Judicial_2,
          DISTRICT,
          yr_file, # only in x-year data
          yr_term, # only in x-year data
          PLT_typ,
          PLT_wl,
          tot,
          d,
          d_pct,
          pt,
          pt_term,
          pt_dur, # only in x-year data
          pt_pct,
          pt_lab,
          pt_pwl,
          pt_pwl_pct,
          pt_pwl_lab,
          pt_wlr,
          pt_wlr_lab,
          d_pt,
          d_pt_term,
          d_pt_pct,
          d_pt_pwl,
          d_pt_pwl_pct,
          #d_pt_pwl_pwl_stdev, # only in non-yer data
          r_pt,
          r_pt_term,
          r_pt_pwl,
          r_pt_pwl_pct,
          #d_pt_r_pct # only in non-yer data
          ) %>%
        print("Done! District-year df built!")
      else
        group_by(., DISTRICT,PLT_typ,PLT_wl) %>%
        filter(
          row_number() == 1
        ) %>%
        select(
          REGION,
          STATE_TERR,
          Judicial_2,
          DISTRICT,
          #yr_file, # only in x-year data
          #yr_term, # only in x-year data
          PLT_typ,
          PLT_wl,
          tot,
          d,
          d_pct,
          pt,
          pt_term,
          #pt_dur, # only in x-year data
          pt_pct,
          pt_lab,
          pt_pwl,
          pt_pwl_pct,
          pt_pwl_lab,
          pt_wlr,
          pt_wlr_lab,
          d_pt,
          d_pt_term,
          d_pt_pct,
          d_pt_pwl,
          d_pt_pwl_pct,
          d_pt_pwl_pwl_stdev,
          r_pt,
          r_pt_term,
          r_pt_pwl,
          r_pt_pwl_pct,
          d_pt_r_pct
          )
    } %>%
    # for data for all years, across districts, group by  plaintiff type to
    # calculate mean and standard deviation across districts for number of
    # plaintiffs in a district
    {
      if(yr != TRUE)
        group_by(., PLT_typ) %>%
        mutate(
          pt_mean = mean(d_pt, na.rm = T),
          pt_stdev = sd(d_pt, na.rm = T)
          ) %>%
        # group by plaintiff type by district to calculate z-score for number of
        # plaintiffs types by district. make categorical version.
        group_by(DISTRICT,PLT_typ) %>%
        mutate(
          d_pt_z = (d_pt-pt_mean)/pt_stdev,
          d_pt_z_cat = case_when(
            d_pt_z <= -2.5 ~ "-2.5 > z",
            d_pt_z > -2.5 & d_pt_z <= -1.5 ~ "-2.5 < z <= -1.5",
            d_pt_z > -1.5 & d_pt_z <= -0.5 ~ "-1.5 < z <= -0.5",
            d_pt_z > -0.5 & d_pt_z <= 0.5 ~ "-0.5 < z <= 0.5",
            d_pt_z > 0.5 & d_pt_z <= 1.5 ~ "0.5 < z <= 1.5",
            d_pt_z > 1.5 & d_pt_z <= 2.5 ~ "1.5 < z <= 2.5",
            d_pt_z > 2.5 ~ "2.5 < z",
            TRUE ~ NA_character_
            )
          ) %>%
          # group by  plaintiff type and plaintiff win-loss to calculate standard
          # deviation across districts for win-loss by district
        group_by(
          PLT_typ,PLT_wl
          ) %>%
        mutate(
            pt_pwl_pct_stdev = sd(d_pt_pwl_pct, na.rm = T),
            pt_pwl_stdev_lab = str_c("σ = ",round(pt_pwl_pct_stdev, 1), sep = "")
            ) %>%
          # group by plaintiff type and plaintiff type win-loss by district to calculate
          # z-score
          group_by(DISTRICT,PLT_typ,PLT_wl) %>%
          mutate(
            d_pt_pwl_pct_z = (d_pt_pwl_pct-pt_pwl_pct)/pt_pwl_pct_stdev
            ) %>%
        ungroup()
      } %>%
    print("Done! District-level df built!")
    
    
    
    
    
    
    
    
    
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
}