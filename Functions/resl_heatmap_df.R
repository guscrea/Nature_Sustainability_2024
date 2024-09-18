# function to build RESL-based df with all NOS codes, only NOS == 893, or only
# NOS != 893

# NOS_codes can be:
# - "all"
# - "893"
# - "no893"

 resl_heat_df <- function(NOS_codes){
   # using docket numbers, estimate the number of CASES per year
   resl_heatmap <- resl_fjc %>%
     rowwise() %>%
     # first, go to cases, not decisions.
     mutate(
       docket = str_split(docket, "%")
     ) %>%
     unnest_wider(
       docket,
       names_sep = "_"
     ) %>%
     pivot_longer(
       cols = docket_1:docket_23,
       names_to = "names_val",
       values_to = "docket",
       names_prefix = "docket_"
     ) %>%
     filter(
       !is.na(docket),
       docket != "#N/A"
     ) %>%
     mutate(
       dist_docket = str_c(district, docket, sep = "-")
     ) %>%
     group_by(
       dist_docket
     ) %>%
     filter(
       row_number() == 1
     ) %>%
     select(
       resl_ID, fjc_NOS, case_date, yr_term, plt_typ, def_typ
     ) %>%
     unnest_wider(
       def_typ,
       names_sep = "_"
     ) %>%
     pivot_longer(
       cols = def_typ_1:last_col(),
       #cols = def_typ_1:def_typ_1,  #keep only lead defendnat
       names_to = "names_val",
       values_to = "def_typ",
       names_prefix = "def_typ_"
     ) %>%
     filter(
       !is.na(def_typ),
       yr_term >= 1988
     ) %>%
     select(
       resl_ID, fjc_NOS, case_date, yr_term, def_typ, plt_typ
     ) %>%
     # now unnest and make longer plaintiff names...
     unnest_wider(
       plt_typ,
       names_sep = "_"
     ) %>%
     pivot_longer(
       cols = plt_typ_1:last_col(),
       #cols = plt_typ_1:plt_typ_1, # keep only lead plaintiff
       names_to = "names_val",
       values_to = "plt_typ",
       names_prefix = "plt_typ_"
     ) %>%
     {
       if(NOS_codes == "all"){
         filter(
           .,
           !is.na(plt_typ)
           )
         }
       else if(NOS_codes == "893"){
         filter(
           .,
           !is.na(plt_typ),
           fjc_NOS == 893
           )
       }
       else if(NOS_codes == "no893"){
         filter(
           .,
           !is.na(plt_typ),
           fjc_NOS != 893
           )
         }
       } %>%
     select(
       resl_ID,  fjc_NOS, case_date, yr_term, plt_typ, def_typ
     ) %>%
     # recode types to match heatmap inputs
     mutate(
       plt_typ = case_when(
         plt_typ == "fed" ~ "FED",
         plt_typ == "individual" ~ "IND",
         plt_typ == "ngo" ~ "NGO",
         plt_typ == "civic_assn" ~ "CIVIC",
         plt_typ == "industry" ~ "BIZ",
         plt_typ == "state" ~ "STA",
         plt_typ == "trade_assn" ~ "BIZ",
         plt_typ == "local" ~ "LOC",
         plt_typ == "other" ~ "OTHER",
         plt_typ == "tribe" ~ "TRIBE",
         plt_typ == "public_org" ~ "LOC",
         plt_typ == "union" ~ "OTHER",
         plt_typ == "religious_org" ~ "NGO_O",
         plt_typ == "none" ~ "OTHER",
         plt_typ == "military" ~ "FED"
       ),
       def_typ = case_when(
         def_typ == "fed" ~ "FED",
         def_typ == "individual" ~ "IND",
         def_typ == "ngo" ~ "NGO",
         def_typ == "civic_assn" ~ "CIVIC",
         def_typ == "industry" ~ "BIZ",
         def_typ == "state" ~ "STA",
         def_typ == "trade_assn" ~ "BIZ",
         def_typ == "local" ~ "LOC",
         def_typ == "other" ~ "OTHER",
         def_typ == "tribe" ~ "TRIBE",
         def_typ == "public_org" ~ "LOC",
         def_typ == "union" ~ "OTHER",
         def_typ == "religious_org" ~ "NGO_O",
         def_typ == "none" ~ "OTHER",
         def_typ == "military" ~ "FED"
       ),
       yr_file = yr_term
     ) %>%
     rename(
       "PLT_typ" = "plt_typ",
       "DEF_typ" = "def_typ"
     )
 }