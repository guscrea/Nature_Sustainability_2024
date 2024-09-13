# simple function for constructing df from RESL data of substantive focus 
# of environmental litigation

# f_short = text strong describing policy focus. Must be one of: 
# - "climate"
# - "ej"

f_short = "climate"

resl_policy_focus_df <- function(f_short){
  
  # make long variable based on short
  if(f_short == "climate"){
    f_long = "Climate"
  } else if(f_short == "ej") {
    f_long = "Environmental Justice"
  } else {
      print("Not able to determine if this should be a climate or ej case df. Did you specify f_short as \"climate\" or \"ej\"?")
  }
  
  # build df
  df <- resl %>%
    # drop extra ICR rows
    group_by(
      ID
    ) %>%
    mutate(
      n = row_number()
    ) %>%
    filter(
      n == 1
    ) %>%
    group_by(
      yr_term
    ) %>%
    mutate(
      tot_cases = n()
    ) %>%
    {
      if(f_short == "climate"){
        filter(
          .,
          climate_count != "unknown",
          climate_count != "none"
        ) %>%
          mutate(
            .,
            climate_count = as.numeric(climate_count)
          ) %>%
          filter(
            .,
            climate_count >= 1
          )
      } else if(f_short == "ej"){
        filter(
          .,
          ej_count != "unknown",
          ej_count != "none"
        ) %>%
          mutate(
            .,
            ej_count = as.numeric(ej_count)
          ) %>%
          filter(
            .,
            ej_count >= 1
          )
      } 
    } %>%
    mutate(
      case_cnt = n(),
      case_pct = case_cnt/tot_cases*100,
      ngo_PLT = case_when(
        str_detect(plt_typ, "ngo") ~ 1,
        TRUE ~ 0
      ),
      fed_PLT = case_when(
        str_detect(plt_typ, "fed") ~ 1,
        TRUE ~ 0
      ),
      biz_PLT = case_when(
        str_detect(plt_typ, "biz") ~ 1,
        TRUE ~ 0
      ),
      ngo_plt_pct = sum(ngo_PLT, na.rm = T)/tot_cases*100,
      fed_plt_pct = sum(fed_PLT, na.rm = T)/tot_cases*100,
      biz_plt_pct = sum(biz_PLT, na.rm = T)/tot_cases*100,
      label = f_long
    ) %>%
    select(
      yr_term,
      label,
      tot_cases,
      case_pct,
      ngo_plt_pct,
      biz_plt_pct,
      fed_plt_pct
    ) %>%
    filter(
      row_number() == 1
    ) %>%
    pivot_longer(
      cols = case_pct:fed_plt_pct,
      names_to = "plt_typ",
      values_to = "pct"
    ) %>%
    mutate(
      plt_typ = case_when(
        plt_typ == "case_pct" ~ "All Plaintiff Types",
        plt_typ == "ngo_plt_pct" ~ "Environmental Advocacy Groups",
        plt_typ == "fed_plt_pct" ~ "Federal Government",
        plt_typ == "biz_plt_pct" ~ "Firms and Trade Associations",
        TRUE ~ "Unknown"
      )
    ) %>%
    # drop all plaintiff types
    filter(
      plt_typ != "All Plaintiff Types"
    )
  
}
