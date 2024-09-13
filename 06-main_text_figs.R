# Script to plot main-text figures
# By Chris Rea
# Last Modified Sept 13, 2024

# Notes ####

# Load Packages ####
library(tidyverse)
library(patchwork)
library(PupillometryR)
library(ggthemes)
library(grDevices)

#### Plot heat maps - district ####

# call function for generating graphics input dfs ####
source("Functions/simple_filter.R")

# call heat map plotting function ####
source("Functions/heat_map.R")

# set plaintiff types for heat map ####
l_typs <- c("BIZ",
            #"CIVIC",
            "FED",
            "IND",
            "LOC",
            "NGO",
            #"NGO_O",
            #"OTHER",
            #"TRIBE",
            "STA"
            )
# Plot heat maps of litigant types ####


# plot heat map for all DISTRICT cases and all dispositions
plot_PD_combo_heatmap(
  df = simp_filt(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = TRUE
    ),
  yr_start = 1988,
  yr_end = 2022,
  l_typs = l_typs,
  title = "Plaintiff and Defendant Type Combinations - District Courts 1988-2022",
  court_level = "D"
  )

ggsave(
  "Fig_1_HEATMAP_plt_def_combos_all_noBP_no_IMC.pdf",
  path = "Figures",
  units = "mm",
  height = 144,
  width = 180
  )


#### Plot time trends - district data ####

# call function for building dfs ####
source("Functions/dist_df.R")

# call function for plotting time-trends ####
source("Functions/plot_time_trends.R")

# plot district court time trends #####

# Plot time trends of # of cases by PLT type
tt_cases <- plot_ttrends(
  df = build_dist_df(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = TRUE,
    yr = TRUE
  ),
  y_var = pt,
  y_upper = 300,
  PLT_typs = c("NGO","BIZ","FED"),
  y_axis_lab = "No. Cases",
  yr_i = 1988,
  yr_f = 2022,
  plot_var = "pt"
)

# show plot
tt_cases

# save
# ggsave(
#   "SCATTER_time_trends_no_suits_over_time_by_plt_type_noBP_noIMC.png",
#   width = 9,
#   height = 4,
#   path = "Figures"
#   )

# Plot time trends of win rates by PLT type
# note: win rate excludes intra-type suits (e.g. firms suing firms)
tt_win <- plot_ttrends(
  df = build_dist_df(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = FALSE, # intra-type suits excluded
    yr = TRUE
  ),
  y_var = pt_pwl_pct,
  y_upper = 80,
  PLT_typs = c("NGO","BIZ","FED"),
  y_axis_lab = "Win Rate (%)",
  yr_i = 1988,
  yr_f = 2022,
  plot_var = "pt_pwl_pct"
)

# show plot
tt_win

# save
# ggsave(
#   "SCATTER_time_trends_win_rates_over_time_by_plt_type_noBP_noIMC.png",
#   width = 9,
#   height = 4,
#   path = "Figures"
# )

# Plot time trends of duration by PLT type
tt_dur <- plot_ttrends(
  df = build_dist_df(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = TRUE,
    yr = TRUE
  ),
  y_var = pt_dur,
  y_upper = 1000,
  PLT_typs = c("NGO","BIZ","FED"),
  y_axis_lab = "Duration (days)",
  yr_i = 1988,
  yr_f = 2019,
  plot_var = "pt_dur"
)

# show plot
tt_dur

# save
# ggsave(
#   "SCATTER_time_trends_duration_over_time_by_plt_type_noBP_noIMC.png",
#   width = 9,
#   height = 4,
#   path = "Figures"
# )

# plot all three above together as combined ####

# Plot no. cases, duration, and win rates as combined plot 
tt_cases / tt_dur / tt_win +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

  # save
  ggsave(
    "Fig_2_SCATTER_time_trends_cases_wins_duration_over_time_by_plt_type_noBP_noIMC.pdf",
    units = "mm",
    width = 180,
    height =  140,
    path = "Figures"
    )


# remove extraneous plots and data frames
rm(
  tt_cases,
  tt_dur,
  tt_win
  )

#### Plot win-loss - district ####
# Plot win-loss and win rates by district and by PLT_typ - STANDARD DATA ####

# plot win-loss as scatter and win rates as box plots by region and by
# plaintiff type
# --> exclude suits without resolution
# --> don't exclude any other outcomes (dispositions)
# --> exclude intra-type suits
# --> look at all suits across years, not by year
# --> include settlements as wins
# --> include mixed as wins

# call function for plotting win-loss scatter by region by PLT_typ
source("Functions/win-loss_scatter.R")

# plot x-y scatter
plot1 <- plot_win_los_scatter(
  df = build_dist_df(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = FALSE,
    yr = FALSE
    )
  )

# save
# ggsave(
#   "SCATTER_win_loss_by_district_and_plt_type_DIST_noBP_noIMC_no_D.png",
#   plot = plot1,
#   width = 12,
#   height = 3.5,
#   path = "Figures"
#   )

# call function for plotting win rates by region (box plots) by PLT_typ
source("Functions/win_rate_box_plots_by_reg.R")

# plot win rate box plots
plot2 <- plot_wr_box_plots_reg(
  df = build_dist_df(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = FALSE,
    yr = FALSE
  )
)

#save
# ggsave(
#   "WIN_RATES_by_plt_type_by_region_noBP_noIMC_no_D.png",
#   plot = last_plot(),
#   width = 11,
#   height = 6,
#   path = "Figures"
#   )

# plot as combined plot
plot1 / plot2 +
  plot_layout(heights = c(1, 2)) + 
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save
ggsave(
  "Fig_3_SCATTER_district_win_loss_scattter_and_win-loss_boxplots_by_plt_type_noBP_noIMC.pdf",
  device = cairo_pdf,
  units = "mm",
  width = 180,
  height = 150,
  path = "Figures")

rm(plot1, plot2)



#### Plot substantive focus of judicial decisions ####
# build climate and ej litigation dfs ####

# call dataframe building function
source("Functions/resl_climate_ej_df.R")

# build climate df
resl_climate <- resl_policy_focus_df("climate")

# build ej df
resl_ej <- resl_policy_focus_df("ej")

# bind dfs; drop cases prior to 1988
resl_climate_ej <- bind_rows(resl_ej,resl_climate) %>%
  filter(
    yr_term >= 1988
  )

# drop individual dfs
rm(resl_ej,resl_climate)

# Calculate mean and sd for plaintiff types by topic
resl_climate_ej <- resl_climate_ej %>%
  group_by(
    plt_typ, label
  ) %>%
  mutate(
    avg = mean(pct, na.rm = T),
    sd = sd(pct, na.rm = T)
  )

# call plotting function
source("Functions/resl_plot_focus.R")

# build plot
resl_climate_ej_plot <- plot_policy_focus(resl_climate_ej,"ejc", "label", -1, 10)

# view plot
resl_climate_ej_plot


# build meta-category dfs ####

resl_OOC <- resl %>%
  # drop extraICR rows
  group_by(
    ID
    ) %>%
  mutate(
    n = row_number()
    ) %>%
  filter(
    n == 1
    ) %>%
  # disaggregate OOC meta-categories, since decisions can have multiple
  mutate(
    # make ooc_mc1 into list
    OOC_list = ooc_mc1,
    OOC_list = str_split(OOC_list,"%")
    ) %>%
  rowwise() %>%
  mutate(
    len_OOC_list = length(OOC_list)
    ) %>%
  # make longer for OOC codes
  unnest_wider(
    OOC_list,
    names_sep = "_"
    ) %>%
  pivot_longer(
    cols = OOC_list_1:OOC_list_3,
    names_to = "list_num",
    values_to = "OOC_new"
    ) %>%
  filter(
    !is.na(OOC_new)
    ) %>%
  # disaggregate plaintiff types, since decisions can have multiple
  unnest_wider(
    plt_typ,
    names_sep = "_"
    ) %>%
  pivot_longer(
    cols = plt_typ_1:plt_typ_8,
    names_to = "plt_typ_num",
    values_to = "plt_typ_new"
    ) %>%
  filter(
    !is.na(plt_typ_new)
    )

# now calculate share of OCC codes by plaintiff type by year
resl_OOC_yr_cnt <- resl_OOC %>%
  group_by(
    yr_term, plt_typ_new
  ) %>%
  mutate(
    cases_tot_yr = n()
  ) %>%
  group_by(
    plt_typ_new, OOC_new
  ) %>%
  mutate(
    cases_tot = n()
  ) %>%
  group_by(
    plt_typ_new
  ) %>%
  mutate(
    occ_rank = dense_rank(desc(cases_tot))
  ) %>%
  group_by(
    yr_term, plt_typ_new, OOC_new
  ) %>%
  mutate(
    ooc_plt_cnt = n(),
    pct = ooc_plt_cnt/cases_tot_yr*100
  ) %>%
  filter(
    plt_typ_new %in% c("ngo","fed","industry"),
    #occ_rank <= 3
  ) %>%
  mutate(
    plt_typ_new = case_when(
      plt_typ_new == "ngo" ~ "Environmental Advocacy Groups",
      plt_typ_new == "fed" ~ "Federal Government",
      plt_typ_new == "industry" ~ "Firms and Trade Associations",
      TRUE ~ plt_typ_new
    ),
    OOC_new = str_to_title(OOC_new)
  ) %>%
  # keep only one observation per year by plt_typ and OOC
  group_by(
    plt_typ_new, yr_term,OOC_new
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  # calculate mean and sd by plt_typ and OOC
  group_by(
    plt_typ_new, OOC_new
  ) %>%
  mutate(
    avg = mean(pct, na.rm = T),
    sd = sd(pct, na.rm = T)
  )
  
  
#annual counts test
test <- resl_OOC_yr_cnt %>%
  group_by(
    plt_typ_new, yr_term,OOC_new
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    yr_term,
    cases_tot,
    cases_tot_yr,
    ooc_plt_cnt,
    pct,
    occ_rank,
    plt_typ_new, 
    OOC_new,ooc_mc1,
    AIM,TON
  )

rm(test)

# overall counts
resl_OOC_overall_cnts <- resl_OOC %>%
  group_by(
    ID
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  group_by(
    OOC_new
  ) %>%
  mutate(
    ooc_cases_tot = n()
  ) %>%
  select(
    OOC_new,ooc_cases_tot
  ) %>%
  filter(
    row_number() == 1
  )

 #plot meta-category plots ####

# build plot
resl_OOC_plot <- plot_policy_focus(resl_OOC_yr_cnt,"ooc", "OOC_new", 0, 100)

# view plot
resl_OOC_plot

# save combo plot
# ggsave(
#   filename = "Figures/OOC_by_PLT_typ_over_time.png",
#   width =10,
#   height = 4
#   )


# plot meta-category and climate/ej plots together ####

resl_OOC_plot / resl_climate_ej_plot +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save combo plot
ggsave(
  filename = "Figures/Fig_4_OOC_climate_EJ_time_trends.pdf",
  units = "mm",
  width =180,
  height = 120
  )

# build and write out dfs on meta-category counts and dic ####

resl_OOC_dict <- resl_OOC %>%
  select(
    #yr_term,plt_typ_new,
    OOC,TON,AIM,statute,
    OOC_new
  ) %>%
  rowwise() %>%
  mutate(
    statute = str_c(statute, collapse = ", ")
  ) %>%
  group_by(
    #yr_term,plt_typ_new,
    OOC,TON,AIM,statute
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  arrange(
    OOC_new,
    OOC
  )

# write out OOC for publication
write_csv(
  resl_OOC_dict,
  "Data/Substantive_Metacategories/OOC_dic.csv"
)

# make chart of counts of OOC for supplemental materials
resl_OOC %>%
  group_by(
    ID
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  group_by(
    OOC_new
  ) %>%
  mutate(
    OCC_count = n()
  ) %>%
  select(
    OOC_new, OCC_count
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  arrange(
    desc(OCC_count)
  ) %>%
  write_csv(
    "Data/Substantive_Metacategories/OOC_counts_table.csv"
  )

# remove unwanted dfs ####
rm(
  resl_climate_ej,
  resl_climate_ej_plot,
  resl_OOC,
  resl_OOC_dict,
  resl_OOC_overall_cnts,
  resl_OOC_plot,
  resl_OOC_yr_cnt
  )

# the end.


