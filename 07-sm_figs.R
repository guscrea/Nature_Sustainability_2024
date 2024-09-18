# Script to plot supplementary materials figures
# By Chris Rea
# Last Modified May 14, 2024

# Notes ####
# This script generates all the figures and analyses presented in the
# Supplemental Information file.

# Load Packages ####
library(tidyverse)
library(patchwork)
library(PupillometryR)
library(sfsmisc)
library(broom)
library(corrplot)
library(vtable)
library(glm2)
library(car)
library(sjPlot)
library(viridis)
library(grDevices)


#### Plot heat map - appellate ####

# call function for generating simple input dfs ####
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
# plot heat map of litigant types ####


# plot heat map for all APPELATE cases and all dispositions
plot_PD_combo_heatmap(
  df = fjc_e_da,
  yr_start = 1988,
  yr_end = 2022,
  l_typs = l_typs,
  title = "Plaintiff and Defendant Type Combinations - Appellate Courts 1988-2021",
  court_level = "A"
  )

ggsave(
  "HEATMAP_plt_def_combos_all_appellate.png",
  path = "Figures",
  units = "mm",
  height = 144,
  width = 180
  )


#### Plot time trends - appellate data ####

# call function for building dfs ####
source("Functions/dist_df.R")

# call function for plotting time-trends ####
source("Functions/plot_time_trends.R")

# plot time trends for appellate data ####

# Plot time trends of # of cases by PLT type
tt_cases_a <- plot_ttrends(
  #df = fjc_district_app_yr,
  df = build_dist_df(
    df = "fjc_e_da",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = TRUE,
    yr = TRUE,
    appeals = TRUE
  ),
  y_var = pt,
  y_upper = 55,
  PLT_typs = c("NGO","BIZ","FED"),
  y_axis_lab = "No. Appeals",
  yr_i = 1988,
  yr_f = 2021,
  plot_var = "pt"
)

# show plot
tt_cases_a

# save
# ggsave(
#   "SCATTER_time_trends_no_suits_over_time_by_plt_type_appeal.png",
#   width = 9,
#   height = 4,
#   path = "Figures"
#   )

  
# plot appeals rate

# first, build a df of appeals cases; of total district case; join them; calcuate appeals rate 
ar_df <- left_join(
  # build appeals cases dataframe
  (build_dist_df(
    df = "fjc_e_da",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = TRUE,
    yr = TRUE,
    appeals = TRUE
    ) %>%
    # make joining var
    mutate(
      PLT_yr = str_c(PLT_typ, yr_file, sep = "-")
    ) %>%
    ungroup()
   ),
  # build totals of all district cases df
  (build_dist_df(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = TRUE,
    yr = TRUE
    ) %>%
    ungroup() %>%
    select(
      yr_file, PLT_typ, pt
      ) %>%
    group_by(
      yr_file, PLT_typ
      ) %>%
    filter(
      row_number() == 1
      ) %>%
    # make joining var
    mutate(
      PLT_yr = str_c(PLT_typ, yr_file, sep = "-"),
    ) %>%
    # rename totals var for join
    rename(
      "pt_tot_dist" = "pt"
      ) %>%
      ungroup() %>%
     # keep only tot_dist and join var
     select(
       pt_tot_dist, PLT_yr
       )
   ),
  # join by variable
  by = "PLT_yr"
  ) %>%
  # calculate appeals rate (appeals cases per district cases)
  mutate(
    appeal_rate = pt/pt_tot_dist*100
    )


tt_rate_a <- plot_ttrends(
  df = ar_df,
  y_var = appeal_rate,
  y_upper = 33,
  PLT_typs = c("NGO","BIZ","FED"),
  y_axis_lab = "Appeals Rate\n(% of District Cases)",
  yr_i = 1988,
  yr_f = 2021,
  plot_var = "appeal_rate"
)

# show plot
tt_rate_a

# save
# ggsave(
#   "SCATTER_time_trends_appeals_rate_over_time_by_plt_type_appeal.png",
#   width = 9,
#   height = 4,
#   path = "Figures"
#   )

# plot appeals win rate
tt_win_a <- plot_ttrends(
  df = build_dist_df(
    df = "fjc_e_da",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = FALSE,
    yr = TRUE,
    appeals = TRUE
    ),
  y_var = pt_pwl_pct,
  y_upper = 100,
  PLT_typs = c("NGO","BIZ","FED"),
  y_axis_lab = "Appeals Win Rate (%)",
  yr_i = 1988,
  yr_f = 2021,
  plot_var = "pt_pwl_pct"
)

# show plot
tt_win_a

# save
# ggsave(
#   "SCATTER_time_trends_appeals_win_rate_over_time_by_plt_type_appeal.png",
#   width = 9,
#   height = 4,
#   path = "Figures"
#   )

# Plot no. cases, appeal rates, and win rates as combined plot 
tt_cases_a / tt_rate_a / tt_win_a +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save
ggsave(
  "SCATTER_time_trends_cases_wins_duration_over_time_by_plt_type_appeal.png",
  units = "mm",
  width = 180,
  height =  140,
  path = "Figures"
)

# remove extraneous plots and data frames
rm(
  ar_df,
  tt_cases_a,
  tt_rate_a,
  tt_win_a
  )

#### Plot win rates across districts by plaintiff types - appeals data ####

# call function for plotting win rate box plots for appeals data ####
source("Functions/win_rate_box_plots_appeals.R")

# plot box plots for appellate win rates ####

# first plot net appellate win rates by plaintiff type
plot1 <- plot_wr_box_plots_appeals(
  "fjc_e_da",
  targeted = FALSE
  )

# now plot appellate win rates for plaintiff types targeting their most common
# targets
plot2 <- plot_wr_box_plots_appeals(
  "fjc_e_da",
  targeted = TRUE
  )

# plot as combined plot
plot1 / plot2 +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save
ggsave(
  "WIN_RATES_by_plt_type_Appellate_all_dist_AND_target_DEF_typ.png",
  width = 11,
  height = 9,
  path = "Figures"
)

rm(plot1, plot2)

# Plot win-loss and win rates by district and by PLT_typ - SETTLEMENTS RECODED ####

# plot win-loss as scatter and win rates as box plots by region and by
# plaintiff type
# --> exclude suits without resolution
# --> don't exclude any other outcomes (dispositions)
# --> exclude intra-type suits
# --> look at all suits across years, not by year
# --> recode settlements as neither wins nor losses
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
    yr = FALSE,
    recode_S = TRUE
    )
  )

# save
# ggsave(
#   "SCATTER_win_loss_by_district_and_plt_type_DIST_noBP_noIMC_settle_not_as_win.png",
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
    yr = FALSE,
    recode_S = TRUE
  )
)

#save
# ggsave(
#   "WIN_RATES_by_plt_type_by_region_noBP_noIMC_settle_not_as_win.png",
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
  "SCATTER_district_win_loss_scattter_and_win-loss_boxplots_by_plt_type_noBP_noIMC_settle_not_as_win.png.png",
  units = "mm",
  width = 180,
  height = 150,
  path = "Figures"
)

rm(plot1, plot2)


# Plot win-loss and win rates by district and by PLT_typ - MIXED RECODED ####

# plot win-loss as scatter and win rates as box plots by region and by
# plaintiff type
# --> exclude suits without resolution
# --> don't exclude any other outcomes (dispositions)
# --> exclude intra-type suits
# --> look at all suits across years, not by year
# --> include settlements as wins
# --> recode mixed as neither wins nor losses

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
    yr = FALSE,
    recode_M = TRUE
    )
  )

# save
# ggsave(
#   "SCATTER_win_loss_by_district_and_plt_type_DIST_noBP_noIMC_mix_not_as_win.png",
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
    yr = FALSE,
    recode_M = TRUE
  )
)

#save
# ggsave(
#   "WIN_RATES_by_plt_type_by_region_noBP_noIMC_mix_not_as_win.png",
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
  "SCATTER_district_win_loss_scattter_and_win-loss_boxplots_by_plt_type_noBP_noIMC_mix_not_as_win.png.png",
  units = "mm",
  width = 180,
  height = 150,
  path = "Figures"
  )

rm(plot1, plot2)




# Plot decadal distributions of case duration by plaintiff type ####

# call function to build dataframe 
source("Functions/dist_df.R")

# first 1988 to 2000
build_dist_df(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE,
  yr = TRUE
  ) %>%  
  group_by(
    PLT_typ, yr_file
  ) %>%
  filter(
    PLT_wl == "w"
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  filter(
    yr_file >= 1988 & yr_file <= 2019 # don't plot lst three years to avoid bias
    # towards settlements in recent years ()
  ) %>%
  filter(
    PLT_typ %in% c("BIZ","FED",#"IND",
                   "NGO")#"STA","LOC")
  ) %>%
  mutate(
    # make ten-year time increments 
    decade = case_when(
      yr_file <= 1997 ~ "1988-1997",
      yr_file > 1997 & yr_file <= 2007 ~ "1998-2007",
      yr_file > 2007 & yr_file <= 2018 ~ "2008-2018"
    ),
    PLT_typ = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      TRUE ~ PLT_typ
    )
  ) %>%
  ggplot() +
  geom_density(
    aes(
      x = pt_dur,
      #y = pt_dur,
      group = decade,
      fill = PLT_typ,
      linetype = decade,
      color = PLT_typ
    ),
    size = 0.8,
    alpha = 0.25
  ) +
  labs(
    y = "Density",
    x = "Case Duration (days)",
    linetype = "Decade"
  ) +
  facet_wrap(
    vars(PLT_typ),
    ncol = 3
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  #xlim(1988, 2022) +
  #ylim(0,1000) +
  guides(
    colour = "none",
    fill = "none"
    ) +
  theme_linedraw()

# save
ggsave(
  "Duration_distributions_by_decade_by_plt_type_noBP_noIMC.png",
  width = 9,
  height = 3,
  path = "Figures"
)


# Plot geographic distributions of cases by district and plaintiff type ####

# all plaintiff types
dist_plot <- simp_filt(
  df = "fjc_e",
  jud = FALSE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE
  ) %>%
  #filter(
  #  PLT_typ %in% c("BIZ","FED","IND","NGO")#"STA","LOC")
  #) %>%
  mutate(
    PLT_typ1 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Civic Association",
      PLT_typ == "TRIBE" ~ "Tribe",
      PLT_typ == "NGO_O" ~ "Non-Environmental Non-Proft",
      PLT_typ == "PUB_ORG" ~ "Public Organizations",
      TRUE ~ PLT_typ
    ),
    PLT_typ2 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Others",
      PLT_typ == "TRIBE" ~ "Others",
      PLT_typ == "NGO_O" ~ "Others",
      PLT_typ == "PUB_ORG" ~ "Others",
      TRUE ~ "Others"
    )
  )

geo_dist <-  dist_plot %>%
  mutate(
    dist_name_flag = case_when(
      str_detect(Judicial_2,state_name_lower_orlist) == TRUE ~ 1,
      TRUE ~ 0
    ),
    dist_name = case_when(
      dist_name_flag == 1 ~ Judicial_2,
      TRUE ~ str_c(Judicial_2,STATE_TERR, sep = " of ")
    )
  ) %>%
  filter(
    !is.na(dist_name)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = reorder(dist_name,dist_name,function(x)-length(x)),
      group = PLT_typ2,
      fill = PLT_typ2
    )
  ) +
  labs(
    x = NULL,
    y = "No. Cases",
    fill = "Plaitiff Types"
  ) +
  #facet_wrap(
  #  vars(PLT_typ),
  #  ncol = 2
  #) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  #guides(colour = "none", fill = "none") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# plot
geo_dist

# save
ggsave(
  "DISTRIBUTION_no_suits_by_dist_by_plt_type_noBP_noIMC_ALL.png",
  width = 17,
  height = 7,
  path = "Figures")

rm(dist_plot, geo_dist)

# federal plaintiffs
FED_dist_plot <- simp_filt(
  df = "fjc_e",
  jud = FALSE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE
  ) %>%
  filter(
    PLT_typ %in% c(#"BIZ",
      "FED")#),"IND","NGO")#"STA","LOC")
  ) %>%
  mutate(
    PLT_typ1 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Civic Association",
      PLT_typ == "TRIBE" ~ "Tribe",
      PLT_typ == "NGO_O" ~ "Non-Environmental Non-Proft",
      PLT_typ == "PUB_ORG" ~ "Public Organizations",
      TRUE ~ PLT_typ
    ),
    PLT_typ2 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Others",
      PLT_typ == "TRIBE" ~ "Others",
      PLT_typ == "NGO_O" ~ "Others",
      PLT_typ == "PUB_ORG" ~ "Others",
      TRUE ~ "Others"
    )
  )

FED_geo_dist <-  FED_dist_plot %>%
  mutate(
    dist_name_flag = case_when(
      str_detect(Judicial_2,state_name_lower_orlist) == TRUE ~ 1,
      TRUE ~ 0
    ),
    dist_name = case_when(
      dist_name_flag == 1 ~ Judicial_2,
      TRUE ~ str_c(Judicial_2,STATE_TERR, sep = " of ")
    )
  ) %>%
  filter(
    !is.na(dist_name)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = reorder(dist_name,dist_name,function(x)-length(x)),
      group = PLT_typ2
    ),
    fill = "#423d7f"
  ) +
  labs(
    x = NULL,
    y = "No. Cases",
    fill = NULL
  ) +
  facet_wrap(
    vars(PLT_typ1),
    nrow = 1
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(colour = "none", fill = "none") +
  #ylim(0,650) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# plot
FED_geo_dist

# save
ggsave(
  "DISTRIBUTION_no_suits_by_dist_by_plt_type_noBP_noIMC_FED.png",
  width = 17,
  height = 7,
  path = "Figures")

rm(FED_dist_plot, FED_geo_dist)

# engo plaintiffs
NGO_dist_plot <- simp_filt(
  df = "fjc_e",
  jud = FALSE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE
  ) %>%
  filter(
    PLT_typ %in% c(#"BIZ","FED","IND",
      "NGO")#"STA","LOC")
  ) %>%
  mutate(
    PLT_typ1 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Civic Association",
      PLT_typ == "TRIBE" ~ "Tribe",
      PLT_typ == "NGO_O" ~ "Non-Environmental Non-Proft",
      PLT_typ == "PUB_ORG" ~ "Public Organizations",
      TRUE ~ PLT_typ
    ),
    PLT_typ2 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Others",
      PLT_typ == "TRIBE" ~ "Others",
      PLT_typ == "NGO_O" ~ "Others",
      PLT_typ == "PUB_ORG" ~ "Others",
      TRUE ~ "Others"
    )
  )

NGO_geo_dist <-  NGO_dist_plot %>%
  mutate(
    dist_name_flag = case_when(
      str_detect(Judicial_2,state_name_lower_orlist) == TRUE ~ 1,
      TRUE ~ 0
    ),
    dist_name = case_when(
      dist_name_flag == 1 ~ Judicial_2,
      TRUE ~ str_c(Judicial_2,STATE_TERR, sep = " of ")
    )
  ) %>%
  filter(
    !is.na(dist_name)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = reorder(dist_name,dist_name,function(x)-length(x)),
      group = PLT_typ2
    ),
    fill = "#3e1451"
  ) +
  labs(
    x = NULL,
    y = "No. Cases",
    fill = NULL
  ) +
  facet_wrap(
    vars(PLT_typ1),
    nrow = 1
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(colour = "none", fill = "none") +
  #ylim(0,650) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# plot
NGO_geo_dist

# save
ggsave(
  "DISTRIBUTION_no_suits_by_dist_by_plt_type_noBP_noIMC_NGO.png",
  width = 17,
  height = 7,
  path = "Figures")

rm(NGO_dist_plot, NGO_geo_dist)


# firms and industry plaintiffs plot
BIZ_dist_plot <- simp_filt(
  df = "fjc_e",
  jud = FALSE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE
  ) %>%
  filter(
    PLT_typ %in% c("BIZ")#,"FED","IND","NGO")#"STA","LOC")
  ) %>%
  mutate(
    PLT_typ1 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Civic Association",
      PLT_typ == "TRIBE" ~ "Tribe",
      PLT_typ == "NGO_O" ~ "Non-Environmental Non-Proft",
      PLT_typ == "PUB_ORG" ~ "Public Organizations",
      TRUE ~ PLT_typ
    ),
    PLT_typ2 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Others",
      PLT_typ == "TRIBE" ~ "Others",
      PLT_typ == "NGO_O" ~ "Others",
      PLT_typ == "PUB_ORG" ~ "Others",
      TRUE ~ "Others"
    )
  )

BIZ_geo_dist <-  BIZ_dist_plot %>%
  mutate(
    dist_name_flag = case_when(
      str_detect(Judicial_2,state_name_lower_orlist) == TRUE ~ 1,
      TRUE ~ 0
    ),
    dist_name = case_when(
      dist_name_flag == 1 ~ Judicial_2,
      TRUE ~ str_c(Judicial_2,STATE_TERR, sep = " of ")
    )
  ) %>%
  filter(
    !is.na(dist_name)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = reorder(dist_name,dist_name,function(x)-length(x)),
      group = PLT_typ2
    ),
    fill = "#3e768a"
  ) +
  labs(
    x = NULL,
    y = "No. Cases",
    fill = NULL
  ) +
  facet_wrap(
    vars(PLT_typ1),
    nrow = 1
  ) +
  #scale_color_viridis_d() +
  #scale_fill_viridis_d() +
  guides(colour = "none", fill = "none") +
  #ylim(0,650) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# plot
BIZ_geo_dist

# save
ggsave(
  "DISTRIBUTION_no_suits_by_dist_by_plt_type_noBP_noIMC_BIZ.png",
  width = 17,
  height = 7,
  path = "Figures")

rm(BIZ_dist_plot, BIZ_geo_dist)



# Plot Lorenz curves of cases by district by plaintiff type ####

fjc_district_L <- build_dist_df(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = TRUE,
    yr = FALSE
  ) %>%
  group_by(
    PLT_typ, DISTRICT
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  group_by(
    PLT_typ
  ) %>%
  arrange(
    desc(d_pt)
  ) %>%
  filter(
    PLT_typ %in% c("BIZ","FED","NGO"),#"NGO")#"STA","LOC")
  ) %>%
  mutate(
    d_pt_cum = cumsum(d_pt),
    d_pt_cum_pct = round(d_pt_cum/pt,4),
    #d_pt_rank = rank(-d_pt, ties.method = "random"),
    d_pt_rank = row_number(),
    d_pt_rank_pctile = d_pt_rank*(1/n()),
    # variables for plotting horizontal and vertical 75th-ile lines
    d_pt_75 =  case_when(
      d_pt_cum_pct >= .75 ~ 1,
      TRUE ~ 0
    ),
    d_pt_75_lag = dplyr::lag(d_pt_75),
    d_pt_75 = d_pt_75 - d_pt_75_lag,
    d_pt_75 = case_when(
      is.na(d_pt_75) ~ 0,
      TRUE ~ d_pt_75
    ),
    d_pt_75x = d_pt_rank_pctile[d_pt_75==1],
    d_pt_75y = d_pt_cum_pct[d_pt_75==1]
  ) %>%
  mutate(
    # gini calcs
    parity_int = integrate.xy(d_pt_rank_pctile,d_pt_rank_pctile),
    dist_int = integrate.xy(d_pt_rank_pctile,d_pt_cum_pct),
    gini = (dist_int-parity_int)/parity_int
  ) %>%
  mutate(
    PLT_typ = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      TRUE ~ PLT_typ
    )
  )

# plot
fjc_district_L %>%
  ggplot() +
  geom_segment( # vert line at 75th %ile
    data = fjc_district_L %>%
      group_by(PLT_typ) %>%
      filter(
        row_number()==1
      ),
    aes(
      x = d_pt_75x,
      xend = d_pt_75x,
      y = -Inf,
      yend = Inf
    ),
    linetype = "dashed",
    alpha = .5
  ) + 
  geom_segment(# horiz line at 75th %ile
    data = fjc_district_L %>%
      group_by(PLT_typ) %>%
      filter(
        row_number()==1
      ),
    aes(
      x = -Inf,
      xend = Inf,
      y = d_pt_75y,
      yend = d_pt_75y
    ),
    linetype = "dashed",
    alpha = .5
  ) +
  geom_line(# Lorenz curve
    aes(
      x = d_pt_rank_pctile,
      y = d_pt_cum_pct,
      color = PLT_typ
    ),
    size = 2,
  ) +
  geom_line(# parity line
    aes(
      x = d_pt_rank_pctile,
      y = d_pt_rank_pctile,
    ),
    size = .5,
    linetype = "dotted"
  ) +
  geom_point(# point at 75th %ile
    data = fjc_district_L %>%
      group_by(PLT_typ) %>%
      filter(
        row_number()==1
      ),
    aes(
      x = d_pt_75x,
      y = d_pt_75y
    ),
    size = 3,
    alpha = .5
  ) +
  geom_text(# label of district number for 75th %ile
    data = fjc_district_L %>%
      group_by(PLT_typ) %>%
      filter(
        row_number()==1
      ),
    aes(
      x = d_pt_75x + .01,
      y = 0.08,
      label = str_c("x = ", round(d_pt_75x,2), sep = "")
    ),
    size = 4,
    alpha = .6,
    angle = 90,
    vjust = 1
  ) +
  geom_text(# label of gini
    data = fjc_district_L %>%
      group_by(PLT_typ) %>%
      filter(
        row_number()==1
      ),
    aes(
      x = .95,
      y = 0.03,
      label = str_c("gini = ", round(gini,2), sep = "")
    ),
    size = 4,
    alpha = .6,
    angle = 0,
    vjust = .5
  ) +
  labs(
    x = "fraction of court districts",
    y = "fraction of litigation",
    color = "Plaitiff Types"
  ) +
  facet_wrap(
    vars(PLT_typ),
    ncol = 3
  ) +
  scale_color_viridis_d() +
  #scale_fill_viridis_d() +
  guides(colour = "none", fill = "none") +
  theme_linedraw()
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave(
  "LORENZ_CURVE_no_suits_by_plt_type_noBP_noIMC.png",
  plot = last_plot(),
  width = 15,
  height = 5,
  path = "Figures"
  )

rm(fjc_district_L)

#### Plot litigation rates v. district features ####


# read in data on district features
dist_feat <- read_csv(
  "Data/District_Characteristics/features_by_federal_court_district.csv"
)

# build df for suits v. features analysis
dist_feat <- left_join(
  build_dist_df(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = TRUE,
    yr = FALSE
    ) %>%
    mutate(
      join_var = str_c(
        STATE_TERR,Judicial_2, sep = ""
      )
    ) %>%
    select(
      -c(STATE_TERR, Judicial_2)
    ),
  dist_feat,
  by = "join_var"
  )

# make df
pop_cases <-  dist_feat %>%  
  group_by(
    STATE_TERR,Judicial_2,PLT_typ,
  ) %>%
  filter(
    row_number() == 1,
    STATE_TERR != "District of Columbia"
  ) %>%
  filter(
    PLT_typ %in% c("BIZ","FED",#"IND",
                   "NGO")#"STA","LOC")
  ) %>%
  mutate(
    PLT_typ = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      TRUE ~ PLT_typ
    ),
    # fill in NAs in forest service area as 0
    fs_area = case_when(
      is.na(fs_area) ~ 0,
      TRUE ~ fs_area
    ),
    tot_pop = tot_pop/1000000,
    pad_pct = pad_area/d_area,
    fs_pct = fs_area/d_area
  )

# plot population correlation ####

# run regression by plaintiff type; get function values
pop_cases_tidy = pop_cases %>% 
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ tot_pop, data = data))) %>%
  summarise(tidy(fitpop))


# run regression by plaintiff type; get r-squared
pop_cases_glance = pop_cases %>% 
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ tot_pop, data = data))) %>%
  summarise(glance(fitpop)) %>%
  mutate(
    r.squared = round(r.squared, 2),
    r_squared_lab = str_c("italic(r)^2 == ",r.squared)
  )


# run regression by plaintiff type; get r-squared
pop_cases_aug = pop_cases %>% 
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ tot_pop, data = data))) %>%
  summarise(augment(fitpop))


pop_cases_plot <- pop_cases %>%
  ggplot() +
  geom_point(
    aes(
      x = tot_pop,
      y = d_pt,
      group = PLT_typ,
      fill = PLT_typ
    ),
    color = "#00000080",
    shape = 21,
    size = 1.5
  ) +
  # smoothed line
  geom_smooth(
    #data = pop_cases_aug,
    aes(
      x = tot_pop,
      y = d_pt,
      group = PLT_typ,
    ),
    alpha = 0.3,
    color = "#ffffff",
    method = "lm"
  ) +
  # fitted line
  geom_line(
    data = pop_cases_aug,
    aes(
      x = tot_pop,
      y = .fitted,
      group = PLT_typ,
      color = PLT_typ,
    ),
    alpha = 0.5,
    #stat = "smooth",
    size = 1
  ) +
  # r squared
  geom_text(
    data = pop_cases_glance,
    aes(
      x = 16,
      y = 580,
      label = r_squared_lab
    ),
    parse = TRUE,
    color = "#000000"
  ) +
  labs(
    x = "District Population (M)",
    y = "No. Suits"
  ) +
  facet_wrap(
    vars(PLT_typ),
    ncol = 3
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(colour = "none", fill = "none") +
  #ylim(0,400) +
  theme_linedraw()

# plot
pop_cases_plot




# plot all manufacturing establishments correlation ####

# run regression by plaintiff type; get function values
pop_cases_tidy = pop_cases %>% 
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ est, data = data))) %>%
  summarise(tidy(fitpop))


# run regression by plaintiff type; get r-squared
pop_cases_glance = pop_cases %>% 
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ est, data = data))) %>%
  summarise(glance(fitpop)) %>%
  mutate(
    r.squared = round(r.squared, 2),
    r_squared_lab = str_c("italic(r)^2 == ",r.squared)
  )


# run regression by plaintiff type; get r-squared
pop_cases_aug = pop_cases %>% 
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ est, data = data))) %>%
  summarise(augment(fitpop))


est_cases_plot <- pop_cases %>%
  ggplot() +
  geom_point(
    aes(
      x = est,
      y = d_pt,
      group = PLT_typ,
      fill = PLT_typ
    ),
    color = "#00000080",
    shape = 21,
    size = 1.5
  ) +
  # smoothed line
  geom_smooth(
    #data = pop_cases_aug,
    aes(
      x = est,
      y = d_pt,
      group = PLT_typ,
    ),
    alpha = 0.3,
    color = "#ffffff",
    method = "lm"
  ) +
  # fitted line
  geom_line(
    data = pop_cases_aug,
    aes(
      x = est,
      y = .fitted,
      group = PLT_typ,
      color = PLT_typ,
    ),
    alpha = 0.5,
    #stat = "smooth",
    size = 1
  ) +
  # r squared
  geom_text(
    data = pop_cases_glance,
    aes(
      x = 17500,
      y = 580,
      label = r_squared_lab
    ),
    parse = TRUE,
    color = "#000000"
  ) +
  labs(
    x = "Manufacturing Establishments",
    y = "No. Suits"
  ) +
  facet_wrap(
    vars(PLT_typ),
    ncol = 3
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(colour = "none", fill = "none") +
  #ylim(0,400) +
  theme_linedraw()

# plot
est_cases_plot



# plot large manufacturing establishments correlation ####

# run regression by plaintiff type; get function values
pop_cases_tidy = pop_cases %>% 
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ lrg_est, data = data))) %>%
  summarise(tidy(fitpop))


# run regression by plaintiff type; get r-squared
pop_cases_glance = pop_cases %>% 
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ lrg_est, data = data))) %>%
  summarise(glance(fitpop)) %>%
  mutate(
    r.squared = round(r.squared, 2),
    r_squared_lab = str_c("italic(r)^2 == ",r.squared)
  )


# run regression by plaintiff type; get r-squared
pop_cases_aug = pop_cases %>% 
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ lrg_est, data = data))) %>%
  summarise(augment(fitpop))


lrg_est_cases_plot <- pop_cases %>%
  ggplot() +
  geom_point(
    aes(
      x = lrg_est,
      y = d_pt,
      group = PLT_typ,
      fill = PLT_typ
    ),
    color = "#00000080",
    shape = 21,
    size = 1.5
  ) +
  # smoothed line
  geom_smooth(
    #data = pop_cases_aug,
    aes(
      x = lrg_est,
      y = d_pt,
      group = PLT_typ,
    ),
    alpha = 0.3,
    color = "#ffffff",
    method = "lm"
  ) +
  # fitted line
  geom_line(
    data = pop_cases_aug,
    aes(
      x = lrg_est,
      y = .fitted,
      group = PLT_typ,
      color = PLT_typ,
    ),
    alpha = 0.5,
    #stat = "smooth",
    size = 1
  ) +
  # r squared
  geom_text(
    data = pop_cases_glance,
    aes(
      x = 300,
      y = 580,
      label = r_squared_lab
    ),
    parse = TRUE,
    color = "#000000"
  ) +
  labs(
    x = "Large Manufacturing Establishments (250+ emp.)",
    y = "No. Suits"
  ) +
  facet_wrap(
    vars(PLT_typ),
    ncol = 3
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(colour = "none", fill = "none") +
  #ylim(0,400) +
  theme_linedraw()

# plot
lrg_est_cases_plot

# plot protected areas correlation ####

# run regression by plaintiff type; get function values
pop_cases_tidy = pop_cases %>% 
  filter(
    join_var != "AlaskaDistrict of Alaska"
  ) %>%
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ pad_area, data = data))) %>%
  summarise(tidy(fitpop))


# run regression by plaintiff type; get r-squared
pop_cases_glance = pop_cases %>% 
  filter(
    join_var != "AlaskaDistrict of Alaska"
  ) %>%
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ pad_area, data = data))) %>%
  summarise(glance(fitpop)) %>%
  mutate(
    r.squared = round(r.squared, 2),
    r_squared_lab = str_c("italic(r)^2 == ",r.squared)
  )


# run regression by plaintiff type; get r-squared
pop_cases_aug = pop_cases %>% 
  filter(
    join_var != "AlaskaDistrict of Alaska"
  ) %>%
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ pad_area, data = data))) %>%
  summarise(augment(fitpop))


pad_cases_plot <- pop_cases %>%
  filter(
    join_var != "AlaskaDistrict of Alaska"
  ) %>%
  ggplot() +
  geom_point(
    aes(
      x = pad_area,
      y = d_pt,
      group = PLT_typ,
      fill = PLT_typ
    ),
    color = "#00000080",
    shape = 21,
    size = 1.5
  ) +
  # smoothed line
  geom_smooth(
    #data = pop_cases_aug,
    aes(
      x = pad_area,
      y = d_pt,
      group = PLT_typ,
    ),
    alpha = 0.3,
    color = "#ffffff",
    method = "lm"
  ) +
  # fitted line
  geom_line(
    data = pop_cases_aug,
    aes(
      x = pad_area,
      y = .fitted,
      group = PLT_typ,
      color = PLT_typ,
    ),
    alpha = 0.5,
    #stat = "smooth",
    size = 1
  ) +
  # r squared
  geom_text(
    data = pop_cases_glance,
    aes(
      x = 57000,
      #x = 0.4,
      y = 580,
      label = r_squared_lab
    ),
    parse = TRUE,
    color = "#000000"
  ) +
  labs(
    x = "Protected Area (sq km)",
    y = "No. Suits"
  ) +
  facet_wrap(
    vars(PLT_typ),
    ncol = 3
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(colour = "none", fill = "none") +
  #ylim(0,400) +
  #xlim(0,100000)
  theme_linedraw()

# plot
pad_cases_plot


# plot national forests correlation ####

# run regression by plaintiff type; get function values
pop_cases_tidy = pop_cases %>% 
  filter(
    join_var != "AlaskaDistrict of Alaska"
  ) %>%
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ fs_area, data = data))) %>%
  summarise(tidy(fitpop))


# run regression by plaintiff type; get r-squared
pop_cases_glance = pop_cases %>% 
  filter(
    join_var != "AlaskaDistrict of Alaska"
  ) %>%
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ fs_area, data = data))) %>%
  summarise(glance(fitpop)) %>%
  mutate(
    r.squared = round(r.squared, 2),
    r_squared_lab = str_c("italic(r)^2 == ",r.squared)
  )


# run regression by plaintiff type; get r-squared
pop_cases_aug = pop_cases %>% 
  filter(
    join_var != "AlaskaDistrict of Alaska"
  ) %>%
  ungroup() %>%
  nest_by(PLT_typ) %>%
  mutate(fitpop = list(lm(d_pt ~ fs_area, data = data))) %>%
  summarise(augment(fitpop))


fs_cases_plot <- pop_cases %>%
  filter(
    join_var != "AlaskaDistrict of Alaska"
  ) %>%
  ggplot() +
  geom_point(
    aes(
      x = fs_area,
      y = d_pt,
      group = PLT_typ,
      fill = PLT_typ
    ),
    color = "#00000080",
    shape = 21,
    size = 1.5
  ) +
  # smoothed line
  geom_smooth(
    #data = pop_cases_aug,
    aes(
      x = fs_area,
      y = d_pt,
      group = PLT_typ,
    ),
    alpha = 0.3,
    color = "#ffffff",
    method = "lm"
  ) +
  # fitted line
  geom_line(
    data = pop_cases_aug,
    aes(
      x = fs_area,
      y = .fitted,
      group = PLT_typ,
      color = PLT_typ,
    ),
    alpha = 0.5,
    #stat = "smooth",
    size = 1
  ) +
  # r squared
  geom_text(
    data = pop_cases_glance,
    aes(
      x = 70000,
      #x = .3,
      y = 580,
      label = r_squared_lab
    ),
    parse = TRUE,
    color = "#000000"
  ) +
  labs(
    x = "National Forest System Area (sq km)",
    y = "No. Suits"
  ) +
  facet_wrap(
    vars(PLT_typ),
    ncol = 3
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(colour = "none", fill = "none") +
  #ylim(0,400) +
  #xlim(0,100000)
  theme_linedraw()

# plot
fs_cases_plot


# plot all suits v. district feature plots together ####

pop_cases_plot / est_cases_plot / lrg_est_cases_plot / pad_cases_plot / fs_cases_plot +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save
ggsave(
  "No_Cases_as_Function_of_District_features.png",
  width = 9,
  height = 12,
  path = "Figures"
)

# remove unneeded objects ####
rm(
  dist_feat,
  est_cases_plot,
  fs_cases_plot,
  lrg_est_cases_plot,
  pad_cases_plot,
  pop_cases,
  pop_cases_aug,
  pop_cases_glance,
  pop_cases_plot,
  pop_cases_tidy
  )


#### Win-loss regression and related ####

# investigate cases that are neither wins nor losses
simp_filt(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE
  ) %>%
  filter(
    yr_file >= 1988,
    yr_file <= 2020
  ) %>%
  # keep only cases that are neither wins nor losses 
  filter(
    PLT_wl == "n"
  ) %>%
  # recode judgement = 0 as -8, since both indicate "unknown"
  mutate(
    JUDGMENT = case_when(
      JUDGMENT == 0 ~ -8,
      TRUE ~ JUDGMENT
    )
  ) %>%
  # count by judgement and by disposition
  group_by(
    JUDGMENT, DISP
  ) %>%
  mutate(
    disp_n = n(),
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    JUDGMENT, DISP, disp_n
  ) %>%
  ungroup() %>%
  mutate(
    tot = sum(disp_n, na.rm = T),
    disp_pct = round(disp_n/tot*100,3)
  ) %>%
  arrange(
    desc(disp_pct)
  )

# --> plurality (47%) are judgment "missing" (-8) and disposition "statistical
# closing" (disposition = 18); rest are judgment "missing" (-8) or "unknown" (4)
# and a variety of other dispositions: 17 ("other"; 14.8%); 5 ("judgment on
#consent"; 11.4% "missing" judgement and 11% "unknown" judgment); 6 ("judgment
# on motion before trial"; 7%) and so on. In all these instances, although
# we have information on the ending disposition of the case, we do not know if
# that ending disposition favored the plaintiff or the defendant. Even
# statistical closings, which are most prevalent and one might be tempted to
# code as plaintiff losses - after all, in these instances, whatever claims
# had been brought by the plaintiffs against the defendants have stagnated 
# and the defendant has faced no formal legal sanction or settlement - cannot
# be reliably coded as such, given that we do not know how the conflict has
# actually been resolved. The plaintiff might have simply decided not to pursue
# legal action without formally notifying or withdrawing the case, or the
# parties might have reached an informal agreement, or the plaintiff might be
# unwilling to formally relent but unable to continue to pursue the case, and so
# on. We simply do not know.

# As such, we judge the most defensible methodological choice to be to drop
# these unknown case outcomes. Leaving them in and coding them as "non-wins"
# akin to cases we code as losses is the same as assuming that plaintiffs lose
# 100% of these unknown cases - a very strong assumption. Clearly, we also
# cannot code them as 100% wins - this is even less defensible than coding them
# all as losses. So, we drop them from our logistic regressions.

# call win-loss regression
source("Functions/win_loss_logit_reg.R")

# regression of plaintiff wins and losses
# --> excluding BP and IMC
# --> testing for association with party of president (partisan effect)
win_los_reg(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE,
  yr_i = 1988,
  yr_f = 2020,
  party_or_admin = "PARTY"
  )


# regression of plaintiff wins and losses
# --> excluding BP and IMC
# --> testing for association with presidential admin (period effect)
win_los_reg(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE,
  yr_i = 1988,
  yr_f = 2020,
  party_or_admin = "ADMIN"
  )

# regression of plaintiff wins and losses
# --> including BP and IMC
# --> testing for association with party of president (partisan effect)
win_los_reg(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = FALSE,
  diag = FALSE,
  yr_i = 1988,
  yr_f = 2020,
  party_or_admin = "PARTY"
  )


# regression of plaintiff wins and losses
# --> including BP and IMC
# --> testing for association with presidential admin (period effect)
win_los_reg(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = FALSE,
  diag = FALSE,
  yr_i = 1988,
  yr_f = 2020,
  party_or_admin = "ADMIN"
  )

# regression of plaintiff wins and losses
# --> excluding BP and IMC
# --> recode settlements as losses
# --> testing for association with party of president (partisan effect)
win_los_reg(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE,
  yr_i = 1988,
  yr_f = 2020,
  party_or_admin = "PARTY",
  recode_set = TRUE
)


# regression of plaintiff wins and losses
# --> excluding BP and IMC
# --> recode settlements as losses
# --> testing for association with presidential admin (period effect)
win_los_reg(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE,
  yr_i = 1988,
  yr_f = 2020,
  party_or_admin = "ADMIN",
  recode_set = TRUE
)








# regression of plaintiff wins and losses
# --> excluding BP and IMC
# --> testing for association with party of president (partisan effect)
# --> WITH diagonal
win_los_reg(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE,
  yr_i = 1988,
  yr_f = 2020,
  party_or_admin = "PARTY"
)


# regression of plaintiff wins and losses
# --> excluding BP and IMC
# --> testing for association with presidential admin (period effect)
# --> WITH diagonal
win_los_reg(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE,
  yr_i = 1988,
  yr_f = 2020,
  party_or_admin = "ADMIN"
)


  




test <- fjc_e_da %>%
  filter(
    DEF_typ == "FED",
    PLT_typ != "NGO",
    PLT_typ != "BIZ"
  )

test <- fjc_e_da %>%
  filter(
    DEF_typ == "FED",
    PLT_typ == "NGO"
  )

test <- simp_filt(
  df = "fjc_e",
  jud = FALSE,
  disp_drop = NULL,
  noBP_IMC = FALSE,
  diag = TRUE
  ) %>%
  filter(
    yr_file >= 1988,
    yr_file <= 2022
  ) %>%
  filter(
    PLT_typ == "FED" | 
    DEF_typ == "FED"
    #PLT_typ %in% c("NGO","BIZ","FED")
    #PLT_typ %in% c("LOC","STA")
    #PLT_typ %in% c("NGO","BIZ","FED","IND","LOC","STA")
  )

test1 <- test %>%
  filter(
    PLT_wl == "n"
  ) %>%
  mutate(
    JUDGMENT = case_when(
      JUDGMENT == 0 ~ -8,
      TRUE ~ JUDGMENT
    )
  ) %>%
  group_by(
    JUDGMENT, DISP
  ) %>%
  mutate(
    disp_n = n(),
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    JUDGMENT, DISP, disp_n
  ) %>%
  ungroup() %>%
  mutate(
    tot = sum(disp_n, na.rm = T),
    disp_pct = round(disp_n/tot*100,3)
  )


#test <- tt_win$data

# call function to build df

source("Functions/dist_df.R")
df = build_dist_df(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE, # intra-type suits excluded
  yr = FALSE
)

# resl
test <- resl %>%
  group_by(
   ID 
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  filter(
    yr_term <= 2022,
    yr_term >= 1988
  )





# regression of plaintiff wins and losses
# --> excluding BP and IMC
# --> testing for association with party of president (partisan effect)
win_los_reg(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE,
  yr_i = 1988,
  yr_f = 2020,
  party_or_admin = "PARTY"
)


# regression of plaintiff wins and losses
# --> excluding BP and IMC
# --> testing for association with presidential admin (period effect)
win_los_reg(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE,
  yr_i = 1988,
  yr_f = 2020,
  party_or_admin = "ADMIN"
)



test <- 



























































### RESL STATUTES BY PLAINTIFF TYPE ######

# first do FED
resl_pfed <- resl %>%
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
  rowwise() %>%
  filter(
    "fed" %in% plt_typ
  )

# list of statutes contained in cases with federal plaintiffs
#Agency_list <- sort(unique(unlist(resl$Agency)))
statute_list_pfed <- sort(unique(unlist(resl_pfed$statute)))

# make df of just statutes
# !! note: unnest_wider is very slow without names_sep argument_added.
resl_pfed_stat <- resl_pfed %>%
  #filter(
  #  row_number()<= 10
  #  ) %>%
  select(
    statute
  ) %>%
  unnest_wider(
    statute,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = statute_1:last_col(),
    names_to = "statute_num",
    values_to = "statute",
    names_prefix = "statute_"
  ) %>%
  filter(
    !is.na(statute)
  ) %>%
  select(
    statute
  ) %>%
  group_by(
    statute
  ) %>%
  count() %>%
  mutate(
    plt_typ = "fed"
  )


# next do NGO
resl_pngo <- resl %>%
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
  rowwise() %>%
  filter(
    "ngo" %in% plt_typ,
    !("ngo_o" %in% plt_typ)
  )
  

# list of statutes contained in cases with federal plaintiffs
#Agency_list <- sort(unique(unlist(resl$Agency)))
statute_list_pngo <- sort(unique(unlist(resl_pngo$statute)))

# make df of just statutes
# !! note: unnest_wider is very slow without names_sep argument_added.
resl_pngo_stat <- resl_pngo %>%
  #filter(
  #  row_number()<= 10
  #  ) %>%
  select(
    statute
  ) %>%
  unnest_wider(
    statute,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = statute_1:last_col(),
    names_to = "statute_num",
    values_to = "statute",
    names_prefix = "statute_"
  ) %>%
  filter(
    !is.na(statute)
  ) %>%
  select(
    statute
  ) %>%
  group_by(
    statute
  ) %>%
  count() %>%
  mutate(
    plt_typ = "ngo"
  ) 




# next do BIZ
resl_pbiz <- resl %>%
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
  rowwise() %>%
  filter(
    "industry" %in% plt_typ |
    "trade_assn" %in% plt_typ
  )


# list of statutes contained in cases with federal plaintiffs
#Agency_list <- sort(unique(unlist(resl$Agency)))
statute_list_pbiz <- sort(unique(unlist(resl_pbiz$statute)))

# make df of just statutes
# !! note: unnest_wider is very slow without names_sep argument_added.
resl_pbiz_stat <- resl_pbiz %>%
  #filter(
  #  row_number()<= 10
  #  ) %>%
  select(
    statute
  ) %>%
  unnest_wider(
    statute,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = statute_1:last_col(),
    names_to = "statute_num",
    values_to = "statute",
    names_prefix = "statute_"
  ) %>%
  filter(
    !is.na(statute)
  ) %>%
  select(
    statute
  ) %>%
  group_by(
    statute
  ) %>%
  count() %>%
  mutate(
    plt_typ = "biz"
  )

  
# bind data frames
resl_stat_cnts <- rbind(
  resl_pfed_stat,resl_pngo_stat,resl_pbiz_stat
  )

# categorize and then name statutes
resl_stat_cnts <- resl_stat_cnts %>%
  # categorize statute types
  mutate(
    stat_type = case_when(
      statute == "nepa" ~ "Conservation/Pollution",
      statute == "cwa" ~ "Conservation/Pollution",
      statute == "esa" ~ "Conservation",
      statute == "nfma" ~ "Conservation",
      statute == "caa" ~ "Pollution",
      statute == "flpma" ~ "Conservation",
      statute == "rcra" ~ "Pollution",
      statute == "smcra" ~ "Conservation/Pollution",
      statute == "wilderness act" ~ "Conservation",
      statute == "rivers and harbors act" ~ "Secondary Link to\nEnvironmental Regulation/Harm",
      statute == "cercla" ~ "Pollution",
      statute == "sdwa" ~ "Pollution",
      statute == "lacey act" ~ "Conservation",
      statute == "mbta" ~ "Conservation",
      statute == "magnuson-stevens act" ~ "Conservation",
      statute == "opa" ~ "Pollution",
      TRUE ~ "unknown"
    )
  ) %>%
  mutate(
    statute = case_when(
      statute == "nepa" ~ "National Environmental Policy Act",
      statute == "cwa" ~ "Clean Water Act",
      statute == "esa" ~ "Endangered Species Act",
      statute == "nfma" ~ "National Forest Management Act",
      statute == "caa" ~ "Clean Air Act",
      statute == "flpma" ~ "Federal Land Policy and Management Act",
      statute == "rcra" ~ "Resource Conservation and Recovery Act",
      statute == "smcra" ~ "Surface Mining Control and Reclamation Act",
      statute == "wilderness act" ~ "Wilderness Act",
      statute == "rivers and harbors act" ~ "Rivers and Harbors Act",
      statute == "cercla" ~ "Superfund (CERCLA)",
      statute == "sdwa" ~ "Safe Drinking Water Act",
      statute == "lacey act" ~ "Lacey Act",
      statute == "mbta" ~ "Migratory Bird Treaty Act",
      statute == "magnuson-stevens act" ~ "Magnuson-Stevens act",
      statute == "opa" ~ "Oil Pollution Act",
      TRUE ~ statute
      )
    )


rm(resl_pfed_stat,resl_pngo_stat,resl_pbiz_stat)

# calculate percentages of suits by statute by PLT_typ
resl_stat_cnts  <- resl_stat_cnts %>%
  group_by(
    plt_typ
  ) %>%
  mutate(
    plt_n_tot = sum(n, na.rm = T),
    stat_pt_pct = round(n/plt_n_tot*100,4)
  ) %>%
  arrange(
    plt_typ, desc(stat_pt_pct)
  ) %>%
  mutate(
    stat_pt_cumsum = cumsum(stat_pt_pct)
  ) %>%
  ungroup()
  

# SET CUSTOM   COLOR SCALES ######

# set fixed fill scale
fixed_viridis_d4_scale = 
  scale_fill_manual(
    values = setNames(
      c(viridis(4)),
      c("Conservation",
        "Conservation/Pollution",
        "Pollution",
        "Secondary Link to\nEnvironmental Regulation/Harm")
    ),
    aesthetics = "fill",
    na.value = "grey50",
    # include function for wrapping legend label text
    labels = function(x) str_wrap(x, width = 30)
  )

### PLOT STATUTES BY PLAINTIFF TYPES ######




# plot function
plot_statues_by_plt_typ <- function(df, typ, leg = TRUE){
  df <- df %>%
    group_by(plt_typ) %>%
    mutate(
      plt_typ = case_when(
        plt_typ == "biz" ~ "Firms and Trade Associations",
        plt_typ == "fed" ~ "Federal Government",
        plt_typ == "ngo" ~ "Environmental Advocacy Groups",
        TRUE ~ "Unknown"
      )
    ) %>%
    mutate(
      rank = rank(desc(n), ties.method = "random")
    ) %>%
    filter(
      rank <= 10
    ) %>%
    filter(
      plt_typ == typ
    ) %>%
    arrange(plt_typ,rank) %>%
    group_by(plt_typ) %>%
    # make rank a factor
    mutate(
      statute = factor(
        x = rank,
        levels = rank,
        labels = statute,
        ordered = is.ordered(statute)
      )
    ) %>%
    ggplot() +
    geom_bar(
      aes(
        x = statute,
        y = n,
        fill = stat_type
      ),
      color = "grey",
      stat = "identity"
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(x, width = 25)
    ) +
    facet_wrap(
      vars(plt_typ),
      scales="free_x"
    ) +
    fixed_viridis_d4_scale +
    theme_linedraw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    labs(
      fill = "Regulatory Focus\n of Statute",
      x = NULL,
      y = "No. of Decisions"
    ) +
    {
      if (leg == FALSE)
        guides(
          color = "none",
          fill = "none"
        )
    } #+
  #ylim(0,800)
}

# ngo plot
ngo_plot <- plot_statues_by_plt_typ(
  resl_stat_cnts,
  "Environmental Advocacy Groups",
  leg = FALSE
)

# fed plot
fed_plot <- plot_statues_by_plt_typ(
  resl_stat_cnts,
  "Federal Government",
  leg = TRUE
)

# biz plot
biz_plot <- plot_statues_by_plt_typ(
  resl_stat_cnts,
  "Firms and Trade Associations",
  leg = TRUE
)

# plot all three together
ngo_plot + fed_plot + biz_plot + 
  plot_layout(
    axes = 'collect',
    guides = 'collect'
    )

# save
ggsave(
  "Most_Common_statutes_by_PLT_typ.png",
  width = 14,
  height = 9,
  path = "Figures"
)

# remove unecessary dfs
rm(resl_stat_cnts,
   biz_plot, fed_plot, ngo_plot)

  

  
### RESL AGENCIES TARGETED BY NGO PLAINTIFFS #####

# make df of just agencies targeted by engos
# !! note: unnest_wider is very slow without names_sep argument_added.
resl_pngo_agy <- resl_pngo %>%
  #filter(
  #  row_number()<= 10
  #  ) %>%
  select(
    agy
  ) %>%
  unnest_wider(
    agy,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = agy_1:agy_5,
    names_to = "agy_num",
    values_to = "agy",
    names_prefix = "agy_"
  ) %>%
  filter(
    !is.na(agy)
  ) %>%
  select(
    agy
  ) %>%
  group_by(
    agy
  ) %>%
  count() %>%
  mutate(
    plt_typ = "ngo"
  ) %>%
  # categorize statute types
  mutate(
    stat_type = case_when(
      agy == "fs" ~ "Conservation",
      agy == "fws" ~ "Conservation",
      agy == "acoe" ~ "Conservation/Pollution",
      agy == "epa" ~ "Pollution",
      agy == "blm" ~ "Conservation",
      agy == "nmfs" ~ "Conservation",
      agy == "fha" ~ "Secondary Link to\nEnvironmental Regulation/Harm",
      agy == "noaa" ~ "Conservation",
      agy == "bor" ~ "Conservation/Pollution",
      agy == "nps" ~ "Conservation",
      agy == "usn" ~ "Secondary Link to\nEnvironmental Regulation/Harm",
      agy == "doe" ~ "Pollution",
      agy == "fda" ~ "Secondary Link to\nEnvironmental Regulation/Harm",
      agy == "osmre" ~ "Conservation/Pollution",
      agy == "usda" ~ "Conservation",
      agy == "aphis" ~ "Conservation/Pollution",
      TRUE ~ "unknown"
    )
  ) %>%
  # provide full names of agencies
  mutate(
    agy = case_when(
      agy == "fs" ~ "Forest Service",
      agy == "fws" ~ "Fish and Wildlife Service",
      agy == "acoe" ~ "Army Corps of Engineers",
      agy == "epa" ~ "Environmental Protection agy",
      agy == "blm" ~ "Bureau of Land Management",
      agy == "nmfs" ~ "National Marine Fisheries Service",
      agy == "fha" ~ "Federal Housing Administration",
      agy == "noaa" ~ "National Oceanic and Atmospheric\nAdministration",
      agy == "bor" ~ "Bureau of Reclamation",
      agy == "nps" ~ "National Park Service",
      TRUE ~ "Unknown"
    )
  ) 

# plot agencies targeted by engos
ngo_agy_plot <- resl_pngo_agy %>%
  filter(
    agy != "Unknown"
  ) %>%
  group_by(plt_typ) %>%
  mutate(
    plt_typ = case_when(
      plt_typ == "biz" ~ "Firms and Trade Associations",
      plt_typ == "fed" ~ "Federal Gvoernment",
      plt_typ == "ngo" ~ "Environmental Advocacy Groups",
      TRUE ~ "Unknown"
    )
  ) %>%
  mutate(
    rank = rank(desc(n), ties.method = "random")
  ) %>%
  filter(
    rank <= 10
  ) %>%
  filter(
    plt_typ == "Environmental Advocacy Groups"
  ) %>%
  arrange(plt_typ,rank) %>%
  group_by(plt_typ) %>%
  # make rank a factor
  mutate(
    agy = factor(
      x = rank,
      levels = rank,
      labels = agy,
      ordered = is.ordered(agy)
    )
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = agy,
      y = n,
      fill = stat_type
    ),
    color = "grey",
    stat = "identity"
  ) +
  facet_wrap(
    vars(plt_typ),
    scales="free_x"
  ) +
  #scale_fill_viridis_d() +
  fixed_viridis_d4_scale +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  labs(
    fill = "Substantive Focus of\nAgy Defendants",
    x = NULL,
    y = "No. of Decisions"
  ) #+
  #ylim(0,325)

#ngo_agy_plot


### RESL AGENCIES TARGETED BY BIZ PLAINTIFFS #####

# make df of agencies targeted by firms and trade associations

# !! note: unnest_wider is very slow without names_sep argument_added.
resl_pbiz_agy <- resl_pbiz %>%
  #filter(
  #  row_number()<= 10
  #  ) %>%
  select(
    agy
  ) %>%
  unnest_wider(
    agy,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = agy_1:agy_3,
    names_to = "agy_num",
    values_to = "agy",
    names_prefix = "agy_"
  ) %>%
  filter(
    !is.na(agy)
  ) %>%
  select(
    agy
  ) %>%
  group_by(
    agy
  ) %>%
  count() %>%
  mutate(
    plt_typ = "biz"
  ) %>%
  # categorize statute types
  mutate(
    stat_type = case_when(
      agy == "fs" ~ "Conservation",
      agy == "fws" ~ "Conservation",
      agy == "acoe" ~ "Conservation/Pollution",
      agy == "epa" ~ "Pollution",
      agy == "blm" ~ "Conservation",
      agy == "nmfs" ~ "Conservation",
      agy == "fha" ~ "Secondary Link to\nEnvironmental Regulation/Harm",
      agy == "noaa" ~ "Conservation",
      agy == "bor" ~ "Conservation/Pollution",
      agy == "nps" ~ "Conservation",
      agy == "usn" ~ "Secondary Link to\nEnvironmental Regulation/Harm",
      agy == "doe" ~ "Pollution",
      agy == "fda" ~ "Secondary Link to\nEnvironmental Regulation/Harm",
      agy == "osmre" ~ "Conservation/Pollution",
      agy == "usda" ~ "Conservation",
      agy == "aphis" ~ "Conservation/Pollution",
      TRUE ~ "unknown"
    )
  ) %>%
  # provide full names of agencies
  mutate(
    agy = case_when(
      agy == "fs" ~ "Forest Service",
      agy == "fws" ~ "Fish and Wildlife Service",
      agy == "acoe" ~ "Army Corps of Engineers",
      agy == "epa" ~ "Environmental Protection Agy",
      agy == "blm" ~ "Bureau of Land Management",
      agy == "nmfs" ~ "National Marine Fisheries Service",
      agy == "fha" ~ "Federal Housing Administration",
      agy == "noaa" ~ "National Oceanic and Atmospheric\nAdministration",
      agy == "bor" ~ "Bureau of Reclamation",
      agy == "nps" ~ "National Park Service",
      agy == "osmre" ~ "Office of Surface Mining\nReclamation and Enforcement",
      TRUE ~ "Unknown"
    )
  ) 

# plot agencies targeted by firms and trade associations
biz_agy_plot <- resl_pbiz_agy %>%
  filter(
    agy != "Unknown"
  ) %>%
  group_by(plt_typ) %>%
  mutate(
    plt_typ = case_when(
      plt_typ == "biz" ~ "Firms and Trade Associations",
      plt_typ == "fed" ~ "Federal Gvoernment",
      plt_typ == "ngo" ~ "Environmental Advocacy Groups",
      TRUE ~ "Unknown"
    )
  ) %>%
  mutate(
    rank = rank(desc(n), ties.method = "random")
  ) %>%
  filter(
    rank <= 10
  ) %>%
  filter(
    plt_typ == "Firms and Trade Associations"
  ) %>%
  arrange(plt_typ,rank) %>%
  group_by(plt_typ) %>%
  # make rank a factor
  mutate(
    agy = factor(
      x = rank,
      levels = rank,
      labels = agy,
      ordered = is.ordered(agy)
    )
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = agy,
      y = n,
      fill = stat_type
    ),
    color = "grey",
    stat = "identity"
  ) +
  facet_wrap(
    vars(plt_typ),
    scales="free_x"
  ) +
  #scale_fill_viridis_d() +
  fixed_viridis_d4_scale +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  labs(
    fill = "Substantive Focus of\nAgy Defendants",
    x = NULL,
    y = "No. of Decisions"
  ) #+
  #ylim(0,325)

#biz_agy_plot


# plot both plots together ####

# plot all both together
ngo_agy_plot + biz_agy_plot +
  plot_layout(
    axes = 'collect',
    guides = 'collect'
    )

# save
ggsave(
  "Most_Common_Agencies_by_PLT_typ.png",
  width = 10.25,
  height = 9,
  path = "Figures"
)




# look at RESL litigants most commonly associated with ESA



# examine NOS distributions of RESL data ####


# read in fjc-resl match
resl_fjc <- read_csv(
  "Data/RESL/resl_fjc_match.csv"
  ) %>%
  # make resl_ID lowercase for join
  mutate(
    resl_ID = str_to_lower(resl_ID)
  ) %>%
  # drop matches most likely to be wrong (helper2 == 3 or 4)
  filter(
    helper2 == 1 | helper2 == 2
    ) %>%
  # keep only key vars (mostly fjc)
  select(
    resl_ID,
    fjc_ID,
    contains("fjc_")
    ) %>%
  # join full resl data
  left_join(
    .,
    (resl %>%
      rename("resl_ID" = "ID") %>%
      # drop extra ICR rows
      group_by(
        resl_ID
        ) %>%
      mutate(
        n = row_number()
        ) %>%
      filter(
        n == 1
        ) %>%
       ungroup()
     ),
    by = "resl_ID"
  ) %>%
  mutate(
    # add readable names to NOS codes
    NOS_t = case_when(
        fjc_NOS == 893 ~ "Environmental Matters (893)",
        fjc_NOS == 895 ~ "Freedom of Information Act (895)",
        fjc_NOS == 890 ~ "Other Statutory Actions (890)",
        fjc_NOS == 440 ~ "Civil Rights - Other (440)",
        fjc_NOS == 442 ~ "Civil Rights - Employment (442)",
        fjc_NOS == 899 ~ "Administrative Procedure Act/Appeal of Agy. Descion (899)",
        fjc_NOS == 190 ~ "Contract - Other (190)",
        fjc_NOS == 550 ~ "Prisoner Petition - Civil Rights (550)",
        fjc_NOS == 360 ~ "Personal Injury - (360)",
        fjc_NOS == 423 ~ "Bankruptcy  - Withdrawal of Reference (28 USC § 157) (423)",
        fjc_NOS == 550 ~ "Bankruptcy - Appeal 28 USC § 158 (422)",
        fjc_NOS == 720 ~ "Labor/Management Relations (Union) (720)",
        fjc_NOS == 290 ~ "All Other Real Property (290)",
        fjc_NOS == 350 ~ "Torts/Personal Injury - Motor Vehicle (350)",
        fjc_NOS == 240 ~ "Real Property - Torts to Land (240)",
        fjc_NOS == 380 ~ "Other Personal Property Damage (380)",
        fjc_NOS == 891 ~ "Agricultural Acts (891)",
        fjc_NOS == 470 ~ "Racketeer Influenced and Corrupt Organizations (470)",
        fjc_NOS == 791 ~ "Employee Retirement Income Security Act (791)",
        fjc_NOS == 340 ~ "Torts/Personal Injury - Marine (340)",
        fjc_NOS == 210 ~ "Real Property - Land Condemnation (210)",
        fjc_NOS == 330 ~ "Torts/Personal Injury - Federal Employers' Liability (330)",
        fjc_NOS == 220 ~ "Real Property - Foreclosure (220)",
        fjc_NOS == 710 ~ "Fair Labor Standards Act (Non-Union) (710)",
        fjc_NOS == 365 ~ "Personal Injury - Product Liability (Excludes a marine or airplane product) (365)",
        fjc_NOS == 490 ~ "Cable/Satellite TV (490)",
        fjc_NOS == 110 ~ "Contract - Insurance (110)",
        fjc_NOS == 555 ~ "Prisoner Petitions - Prison Condition (555)",
        fjc_NOS == 444 ~ "Unrecognized fjc_NOS Code (444)",
        fjc_NOS == 790 ~ "Labor - Other Labor Litigation (790)",
        fjc_NOS == 950 ~ "Constitutionality of State Statutes (950)",
        fjc_NOS == 245 ~ "Tort Product Liability (245)",
        fjc_NOS == 410 ~ "Antitrust (410)",
        fjc_NOS == 385 ~ "Property Damage - Product Liability (385)",
        fjc_NOS == 120 ~ "Marine Contract Actions (120)",
        fjc_NOS == 690 ~ "OTHER FORFEITURE AND PENALTY SUITS (690)",
        fjc_NOS == 370 ~ "OTHER FRAUD (370)",
        fjc_NOS == 422 ~ "BANKRUPTCY APPEALS RULE 28 USC 158 (422)",
        fjc_NOS == 510 ~ "PRISONER PETITIONS - VACATE SENTENCE (510)",
        fjc_NOS == 195 ~ "CONTRACT PRODUCT LIABILITY (195)",
        fjc_NOS == 840 ~ "TRADEMARK (840)",
        fjc_NOS == 450 ~ "INTERSTATE COMMERCE (450)",
        fjc_NOS == 530 ~ "PRISONER PETITIONS - HABEAS CORPUS (530)",
        fjc_NOS == 850 ~ "SECURITIES, COMMODITIES, EXCHANGE (850)",
        fjc_NOS == 368 ~ "ASBESTOS PERSONAL INJURY - PROD.LIAB. (368)",
        fjc_NOS == 863 ~ "D.I.W.C./D.I.W.W. (863)",
        fjc_NOS == 870 ~ "TAX SUITS (870)",
        fjc_NOS == 892 ~ "ECONOMIC STABILIZATION ACT (892)",
        fjc_NOS == 864 ~ "S.S.I.D. (864)",
        fjc_NOS == 320 ~ "ASSAULT, LIBEL, AND SLANDER (950)",
        TRUE ~ "Other NOS"
        ),
    NOS_t = str_to_title(NOS_t),
    NOS_t = str_replace_all(NOS_t, "Of", "of"),
    NOS_t = str_replace_all(NOS_t, "And", "and"),
    NOS_t = str_replace_all(NOS_t, "To", "to"),
    NOS_t = str_replace_all(NOS_t, "tort", "Tort"),
    NOS_t = str_replace_all(NOS_t, "D.i.w.c./D.i.w.w.", "D.I.W.C./D.I.W.W."),
    NOS_t = str_replace_all(NOS_t, "S.s.i.d.", "S.S.I.D.")
  )

resl_fjc_NOS <- resl_fjc %>%
  group_by(
    fjc_ID
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  group_by(
    fjc_NOS
  ) %>%
  mutate(
    nos_n = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  select(
    fjc_NOS,
    nos_n,
    NOS_t
  ) %>%
  ungroup() %>%
  arrange(
    desc(nos_n)
  ) %>%
  mutate(
    tot = sum(nos_n),
    pct = round(nos_n/tot*100,2),
    tot_cum = cumsum(nos_n),
    pct_cum = round(tot_cum/tot*100,2),
    rank = row_number(),
    bar_label = str_c(
      pct,"% (", nos_n, ")"
    ),
    name_ord = factor(
      rank,
      levels = rank,
      labels = NOS_t
    )
  )

#plot
NOS_resl_dist <- resl_fjc_NOS %>%
  filter(
    # pct_cum <= 99.05 # this gets us the top 42 NOS codes
    # pct_cum <= 96.00 # this gets us the top 20 NOS codes
     pct_cum <= 93.00 # this gets us the top X NOS codes
  ) %>%
  ggplot(
    aes(
      y = name_ord,
      x = pct,
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(
    aes(label = bar_label),
    vjust = .5,
    hjust = -.1
    ) +
  scale_y_discrete(
    limits=rev,
    labels = function(x) stringr::str_wrap(x, width = 45)
    ) +
  labs(
    y = "Nature of Suit (NOS)",
    x = "% of all RESL ELD cases matched to FJC IDB data"
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
  coord_cartesian(
    xlim = c(0,72)
  )

# view plot
NOS_resl_dist

# save
ggsave(
  "Top_96pct_of_NOS_codes_in_RESL.png",
  width = 12,
  height = 9,
  path = "Figures"
)

ggsave(
  "Top_93pct_of_NOS_codes_in_RESL.png",
  width = 11,
  height = 4,
  path = "Figures"
)

# count and plot statutes in NOS == 893 portion of data ####

# first do statutes in NOS == 893 set
resl_893s_stat <- resl_fjc %>%
  ungroup() %>%
  filter(
    fjc_NOS == 893
  ) %>%
  select(
    statute
  ) %>%
  unnest_wider(
    statute,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = statute_1:last_col(),
    names_to = "statute_num",
    values_to = "statute",
    names_prefix = "statute_"
  ) %>%
  filter(
    !is.na(statute)
  ) %>%
  select(
    statute
  ) %>%
  group_by(
    statute
  ) %>%
  count() %>%
  mutate(
    nos_cat = "Environmental Matters (NOS = 893)"
  ) %>%
  ungroup() %>%
  arrange(
    desc(n)
  ) %>%
  mutate(
    tot = sum(n),
    pct = round(n/tot*100,2),
    tot_cum = cumsum(n),
    pct_cum = round(tot_cum/tot*100,2),
    rank = row_number(),
    bar_label = str_c(
      pct,"% (", n, ")"
    ),
    name_ord = factor(
      rank,
      levels= rank,
      labels = statute
    )
  )
  
#plot
resl_893s_plot <- resl_893s_stat %>%
  filter(
    # pct_cum <= 99.05 # this gets us the top 42 NOS codes
    #pct_cum <= 95.00 # this gets us the top 20 NOS codes
    rank <= 20
  ) %>%
  ggplot(
    aes(
      y = name_ord,
      x = pct,
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(
    aes(label = bar_label),
    vjust = .5,
    hjust = -.1
    ) +
  scale_y_discrete(
    limits=rev,
    labels = function(x) stringr::str_wrap(x, width = 45)
    ) +
  labs(
    y = "Statutes",
    x = '% of Enviro. Matters (NOS = 893) in RESL ELD data'
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
  coord_cartesian(
    xlim = c(0,30)
  )

# show plot
resl_893s_plot

# count and plot statutes in NOS != 893 portion of data

# do statutes in NOS != 893 set
resl_NOT_893s_stat <- resl_fjc %>%
  ungroup() %>%
  filter(
    fjc_NOS != 893
  ) %>%
  select(
    statute
  ) %>%
  unnest_wider(
    statute,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = statute_1:last_col(),
    names_to = "statute_num",
    values_to = "statute",
    names_prefix = "statute_"
  ) %>%
  filter(
    !is.na(statute)
  ) %>%
  select(
    statute
  ) %>%
  group_by(
    statute
  ) %>%
  count() %>%
  mutate(
    nos_cat = "All Other NOS Codes"
  ) %>%
  ungroup() %>%
  arrange(
    desc(n)
  ) %>%
  mutate(
    tot = sum(n),
    pct = round(n/tot*100,2),
    tot_cum = cumsum(n),
    pct_cum = round(tot_cum/tot*100,2),
    rank = row_number(),
    bar_label = str_c(
      pct,"% (", n, ")"
    ),
    name_ord = factor(
      rank,
      levels= rank,
      labels = statute
    )
  )
  
#plot
resl_NOT_893s_plot <- resl_NOT_893s_stat %>%
  filter(
    # pct_cum <= 99.05 # this gets us the top 42 NOS codes
    #pct_cum <= 95.00 # this gets us the top 20 NOS codes
    rank <= 20
  ) %>%
  ggplot(
    aes(
      y = name_ord,
      x = pct,
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(
    aes(label = bar_label),
    vjust = .5,
    hjust = -.1
    ) +
  scale_y_discrete(
    limits=rev,
    labels = function(x) stringr::str_wrap(x, width = 45)
    ) +
  labs(
    y = "Statutes",
    x = '% of all other NOS codes in RESL ELD data'
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
  coord_cartesian(
    xlim = c(0,30)
  )

# show plot
resl_NOT_893s_plot


# count and plot agencies in NOS == 893 portion of data ####

# first do agys in NOS == 893 set
resl_893s_agy <- resl_fjc %>%
  ungroup() %>%
  filter(
    fjc_NOS == 893
  ) %>%
  select(
    agy
  ) %>%
  unnest_wider(
    agy,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = agy_1:last_col(),
    names_to = "agy_num",
    values_to = "agy",
    names_prefix = "agy_"
  ) %>%
  filter(
    !is.na(agy)
  ) %>%
  select(
    agy
  ) %>%
  group_by(
    agy
  ) %>%
  count() %>%
  mutate(
    nos_cat = "Environmental Matters (NOS = 893)"
  ) %>%
  ungroup() %>%
  arrange(
    desc(n)
  ) %>%
  mutate(
    tot = sum(n),
    pct = round(n/tot*100,2),
    tot_cum = cumsum(n),
    pct_cum = round(tot_cum/tot*100,2),
    rank = row_number(),
    bar_label = str_c(
      pct,"% (", n, ")"
    ),
    name_ord = factor(
      rank,
      levels= rank,
      labels = agy
    )
  )
  
#plot
resl_893s_plot_agy <- resl_893s_agy %>%
  filter(
    # pct_cum <= 99.05 # this gets us the top 42 NOS codes
    #pct_cum <= 95.00 # this gets us the top 20 NOS codes
    rank <= 20
  ) %>%
  ggplot(
    aes(
      y = name_ord,
      x = pct,
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(
    aes(label = bar_label),
    vjust = .5,
    hjust = -.1
    ) +
  scale_y_discrete(
    limits=rev,
    labels = function(x) stringr::str_wrap(x, width = 45)
    ) +
  labs(
    y = "Agencies",
    x = '% of Enviro. Matters (NOS = 893) in RESL ELD data'
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
  coord_cartesian(
    xlim = c(0,80)
  )

# show plot
resl_893s_plot_agy

# count and plot agys in NOS != 893 portion of data

# do agys in NOS != 893 set
resl_NOT_893s_agy <- resl_fjc %>%
  ungroup() %>%
  filter(
    fjc_NOS != 893
  ) %>%
  select(
    agy
  ) %>%
  unnest_wider(
    agy,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = agy_1:last_col(),
    names_to = "agy_num",
    values_to = "agy",
    names_prefix = "agy_"
  ) %>%
  filter(
    !is.na(agy)
  ) %>%
  select(
    agy
  ) %>%
  group_by(
    agy
  ) %>%
  count() %>%
  mutate(
    nos_cat = "All Other NOS Codes"
  ) %>%
  ungroup() %>%
  arrange(
    desc(n)
  ) %>%
  mutate(
    tot = sum(n),
    pct = round(n/tot*100,2),
    tot_cum = cumsum(n),
    pct_cum = round(tot_cum/tot*100,2),
    rank = row_number(),
    bar_label = str_c(
      pct,"% (", n, ")"
    ),
    name_ord = factor(
      rank,
      levels= rank,
      labels = agy
    )
  )
  
#plot
resl_NOT_893s_plot_agy <- resl_NOT_893s_agy %>%
  filter(
    # pct_cum <= 99.05 # this gets us the top 42 NOS codes
    #pct_cum <= 95.00 # this gets us the top 20 NOS codes
    rank <= 20
  ) %>%
  ggplot(
    aes(
      y = name_ord,
      x = pct,
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(
    aes(label = bar_label),
    vjust = .5,
    hjust = -.1
    ) +
  scale_y_discrete(
    limits=rev,
    labels = function(x) stringr::str_wrap(x, width = 45)
    ) +
  labs(
    y = "Agencies",
    x = '% of all other NOS codes in RESL ELD data'
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
  coord_cartesian(
    xlim = c(0,80)
  )

# show plot
resl_NOT_893s_plot_agy


# plot total NOS distribution, and statutes distributions together ####

plots <- NOS_resl_dist / (resl_893s_plot + resl_NOT_893s_plot) / (resl_893s_plot_agy + resl_NOT_893s_plot_agy)
plots[[2]] <- plots[[2]] + plot_layout(tag_level = 'new', axes = "collect")
plots[[3]] <- plots[[3]] + plot_layout(tag_level = 'new', axes = "collect")
plots + plot_layout(heights = c(3, 2, 2)) +
  plot_annotation(tag_levels = c('A',"1"))  & 
  theme(plot.tag = element_text(face = "bold"))

# save
ggsave(
  "Comparing_attributes_accross_NOS_codes.png",
  width = 12,
  height = 14,
  path = "Figures"
)




# look at civil rights cases (NOS = 440)

# do civil right statutes
resl_civil_rights_stat <- resl_fjc %>%
  ungroup() %>%
  filter(
    fjc_NOS == 440
  ) %>%
  select(
    statute
  ) %>%
  unnest_wider(
    statute,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = statute_1:last_col(),
    names_to = "statute_num",
    values_to = "statute",
    names_prefix = "statute_"
  ) %>%
  filter(
    !is.na(statute)
  ) %>%
  select(
    statute
  ) %>%
  group_by(
    statute
  ) %>%
  count() %>%
  mutate(
    nos_cat = "Civil Rights"
  ) %>%
  ungroup() %>%
  arrange(
    desc(n)
  ) %>%
  mutate(
    tot = sum(n),
    pct = round(n/tot*100,2),
    tot_cum = cumsum(n),
    pct_cum = round(tot_cum/tot*100,2),
    rank = row_number(),
    bar_label = str_c(
      pct,"% (", n, ")"
    ),
    name_ord = factor(
      rank,
      levels= rank,
      labels = statute
    )
  )
  
#plot
resl_civil_rights_plot <- resl_civil_rights_stat %>%
  filter(
    # pct_cum <= 99.05 # this gets us the top 42 NOS codes
    #pct_cum <= 95.00 # this gets us the top 20 NOS codes
    rank <= 20
  ) %>%
  ggplot(
    aes(
      y = name_ord,
      x = pct,
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(
    aes(label = bar_label),
    vjust = .5,
    hjust = -.1
    ) +
  scale_y_discrete(
    limits=rev,
    labels = function(x) stringr::str_wrap(x, width = 45)
    ) +
  labs(
    y = "Statutes",
    x = '% of all civil rights (NOS = 440) codes in RESL ELD data'
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
  coord_cartesian(
    xlim = c(0,38)
  )

# show plot
resl_civil_rights_plot


# look at real property - torts to land cases (NOS = 240)

# do torts to land statutes
resl_ttl_stat <- resl_fjc %>%
  ungroup() %>%
  filter(
    fjc_NOS == 240
  ) %>%
  select(
    statute
  ) %>%
  unnest_wider(
    statute,
    names_sep = "_"
  ) %>%
  pivot_longer(
    cols = statute_1:last_col(),
    names_to = "statute_num",
    values_to = "statute",
    names_prefix = "statute_"
  ) %>%
  filter(
    !is.na(statute)
  ) %>%
  select(
    statute
  ) %>%
  group_by(
    statute
  ) %>%
  count() %>%
  mutate(
    nos_cat = "Torts to Land"
  ) %>%
  ungroup() %>%
  arrange(
    desc(n)
  ) %>%
  mutate(
    tot = sum(n),
    pct = round(n/tot*100,2),
    tot_cum = cumsum(n),
    pct_cum = round(tot_cum/tot*100,2),
    rank = row_number(),
    bar_label = str_c(
      pct,"% (", n, ")"
    ),
    name_ord = factor(
      rank,
      levels= rank,
      labels = statute
    )
  )
  
#plot
resl_ttl_plot <- resl_ttl_stat %>%
  filter(
    # pct_cum <= 99.05 # this gets us the top 42 NOS codes
    #pct_cum <= 95.00 # this gets us the top 20 NOS codes
    rank <= 20
  ) %>%
  ggplot(
    aes(
      y = name_ord,
      x = pct,
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(
    aes(label = bar_label),
    vjust = .5,
    hjust = -.1
    ) +
  scale_y_discrete(
    limits=rev,
    labels = function(x) stringr::str_wrap(x, width = 45)
    ) +
  labs(
    y = "Statutes",
    x = '% of all torts to land (NOS = 240) codes in RESL ELD data'
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
  coord_cartesian(
    xlim = c(0,38)
  )

# show plot
resl_ttl_plot


# plot total NOS distribution, and statutes distributions together ####

resl_ttl_plot + resl_civil_rights_plot +
  plot_layout(axes = "collect") +
  plot_annotation(tag_levels = c('A',"1"))  & 
  theme(plot.tag = element_text(face = "bold"))

# save
ggsave(
  "Comparing_civil_rights_and_ttl_NOS_codes.png",
  width = 12,
  height = 5,
  path = "Figures"
  )


# plot RESL heatmap ####

# call function for building resl df for heatmap ####
source("Functions/resl_heatmap_df.R")

# build resl df for heat map
resl_heatmap <- resl_heat_df("all")


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
# Plot RESL heat maps of litigant types ####


# plot RESL heat map
resl_heat <- plot_PD_combo_heatmap(
  df = resl_heatmap,
  yr_start = 1988,
  yr_end = 2022,
  l_typs = l_typs,
  title = "Plaintiff and Defendant Type Combinations - RESL ELD District Courts - 1988-2021\nUnweighted; all NOS codes",
  court_level = "D"
  )

# plot FJC heat map for all DISTRICT cases and all dispositions
fjc_heat <- plot_PD_combo_heatmap(
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
  title = "Plaintiff and Defendant Type Combinations - FJC IDB District Courts 1988-2022",
  court_level = "D"
)

resl_heat / fjc_heat +
  plot_annotation(tag_levels = 'A')  &
  #plot_layout(guides = 'collect') &
  theme(plot.tag = element_text(face = "bold"))


ggsave(
  "Fig_X_HEATMAP_fjc_resl_combo.png",
  path = "Figures",
  units = "mm",
  height = 300,
  width = 180
)


# count settlements and dismissals v. judgements in FJC data
df = simp_filt(
  df = "fjc_e",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE
  ) %>%
  ungroup() %>%
  mutate(
    dissmissal = case_when(
      DISP %in% c(0,1,2,3,10,11,12,13,14) ~ "Dissmissal",
      TRUE ~ "Judgment"
    )
  ) %>%
  select(
    PLT_typ, DEF_typ, DISP, dissmissal
  ) %>%
  group_by(PLT_typ, DEF_typ, dissmissal) %>%
  mutate(
    diss_n = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  group_by(PLT_typ, DEF_typ) %>%
  mutate(
    PLT_DEF_n = sum(diss_n),
    diss_pct = round(diss_n/PLT_DEF_n*100,2),
    PLT_DEF = str_c(PLT_typ, DEF_typ)
  ) %>%
  ungroup()


# build heatmap to be adjusted by dismissal rate ONLY NOS != 893 cases

resl_heatmap_weighted <- resl_heat_df("no893") %>% # calling df building function; no 893 cases
  ungroup() %>%
  mutate(
    # count all the cases in [possibly filtered] df
    tot_cases = n()
  ) %>%
  group_by(PLT_typ,DEF_typ) %>%
  mutate(
    PD_freq = n(),
    PD_pct = round(PD_freq/tot_cases*100,2)
  ) %>% select(
    PLT_typ, DEF_typ, PD_freq, PD_pct, tot_cases
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  mutate(
    PLT_DEF = str_c(PLT_typ, DEF_typ)
  ) %>%
  left_join(
    df %>%
      filter(
        dissmissal == "Judgment"
      ) %>%
      select(
        PLT_DEF, diss_pct
      ),
    by = "PLT_DEF"
  ) %>%
  mutate(
    PD_freq_w = round(PD_freq/(diss_pct/100),0),
    tot = sum(PD_freq, na.rm = T),
    tot_w = sum(PD_freq_w, na.rm = T),
    PD_pct_w = round(PD_freq_w/tot_w*100,2)
    ) %>%
  filter(
    # retain only specified plaintiff and defendant types
    PLT_typ %in% l_typs,
    DEF_typ %in% l_typs
  ) %>%
  # rename plaintiff types to easy-to-read formats
  mutate(
    PLT_typ = case_when(
      PLT_typ == "BIZ" ~ "Firm & Trade Assn",
      PLT_typ == "FED" ~ "Federal Gov't",
      PLT_typ == "IND" ~ "Individual",
      PLT_typ == "LOC" ~ "Local Gov't",
      PLT_typ == "NGO" ~ "Enviro. NGOs",
      PLT_typ == "STA" ~ "State Gov't",
      PLT_typ == "CIVIC" ~ "Civic Orgs",
      PLT_typ == "NGO_O" ~ "Other NGOs",
      PLT_typ == "OTHER" ~ "Other Orgs",
      PLT_typ == "TRIBE" ~ "Tribes",
      TRUE ~ PLT_typ
    ),
    DEF_typ = case_when(
      DEF_typ == "BIZ" ~ "Firm & Trade Assn",
      DEF_typ == "FED" ~ "Federal Gov't",
      DEF_typ == "IND" ~ "Individual",
      DEF_typ == "LOC" ~ "Local Gov't",
      DEF_typ == "NGO" ~ "Enviro. NGOs",
      DEF_typ == "STA" ~ "State Gov't",
      DEF_typ == "CIVIC" ~ "Civic Orgs",
      DEF_typ == "NGO_O" ~ "Other NGOs",
      DEF_typ == "OTHER" ~ "Other Orgs",
      DEF_typ == "TRIBE" ~ "Tribes",
      TRUE ~ DEF_typ
    )
  ) %>%
  # reorder factors for plot
  mutate(
    DEF_typ = factor(
      DEF_typ,
      levels = sort(unique(DEF_typ), decreasing = TRUE),
      labels = sort(unique(DEF_typ), decreasing = TRUE)
    )
  )

# build plot
resl_heat_w <- resl_heatmap_weighted %>%
  ggplot() +
  geom_tile(
    aes(
      x = PLT_typ,
      y = DEF_typ,
      fill = log(PD_pct_w)
    )
  ) +
  geom_text(
    aes(
      x = PLT_typ,
      y = DEF_typ,
      label = PD_pct_w
    ),
    size.unit = "pt",
    size = 7
  ) + 
  scale_fill_viridis_c(
    breaks = c(-4.605, -2.30, 0, 2.302, 3.688),
    labels = c(0.01, 0.1, 1, 10, 40)
    #limits = c(-4.60517,4.0943)
  ) +
  scale_x_discrete(position = "top") +
  labs(
        title = "Plaintiff and Defendant Type Combinations - RESL ELD District Courts\nWeighted by est. Dismissal Rate; NOS != 893 Environmental Matters",
        subtitle = str_c("n = ", prettyNum(first(resl_heatmap_weighted$tot_w), big.mark = ","), sep = ""),
        x = "Plaintiff Types",
        y = "Defendant Types",
        fill = "Frequency\n(% of all cases\n weighted by dissmissal rate)"
      ) +
  theme_linedraw() +
  theme(plot.title = element_text(size = 7),
        plot.subtitle = element_text(size = 7),
        text = element_text(size = 7)
        )

resl_heat_w / fjc_heat +
  plot_annotation(tag_levels = 'A')  &
  #plot_layout(guides = 'collect') &
  theme(plot.tag = element_text(face = "bold"))


ggsave(
  "Fig_X_HEATMAP_fjc_resl_combo_WEIGHTED_no893.png",
  path = "Figures",
  units = "mm",
  height = 300,
  width = 180
)

resl_heat_w

ggsave(
  "Fig_X_HEATMAP_fjc_resl_only_WEIGHTED_no893.png",
  path = "Figures",
  units = "mm",
  height = 144,
  width = 180
)




# build heatmap to be adjusted by dismissal rate - ONLY NOS == 893 cases

resl_heatmap_weighted <- resl_heat_df("893") %>% # calling df building function; only 893 cases
  ungroup() %>%
  mutate(
    # count all the cases in [possibly filtered] df
    tot_cases = n()
  ) %>%
  group_by(PLT_typ,DEF_typ) %>%
  mutate(
    PD_freq = n(),
    PD_pct = round(PD_freq/tot_cases*100,2)
  ) %>% select(
    PLT_typ, DEF_typ, PD_freq, PD_pct, tot_cases
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  mutate(
    PLT_DEF = str_c(PLT_typ, DEF_typ)
  ) %>%
  left_join(
    df %>%
      filter(
        dissmissal == "Judgment"
      ) %>%
      select(
        PLT_DEF, diss_pct
      ),
    by = "PLT_DEF"
  ) %>%
  mutate(
    PD_freq_w = round(PD_freq/(diss_pct/100),0),
    tot = sum(PD_freq, na.rm = T),
    tot_w = sum(PD_freq_w, na.rm = T),
    PD_pct_w = round(PD_freq_w/tot_w*100,2)
  ) %>%
  filter(
    # retain only specified plaintiff and defendant types
    PLT_typ %in% l_typs,
    DEF_typ %in% l_typs
  ) %>%
  # rename plaintiff types to easy-to-read formats
  mutate(
    PLT_typ = case_when(
      PLT_typ == "BIZ" ~ "Firm & Trade Assn",
      PLT_typ == "FED" ~ "Federal Gov't",
      PLT_typ == "IND" ~ "Individual",
      PLT_typ == "LOC" ~ "Local Gov't",
      PLT_typ == "NGO" ~ "Enviro. NGOs",
      PLT_typ == "STA" ~ "State Gov't",
      PLT_typ == "CIVIC" ~ "Civic Orgs",
      PLT_typ == "NGO_O" ~ "Other NGOs",
      PLT_typ == "OTHER" ~ "Other Orgs",
      PLT_typ == "TRIBE" ~ "Tribes",
      TRUE ~ PLT_typ
    ),
    DEF_typ = case_when(
      DEF_typ == "BIZ" ~ "Firm & Trade Assn",
      DEF_typ == "FED" ~ "Federal Gov't",
      DEF_typ == "IND" ~ "Individual",
      DEF_typ == "LOC" ~ "Local Gov't",
      DEF_typ == "NGO" ~ "Enviro. NGOs",
      DEF_typ == "STA" ~ "State Gov't",
      DEF_typ == "CIVIC" ~ "Civic Orgs",
      DEF_typ == "NGO_O" ~ "Other NGOs",
      DEF_typ == "OTHER" ~ "Other Orgs",
      DEF_typ == "TRIBE" ~ "Tribes",
      TRUE ~ DEF_typ
    )
  ) %>%
  # reorder factors for plot
  mutate(
    DEF_typ = factor(
      DEF_typ,
      levels = sort(unique(DEF_typ), decreasing = TRUE),
      labels = sort(unique(DEF_typ), decreasing = TRUE)
    )
  )

# build plot
resl_heat_w <- resl_heatmap_weighted %>%
  ggplot() +
  geom_tile(
    aes(
      x = PLT_typ,
      y = DEF_typ,
      fill = log(PD_pct_w)
    )
  ) +
  geom_text(
    aes(
      x = PLT_typ,
      y = DEF_typ,
      label = PD_pct_w
    ),
    size.unit = "pt",
    size = 7
  ) + 
  scale_fill_viridis_c(
    breaks = c(-4.605, -2.30, 0, 2.302, 3.688),
    labels = c(0.01, 0.1, 1, 10, 40)
    #limits = c(-4.60517,4.0943)
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title = "Plaintiff and Defendant Type Combinations - RESL ELD District Courts\nWeighted by est. Dismissal Rate; NOS == 893 Environmental Matters",
    subtitle = str_c("n = ", prettyNum(first(resl_heatmap_weighted$tot_w), big.mark = ","), sep = ""),
    x = "Plaintiff Types",
    y = "Defendant Types",
    fill = "Frequency\n(% of all cases\n weighted by dissmissal rate)"
  ) +
  theme_linedraw() +
  theme(plot.title = element_text(size = 7),
        plot.subtitle = element_text(size = 7),
        text = element_text(size = 7)
  )

resl_heat_w / fjc_heat +
  plot_annotation(tag_levels = 'A')  &
  #plot_layout(guides = 'collect') &
  theme(plot.tag = element_text(face = "bold"))


ggsave(
  "Fig_X_HEATMAP_fjc_resl_combo_WEIGHTED_893.png",
  path = "Figures",
  units = "mm",
  height = 300,
  width = 180
)

# the end
