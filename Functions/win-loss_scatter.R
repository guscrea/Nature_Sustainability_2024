# function to plot x-y scatter of win v loss by region by plt_typ
# By Chris Rea
# Last modified April 26, 2024

# inputs
# df = df to build box plots from. Note: this may be a df built from the
#      build_dist_df() function, with yr == FALSE.

plot_win_los_scatter <- function(df){
  # create plot-specific dataframe for map
  plot_data_points <- df %>%
    ungroup() %>%
    # keep only NGO, BIZ, and FED plaintiff types
    filter(
      PLT_typ == "NGO"|
        PLT_typ == "BIZ" |
        #PLT_typ == "IND" |
        PLT_typ == "FED"
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
    ) %>%
    filter(
      PLT_wl == "w" | PLT_wl == "l"  # look at plaintiff wins and losses
    ) %>%
    select(
      DISTRICT,
      REGION,
      PLT_typ,
      PLT_wl,
      pt_pct,
      #pt_pwl_pct,
      pt_wlr_lab,
      d_pt,
      d_pt_term,
      d_pt_pwl,
      #d_pt_pwl_pct_z,
      pt_lab #pt_pwl_lab,pt_pwl_stdev_lab
    ) %>%
    #mutate(
    #  PLT_wl2 = PLT_wl
    #) %>%
    pivot_wider(
      names_prefix = "PLT_",
      names_from = PLT_wl,
      values_from = d_pt_pwl,
      values_fill = 0
    ) %>%
    group_by(
      DISTRICT, PLT_typ
    ) %>%
    mutate(
      pt_pw_pct = round(PLT_w/d_pt_term*100,4)
    ) %>%
    group_by(
      PLT_typ
    ) %>%
    mutate(
      pt_pw_pct_mean = round(sum(PLT_w, na.rm = T)/sum(d_pt_term, na.rm = T)*100,4),
      pt_pw_pct_mean_lab = str_c("μ = ",round(pt_pw_pct_mean,1), sep = ""),
      pt_pw_pct_med = round(median(pt_pw_pct, na.rm = T),4),
      pt_pw_pct_med_lab = str_c("M = ",round(pt_pw_pct_med,1)),
      pt_pw_pct_stdev = sd(pt_pw_pct, na.rm = T),
      mean_line = (1-pt_pw_pct_mean/100)*PLT_l
    ) %>%
    group_by(
      DISTRICT, PLT_typ
    ) %>%
    mutate(
      d_pt_pw_pct_z = (pt_pw_pct-pt_pw_pct_mean)/pt_pw_pct_stdev
    ) %>%
    ungroup() %>%
    filter(
      !(is.na(REGION))
    ) %>%
    mutate(
      REGION = str_to_title(REGION)
    )
  
  # build df for diagonal
  diag_x = seq(1,600,10)
  diag_y = seq(1,600,10)
  diagonal <- data.frame(diag_x,diag_y)
  
  # plot scatter of districts
  plot_data_points %>%
    ggplot() +
    geom_point(#background ouline points
      aes(
        x = log(PLT_l+1),
        y = log(PLT_w+1),
        shape = REGION,
        size = d_pt+(d_pt*.25)
      ),
      color = "grey",
      fill = "grey",
      stroke = 1
    ) +
    geom_point(#primary points
      aes(
        x = log(PLT_l+1),
        y = log(PLT_w+1),
        shape = REGION,
        color = REGION,
        fill = REGION,
        size = d_pt
      ),
      stroke = 1
    ) +
    geom_line(#equal win-loss line
      data = diagonal,
      aes(
        x = log(diag_x+1),
        y = log(diag_y+1)
      ),
      linetype = "dotted",
      size = .3
    ) +
    geom_text(# label total number of cases by plaintiff type
      data = plot_data_points %>%
          group_by(PLT_typ) %>%
          filter(
            row_number() == 1
          ),
      aes(
        x = log(80),
        y = log(6),
        label = pt_lab,
        hjust = 0
      ),
      size.unit = "pt",
      size = 6
    ) +
    geom_text(# label win-loss ratio
      data = plot_data_points %>%
          group_by(PLT_typ) %>%
          filter(
            row_number() == 1
          ),
      aes(
        x = log(80),
        y = log(3.5),
        label = pt_wlr_lab,
        hjust = 0
      ),
      size.unit = "pt",
      size = 6
    ) +
    geom_text(# label average win rate by plaintiff type
      data = plot_data_points %>%
        group_by(PLT_typ) %>%
        filter(
          row_number() == 1
        ),
      aes(
        x = log(80),
        y = log(2),
        label = pt_pw_pct_mean_lab,
        hjust = 0
      ),
      size.unit = "pt",
      size = 6
    ) +
    geom_text(# label median win rate by plaintiff type
      data = plot_data_points %>%
        group_by(PLT_typ) %>%
        filter(
          row_number() == 1
        ),
      aes(
        x = log(80),
        y = log(1),
        label = pt_pw_pct_med_lab,
        hjust = 0
      ),
      size.unit = "pt",
      size = 6
    ) +
    scale_color_viridis_d() +
    scale_x_continuous(
      breaks = c(0,0.6931,1.6094,2.3025,3.9120,4.6051,6.2146,6.9077),
      labels = c(0,1,5,10,50,100,500,1000)
    ) +
    scale_y_continuous(
      breaks = c(0,0.6931,1.6094,2.3025,3.9120,4.6051,6.2146,6.9077),
      labels = c(0,1,5,10,50,100,500,1000)
    ) +
    scale_size_area(
      max_size = 6,
      breaks=c(10,50,100,500,1000)
      ) +
    labs(
      x = "Plaintiff Losses",
      y = "Plaintiff Wins",
      color = "region",
      fill = "region",
      size = "total cases",
      shape = "region"
    ) +
    facet_wrap(
      vars(PLT_typ),
      ncol = 3
    ) +
    guides(
      shape = guide_legend(override.aes = list(size = 2.45))
      ) +
    theme_linedraw() +
    theme(
      panel.grid.major = element_blank(),
      plot.title = element_text(size = 7),
      plot.subtitle = element_text(size = 7),
      text = element_text(size = 7),
      legend.spacing.y = unit(-2, 'mm'),
      legend.key.height = unit(3.5, 'mm')
    )
} 