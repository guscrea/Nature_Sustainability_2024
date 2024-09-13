# function to plot box plots of win rates by region
# By Chris Rea
# Last modified April 26, 2024

# inputs
# df = df to build box plots from. Note: this may be a df built from the
#      build_dist_df() function, with yr == FALSE.

plot_wr_box_plots_reg <- function(df){
  
  plot_win_rate <- df %>%
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
        TRUE ~ PLT_typ
      )
    ) %>%
    # add line break in Northeast and Western Interior region labels
    mutate(
      REGION = case_when(
        REGION == "Northeast/Mid-Atlantic" ~ "Northeast/\nMid-Atlantic",
        REGION == "Western Interior" ~ "Western\nInterior",
        TRUE ~ REGION
      )
    ) %>%
    filter(
      # keep only stats for wins
      PLT_wl == "w"
    ) %>%
    group_by(
      DISTRICT, REGION, PLT_typ
    ) %>%
    filter(
      row_number() ==1
    ) %>%
    filter(
      !(is.na(REGION))
    ) %>%
    group_by(
      REGION, PLT_typ
    ) %>%
    mutate(
      n_dist = n(),
      REGION = str_to_title(REGION),
      REGION = str_c(
        REGION,
        "\n",
        format(r_pt_term, big.mark = ",", scientific = FALSE),
        " cases; n = ",
        n_dist,
        " dist."
        )
    ) %>%
    ungroup()
  
  # build plot
  plot_win_rate %>%
    ggplot() +
    geom_segment(# net win rate for plt_typ across regions
      data = plot_win_rate %>%
        group_by(PLT_typ,REGION) %>%
        filter(
          row_number() ==1
        ) %>%
        ungroup,
      aes(
        x = -Inf,
        xend = Inf,
        y = pt_pwl_pct,
        yend = pt_pwl_pct
      ),
      linetype = "solid",
      alpha = .7,
      color = "GREY",
      size = .7
    ) +
    geom_flat_violin(# distribution of cases by district by region
      #data = 
      aes(
        x = REGION,
        #y = d_pt_pwl_pct,
        y = d_pt_r_pct,
        fill = REGION
      ),
      color = "#ffffff00",
      alpha = .66
    ) +
    geom_point(# distribution of cases by district by region - points
      #data = 
      aes(
        x = REGION,
        #y = d_pt_pwl_pct,
        y = d_pt_r_pct,
        fill = REGION
      ),
      color = "#00000080",
      alpha = .66,
      shape = 23,
      size = 1.5
    ) +
    geom_boxplot(# box plot of win rates by district by region
      aes(
        x = REGION,
        y = d_pt_pwl_pct
      ),
      alpha = 0,
      size = .4
    ) +
    geom_point(# regional net (weighted) win rate
      data = plot_win_rate %>%
        group_by(REGION, PLT_typ) %>%
        filter(
          row_number() == 1
        ),
      aes(
        x = REGION,
        y = r_pt_pwl_pct,
        #fill = REGION
      ),
      alpha = 1,
      size = 2,
      shape = 21,
      fill = "#adadad80",
      color = "#00000080"
    ) +
    scale_y_continuous(
      breaks = seq(0,100,10),
      sec.axis = sec_axis(
        ~.,
        breaks = seq(0,100,10),
        name = "% cases per district\n(diamonds)"
      )
    ) +
    labs(
      x = NULL,
      y = "% plaintiff wins\n(box plots)",
      color = "Plaitiff Types"
    ) +
    facet_wrap(
      vars(PLT_typ),
      ncol = 3,
      scales = "free_x"
    ) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    guides(colour = "none", fill = "none") +
    theme_linedraw() +
    theme(
      plot.title = element_text(size = 7),
      plot.subtitle = element_text(size = 7),
      text = element_text(size = 7),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      axis.title.y.right = element_text(angle = 90, hjust = .5)
    ) +
    coord_cartesian(
      ylim = c(0,92)
    )
}