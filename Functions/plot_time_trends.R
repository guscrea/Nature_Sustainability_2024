# function to plot time trends of litigation

# inputs
# df = df to build box plots from. Note: this may be a df built from the
#      build_dist_df() function, with yr == FALSE.
# y_var = the variable in df to plot on the y-axis
# y_upper = upper limit of y-axis
# y_axis_lab = the label for the y-axis
# PLT_typs = which plaintiff types to include (these are facets in the output)
# yr_i = starting year
# yr_f = ending year,
# plot_var ="pt_pwl_pct" OR "pt_dur" OR "pt"

#for testing
# plot_var = "pt"
# yr_i = 1988
# yr_f = 2021
# PLT_typs = c("NGO","BIZ","FED")

plot_ttrends <- function(df, y_var,y_upper, PLT_typs, y_axis_lab, yr_i, yr_f,plot_var){
 df %>%
    group_by(
    PLT_typ, yr_file
    ) %>%
    {
      if(plot_var == "pt_pwl_pct" | plot_var == "pt_dur" | plot_var == "pt_yr_pwl_pct")
        filter(
          .,
          PLT_wl == "w"
          ) %>%
        filter(
          row_number() == 1
          )
      else
        filter(
          .,
          row_number() == 1
          )
      } %>%
  filter(
    yr_file >= yr_i,
    yr_file <= yr_f
  ) %>%
  filter(
    PLT_typ %in% PLT_typs
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
  ggplot() +
    geom_point(
      aes(
        x = yr_file,
        y = {{y_var}},
        group = PLT_typ,
        fill = PLT_typ
      ),
      color = "#00000080",
      shape = 21,
      size = 1.5
    ) +
    geom_line(
      aes(
        x = yr_file,
        y = {{y_var}},
        group = PLT_typ,
        color = PLT_typ,
      ),
      alpha = 0.5,
      stat="smooth",
      size = 1
    ) +
    geom_ribbon(
      aes(
        x = yr_file,
        y = {{y_var}},
        group = PLT_typ,
        color = NULL,
      ),
      alpha = 0.33,
      stat="smooth"
    ) +
    labs(
      x = NULL,
      y = y_axis_lab
    ) +
    facet_wrap(
      vars(PLT_typ),
      ncol = 3
    ) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    guides(colour = "none", fill = "none") +
    xlim(1988, 2022) +
    ylim(0, y_upper) +
    theme_linedraw() +
    theme(plot.title = element_text(size = 7),
          plot.subtitle = element_text(size = 7),
          text = element_text(size = 7)
    )
}