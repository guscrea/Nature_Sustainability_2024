# function to plot box plots of win rates by plaintiff type FOR APPELATE DATA
# By Chris Rea
# Last modified May 16, 2024

# inputs
# df = df to build box plots from, typically fjc_e_da
# targeted = if TRUE, keep only specified pairings of plaintiff and defendnat types

# for testing
# df = "fjc_e_da"
# targeted = TRUE

plot_wr_box_plots_appeals <- function(df, targeted){
  
  # call function to build district-level data
  source("Functions/dist_df.R")
  
  # get starting df
  df1 = get(df)
  
  # if targeted == TRUE, keep only specific pairings of Plt.-Def. combos
  
  if(targeted == TRUE){
    df2 = df1 %>%
    filter(
      (PLT_typ == "NGO" & DEF_typ == "FED") | # NGO appealing FED wins
        (PLT_typ == "BIZ" & DEF_typ == "FED") | # BIZ appealing FED wins
        (PLT_typ == "FED" & DEF_typ == "NGO") # FED appealing NGO wins
      ) %>%
      # count the number of cases across all districts by plaintiff types used
      # for calculations
      group_by(
        PLT_typ
      ) %>%
      mutate(
        cases_n = case_when(
          !is.na(PLT_wl) & jud_or_set == 1 ~ 1,
          TRUE ~ 0
        ),
        cases_n = sum(cases_n),
        cases_w = case_when(
          !is.na(PLT_wl) & jud_or_set == 1 & PLT_wl == "w" ~ 1,
          TRUE ~ 0
        ),
        cases_w = sum(cases_w)
      )
  } else if (targeted == FALSE) {
    # if targeted == FALSE, keep just NGO, BIZ, and FED plaintiffs, regardless
    # of def type targeted.
    df2 = df1 %>%
      filter(
        PLT_typ == "NGO"|
          PLT_typ == "BIZ" |
          PLT_typ == "FED"
      ) %>%
      # count the number of cases across all districts by plaintiff types used
      # for calculations
      group_by(
        PLT_typ
      ) %>%
      mutate(
        cases_n = case_when(
          !is.na(PLT_wl) & jud_or_set == 1 ~ 1,
          TRUE ~ 0
          ),
        cases_n = sum(cases_n),
        cases_w = case_when(
          !is.na(PLT_wl) & jud_or_set == 1 & PLT_wl == "w" ~ 1,
          TRUE ~ 0
        ),
        cases_w = sum(cases_w)
      )
  } else {
    stop('Cannot tell if data should focus on targeted PLT-DEF type pairing or not. Did you set "targeted" to TRUE or FALSE?')
  }
  
  # make df of win counts - PLTs
  win_counts <- df2 %>%
    select(
      PLT_typ, cases_n, cases_w
    ) %>%
    group_by(
      PLT_typ
    ) %>%
    filter(
      row_number() == 1
    ) %>%
    ungroup()
  
  # after filtering (if necessary) build df using build_dist_df(); pipe into plot.
  plot_df <- build_dist_df(
    df = df2,
    #df = fjc_e_da, # for troubleshooting
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = FALSE,
    diag = TRUE,
    yr = FALSE,
    appeals = TRUE
    ) %>%
    ungroup() %>%
    filter(
      # keep only stats for wins
      PLT_wl == "w",
      # keep only districts with 0 or more appeals
      d_pt_pwl >= 0
    ) %>%
    # keep only first observation of district by plt_typ
    group_by(
      DISTRICT, PLT_typ,
    ) %>%
    filter(
      row_number() ==1
    ) %>%
    # join number of cases counts from above
    left_join(
      win_counts,
      by = "PLT_typ"
    ) %>%
    # count number of districts for each plaintiff type
    group_by(
      PLT_typ
    ) %>%
    mutate(
      dist_n = n()
    ) %>%
    ungroup() %>%
    filter(
      !(is.na(DISTRICT))
    ) %>%
    mutate(
      fake_x_axis = "A"
    ) %>%
    {
      if (targeted == FALSE)
        mutate(
          .,
          PLT_typ = case_when(
            PLT_typ == "NGO" ~ str_c("Environmental Advocacy Groups\n",cases_n, " cases; n = ",dist_n ," dist."),
            PLT_typ == "BIZ" ~ str_c("Firms and Trade Associations\n",cases_n, " cases; n = ",dist_n ," dist."),
            PLT_typ == "FED" ~ str_c("Federal Government\n(appeals from all districts)\n",cases_n," cases; n = ",dist_n ," dist."),
            TRUE ~ PLT_typ
          )
        )
      else if (targeted == TRUE)
        mutate(
          .,
          PLT_typ = case_when(
            PLT_typ == "NGO" ~ str_c("Environmental Advocacy Groups\n(appealing fed. gov. wins)\n",cases_n, " cases; n = ",dist_n ," dist."),
            PLT_typ == "BIZ" ~ str_c("Firms and Trade Associations\n(appealing fed. gov. wins)\n",cases_n, " cases; n = ",dist_n ," dist."),
            PLT_typ == "IND" ~ str_c("Individuals\n","n = ",cases_n, " cases; n = ",dist_n ," dist."),
            PLT_typ == "FED" ~ str_c("Federal Government\n(appealing ENGO wins)\n",cases_n, " cases; n = ",dist_n ," dist."),
            TRUE ~ PLT_typ
          )
        )  
    }
  
  plot_df %>%
    ggplot() +
    geom_flat_violin(# distribution of cases by district - density
      #data = 
      aes(
        x = fake_x_axis,
        y = d_pt_pct,
        #y = d_pt_r_pct,
        fill = PLT_typ
      ),
      color = "#ffffff00",
      alpha = .6
    ) +
    geom_point(# distribution of cases by district - points
      #data = 
      aes(
        x = fake_x_axis,
        y = d_pt_pct,
        #y = d_pt_r_pct,
        fill = PLT_typ
      ),
      color = "#00000080",
      alpha = .5,
      shape = 23
    ) +
    geom_boxplot(# box plot of win rates by district
      aes(
        x = fake_x_axis,
        y = d_pt_pwl_pct
      ),
      alpha = 0
    ) +
    geom_point(# regional net (weighted) win rate
      data = plot_df %>%
        group_by(PLT_typ,DEF_typ) %>%
        filter(
          row_number() == 1
        ),
      aes(
        x = fake_x_axis,
        y = pt_pwl_pct,
        #fill = REGION
      ),
      alpha = 1,
      size = 4,
      shape = 21,
      fill = "#adadad80",
      color = "#00000080"
    ) +
    scale_y_continuous(
      breaks = seq(0,100,10),
      sec.axis = sec_axis(
        ~.,
        breaks = seq(0,100,10),
        name = "% of all cases in each district\n(diamonds)"
      )
    ) +
    labs(
      x = NULL,
      y = "% appellant wins across districts\n(box plots)",
      color = "Plaitiff Types"
    ) +
    facet_wrap(
      vars(PLT_typ),
      ncol = 3
    ) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    guides(colour = "none", fill = "none") +
    theme_linedraw() +
    theme(
      axis.text.x = element_blank()
      #axis.title.y.right = element_text(angle = 90, hjust = .5)
    ) +
    coord_cartesian(
      ylim = c(0,100)
    )

  # the end.
  
}