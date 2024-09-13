# function to plot substantive focus of RESL decision documents

# df is input df to plot
# ejc_ooc must be either 
# - "ejc" (referring to a plot of climate and environmental justice)
# - "ooc" (referring to the ooc substantive policy focus),
# group_color_var specifies the variable to group by and set colors by
# y_min = lower limit of y-axis
# y_max = upper limit of y-axis

plot_policy_focus <- function(df, ejc_ooc, group_color_var, y_min, y_max){
  
  if(ejc_ooc != "ejc" &
     ejc_ooc != "ooc"){
    print(
      "Error: did you specify ejc_ooc as either \"ejc\" or \"ooc\"?"
    )
  }
  
  # convert quoted string input for grouping variable to symbol 
  group_color_var <- sym(group_color_var)
  
  # make plot
  df %>%
    {
    if(ejc_ooc == "ejc"){
      group_by(
        .,
        plt_typ, yr_term,label
      ) %>%
        filter(
          .,
          row_number() == 1
        )
      } else if (ejc_ooc == "ooc"){
        filter(
          .,
          yr_term >=1988,
          OOC_new %in% c("Waste & Pollution","Energy & Mineral Resources",
                       "Conservation")
          )
        }
      } %>%
    # foundational plot function
      ggplot(
        aes(
          x = yr_term,
          y = pct,
          group = !!group_color_var,
          color = !!group_color_var
        )
      ) +
      geom_point(# plot actual data points
        aes(
          fill = !!group_color_var,
          shape = !!group_color_var
        ),
        alpha = .6,
        color = "#00000080",
        #shape = 21,
        size = 1.5
      ) +
      geom_line( # plot smoothed line
        alpha = 0.7,
        stat="smooth",
        size = .8
      ) +
      geom_ribbon( # plot confidence interval around line
        aes(
          color = NULL
        ),
        alpha = 0.15,
        stat="smooth",
        show_guide = FALSE
      ) +
    {
        if(ejc_ooc == "ejc"){
          # use orange and black colorblind friendly COLOR palate for ejc
          scale_color_colorblind()
        } else if (ejc_ooc == "ooc"){
          # use viridis colorblind friendly COLOR palate for ooc
            scale_color_viridis_d()
        }
      } +
    {
      if(ejc_ooc == "ejc"){
        # use orange and black colorblind friendly FILL palate for ejc
          scale_fill_colorblind()
      } else if (ejc_ooc == "ooc"){
        # use viridis colorblind friendly FILL palate for ooc
        scale_fill_viridis_d()
      }
    } +
    scale_shape_manual(
      values = c(21,22,23,24)
      ) +
      labs(
        x = NULL,
        y = "Percent of all Decisions",
        color = "Substantive Focus\nof Judicial Decisions",
        fill = "Substantive Focus\nof Judicial Decisions",
        shape = "Substantive Focus\nof Judicial Decisions"
      ) +
    {
      if(ejc_ooc == "ejc"){
        facet_wrap(
          vars(plt_typ)
        )
      } else if(ejc_ooc == "ooc"){
        facet_wrap(
          vars(plt_typ_new)
        )
      }
    } +
      theme_linedraw() +
      theme(
        plot.title = element_text(size = 7),
        plot.subtitle = element_text(size = 7),
        text = element_text(size = 7)
      ) +
      ylim(y_min,y_max)
}