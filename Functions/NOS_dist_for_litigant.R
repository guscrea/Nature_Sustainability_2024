# Function for plotting distributions of NOS codes for selected plaintiffs

# for testing
#in_terms <- "RESOURCES D|RES DEF"
#out_terms <- "COMPANY|PROF|PROD|PROP|PROC|CORP|EPA[A-Z]|[A-R]EPA|[T-Z]EPA|SIERRA C[A-K]|SIERRA C[M-Z]|ARES"

# function for plotting NOS distributions by litigant
NOS_dis_plot <- function(in_terms,out_terms,facet_title){
  
  # first filter complete dataset by terms to isolate litigant
  fjc_tots_litigant <- fjc_tots %>%
    filter(
      ((str_detect(DEF, in_terms) & !str_detect(DEF, out_terms))|
         (str_detect(PLT, in_terms) & !str_detect(PLT, out_terms)))
    )
  
  # count litignat NOS codes; add full NOS names
  # count NOS codes - EPA
  fjc_tots_litigant_NOS <- fjc_tots_litigant %>%
    group_by(
      NOS
    ) %>%
    mutate(
      nos_n = n()
    ) %>%
    filter(
      row_number() == 1
    ) %>%
    ungroup() %>%
    select(
      NOS,nos_n
    ) %>%
    mutate(
      tot_n = sum(nos_n, na.rm = T),
      nos_pct = nos_n/tot_n*100,
      NOS_name = case_when(
        NOS == 893 ~ "Environmental Matters (893)",
        NOS == 895 ~ "Freedom of Information Act (895)",
        NOS == 890 ~ "Other Statutory Actions (890)",
        NOS == 440 ~ "Civil Rights - Other (440)",
        NOS == 442 ~ "Civil Rights - Employment (442)",
        NOS == 899 ~ "Administrative Procedure Act/Appeal of Agy. Descion (899)",
        NOS == 190 ~ "Contract - Other (190)",
        NOS == 550 ~ "Prisoner Petition - Civil Rights (550)",
        NOS == 360 ~ "Personal Injury - (360)",
        NOS == 423 ~ "Bankruptcy  - Withdrawal of Reference (28 USC § 157) (423)",
        NOS == 550 ~ "Bankruptcy - Appeal 28 USC § 158 (422)",
        NOS == 720 ~ "Labor/Management Relations (Union) (720)",
        NOS == 290 ~ "All Other Real Property (290)",
        NOS == 350 ~ "Torts/Personal Injury - Motor Vehicle (350)",
        NOS == 240 ~ "Real Property - Torts to Land (240)",
        NOS == 380 ~ "Other Personal Property Damage (380)",
        NOS == 891 ~ "Agricultural Acts (891)",
        NOS == 470 ~ "Racketeer Influenced and Corrupt Organizations (470)",
        NOS == 791 ~ "Employee Retirement Income Security Act (791)",
        NOS == 340 ~ "Torts/Personal Injury - Marine (340)",
        NOS == 210 ~ "Real Property - Land Condemnation (210)",
        NOS == 330 ~ "Torts/Personal Injury - Federal Employers' Liability (330)",
        NOS == 220 ~ "Real Property - Foreclosure (220)",
        NOS == 710 ~ "Fair Labor Standards Act (Non-Union) (710)",
        NOS == 365 ~ "Personal Injury - Product Liability (Excludes a marine or airplane product) (365)",
        NOS == 490 ~ "Cable/Satellite TV (490)",
        NOS == 110 ~ "Contract - Insurance (110)",
        NOS == 555 ~ "Prisoner Petitions - Prison Condition (555)",
        NOS == 444 ~ "Unrecognized NOS Code (444)",
        NOS == 790 ~ "Labor - Other Labor Litigation (790)",
        TRUE ~ "Other NOS"
      )
    )
  
  # plot!
  fjc_tots_litigant_NOS %>%
    arrange(desc(nos_n)) %>%
    filter(
      row_number() <= 12
    ) %>%
    mutate(
      facet_label = facet_title
    ) %>%
    ggplot(
      aes(
        x = NOS_name,
        y = nos_pct,
        color = NOS_name,
        fill = NOS_name,
        label = round(nos_pct,1)
      )
    ) +
    geom_bar(
      stat = "identity"
    ) +
    geom_text(
      color = "black",
      nudge_y = 2
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(x, width = 25)
    ) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    guides(
      color = "none",
      fill = "none"
    ) +
    labs(
      x = NULL,
      y = "Percent of Cases"
    ) +
    facet_wrap(
      vars(facet_label)
    ) +
    theme_linedraw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
    ylim(0,65)
  
}