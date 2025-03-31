  options(box.path = here::here())
  box::use(
    rio[import],
    pl = paletteer,
    data.table[...],
    forcats[fct_recode],
    ggplot2[...],
    waffle[geom_waffle],
    ggtext[element_textbox_simple],
    gridtext[richtext_grob],
    cowplot[ggdraw, draw_grob],
    showtext[showtext_auto],
    sysfonts[font_add_google],
    owidapi[owid_get],
    patchwork[...],
    gghighlight[gghighlight],
    scico
  )


  # get idps data
  dat <- refugees::idmc
  iso <- refugees::countries
  setDT(dat)

  # keep only 2024
  dat <- dat[year == 2024]

  # left merge countries coa_iso == iso_code
  dat <- merge(dat, iso, by.x = "coa_iso", by.y = "iso_code")

  # get population data
  pop <- owid_get("population")
  setDT(pop)
  # pop data from 2023
  pop <- pop[year == 2023 & entity_id != ""]

  # merge with dat on coa_iso = entity_id
  dat <- merge(dat, pop, by.x = "coa_iso", by.y = "entity_id")

  # calculate proportion of idp per pop
  dat[, prop := total / population_historical * 100]

  # State of Palestine to Palestinee
  # Syrian Arab Rep. to Syria
  dat[name == "State of Palestine", name := "Palestine"]
  dat[name == "Syrian Arab Rep.", name := "Syria"]
  dat[name == "Dem. Rep. of the Congo", name := 'Congo Kinshasa']


  # Order FIRST before creating position indexes
  setorderv(dat, "total", order = -1)
  top10_total <- dat[1:30]

  # Now create position indexes AFTER sorting
  top10_total[, id := 1:.N]  # Reset name based on sorted order
  top10_total[, coa_iso := factor(coa_iso, levels = coa_iso)]  # Preserve order in factor

  # Calculate angles based on NEW order
  angle <- 90 - 360 * (top10_total$id - 0.5)/nrow(top10_total)
  top10_total$hjust <- ifelse(angle < -90, 1, 0)
  top10_total$angle <- ifelse(angle < -90, angle + 180, angle)


  # do the same for proportions
  setorderv(dat, "prop", order = -1)
  top10_prop <- dat[1:30]
  top10_prop[, id := 1:.N]  # Reset name based on sorted order
  top10_prop[, coa_iso := factor(coa_iso, levels = coa_iso)]  # Preserve order in factor
  angle <- 90 - 360 * (top10_prop$id - 0.5)/nrow(top10_prop)
  top10_prop$hjust <- ifelse(angle < -90, 1, 0)
  top10_prop$angle <- ifelse(angle < -90, angle + 180, angle)


  # circular bar plot for total
  p1 <- ggplot(top10_total, aes(x = id, y = total, fill = total)) +
    # Main bars
    geom_col(alpha = 0.8, width = 1) +
    gghighlight(
      id <= 12, 
      unhighlighted_params = list(fill = "lightgray", alpha = 1)
    ) +
    # Central white circle
    # annotate(
    #   "rect",
    #   xmin = -Inf, xmax = Inf,
    #   ymin = -Inf, ymax = max(top10_total$total) * 0.02,
    #   fill = "white"
    # ) +
    coord_polar() +
    ylim(0, max(top10_total$total) * 1.05) + # Add extra space (5%) above the tallest bar
    # Labels
    geom_text(
      aes(
        y = total + (max(total) * 0.05),
        label = name,
        hjust = hjust
      ),
      color = "black",
      size = 4,
      angle = top10_total$angle[1:12],
      family = 'Nunito'
    ) +
    theme_minimal(
      base_family = 'Nunito'
    ) +
    labs(x = NULL, y = NULL, fill = "Number of IDPs") +
    theme(
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = c(0.5, 0.1),
      legend.title = element_text(size = 12, margin = margin(b = 5)),  # Added bottom margin
      legend.text = element_text(size = 10),
      legend.margin = margin(t = 15, b = 0),  # Tightened top/bottom legend margins
      legend.box.spacing = unit(0, "pt"),  # Eliminated box spacing
      plot.margin = margin(t = 10, b = 0)  # Reduced overall plot margins
    ) +
    pl$scale_fill_paletteer_c("scico::acton",
        direction = -1,
        na.value = "lightgray",
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        guide = guide_colorbar(
          direction = "horizontal", # Horizontal layout
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          label.hjust = 0.5,
          nrow = 1,
          keywidth = unit(150, "pt"),  # Balanced key size
      )
    )
    # scale_fill_viridis_b(
    #   option = "magma",
    #   direction = -1,
    #   breaks = scales::breaks_extended(n = 6),
    #   na.value = "lightgray",  
    #   labels = scales::label_number(
    #     scale_cut = scales::cut_short_scale(),
    #     accuracy = 0.1
    #   ),
    #   guide = guide_legend(
    #     direction = "horizontal",
    #     title.position = "top",
    #     title.hjust = 0.5,
    #     label.position = "bottom",
    #     label.hjust = 0.5,
    #     keywidth = unit(25, "pt"),  # Balanced key size
    #     keyheight = unit(15, "pt"),
    #     nrow = 1,
    #     override.aes = list(alpha = 0.8)  # Match bar transparency
    #   )
    #)

  # same for prop
  p2 <- ggplot(top10_prop, aes(x = id, y = prop, fill = prop)) +
    # White central base
    # geom_col(
    #   aes(y = max(prop) * 0.2),
    #   fill = "white",
    #   width = 1,
    #   show.legend = FALSE
    # ) +
    # Main bars
    geom_col(alpha = 0.8, width = 1) +
    gghighlight(
      id <= 12, 
      unhighlighted_params = list(fill = "lightgray", alpha = 1)
    ) +
    # Central white circle
    # annotate(
    #   "rect",
    #   xmin = -Inf, xmax = Inf,
    #   ymin = -Inf, ymax = max(top10_prop$prop) * 0.2,
    #   fill = "white"
    # ) +
    coord_polar() +
    ylim(0, max(top10_prop$prop) * 1.05) + # Add extra space (20%) above the tallest bar
    # Labels
    geom_text(
      aes(
        y = prop + (max(prop) * 0.05),
        label = name,
        hjust = hjust
      ),
      color = "black",
      #fontface = "bold",
      size = 4,
      angle = top10_prop$angle[1:12],
      family = 'Nunito'
    ) +
    theme_minimal(
      base_family = 'Nunito'
    ) +
    labs(x = NULL, y = NULL, fill = "Proportion of IDPs") +
    theme(
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = c(0.5, 0.1),
      legend.title = element_text(size = 12, margin = margin(b = 5)),  # Added bottom margin
      legend.text = element_text(size = 10),
      legend.margin = margin(t = 15, b = 0),  # Tightened top/bottom legend margins
      legend.box.spacing = unit(0, "pt"),  # Eliminated box spacing
      plot.margin = margin(t = 10, b = 0)  # Reduced overall plot margins
    ) +
    pl$scale_fill_paletteer_c("scico::acton",
      direction = -1,
      na.value = "lightgray",
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      guide = guide_colorbar(
        direction = "horizontal", # Horizontal layout
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        label.hjust = 0.5,
        nrow = 1,
        keywidth = unit(150, "pt"),  # Balanced key size
  )
  )
  

  # use patchwork to add both graphs together
  # Combine plots
  # Create a text placeholder plot (adjust height ratio as needed)
  # title_plot <- ggplot() + 
  #   annotate(
  #     "text", 
  #     x = 0.5, 
  #     y = 0.5, 
  #     label = "Top 30 Countries by IDP Statistics (2024)\nLeft: Absolute Numbers | Right: Proportion of Population",
  #     size = 5, 
  #     family = "Nunito",
  #     fontface = "bold"
  #   ) +
  #   theme_void() +
  #   theme(plot.margin = margin(b = 5))  # Reduce bottom margin



  # Create final layout
  # Combine everything with title on top
  final_layout <- (p1 + p2) +
    plot_layout(widths = c(1, 1)) +
    inset_element(
      ggplot() +
        geom_curve(
          aes(x = 0.68, xend = 0.75, y = 0.8, yend = 0.85),  # Horizontal center alignment
          curvature = -0.3,  # Negative for downward curve
          angle = 120,
          arrow = arrow(length = unit(0.02, "npc")),
          linewidth = 0.5
        ) +
        annotate(
          "text",
          x = 0.50,
          y = 0.76,
          label = paste(strwrap("In Palestine and in Syria, 1 person out of 3 is internally displaced.", width = 36), collapse = "\n"), # Wrap text
          size = 3.5,
          hjust = 0,
          family = "Nunito"
        ) +
        theme_void() +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE),
      left = 0,
      right = 1,
      bottom = 0,
      top = 1
    ) +
    plot_annotation(
      title = "<b style='font-size:24px; color:#2a475e;'>Internal Displacement Crisis (2024)</b><br>
               <span style='font-size:16px; color:#6b6b6b;'>Absolute Numbers vs. Population Proportions</span><br><br>
               <span style='font-size:14px; color:#404040;'>Comparative metrics reveal different dimensions of displacement: 
               Absolute numbers (left) show crisis magnitude, while proportions 
               (right) reveal population impact. Syria and Palestine demonstrate 
               how high proportions (1 in 3 displaced) indicate systemic collapse, 
               despite smaller absolute numbers compared to larger nations. As Desrosières (1993) noted: 'Statistical constructs shape our 
               understanding of social realities - their power lies in making 
               visible what they measure, while hiding what they exclude.'</span><br>
               <span style='font-size:12px; color:#404040;'>Data: UNHCR package showing IDMC displacement data & Our World In Data | Plot: @gnoblet</span>",
      theme = theme(
        plot.title = element_textbox_simple(
          family = "Nunito", 
          size = 18, 
          hjust = 0.5,
          halign = 0.5,
          # no margin below, even negative
          margin = margin(
            l = 20,
            r = 20,
            b = 20,
            t = 10)
        )
      )
    )

  # Display the plot
  final_layout

# save fig
ggsave(
  "2025/day_03.png",
  height = 9,
  width = 10,
  dpi = 600
)
