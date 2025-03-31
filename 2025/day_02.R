options(box.path = here::here())
box::use(
  rio[import],
  pl = paletteer,
  data.table[...],
  forcats[fct_recode],
  ggplot2[...],
  gghighlight[gghighlight],
  refugees
  # waffle[geom_waffle],
  # ggtext[element_textbox_simple],
  # gridtext[richtext_grob],
  # cowplot[ggdraw, draw_grob]
)

# get population data
dat <- refugees::population
setDT(dat)

# sum total number of refugees per year
dat_sum_year <- dat[, 
  lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
  .SDcols = c("refugees", "asylum_seekers", "returned_refugees", "idps", "returned_idps", "stateless"),
  by = .(year)]

# add column of total
dat_sum_year[, all_pop := refugees + asylum_seekers + returned_refugees + idps + returned_idps + stateless]

# melt to long
dat_sum_year_long <- melt(
  dat_sum_year,
  variable.name = "pop_type",
  value.name = "n",
  id.vars = c("year")
)

# recoding categories
cat <- c(
  "refugees" = "Refugees",
  "asylum_seekers" = "Asylum Seekers",
  "returned_refugees" = "Returned Refugees",
  "idps" = "IDPs",
  "returned_idps" = "Returned IDPs",
  "stateless" = "Stateless",
  "all_pop" = "Total"
)


dat_sum_year_long[, pop_type := factor(pop_type, levels = names(cat), labels = cat)]

# add a geom line for all pop
g <- ggplot(dat_sum_year_long[pop_type != "Total"], aes(x = year, y = n, group = pop_type, color = pop_type, fill = pop_type)) +
  geom_line() +
  gghighlight(pop_type %in% c("Refugees", "IDPs"), use_direct_label = T) +
  theme_minimal() +
  labs(
    title = "Sloppy on Displacement Data",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ",", scale_cut = scales::cut_short_scale()),
  ) +
  theme(
    legend.position = "none"
  )

 g