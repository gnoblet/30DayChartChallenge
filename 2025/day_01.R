
library(rio)
library(data.table)
library(ggplot2)
library(forcats)
library(waffle)
library(ggtext)
library(patchwork)

# import 
dat <- import("https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv")
setDT(dat)

# function to round down to the nearest decade
round_to_decade <- function(year) {
  return(year - year %% 10)
}

# sum consumption for france by source, every 10 years from 1900 to 2020
dat_sum <- dat[year >= 190 & year <= 2020 & country == "France", 
lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
.SDcols = c("biofuel_consumption", "coal_consumption", "gas_consumption", 
            "hydro_consumption", "nuclear_consumption", "oil_consumption", 
            "other_renewable_consumption", "solar_consumption", "wind_consumption"),
by = .(country, decade = round_to_decade(year))]

# pivot longer
dat_sum <- melt(dat_sum, id.vars = c("country", "decade"))

# consumption per year as perc of total
dat_sum[, perc := (value / sum(value)) * 100, by = .(country, decade)]
# remove NA
dat_sum <- na.omit(dat_sum, cols = "perc")

# Adjust to ensure sum=100 for each decade
# Replace problematic adjustment block with:
dat_sum[, perc_int := as.integer(round(perc))][,
  perc_int := {
    current_sum <- sum(perc_int)
    if (current_sum != 100) {
      diff <- 100 - current_sum
      perc_int[which.max(value)] <- perc_int[which.max(value)] + diff
    }
    perc_int
  }, by = decade][
  perc_int > 0
]

# legend of energy
energy_sources <- c(
  "Coal" = "coal_consumption",
  "Oil" = "oil_consumption",
  "Gas" = "gas_consumption",
  "Hydro" = "hydro_consumption",
  "Nuclear" = "nuclear_consumption",
  "Biofuel" = "biofuel_consumption",
  "Solar" = "solar_consumption",
  "Wind" = "wind_consumption",
  "Other Renewables" = "other_renewable_consumption"
)
energy_colors = c(
  "Coal" = "#444239FF", # very dark coal grey
  "Oil" = "#035F72FF",
  "Gas" = "#D77186FF",
  "Hydro" = "#A4B7E1FF",
  "Nuclear" = "#E69F00",
  "Biofuel" = "#B0986CFF",
  "Solar" = "#F8D564FF",
  "Wind" = "#56B4E9",
  "Other Renewables" = "#1BB6AFFF"
)
subtitle_text <- glue::glue(
  "In the 1960s, only <span style='color:{energy_colors['Coal']};'><strong>coal</strong></span>, 
    <span style='color:{energy_colors['Oil']};'><strong>oil</strong></span>, 
    <span style='color:{energy_colors['Gas']};'><strong>gas</strong></span>, and 
    <span style='color:{energy_colors['Hydro']};'><strong>hydro</strong></span> were part of the French energetic mix. By the 2020s, <span style='color:{energy_colors['Nuclear']};'><strong>nuclear</strong></span> energy grew to account for almost 40%, and other sources appeared such as 
    <span style='color:{energy_colors['Biofuel']};'><strong>biofuel</strong></span>, 
    <span style='color:{energy_colors['Solar']};'><strong>solar</strong></span>, 
    <span style='color:{energy_colors['Wind']};'><strong>wind</strong></span>, and 
    <span style='color:{energy_colors['Other Renewables']};'><strong>other renewable energy</strong></span>. While proportions help illustrate the balance between different energy sources, they can be misleading, as they hide the significant overall increase in the consumption of all energy sources."
)
# use forcats to add levels and order by energy_sources
dat_sum <- dat_sum[,
  variable := factor(
    fct_recode(variable, !!!energy_sources),
    levels = names(energy_sources)
  )
]
setorder(dat_sum, decade, variable)


energy_palette <- paletteer::paletteer_d("ggsci::category20_d3")
# plot
body_font <- "Carlito"
title_font<- "Carlito"
g <- ggplot(dat_sum, aes(fill = variable, values = perc_int)) +
  geom_waffle(
    color = "white", 
    size = 0.2, 
    n_rows = 5, 
    flip = TRUE,
    make_proportional = FALSE
  ) +
  facet_wrap(~decade, nrow = 1, strip.position = "bottom") +
  theme_minimal() + 
  labs(
    title = "France's Energy Mix Evolution (1960-2020)",
    subtitle = subtitle_text,
    caption = "Each square represents 1% of total energy consumption for each decade from 1960 to 2020. These are proportions of total terawatt hours (TWh) consumed.<br>Data: Our World in Data | Heavily inspired by Muhammad Azhar's waffle plot on R Graph Gallery | Plot: @gnoblet"
  ) +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 5,
    expand = c(0,0)) + 
  scale_fill_manual(
        values = energy_colors,
        breaks = names(energy_sources),  # Explicit legend order
        name = "Energy Source",
        drop = FALSE
      ) +
  #coord_equal() + 
  theme(
    # Grid
    panel.grid = element_blank(),
    # Axes
    axis.title = element_blank(),
    axis.text.x = element_text(family = body_font, size = 12),
    # Strip
    strip.text = element_text(family = body_font, size = 11),
    #axis.text.y = element_blank(),
    # Legend
    legend.position = "none",
    # Title
    plot.title = element_textbox_simple(
      hjust = 0,
      margin = margin(20, 0, 10, 0),
      size = 20,
      family = title_font,
      face = "bold",
      color = "grey15",
      width = unit(0.9, "npc")
    ),
    # Subtitle
    plot.subtitle = element_textbox_simple(
      hjust = 0,
      margin = margin(10, 0, 40, 0),
      width = unit(0.9, "npc"),
      size = 14,
      family = body_font,
      color = "grey15"),
    # Caption
    plot.caption = element_textbox_simple(
      family = body_font,
      face ="plain",
      size = 11, 
      color = "grey40",
      hjust = 0,
      width =unit(0.95, "npc"),
      margin = margin(10,0,0,0)
    ),
    # Background / esp for vector image
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # Add arrow
  # Text added through a grob on figure since facets prevent from annotating without
  # duplicating text or cropping
  geom_curve(
    data = data.frame(x = 3.9, y = 21, xend = 4.9, yend = 24, decade = "1970"),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = -0.3,
    color = "black",
    inherit.aes = FALSE
  )

g <- g + inset_element(
  ggplot() +
    geom_richtext(
      aes(
        x = 0.30,
        y = 0.52,
        label = "<span style='font-family:Carlito; font-size:15px;'>In 1973, nuclear power was 8% of<br>the production of French electricity.</span>"
      ),
      fill = NA,
      label.color = NA,
      vjust = 0,
      hjust = 0,
      family = "Carlito"
      ) +
      theme_void() +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE),
    left = 0, right = 1, bottom = 0, top = 1, align_to = 'full'
)

# save fig
ggsave(
"2025/day_01.png",
height = 6,
width = 9,
dpi = 600,
type = "cairo-png"
)

