# Load required packages
library(owidapi)
library(stringr)
library(data.table)
library(ggplot2)
library(ggtext)
library(paletteer)
library(scales)

# get data for all production categories
catalog <- owid_get_catalog()
ap_search <- owid_search(catalog, "agricultural-production")
setDT(ap_search)
ids <- ap_search[
    type == "LineChart" &
        str_detect(slug, "-production$") &
        !str_detect(slug, "value"),
    slug
]

l_dat <- list()
for (id in ids) {
    l_dat[[id]] <- owid_get(chart_id = id)
    # fourth column rename to 'tonnes'
    colnames(l_dat[[id]])[4] <- "tonnes"
    setDT(l_dat[[id]])
    names(l_dat)[length(l_dat)] <- id
}
dat <- rbindlist(l_dat, idcol = "production")


# sum group by entity_name and year
dat_sum <- dat[,
    lapply(.SD, function(x) sum(x, na.rm = TRUE)),
    .SDcols = c("tonnes"),
    by = .(production, year)
]

# keep only wheat rice maize and wheat
dat_sum <- dat_sum[
    production %in%
        c(
            'wheat-production',
            'rice-production',
            'maize-production'
        )
]

maxmin <- range(dat_sum$tonnes, na.rm = T)
m <- mean(dat_sum$tonnes, na.rm = T)

# plot geom_til y = 1
ggplot(dat_sum, aes(x = year, y = 1, fill = tonnes)) +
    geom_tile() +
    facet_wrap(~production, nrow = 31) +
    # log color scale
    scale_fill_gradientn(
        colors = rev(RColorBrewer::brewer.pal(11, "RdBu")),
        values = scales::rescale(c(maxmin[1], md, maxmin[2]))
    ) +
    theme_minimal() +
    theme(
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = 'top',
        legend.key.width = unit(5, "lines"),
    )


ggplot(dat_sum, aes(x = year, y = 1, fill = tonnes)) +
    geom_tile() +
    facet_wrap(
        ~production,
        nrow = 3,
        labeller = labeller(
            production = c(
                "rice-production" = "Rice",
                "wheat-production" = "Wheat",
                "maize-production" = "Maize"
            )
        )
    ) +
    scale_fill_paletteer_c(
        "scico::bam",
        direction = -1,
        labels = label_number(
            scale_cut = cut_short_scale()
        )
    ) +
    labs(
        title = "<span style='color:#333333; font-size:19pt; font-weight:700'>Global Yearly Production of Rice, Wheat, and Maize</span><br><br><span style='color:#333333; font-size:19pt'>From 1950 to 2020 in Billion of Tonnes",
        caption = "Data: Our World in Data | Viz: @gnoblet"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(
            size = 13,
            color = "gray20"
        ),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.text = element_text(
            size = 13,
            color = "gray20"
        ),
        legend.key.width = unit(5, "lines"),
        plot.title = element_textbox_simple(
            size = 13,
            hjust = 0.5,
            halign = 0.5,
            width = 0.8,
            padding = margin(b = 30, t = 20, l = 10, r = 10),
        ),
        plot.caption = element_text(
            hjust = 0,
            size = 12,
            color = "gray20",
            margin = margin(t = 30)
        ),
        strip.text = element_text(
            hjust = 0.1,
            size = 15,
            color = "gray20",
            margin = margin(r = 15)
        ),
        strip.placement = "outside",
        plot.background = element_rect(
            fill = "white",
            color = NA
        ),
        panel.background = element_rect(
            fill = "white",
            color = NA
        )
    )


ggsave(
    "2025/day_11.png",
    dpi = 600,
    width = 9,
    height = 10,
    units = "in"
)
