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

# for maize production only
dat <- dat[production == 'maize-production']

# remove non country
entity_to_rm <- c(
    'World',
    'Americas (FAO)',
    'High-income countries',
    'North America',
    'Upper-middle-income countries',
    'Asia',
    'Eastern Asia (FAO)',
    'China (FAO)',
    'Europe (FAO)',
    'South America (FAO)',
    'South America',
    'Lower-middle-income countries',
    'European Union (27) (FAO)',
    'European Union (27)',
    'Africa (FAO)',
    'Africa',
    'Eastern Europe (FAO)',
    'Net Food Importing Developing Countries (FAO)',
    'Low Income Food Deficit Countries (FAO)',
    'Least Developed Countries (FAO)',
    'Southern Europe (FAO)',
    'Net Food Importing Developing Countries (FAO)',
    'Low Income Food Deficit Countries (FAO)',
    'Least Developed Countries (FAO)',
    'Southern Europe (FAO)',
    'South-eastern Asia (FAO)',
    'Central America (FAO)',
    'Southern Asia (FAO)',
    'Land Locked Developing Countries (FAO)',
    'Eastern Africa (FAO)',
    'Western Europe (FAO)',
    'Low-income countries',
    'Southern Africa (FAO)',
    'Western Africa (FAO)',
    'Western Asia (FAO)',
    'Middle Africa (FAO)',
    'Northern Africa (FAO)',
    'Small Island Developing States (FAO)',
    'Caribbean (FAO)',
    'Oceania (FAO)',
    'Central Asia (FAO)',
    'Northern Europe (FAO)',
    'Belgium-Luxembourg (FAO)',
    'Asia (FAO)',
    'Northern America (FAO)',
    'Europe',
    'Micronesia (FAO)'
)
dat <- dat[!entity_name %in% entity_to_rm]

# keep only data every 10 years
dat <- dat[year %% 10 == 0]

# dat top 10 countries summarise across years
dat_top10 <- dat[,
    .(tonnes = sum(tonnes, na.rm = TRUE)),
    by = .(entity_name)
][order(-tonnes)][1:10]
entity_top10 <- dat[,
    .(tonnes = sum(tonnes, na.rm = TRUE)),
    by = .(entity_name)
][order(-tonnes)][1:10][, entity_name]
dat_top10 <- dat[entity_name %in% entity_top10]

# plot as sankey
library(ggsankey)
library(ggplot2)
ggplot(
    dat_top10,
    aes(
        x = year,
        y = tonnes,
        node = entity_name,
        fill = entity_name,
        value = tonnes,
        label = entity_name
    )
) +
    geom_sankey_bump(
        space = 1e6,
        alpha = 0.7,
        type = 'alluvial'
    ) +
    scale_fill_paletteer_d('nord', direction = -1)
