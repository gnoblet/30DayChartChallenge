## Load Packages ----
library(terra)
library(tidyverse)
library(ggspatial)
library(rnaturalearth)
library(RColorBrewer)

## 1. Data Preparation ----
# Load population rasters
pop2000 <- rast("data/che_ppp_2000.tif")
pop2020 <- rast("data/che_ppp_2020.tif")

# Get Switzerland boundaries
ch <- ne_countries(country = "Switzerland", returnclass = "sf") |>
  st_transform(crs = crs(pop2000))

# Mask and crop rasters to Switzerland
pop2000_masked <- mask(crop(pop2000, ch), ch)
pop2020_masked <- mask(crop(pop2020, ch), ch)

## 2. Calculate Changes ----
# Absolute change
absolute_change <- pop2020_masked - pop2000_masked

# Relative change (%)
relative_change <- ((pop2020_masked - pop2000_masked) / pop2020_masked) * 100

## 3. Prepare for Visualization ----
# Convert to data frame for ggplot
change_df <- rast(list(absolute_change, relative_change)) |>
  as.data.frame(xy = TRUE) |>
  setNames(c("x", "y", "Absolute", "Relative")) |>
  pivot_longer(cols = -c(x, y), names_to = "ChangeType", values_to = "Value")

# as sf
change_df <- st_as_sf(change_df, coords = c("x", "y"), crs = crs(pop2000))

# Create classification breaks (Jenks natural breaks)
library(classInt)
breaks <- classIntervals(na.omit(change_df$Value), n = 7, style = "jenks")$brks

## 4. Create Map ----
ggplot() +
  geom_tile(data = change_df, aes(x, y, fill = cut(Value, breaks))) +
  geom_sf(data = ch, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_manual(
    values = rev(brewer.pal(7, "RdBu")),
    labels = c("<-50%", "-50:-25", "-25:0", "0:25", "25:50", "50:100", ">100%"),
    na.value = "transparent"
  ) +
  facet_wrap(
    ~ChangeType,
    labeller = labeller(
      ChangeType = c(
        "Absolute" = "Absolute Change (people)",
        "Relative" = "Relative Change (%)"
      )
    )
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr") +
  labs(
    title = "Switzerland Population Change 2000-2020",
    subtitle = "Data source: WorldPop (www.worldpop.org) and CIESIN",
    fill = "Change Category"
  ) +
  theme_void() +
  theme(legend.position = "bottom")


library(tmap)
# Set tmap mode to plotting
tmap_mode("plot")

# Create the map
tm_shape(relative_change) +
  tm_raster(
    col.scale = tm_scale_intervals(
      values = "RdBu", # diverging palette
      style = "jenks", # natural breaks
      n = 7,
      midpoint = 0 # center color scale at zero change
    ),
    col.legend = tm_legend(
      title = "Population Change (2000–2020)",
      size = 0.8
    )
  ) +
  tm_title("Switzerland: Population Change 2000–2020\nData: WorldPop & CIESIN")

tm_shape(change_df)
