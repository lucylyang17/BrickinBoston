# Map of brick buildings in Boston
# Data: FY2026 property assessment + 2025 parcel polygons (WKT in WGS84 lon/lat)
# install.packages(c("tidyverse", "sf"))

library(tidyverse)
library(sf)

# 1. Read assessing data ------------------------------------------------------
# Read everything as text so IDs keep their leading zeros ("0100001000").
assess <- read_csv(
  "G:/My Drive/MIT/iLab/BrickBoston/fy2026-property-assessment-data_rev.csv",
  col_types = cols(.default = col_character())
)



# 2. Flag brick ----------------------------------------------------------------
# EXT_FNISHED mixes two coding schemes ("B - Brick/Stone" and "01 - Brick", etc.)
# Adjust this list to taste (e.g., add "02" for stone, or drop "V" for veneer).
brick_codes <- c("B", "01", "V")

assess <- assess |>
  mutate(
    ext_code = str_extract(EXT_FNISHED, "^[^ ]+"),   # text before the first space
    is_brick = ext_code %in% brick_codes
  )

# 3. Collapse to one row per parcel --------------------------------------------
# Condo buildings have one row per unit, all sharing the same GIS_ID, so
# aggregate to the parcel/building level before joining to geometry.
parcel_brick <- assess |>
  filter(!is.na(ext_code)) |>
  group_by(GIS_ID) |>
  summarise(
    n_records   = n(),
    brick_share = mean(is_brick),
    LU          = first(LU),
    YR_BUILT    = first(YR_BUILT),
    .groups = "drop"
  ) |>
  mutate(brick = brick_share >= 0.5)

# 4. Read parcel geometry -------------------------------------------------------
parcels <- read_csv(
  "G:/My Drive/MIT/iLab/BrickBoston/parcels_2025.csv",
  col_types = cols(.default = col_character())
) |>
  filter(POLY_TYPE == "FEE", !is.na(shape_wkt))   # ~11% of rows have no geometry

parcels_sf <- st_as_sf(parcels, wkt = "shape_wkt", crs = 4326)

# 5. Join: GIS_ID (assessing) == MAP_PAR_ID (parcels) ---------------------------
brick_map <- parcels_sf |>
  inner_join(parcel_brick, by = c("MAP_PAR_ID" = "GIS_ID"))

cat("Parcels mapped:", nrow(brick_map), "\n")
cat("Share brick:", round(mean(brick_map$brick), 3), "\n")

# 6. Plot -------------------------------------------------------------------------
p <- ggplot(brick_map) +
  geom_sf(aes(fill = brick), color = NA) +
  scale_fill_manual(
    values = c(`TRUE` = "#a8321f", `FALSE` = "#d9d9d9"),
    labels = c(`TRUE` = "Brick", `FALSE` = "Other"),
    name = NULL
  ) +
  labs(title = "Brick buildings in Boston",
       caption = "Source: City of Boston FY2026 assessing data; 2025 parcels") +
  theme_void() +
  theme(legend.position = "bottom")

ggsave("boston_brick_map.png", p, width = 10, height = 10, dpi = 300, bg = "white")

# Optional: save for QGIS / ArcGIS or an interactive map
# st_write(brick_map, "boston_brick.gpkg", delete_dsn = TRUE)
# Interactive: mapview::mapview(brick_map, zcol = "brick")


# =============================================================================
# 7. Version with a base map + neighborhood outlines
# =============================================================================
# install.packages(c("ggspatial", "rosm", "prettymapr"))
library(ggspatial)

# Download the "Boston Neighborhoods"  shapefile from data.boston.gov
# (search "Boston Neighborhoods"), and put it in your working directory.
hoods <- st_read("G:/My Drive/MIT/iLab/BrickBoston/Boston_Neighborhood_Boundaries.shp") |>
  st_transform(4326) |>
  st_make_valid()

names(hoods)   # check which column holds the neighborhood name; assumed "name" below

hood_labels <- hoods |>
  mutate(geometry = st_point_on_surface(geometry))

bb <- st_bbox(hoods)


# Transform everything to Web Mercator (same system as the map tiles)
hoods_m  <- st_transform(hoods, 3857)
brick_m  <- st_transform(brick_map, 3857)
labels_m <- st_point_on_surface(hoods_m)   # label points, already in 3857
bb <- st_bbox(hoods_m)

p2 <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 13, progress = "none") +
  geom_sf(data = brick_m, aes(fill = brick), color = NA, alpha = 0.9,
          inherit.aes = FALSE) +
  geom_sf(data = hoods_m, fill = NA, color = "grey20", linewidth = 0.4,
          inherit.aes = FALSE) +
  geom_sf_text(data = labels_m, aes(label = name), size = 2.5,
               fontface = "bold", check_overlap = TRUE, inherit.aes = FALSE) +
  scale_fill_manual(
    values = c(`TRUE` = "#b5301c", `FALSE` = "#6b7b8c"),
    labels = c(`TRUE` = "Brick", `FALSE` = "Other"),
    name = NULL
  ) +
  coord_sf(
    crs    = 3857,
    xlim   = c(bb[["xmin"]], bb[["xmax"]]),
    ylim   = c(bb[["ymin"]], bb[["ymax"]]),
    expand = FALSE
  ) +
  labs(title = "Brick buildings in Boston",
       caption = "Parcels: City of Boston FY2026 assessing. Base map: © OpenStreetMap contributors") +
  theme_void() +
  theme(legend.position = "bottom")


ggsave("boston_brick_map_basemap.png", p2, width = 10, height = 10, dpi = 300, bg = "white")

# Transform everything to Massachusetts State Plane (meters)
hoods_p  <- st_transform(hoods, 26986)
brick_p  <- st_transform(brick_map, 26986)
labels_p <- st_point_on_surface(hoods_p)
bb <- st_bbox(hoods_p)

p3 <- ggplot() +
  # Parcels colored by brick
  geom_sf(data = brick_p, aes(fill = brick), color = NA) +
  # Neighborhood outlines on top
  geom_sf(data = hoods_p, fill = NA, color = "grey20", linewidth = 0.4) +
  geom_sf_text(data = labels_p, aes(label = name), size = 2.5,
               fontface = "bold", check_overlap = TRUE) +
  scale_fill_manual(
    values = c(`TRUE` = "#b5301c", `FALSE` = "#d0d0d0"),
    labels = c(`TRUE` = "Brick", `FALSE` = "Other"),
    name = NULL
  ) +
  coord_sf(
    xlim   = c(bb[["xmin"]], bb[["xmax"]]),
    ylim   = c(bb[["ymin"]], bb[["ymax"]]),
    expand = FALSE
  ) +
  labs(title = "Brick buildings in Boston",
       caption = "Parcels: City of Boston FY2026 assessing and 2025 parcels") +
  theme_void() +
  theme(legend.position = "bottom")

ggsave("boston_brick_map_outlines.png", p3, width = 10, height = 10, dpi = 300, bg = "white")