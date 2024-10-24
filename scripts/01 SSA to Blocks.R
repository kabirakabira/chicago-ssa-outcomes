#! Outline
## Read in dependencies

#! Setting up workspace
## Modifying options
options(scipen = 999)
options(timeout = 180)
## Installing and loading required packages
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("dplyr")
usePackage("tigris")
usePackage("sf")
usePackage("ggplot2")

#! Read in dependencies
chicago.boundary <- st_read("data/raw/City_Boundary_20241006/geo_export_4f794bb9-7923-429b-9b25-5b9434349a2a.shp")
chicago.ssa <- st_read("data/raw/SSA_20241006/geo_export_d45a3c2e-e79e-4fef-b651-14d52f5d1959.shp")

#! Clean shapefile of SSAs
chicago.ssa <- chicago.ssa %>%
  mutate(ID = seq(1, nrow(chicago.ssa), 1)) %>%
  select(ID, name, status, geometry) %>%
  `colnames<-`(c("ID","Name","Status","geometry"))

#! Download Census block boundaries
chicago.blocks <- tigris::blocks(
  state = "IL",
  county = "Cook County",
  year = 2021
) %>%
  select(GEOID20, geometry)

## Intersect with city boundary
chicago.blocks <- chicago.blocks %>%
  st_transform(crs = st_crs(chicago.boundary)) %>%
  st_intersection(chicago.boundary) %>%
  select(GEOID20, geometry)

## Intersect with SSA boundaries
sf_use_s2(FALSE)
chicago.ssa.blocks <- chicago.blocks %>%
  mutate(orig_area = st_area(chicago.blocks))
chicago.ssa.blocks <- chicago.ssa.blocks %>%
  st_intersection(chicago.ssa)
chicago.ssa.blocks <- chicago.ssa.blocks %>%
  mutate(new_area = st_area(chicago.ssa.blocks))
chicago.ssa.blocks <- chicago.ssa.blocks %>%
  mutate(proportion_area = new_area / orig_area) %>%
  select(GEOID20, Name, proportion_area, geometry)
### filter out intersections that are below 50% of original area
chicago.ssa.blocks <- chicago.ssa.blocks %>%
  filter(as.numeric(proportion_area) >= 0.5)

