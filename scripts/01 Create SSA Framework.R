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
usePackage("stringi")

#! Read in dependencies
chicago.ssas <- st_read("dependencies/SSA_20241006/geo_export_d45a3c2e-e79e-4fef-b651-14d52f5d1959.shp")
ssas.info <- read.csv("dependencies/SSA_Terms.csv")

#! Clean shapefile
chicago.ssas <- chicago.ssas %>%
  select(ref_no, name, status, geometry) %>%
  mutate(ID = as.numeric(stri_extract_first_regex(ref_no, "[0-9]+")),
         ref_no = gsub(" ", "", ref_no))

#! Clean SSA info
ssas.info <- ssas.info %>%
  mutate(TAXYRS = trimws(TAXYRS)) %>%
  filter(TAXYRS != "") %>%
  mutate(ID = as.numeric(stri_extract_first_regex(REF_NO, "[0-9]+")),
         REF_NO = gsub(" ", "", REF_NO))

#! Join SSA info to SSA shapefile
chicago.ssas <- chicago.ssas %>%
  left_join(select(ssas.info, ID, START_YR, END_YR),
            by = "ID")

#! Create SSA Dataframe for joining variables
ssas.dataframe <- chicago.ssas %>%
  st_drop_geometry() %>%
  mutate(YR_GAP = END_YR - START_YR + 1)

ssas.dataframe <- as.data.frame(lapply(ssas.dataframe, rep, ssas.dataframe$YR_GAP))
  
ssas.dataframe <- ssas.dataframe %>%
  group_by(ref_no) %>%
  mutate(repcount = row_number() - 1) %>%
  mutate(YEAR = END_YR - repcount) %>%
  select(ID, ref_no, name, YEAR) %>%
  left_join(select(chicago.ssas, ID, geometry),
            by = "ID")

#! Export SSA file
st_write(ssas.dataframe,
         "data/raw/SSAs.shp")
