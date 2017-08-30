# -x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x
# 
# Data prep
#   author: João Augusto
#   date: 29/08/2017
#   description:
#     This script will do as follows: 
#       . read the data and shape files;
#       . clear them both;
#       . set it up for the app;
# 
# -x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x

# PACKAGES ----
library(rgdal)
library(dplyr)
library(leaflet)
library(reshape2)
library(htmltools)

# DATA ----
db <- 
  read.csv2("00_data/20170829_caged2009a2015_mov_UF_COURSERA.csv") %>% 
  group_by(ano_comp, uf, adm_des) %>% 
  summarise(mov = sum(saldo_mov)) %>% 
  dcast(ano_comp + uf ~ adm_des,
        value.var = "mov") %>% 
  mutate(saldo = `1` + `2`) %>% 
  filter(!is.na(uf))
  
names(db) <- c("Year", "State", "Hires", "Separations", "Net_Hires")
db <- melt(db, id = c('Year', 'State'))
names(db) <- c("Year", "State", "mov", "value")

db$mov <- as.character(db$mov)
db$mov <- gsub("_", " ", db$mov, fixed = T)

# MAP ----
# load it
map <- readOGR("00_data/shapes", "BRUFE250GC_SIR", 
               GDAL1_integer64_policy = TRUE, 
               encoding = "utf-8", 
               use_iconv = T)
map@data$CD_GEOCUF <- as.numeric(as.character(map@data$CD_GEOCUF))

# JOIN DATA AND MAP ----
map_data = map@data

map_to_plot <- map
map_to_plot@data <- 
  left_join(map_data,
            db[which(db$Year == 2015),],
            by = c("CD_GEOCUF" = "State"))


# test the Leaflet map
labs <- 
  lapply(
    seq(nrow(map_to_plot@data)), 
    function(i) {
      paste0('<p>', map_to_plot@data[i, "NM_ESTADO"], '</p>',
             '<p>', 'Net Hires:', map_to_plot@data[i, "Net_Hires"], '</p>'
            ) 
  })
leaflet(map_to_plot) %>% 
  addPolygons(
    color = "#444444",
    weight = .8,
    smoothFactor = .5,
    fillColor = ~colorQuantile("YlOrRd", as.numeric(as.character(CD_GEOCUF)))(as.numeric(as.character(CD_GEOCUF))),
    highlightOptions = highlightOptions(color = "white", weight = 4,
                                        bringToFront = TRUE),
    label = ~lapply(labs, HTML)
  )

# SAVE ----
# save the finished db and map
saveRDS(map, "00_data/map.rds")
saveRDS(db, "00_data/db.rds")
