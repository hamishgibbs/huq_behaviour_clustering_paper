suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(ggplot2) 
})

if (interactive()){
  .args <- c(
    "data/OutputAreaClassification/2011_OAC.shp",
    "data/penportraits.csv",
    "output/oac_city_detail.png",
    "output/oac_city_detail.rds",
    "output/oac_categories.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 3)

oac <- st_read(.args[1])

pen_portraits <- readr::read_tsv(.args[2])

oac <- st_simplify(oac, preserveTopology = T, dTolerance = 10)

sprgrp_pal <- c(
  "Rural Residents"="#4DAF4A",
  "Cosmopolitans"="#E41A1C",
  "Ethnicity Central"="#F781BF",
  "Multicultural Metropolitans"="#FF7F00",
  "Urbanites"="#A65628",
  "Suburbanites"="#984EA3",
  "Constrained City Dwellers"="#377EB8",
  "Hard-Pressed Living"="#FFCB1F"
)

oac$SPRGRP <- as.character(oac$SPRGRP)
oac <- oac %>% dplyr::left_join(pen_portraits %>% dplyr::filter(type=="Supergroup") %>% dplyr::select(code, name), by=c("SPRGRP"="code"))

oac_london <- oac %>% dplyr::filter(REGION == "London")

p_london <- ggplot(data=oac_london) + 
  geom_sf(aes(fill = name), size=0) + 
  scale_fill_manual(values = sprgrp_pal, guide = guide_legend(nrow = 4)) + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(fill=NULL)

ggsave(.outputs[1],
       p_london,
       width = 5, 
       height = 6,
       units = 'in')  

readr::write_rds(p_london, .outputs[2])

oac$country <- substr(oac$OA_SA, 1, 1)

oac <- oac %>% dplyr::filter(country %in% c("E", "W"))

plots_list <- list()

for (i in 1:length(sprgrp_pal)){
  
  oac_sprgrp <- oac %>% dplyr::filter(name == names(sprgrp_pal)[i])
  
  plots_list[[i]] <- ggplot(oac_sprgrp) + 
    geom_sf(data=st_transform(ggutils::basemap(), 27700), fill="#EFEFEF", color="black", size=0.1) + 
    geom_sf(aes(fill = name), size=0) +
    scale_fill_manual(values = sprgrp_pal) + 
    theme_void() + 
    theme(legend.position = "none") + 
    labs(title=letters[i],
         subtitle=names(sprgrp_pal)[i]) + 
    ggutils::geo_lims(oac)
  
}

p <- cowplot::plot_grid(plotlist = plots_list,
                   nrow=3, ncol=3)

ggsave(.outputs[3],
       p,
       width = 9, 
       height = 12,
       units = 'in')  
