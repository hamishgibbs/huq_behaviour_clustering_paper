suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2) 
})

if (interactive()){
  .args <- c(
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_oac_2011_sampling_bias.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_oac_2011_uid_to_days.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_oac_2011_days_per_cluster.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_oac_2011_movement_network.csv",
    "output/oac_city_detail.rds",
    "output/oac_2011_sampling_bias.png",
    "output/oac_uid_date_per_cluster.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 2)

sampling_bias <- fread(.args[1])
oac_days_per_cluster <- fread(.args[3])

sprgrp_pal <- c(
  "Rural Residents"="#4DAF4A",
  "Cosmopolitans"="#E41A1C",
  "Ethnicity Central"="#F781BF",
  "Multicultural Metropolitans"="#FF7F00",
  "Urbanites"="#A65628",
  "Suburbanites"="#984EA3",
  "Constrained City Dwellers"="#377EB8",
  "Hard-pressed living"="#FFCB1F"
)
cluster_color_pal <- c(
  "Regular-travel"="#e60049",
  "Stay-at-home"="#0bb4ff",
  "Long-distance"="#50e991",
  "Away-from-home"="#e6d800"
)

sampling_bias[, p_POPULATION := POPULATION / sum(POPULATION)]

sampling_bias <- sampling_bias[order(-p_POPULATION)]

sampling_bias[, SPRGRP_NAME := factor(SPRGRP_NAME, levels = sampling_bias$SPRGRP_NAME)]

p_pop <- ggplot(sampling_bias) + 
  geom_bar(aes(x = SPRGRP_NAME, y = p_POPULATION,
           fill=SPRGRP_NAME), stat="identity", width=0.7) + 
  scale_fill_manual(values=sprgrp_pal) + 
  scale_y_continuous(labels = scales::percent) + 
  theme_classic() + 
  labs(y = "Population percentage",
       x = NULL,
       title="a",
       subtitle="Proportion of England & Wales population") + 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        legend.position="none")

p_sampling <- ggplot(sampling_bias) + 
  geom_bar(aes(x = SPRGRP_NAME, y=sampling_index,
               fill=SPRGRP_NAME), stat="identity", width=0.7) + 
  geom_hline(yintercept=0, linetype="dashed", color="black") + 
  scale_fill_manual(values=sprgrp_pal) + 
  scale_y_continuous(limits=c(-1, 3),
                     breaks=c(-1, -0.5, 0, 1, 2, 3),
                     labels=rev(c("4x", "3x", "2x", "National\nAverage", "-0.5x", "0x"))) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle=40, hjust=1, vjust=1),
        legend.position="none") + 
  labs(y = "Sampling index",
       x = NULL,
       title="b",
       subtitle="Over/under sampling of demographic groups")

p_pop_sampling <- cowplot::plot_grid(p_pop, p_sampling, nrow=1)

ggsave(.outputs[1],
       p_pop_sampling,
       width = 10, 
       height = 4,
       units = 'in')

cluster_reclass <- fread("output/cluster_reclass.csv")

oac_days_per_cluster[cluster_reclass, on = c("cluster"="new_cluster"), cluster_label := cluster_label]
oac_days_per_cluster[, cluster_label := factor(cluster_label, levels = rev(names(cluster_color_pal)))]
oac_days_per_cluster[, SPRGRP_NAME := factor(SPRGRP_NAME, levels = sampling_bias$SPRGRP_NAME)]

oac_days_per_cluster[, p_uid_date_perc_diff2 := (p_uid_date - p_uid_date_per_cluster) / p_uid_date_per_cluster]

p_cluster_per_oac <- ggplot(oac_days_per_cluster) + 
  geom_bar(aes(x = p_uid_date_perc_diff2, y = cluster_label,
               fill=cluster_label), stat="identity", width=0.6) + 
  geom_vline(xintercept=0, linetype='dashed') + 
  facet_wrap(~SPRGRP_NAME, ncol=4, nrow=2) + 
  scale_fill_manual(values=cluster_color_pal) + 
  theme_classic() + 
  labs(y = NULL, x = "Travel days per cluster by demographic group (% difference)") + 
  theme(legend.position="none") + 
  scale_x_continuous(breaks=c(-0.5, 0, 0.5, 1, 1.5),
                     labels = c("-50%", "Overall", "150%", "200%", "250%")) + 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        strip.background = element_rect(size=0))

ggsave(.outputs[2],
       p_cluster_per_oac,
       width = 10, 
       height = 5,
       units = 'in')


