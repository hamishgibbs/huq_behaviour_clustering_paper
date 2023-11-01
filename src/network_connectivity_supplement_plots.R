suppressPackageStartupMessages({
  library(data.table)
  library(igraph)
  library(ggplot2) 
  library(ggraph)
})

if (interactive()){
  .args <- c(
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_connectivity_edge_counts.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_connectivity_degree_distribution.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_connectivity_dist_km_distribution.csv",
    "output/h3_resolution_n_edges_senstivity.png",
    "output/h3_resolution_metric_senstivity.png",
    "output/h3_resolution_metric_senstivity_relative.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 3)

edge_counts <- fread(.args[1])

cluster_labels_ordered <- c("Regular-travel", "Stay-at-home", "Long-distance", "Away-from-home")

cluster_color_pal <- c(
  "Overall"="black",
  "Without Regular-travel"="#e60049",
  "Without Stay-at-home"="#0bb4ff",
  "Without Long-distance"="#50e991",
  "Without Away-from-home"="#e6d800"
)

network_metrics <- edge_counts[, c(
  "h3_resolution", 
  "cluster_removed", 
  "n_edges",
  "density",
  "mean_shortest_path_distance_km", 
  "mean_shortest_path_distance_topological",
  "diameter",
  "transitivity")]

network_metrics_long <- melt(network_metrics, id.vars = c("h3_resolution", "cluster_removed"))

cluster_reclass <- fread("output/cluster_reclass.csv")

network_metrics_long[, cluster_removed := as.integer(cluster_removed)]
network_metrics_long[cluster_reclass, on = c("cluster_removed"="new_cluster"), cluster_label := cluster_label]
network_metrics_long[, label := paste0("Without ", cluster_label, "")]
network_metrics_long[, label := ifelse(label == "Without NA", "Overall", label)]
network_metrics_long[, label := factor(label, 
                                      levels = rev(c("Overall", paste0("Without ", cluster_labels_ordered))))]
network_metrics_long[, h3_resolution := factor(h3_resolution, levels=c(6:3))]

n_edges <- subset(network_metrics_long, variable == "n_edges" & label == "Overall")

h3_resolution_mean_area_km2 <- c(18.23, 127.78, 896.58, 6315.47)
names(h3_resolution_mean_area_km2) <- c(6, 5, 4, 3)

p_n_edges <- ggplot(n_edges) + 
  geom_path(aes(x = h3_resolution, y = value, color=label, group=label)) +
  scale_color_manual(values=cluster_color_pal) + 
  theme_classic() + 
  labs(x = parse(text="Average~Cell~Area~(km^2)"),
       y = "Number of edges",
       title="Number of edges at different spatial resolutions") + 
  scale_x_discrete(breaks = names(h3_resolution_mean_area_km2), labels = h3_resolution_mean_area_km2) + 
  scale_y_continuous(labels = scales::comma) + 
  theme(legend.position = "none",
        plot.title = element_text(face="bold"))

ggsave(.outputs[1],
       p_n_edges,
       width = 10, 
       height = 6,
       units = 'in')

other_network_metrics <- subset(network_metrics_long, variable != "n_edges")

baseline_value <- subset(other_network_metrics, h3_resolution == 6)
baseline_value[, value_baseline := value]
baseline_value[, value := NULL]

other_network_metrics[baseline_value, on=c("variable", "label"), value_baseline := value_baseline]
other_network_metrics[, change_from_baseline := value / value_baseline]

other_network_metrics[, variable := factor(variable, levels=c(
  "density",
  "transitivity",
  "mean_shortest_path_distance_km",
  "mean_shortest_path_distance_topological",
  "diameter"),
  labels = c(
    "Density",
    "Transitivity (Clustering Coefficient)",
    "Mean Shortest Path Distance (km)",
    "Mean Shortest Path Distance (topological)",
    "Diameter (km)"
  ))]

p_absolute <- ggplot(other_network_metrics) + 
  geom_path(aes(x = h3_resolution, y = value, color=label, group=label)) + 
  facet_wrap(~variable, scales="free_y") + 
  scale_color_manual(values=cluster_color_pal) + 
  theme_classic() + 
  scale_x_discrete(breaks = names(h3_resolution_mean_area_km2), labels = h3_resolution_mean_area_km2) + 
  labs(y="Absolute value at different resolutions",
       x=parse(text="Average~Cell~Area~(km^2)"),
       color=NULL)

ggsave(.outputs[2],
       p_absolute,
       width = 12, 
       height = 7,
       units = 'in')

p_relative <- ggplot(other_network_metrics) + 
  geom_path(aes(x = h3_resolution, y = change_from_baseline, color=label, group=label)) + 
  facet_wrap(~variable, scales="free_y") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values=cluster_color_pal) + 
  theme_classic() + 
  scale_x_discrete(breaks = names(h3_resolution_mean_area_km2), labels = h3_resolution_mean_area_km2) + 
  labs(y=parse(text="Relative~change~from~18~km^2~resolution"),
       x=parse(text="Average~Cell~Area~(km^2)"),
       color=NULL)

ggsave(.outputs[3],
       p_relative,
       width = 12, 
       height = 7,
       units = 'in')

