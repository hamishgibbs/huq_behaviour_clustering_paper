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
    "output/example_network_types.png",
    "output/example_networks_sigma.csv",
    "output/network_metrics.png",
    "output/connectivity_degree_distribution.png",
    "output/connectivity_edge_distance_distribution.png",
    "output/network_metrics_table.csv",
    "output/connectivity_distributions.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

cluster_labels_ordered <- c("regular-travel", "stay-at-home", "long-distance", "away-from-home")

.outputs <- tail(.args, 7)

connectivity_values <- fread(.args[1])

cluster_color_pal <- c(
  "Overall"="black",
  "Without regular-travel"="#e60049",
  "Without stay-at-home"="#0bb4ff",
  "Without long-distance"="#50e991",
  "Without away-from-home"="#e6d800"
)
cluster_fill_pal <- c(
  "Without regular-travel"="#e60049",
  "Without stay-at-home"="#0bb4ff",
  "Without long-distance"="#50e991",
  "Without away-from-home"="#e6d800"
)
cluster_linetype_pal <- c(
  "Overall"="dotted",
  "Without regular-travel"="solid",
  "Without stay-at-home"="solid",
  "Without long-distance"="solid",
  "Without away-from-home"="solid"
)

set.seed(1)

# Regular network 
n <- 100  # Number of nodes
k <- 4    # Each node is connected to k nearest neighbors in ring topology
p <- 0.1  # Rewiring probability

# 1. Create and plot a regular ring lattice
ring_graph <- make_lattice(dimvector = c(10, 10), circular=T)

regular_random_graph <-sample_k_regular(n, k, directed = FALSE, multiple = FALSE)

small_world_graph <- watts.strogatz.game(1, n, k, p)

# Calculate small worldness index of each 

graph_plots <- list()
graphs <- list(ring_graph=list(graph=ring_graph, title="Regular Ring"),
               regular_random_graph=list(graph=regular_random_graph, title="Random"),
               small_world_graph=list(graph=small_world_graph, title="Small World"))
sigma <- list()

for (i in 1:3){
  graph_plots[[i]] <- ggraph(graphs[[i]]$graph, layout = "circle") +
    geom_edge_link(width=0.2) +
    geom_node_point(size = 0.4) +
    theme_void() +
    #labs(title = graphs[[i]]$title) + 
    theme(plot.title = element_text(hjust=0.5))
  
  random_network_mean_transitivity <- list()
  random_network_mean_shortest_path_length <- list()
  
  for (ii in 1:100){
    g_random <- sample_degseq(degree(graphs[[i]]$graph), method='vl')
    random_network_mean_transitivity[[ii]] <- transitivity(g_random)
    random_network_mean_shortest_path_length[[ii]] <- mean_distance(g_random)    
  }
  
  sigma[[i]] <- (
    transitivity(graphs[[i]]$graph) / mean(unlist(random_network_mean_transitivity))
  ) / (
    mean_distance(graphs[[i]]$graph) / mean(unlist(random_network_mean_shortest_path_length))
  )
}

p_net <- cowplot::plot_grid(plotlist = graph_plots,
                   nrow=1)

ggsave(.outputs[1],
       p_net,
       width=10,
       height=3.33,
       units="in")

sigma

fwrite(data.table(sigma = unlist(sigma)),
       .outputs[2])

small_world_metrics <- c("sigma", "transitivity", "mean_shortest_path_distance_topological")

connectivity_values_long <- subset(subset(melt(connectivity_values, id.vars = c("h3_resolution", "cluster_removed")), 
       cluster_removed != 'none' & h3_resolution == 6),
       variable %in% small_world_metrics)

connectivity_values_long[, variable := factor(variable, levels=small_world_metrics,
                                              labels = c('Small-worldness~Index~(σ)', 
                                                         'Clustering~Coefficient~(G[g]^Δ)', 
                                                         'Mean~Shortest~Path~Length~(L[g])'))]

cluster_reclass <- fread("output/cluster_reclass.csv")

connectivity_values_long[, cluster_removed := as.integer(cluster_removed)]
connectivity_values_long[cluster_reclass, on = c("cluster_removed"="new_cluster"), cluster_label := cluster_label]

connectivity_values_long[, label := paste0("Without ", stringr::str_to_lower(cluster_label), "")]

connectivity_values_long[, label := factor(label, levels=paste0("Without ", cluster_labels_ordered))]

p_network_metrics <- ggplot(data = subset(connectivity_values_long, variable=='Small-worldness~Index~(σ)')) + 
  geom_bar(aes(y = value, x=label, fill=label), 
           stat='identity', position='dodge',
           width=0.6) + 
  theme_classic() + 
  theme(strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) + 
  labs(x = parse(text='Small-worldness~Index~(σ)'),
       y = NULL,
       fill=NULL) + 
  scale_fill_manual(values=cluster_fill_pal)

ggsave(.outputs[3],
       p_network_metrics,
       width=6,
       height=2.5,
       units="in")

degree_distribution <- fread(.args[2])

degree_distribution <- subset(degree_distribution, h3_resolution == 6)

degree_distribution[, cluster_removed := as.integer(cluster_removed)]
degree_distribution[cluster_reclass, on = c("cluster_removed"="new_cluster"), cluster_label := cluster_label]

degree_distribution[, label := paste0("Without ", stringr::str_to_lower(cluster_label), "")]

degree_distribution[, label := ifelse(label == "Without NA", "Overall", label)]

degree_distribution[, label := factor(label, 
                                      levels = rev(c("Overall", paste0("Without ", cluster_labels_ordered))))]

degree_distribution <- subset(degree_distribution, !is.na(p))

p_degree <- ggplot(data=degree_distribution) + 
  geom_path(aes(x = bin_numeric, y = p, color=label,
                linetype=label)) + 
  scale_y_continuous(trans="log10") + 
  scale_color_manual(values=cluster_color_pal) + 
  scale_linetype_manual(values=cluster_linetype_pal,
                        guide='none') + 
  labs(y = "Proportion of nodes",
       x = "Degree",
       color=NULL,
       linetype=NULL) + 
  theme_classic() + 
  theme(legend.position = "none",
        text = element_text(size=14))

ggsave(.outputs[4],
       p_degree,
       width = 5, 
       height = 4,
       units = 'in')

cluster_labels_ordered <- c("regular-travel", "stay-at-home", "long-distance", "away-from-home")
cluster_reclass[, cluster_label := factor(cluster_labels_ordered, levels = cluster_labels_ordered)]

distance_distribution <- fread(.args[3])

distance_distribution <- subset(distance_distribution, h3_resolution == 6)

distance_distribution[, cluster_removed := as.integer(cluster_removed)]
distance_distribution[cluster_reclass, on = c("cluster_removed"="new_cluster"), cluster_label := cluster_label]

distance_distribution[, label := paste0("Without ", cluster_label, "")]

distance_distribution[, label := ifelse(label == "Without NA", "Overall", label)]

distance_distribution[, p_cum := cumsum(n/sum(n, na.rm=T)), by = c("label")]

distance_distribution[, label := factor(label, 
                                      levels = rev(c("Overall", paste0("Without ", cluster_labels_ordered))))]

p_edge_distance <- ggplot(data=distance_distribution) + 
  geom_path(aes(x = bin_numeric, y = p_cum, color=label,
                linetype=label)) + 
  scale_x_continuous(trans='log10') + 
  theme_classic() + 
  scale_color_manual(values=cluster_color_pal) + 
  scale_linetype_manual(values=cluster_linetype_pal,
                        guide='none') + 
  labs(color=NULL,
       y = "Proportion of edges (cumulative)",
       x = "Edge distance (km)") + 
  theme(legend.position = "none",
        text = element_text(size=14))

ggsave(.outputs[5],
       p_edge_distance,
       width = 5, 
       height = 4,
       units = 'in')

connectivity_values[, cluster_removed := as.integer(cluster_removed)]
connectivity_values[cluster_reclass, on = c("cluster_removed"="new_cluster"), cluster_label := cluster_label]
connectivity_values[cluster_reclass, on = c("cluster_removed"="new_cluster"), old_cluster := old_cluster]

fwrite(connectivity_values[, .(cluster_label, 
                        transitivity,
                        mean_shortest_path_distance_km, 
                        mean_shortest_path_distance_topological, 
                        diameter)],
       .outputs[6])


p <- cowplot::plot_grid(
  p_degree + labs(title="a") + theme(legend.position = c(0.7, 0.9)),
  p_edge_distance + labs(title="b"),
  nrow=1
)

ggsave(.outputs[7],
       p,
       width = 10, 
       height = 5,
       units = 'in')
