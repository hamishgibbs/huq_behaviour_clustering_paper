suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2) 
})

if (interactive()){
  .args <- c(
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_clustering_vars_std.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_clustering_distance_distribution.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_total_distance_distribution_censored.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_clustering_min_dist_from_home_distribution.csv",
    "output/cluster_diagnostics.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

dv_metrics_std <- fread(.args[1])
cluster_total_distance_distribution <- fread(.args[2])
overall_total_distance_distribution <- fread(.args[3])
cluster_min_dist_from_home_distribution <- fread(.args[4])
cluster_visit_distribution <- fread(.args[2])

cluster_color_pal <- c(
  "All Data"="black",
  "Regular-travel"="#e60049",
  "Stay-at-home"="#0bb4ff",
  "Long-distance"="#50e991",
  "Away-from-home"="#e6d800"
)
cluster_linetype_pal <- c(
  "All Data"="dotted",
  "Regular-travel"="solid",
  "Stay-at-home"="solid",
  "Long-distance"="solid",
  "Away-from-home"="solid"
)

variable_levels <- c(
  "Total Distance"="total_distance",
  "Radius of Gyration"="rg",
  "Max. Dist. from Home"="max_dist_from_home",
  "Avg. Dist. from Home"="mean_dist_from_home",
  "Min. Dist. from Home"="min_dist_from_home",
  "Max. Freq. (Normalized)"="max_freq_norm",
  "Avg. Freq. (Normalized)"="mean_freq_norm",
  "Min Freq. (Normalized)"="min_freq_norm",
  "Entropy (Normalized)"="entropy_norm",
  "Number of stop points"="n_visits",
  "Time Spent Home"="time_spent_home"
)

readr::write_rds(variable_levels, "data/variable_levels.rds")

dv_metrics_std[, variable := factor(variable, levels = rev(variable_levels),
                                    labels = rev(names(variable_levels)))]

#dv_metrics_std[, cluster_size_tot := sum(cluster_size), by = c("variable")]
#dv_metrics_std[, p_cluster_size := scales::percent(cluster_size / cluster_size_tot)]
#unique(dv_metrics_std[, .(cluster, p_cluster_size)])[order(cluster)]

# relabel clusters to align with existing manuscript
cluster_reclass <- data.table(
  old_cluster = c(0, 1, 2, 3),
  new_cluster = c(0, 1, 3, 2)
)

cluster_labels_ordered <- c("Regular-travel", "Stay-at-home", "Long-distance", "Away-from-home")
cluster_reclass[, cluster_label := factor(cluster_labels_ordered, levels = cluster_labels_ordered)]

#cluster_reclass[, cluster_label := paste0("Cluster ", old_cluster+1)]

fwrite(cluster_reclass, "output/cluster_reclass.csv")

dv_metrics_std[cluster_reclass, on = c("cluster"="new_cluster"), cluster_label := cluster_label]

p_diagnostic <- ggplot(data = dv_metrics_std) + 
  geom_errorbar(aes(y = variable, xmin = q5_value, xmax=q95_value, color=as.character(cluster_label)),
                alpha=0.4, width=0.45) + 
  geom_errorbar(aes(y = variable, xmin = q25_value, xmax=q75_value, color=as.character(cluster_label)),
                alpha=0.9, width=0.45) + 
  geom_vline(xintercept=0, linetype="dashed", size=0.35) + 
  scale_x_continuous(trans=scales::pseudo_log_trans(base = exp(1), sigma=0.1)) + 
  facet_wrap(~cluster_label, nrow=1) + 
  theme_classic() + 
  labs(y = NULL, 
       x = NULL,
       title = "a") + 
  scale_color_manual(values=cluster_color_pal) + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face="bold"))

cluster_total_distance_distribution[cluster_reclass, on = c("cluster"="new_cluster"), cluster_label := cluster_label]

overall_total_distance_distribution[, cluster_label := "All Data"]
overall_total_distance_distribution[, cluster := NA]

overall_total_distance_distribution <- overall_total_distance_distribution[order(bin_numeric)]

p_cluster_distance_distribution <- ggplot(data = rbind(cluster_total_distance_distribution, overall_total_distance_distribution)) + 
  geom_path(aes(x = bin_numeric, y = p_uid_date, color=cluster_label,
                linetype=cluster_label), size=0.3) + 
  geom_point(aes(x = bin_numeric, y = p_uid_date, color=cluster_label), size=0.2) + 
  scale_y_continuous(trans="log10",
                     breaks = c(0.000001, 0.0001, 0.01, 0.1),
                     labels = c(expression(10^-6), expression(10^-4), expression(10^-2), expression(10^-1))) + 
  scale_x_continuous(trans="log10",
                     breaks = c(1, 10, 100, 1000, 10000),
                     labels = c(expression(10^0), expression(10^1), expression(10^2), expression(10^3), expression(10^4))) + 
  scale_color_manual(values=cluster_color_pal) + 
  scale_linetype_manual(values=cluster_linetype_pal) + 
  theme_classic() + 
  labs(x = "Total Distance Travelled Daily (km)",
       y = "Proportion of travel days",
       title="b") + 
  theme(legend.position = "none",
        plot.title = element_text(face="bold"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 2.59), "cm"))

cluster_min_dist_from_home_distribution[cluster_reclass, on = c("cluster"="new_cluster"), cluster_label := cluster_label]

cluster_min_dist_from_home_distribution[, p_uid_date := n_uid_date / sum(n_uid_date, na.rm = T), by = .(cluster)]

cluster_min_dist_from_home_distribution[, cluster_label := ifelse(is.na(cluster_label), "All Data", as.character(cluster_label))]

cluster_min_dist_from_home_distribution[, cluster_label := factor(cluster_label, levels=c(cluster_labels_ordered, "All Data"))]

cluster_min_dist_from_home_distribution <- cluster_min_dist_from_home_distribution[order(bin_numeric)]

p_cluster_min_dist_from_home_distribution <- ggplot(data = cluster_min_dist_from_home_distribution) + 
  geom_path(aes(x = bin_numeric, y = p_uid_date, color=cluster_label,
                linetype=cluster_label), size=0.3) + 
  geom_point(aes(x = bin_numeric, y = p_uid_date, color=cluster_label), size=0.2) + 
  scale_y_continuous(trans="log10",
                     breaks = c(0.000001, 0.0001, 0.01, 0.1),
                     labels = c(expression(10^-6), expression(10^-4), expression(10^-2), expression(10^-1))) + 
  scale_x_continuous(trans="log10",
                     breaks = c(1, 10, 100),
                     labels = c(expression(10^0), expression(10^1), expression(10^2))) + 
  scale_color_manual(values=cluster_color_pal) + 
  scale_linetype_manual(values=cluster_linetype_pal) + 
  theme_classic() + 
  labs(x = "Total Distance Travelled Daily (km)",
       y = "Proportion of travel days",
       title="c") + 
  theme(legend.position = "none",
        plot.title = element_text(face="bold"))

p <- cowplot::plot_grid(p_diagnostic,
                   cowplot::plot_grid(p_cluster_distance_distribution, p_cluster_min_dist_from_home_distribution,
                                      nrow=1,
                                      rel_widths = c(0.55, 0.45)),
                   nrow=2,
                   rel_heights = c(0.5, 0.5))  

ggsave(.args[5],
       p,
       width = 10, 
       height = 7,
       units = 'in')
