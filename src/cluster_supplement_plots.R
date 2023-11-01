suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2) 
})

if (interactive()){
  .args <- c(
    "data/documents_20230915/20230914_HAMISHGIBBS_905_01_scree_data.csv",
    "data/documents_20230915/20230914_HAMISHGIBBS_905_01_cluster_scores_summary.csv",
    "data/documents_20230915/20230914_HAMISHGIBBS_905_01_input_var_cor.csv",
    "data/documents_20230915/20230914_HAMISHGIBBS_905_01_pca_loadings_cor.csv",
    "output/scree_plot.png",
    "output/cluster_scores_bootstrap.png",
    "output/input_var_cor.png",
    "output/loadings_cor.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 4)

scree_data <- fread(.args[1])
cluster_scores <- fread(.args[2])
input_var_cor <- fread(.args[3])
loadings_cor <- fread(.args[4])

p_scree <- ggplot(data=scree_data) + 
  geom_path(aes(x = component, y = var_explained)) + 
  geom_point(aes(x = component, y = var_explained)) + 
  geom_hline(yintercept=0.95, linetype='dashed') + 
  theme_classic() + 
  scale_y_continuous(labels = scales::percent,
                     breaks=c(0.1, round(min(scree_data$var_explained), 2), 0.5, 0.75, 0.95, 1),
                     limits = c(0, 1)) + 
  scale_x_continuous(breaks=1:11) + 
  labs(x = "Principal Component",
       y = "Variance Explained")

ggsave(.outputs[1],
       p_scree,
       width = 10, 
       height = 5.5,
       units = 'in')

cluster_scores[, metric_label := factor(metric, 
                                        levels=c(
                                          "silhouette_score", 
                                          "davies_bouldin",
                                          "calinski_harabasz"),
                                        labels=c(
                                          "Silhouette Score",
                                          "Davies-Bouldin Index",
                                          "Calinski-Harabasz Index"
                                        ))]

p_cluster_scores <- ggplot(data=cluster_scores) + 
  geom_ribbon(aes(x = k, ymin = q5_value, ymax=q95_value), alpha=0.2) + 
  geom_ribbon(aes(x = k, ymin = q25_value, ymax=q75_value), alpha=0.2) +
  geom_path(aes(x = k, y = mean_value), alpha=0.8) + 
  geom_vline(xintercept=4, linetype='dashed', size=0.2) + 
  facet_wrap(~metric_label, scales='free_y') + 
  theme_classic()

ggsave(.outputs[2],
       p_cluster_scores,
       width = 10, 
       height = 5.5,
       units = 'in')

variable_levels <- readr::read_rds("data/variable_levels.rds")

input_var_cor[, Var1 := factor(Var1, 
                               levels = variable_levels, 
                               labels = names(variable_levels))]
input_var_cor[, Var2 := factor(Var2, 
                               levels = variable_levels, 
                               labels = names(variable_levels))]

loadings_cor[, Var1 := factor(Var1, 
                              levels = variable_levels, 
                              labels = names(variable_levels))]

loadings_cor[, Var2 := factor(Var2, 
                              levels = paste0("PC", 1:11))]

plot_cor <- function(data, low, high){
  ggplot(data = data) + 
    geom_raster(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(aes(x = Var1, y = Var2), fill = NA, 
              color = "black", size = 0.15) + 
    geom_text(aes(x = Var1, y = Var2, label = round(value, 2)),
              size=3) + 
    scale_fill_gradient2(limits=c(-1, 1),
                         low=low,
                         high=high) + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          axis.line = element_blank(),
          axis.ticks = element_blank()) + 
    labs(x = NULL,
         y = NULL,
         fill = "Pearson\nCorrelation")
}

p_input_var_cor <- plot_cor(input_var_cor, '#8c510a', '#01665e')

p_loadings_cor <- plot_cor(loadings_cor, '#762a83', '#1b7837')

ggsave(.outputs[3],
       p_input_var_cor,
       width = 10, 
       height = 9,
       units = 'in')

ggsave(.outputs[4],
       p_loadings_cor,
       width = 10, 
       height = 9,
       units = 'in')
