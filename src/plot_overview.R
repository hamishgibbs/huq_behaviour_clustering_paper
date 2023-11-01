suppressPackageStartupMessages({
  library(data.table)
  library(sf) 
  library(ggplot2) 
  library(mapboxapi)
})

if (interactive()){
  .args <- c(
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

n <- 50
cluster_sd <- 0.15
p_sample_cluster <- ggplot() + 
  geom_point(aes(x = rnorm(n, sd=cluster_sd), y = rnorm(n, sd=cluster_sd)),
             size=0.7) + 
  theme_void() + 
  ylim(-1, 1) + 
  xlim(-1, 1)

ggsave("output/sample_gpt_pt_cluster.png",
       p_sample_cluster, 
       width=5, height=5, units="in")

n <- 3
cluster_sd <- 1
p_sample_cluster <- ggplot() + 
  geom_point(aes(x = rnorm(n, sd=cluster_sd), y = rnorm(n, sd=cluster_sd)),
             size=5,
             color="#2b8cbe") + 
  theme_void() + 
  ylim(-1, 1) + 
  xlim(-1, 1)

ggsave("output/sample_gpt_pt_cluster_color.png",
       p_sample_cluster, 
       width=5, height=5, units="in")

