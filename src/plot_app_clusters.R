suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2) 
})

if (interactive()){
  .args <- c(
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_cluster_per_app_cluster.csv",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_app_cluster_summary.csv",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_visit_distribution.csv",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_total_distance_distribution_total_distance_distribution.csv",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_visits_per_hour.csv",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_visits_per_wday.csv",
    "output/app_cluster_bias.png",
    "output/app_cluster_bias_time.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

reclass_cluster <- function(data, cluster_reclass){
  data[cluster_reclass, on = c("cluster"="new_cluster"), cluster_label := cluster_label]
}
reclass_app_cluster <- function(data){
  data[, app_cluster := ifelse(app_cluster == "All Data", "All Data", LETTERS[as.numeric(app_cluster)])]
}

.outputs <- tail(.args, 2)

cluster_reclass <- fread("output/cluster_reclass.csv")
cluster_per_app_cluster <- fread(.args[1])
reclass_cluster(cluster_per_app_cluster, cluster_reclass)
reclass_app_cluster(cluster_per_app_cluster)

app_cluster_summary <- fread(.args[2])
reclass_app_cluster(app_cluster_summary)

visits_distribution <- fread(.args[3])
reclass_app_cluster(visits_distribution)

distance_distribution <- fread(.args[4])
reclass_app_cluster(distance_distribution)

visits_per_hour <- fread(.args[5])
reclass_app_cluster(visits_per_hour)
visits_per_wday <- fread(.args[6])
reclass_app_cluster(visits_per_wday)

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

p1a <- ggplot(cluster_per_app_cluster, aes(x="", y=p_dev_days, fill=cluster_label)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + 
  scale_fill_manual(values=cluster_color_pal) + 
  theme_void() + 
  facet_wrap(~as.character(app_cluster), nrow=4, ncol=2) + 
  theme(legend.position = "none") + 
  labs(title="a")


scale_factor <- max(app_cluster_summary$n_apps) / max(app_cluster_summary$n_dev_days)

p1b <- ggplot(app_cluster_summary, aes(x=app_cluster)) +
  geom_bar(aes(y=n_dev_days*scale_factor, fill = 'Days of travel'), stat = "identity", 
           width = 0.5, position = position_nudge(x = 0.25)) +
  geom_bar(aes(y=n_apps, fill = 'Applications'), stat = "identity",
           width=0.5, position = position_nudge(x = -0.25)) +
  scale_fill_manual(values=c('#aed9e0','#5e6472')) + 
  scale_y_continuous(
    name = "Number of applications",
    sec.axis = sec_axis(~./scale_factor, name="Days of travel", labels = scales::comma),
    breaks = c(1, 5, 10)
  ) + 
  labs(title="b", 
       x = "App cluster",
       fill=NULL) + 
  theme_classic() + 
  theme(legend.position = c(0.15, 0.9))


app_cluster_levels <- c("All Data", LETTERS[1:8])
visits_distribution[, type := ifelse(app_cluster == "All Data", "solid", "dashed")]
visits_distribution[, app_cluster := factor(app_cluster, 
                                            levels = app_cluster_levels)]
app_cluster_pal <- c(
  "black", "#f94144", "#f3722c", "#f8961e", "#f9c74f", "#90be6d", "#43aa8b", "#577590", "#013a63"
)
names(app_cluster_pal) <- app_cluster_levels
p1c <- ggplot(visits_distribution) + 
  geom_path(aes(x = n_visits, y = p_uid_date, color=app_cluster, linetype=type), size=0.4) + 
  geom_point(aes(x = n_visits, y = p_uid_date, color=app_cluster, linetype=type), size=0.4) + 
  scale_color_manual(values = app_cluster_pal) + 
  scale_y_continuous(trans='log10', labels = scales::percent) + 
  scale_x_continuous(trans='log10') + 
  theme_classic() + 
  guides(linetype="none") + 
  labs(title="c",
       x = "Number of stop points",
       y = "Proportion of travel days",
       color="App cluster") + 
  theme(legend.position = "none")

distance_distribution[, type := ifelse(app_cluster == "All Data", "solid", "dashed")]
distance_distribution[, app_cluster := factor(app_cluster, 
                                            levels = app_cluster_levels)]
p1d <- ggplot(distance_distribution) + 
  geom_path(aes(x = bin_numeric, y = p_uid_date, color=app_cluster, linetype=type), size=0.4) + 
  geom_point(aes(x = bin_numeric, y = p_uid_date, color=app_cluster, linetype=type), size=0.4) + 
  scale_color_manual(values = app_cluster_pal) + 
  scale_y_continuous(trans='log10', labels = scales::percent) + 
  scale_x_continuous(trans='log10') + 
  theme_classic() + 
  guides(linetype="none") + 
  labs(title="d",
       x = "Total distance travelled daily",
       y = NULL,
       color="App cluster")

p1cd <- cowplot::plot_grid(p1c, p1d, rel_widths = c(0.45, 0.55))
p1bcd <- cowplot::plot_grid(p1b, p1cd, ncol = 1)
p1 <- cowplot::plot_grid(p1a, p1bcd, rel_widths = c(0.3, 0.7))

ggsave(.outputs[1],
       p1,
       width = 10, 
       height = 6,
       units = 'in')


p2a <- ggplot(visits_per_hour) + 
  geom_path(aes(x = variable, y = p, color=app_cluster)) + 
  scale_color_manual(values=app_cluster_pal[2:length(app_cluster_pal)]) + 
  theme_classic() + 
  labs(title = "a",
       x = "Hour of the day",
       y = "Proportion of travel days",
       color = NULL) + 
  theme(legend.position = "none")

weekdays_vec <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
visits_per_wday[, weekday := factor(weekdays_vec[wday],
                                    levels = c(
                                      weekdays_vec[2:7], weekdays_vec[1]
                                    ))]

visits_per_wday <- visits_per_wday[order(weekday)]

p2b <- ggplot(visits_per_wday) + 
  geom_path(aes(x = weekday, y = p, color=app_cluster, group=app_cluster)) + 
  scale_color_manual(values=app_cluster_pal[2:length(app_cluster_pal)]) + 
  theme_classic() + 
  labs(title = "b",
       x = "Day of the week",
       color="App cluster",
       y = NULL,
       color = NULL)

p2 <- cowplot::plot_grid(p2a, p2b, rel_widths = c(0.45, 0.55))

ggsave(.outputs[2],
       p2,
       width = 10, 
       height = 4,
       units = 'in')
