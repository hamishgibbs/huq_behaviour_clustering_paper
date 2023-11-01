suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2) 
})

if (interactive()){
  .args <- c(
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_long_tail_perc_of_days_per_uid.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_n_cluster_days_per_time_wt.csv",
    "output/time_series_long_tail_wt.png",
    "output/time_series_long_tail.png",
    "output/overall_time_series_distribution.png",
    "output/long_tailed_days_per_device_days.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 4)

uid_date_per_uid <- fread(.args[1])
uid_date_per_time <- fread(.args[2])

uid_date_per_time
cluster_reclass <- fread("output/cluster_reclass.csv")

cluster_color_pal <- c(
  "All Data"="black",
  "Regular-travel"="#e60049",
  "Stay-at-home"="#0bb4ff",
  "Long-distance"="#50e991",
  "Away-from-home"="#e6d800"
)

uid_date_per_time[, cluster := as.integer(cluster)]
uid_date_per_time[cluster_reclass, on = c("cluster"="new_cluster"), cluster_label := cluster_label]

p_days_of_travel <- ggplot(unique(uid_date_per_time[, .(week, n_data)])) + 
  geom_path(aes(x = week, y = n_data), color="red") + 
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) + 
  scale_x_date(date_labels="%B %Y", 
               breaks=seq(as.Date("2018-01-01"), as.Date("2019-12-31"), by="3 months"),
               expand = c(0,0)) + 
  labs(x = NULL, 
       y = "Total travel days per week",
       title="Overall distribution of travel days per week")

# Note that some of these dates have been changes because of when the actual holiday falls
# with respect to the weekly aggregation (lubridate::floor_date(x, "week))
holiday_dates <- data.table(
  date=as.Date(c("2018-1-1", "2018-4-2", "2018-5-7", "2018-5-28", "2018-8-27", "2018-12-29",
                 "2019-1-1", "2019-4-19", "2019-5-6", "2019-5-27", "2019-8-26", "2019-12-29")),
  date_label=rep(c("New Year's Day", "Easter", "Early May Bank Holiday", "Spring Bank Holiday", "Summer Bank Holiday", "Christmas"), 2)
)

school_term_dates <- data.table(
  start_date = as.Date(c("2018-02-12", "2018-05-26", "2018-07-16", "2018-10-22", "2019-02-18", "2019-05-25", "2019-07-15", "2019-10-21")),
  end_date = as.Date(c("2018-02-23", "2018-06-01", "2018-09-05", "2018-11-02", "2019-03-01", "2019-05-31", "2019-09-06", "2019-11-1")),
  term_name = rep(c("February half term", "May half term", "Summer holidays", "October half term"), 2)
)

uid_date_per_time_wt <- subset(uid_date_per_time, cluster_label %in% c('Long-distance', 'Away-from-home'))
uid_date_per_time_wt[, p_days_per_cluster_wt := p_days_per_cluster * wt]

# normalize from first observation
uid_date_per_time_wt[, first_obs := week == min(week), by = c("cluster")]
first_obs_df <- subset(uid_date_per_time_wt, first_obs)[, .(week, cluster, p_days_per_cluster_wt)]
first_obs_df[, first_p_days_per_cluster_wt := p_days_per_cluster_wt]
first_obs_df[, p_days_per_cluster_wt := NULL]

uid_date_per_time_wt[first_obs_df, on=c("cluster"), first_p_days_per_cluster_wt := first_p_days_per_cluster_wt]

uid_date_per_time_wt[, p_days_per_cluster_wt_norm := p_days_per_cluster_wt / first_p_days_per_cluster_wt]

# max of either value so that label sits on the top of the line
max_value_per_week <- uid_date_per_time_wt[, .(y = max(p_days_per_cluster_wt_norm)), by = c("week")]
min_value_per_week <- uid_date_per_time_wt[, .(y = min(p_days_per_cluster_wt_norm)), by = c("week")]
holiday_dates[, week_date := lubridate::floor_date(date, "week")]

school_term_dates[, start_date_week := lubridate::floor_date(start_date, "week")]
school_term_dates[, end_date_week := lubridate::floor_date(end_date, "week")]
school_term_dates[, min_y := min(uid_date_per_time_wt$p_days_per_cluster_wt_norm)]
school_term_dates[, max_y := max(uid_date_per_time_wt$p_days_per_cluster_wt_norm)]

# Some labels should be nudged up, some down 
labels_up <- subset(holiday_dates, ! date_label %in% c("Early May Bank Holiday"))
labels_down <- subset(holiday_dates, date_label %in% c("Early May Bank Holiday"))
labels_up <- labels_up[max_value_per_week, on=c("week_date"="week"), nomatch = 0]
labels_down <- labels_down[min_value_per_week, on=c("week_date"="week"), nomatch = 0]

plot_date_labels <- function(data, nudge_y){
  ggrepel::geom_label_repel(data = data, 
                            aes(x = week_date, y = y,
                                label = date_label, color=date_label),
                            nudge_y=nudge_y, nudge_x=1,
                            box.padding=0.5,
                            point.padding=0.5,
                            segment.linetype="dotted",
                            fontface="bold")
}

p <- ggplot(uid_date_per_time_wt) +
  geom_rect(data=school_term_dates,
            aes(xmin=start_date_week, xmax=end_date_week, ymin=Inf, ymax=-Inf),
            alpha=0.1, fill="red") + 
  geom_path(aes(x = week, y = p_days_per_cluster_wt_norm, linetype=cluster_label)) + 
  plot_date_labels(labels_up, 1.5) +
  plot_date_labels(labels_down, -1.5) +
  guides(color = "none") +
  theme_classic() + 
  theme(legend.position = "right",
        legend.title = element_text(face="bold")) + 
  scale_x_date(date_labels="%B %Y", 
               breaks=seq(as.Date("2018-01-01"), as.Date("2019-12-31"), by="3 months"),
               expand = c(0,0)) + 
  labs(linetype="Cluster",
       y = "Change in days of travel over time (index)",
       x=NULL) + 
  ylim(min(uid_date_per_time_wt$p_days_per_cluster_wt_norm)-0.5, max(uid_date_per_time_wt$p_days_per_cluster_wt_norm)+1)

ggsave(.outputs[1],
       p,
       width = 10, 
       height = 6,
       units = 'in')

p <- ggplot(subset(uid_date_per_time, cluster_label %in% c('Long-distance', 'Away-from-home'))) +
  geom_path(aes(x = week, y = p_days_per_cluster, linetype=cluster_label)) + 
  theme_classic() + 
  theme(legend.position = c(0.1, 0.8)) + 
  scale_x_date(date_labels="%B %Y", 
               breaks=seq(as.Date("2018-01-01"), as.Date("2019-12-31"), by="3 months"),
               expand = c(0,0)) + 
  labs(linetype="Cluster",
       y = "Proportion of travel days per cluster",
       x=NULL,
       title="Proportion of travel days per cluster per week")


ggsave(.outputs[2],
       p,
       width = 10, 
       height = 6,
       units = 'in')

ggsave(.outputs[3],
       p_days_of_travel,
       width = 10, 
       height = 6,
       units = 'in')

plot_data <- rbind(data.table(p=0, bin_numeric=0),
      uid_date_per_uid[complete.cases(uid_date_per_uid)][, .(p, bin_numeric)])

p1 <- ggplot(data=plot_data) + 
  geom_path(aes(x = bin_numeric, y = cumsum(p))) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_classic() + 
  labs(x = "Proportion of days in 'long-tailed' cluster",
       y = "Proportion of devices",
       title="a")

p2 <- ggplot(data=plot_data) + 
  geom_path(aes(x = bin_numeric, y = cumsum(p))) + 
  scale_y_continuous(trans="log10", labels = scales::percent) + 
  theme_classic() + 
  labs(x = "Proportion of days in 'long-tailed' cluster",
       y = parse(text="Proportion~of~devices~(Log[10]~scale)"),
       title="b") + 
  theme(
    plot.background = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

p <- cowplot::ggdraw(p1) + 
  cowplot::draw_plot(p2, x = 0.35, y = 0.2, width = 0.6, height = 0.6)

ggsave(.outputs[4],
       p,
       width = 10, 
       height = 6,
       units = 'in')
