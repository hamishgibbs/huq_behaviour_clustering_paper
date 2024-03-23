suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2) 
})

if (interactive()){
  .args <- c(
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_total_distance_distribution_censored.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_n_visits_distribution_censored.csv",
    "output/filtering_stages.png",
    "output/overall_total_distance_distribution.png",
    "output/overall_visit_distribution.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 3)

distance_distribution <- fread(.args[1])
visit_distribution <- fread(.args[2])
filtering <- data.table(
  stage=c('Original', 'Daily observation threshold', 'Home location detected', 'Stops in England & Wales'),
  uid=c(1321623, 782131, 718096, 676648),
  uid_date=c(22852425, 12009712, 11791249, 10955833)
)

filtering <- rbind(filtering[1,], filtering)
filtering <- rbind(filtering, filtering[5,])

filtering[, stage_num := 1:6]

filtering_long <- subset(melt(filtering, id.vars = c('stage', 'stage_num')), 
                         variable == 'uid_date')

filtering_labels <- filtering_long[2:5,]

p_filtering <- ggplot(data = filtering_long) + 
  geom_smooth(aes(x = stage_num, y = value),
            position="identity",
            color='#0028D9',
            span=0.4) +
  geom_segment(data=filtering_labels,
               aes(x=stage_num, xend=stage_num, y=10000000, yend=value)) + 
  theme_classic() + 
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size=15),
        legend.position = "none") + 
  scale_y_continuous(labels=scales::comma,
                     breaks=c(100, 150, 200, 250)*100000,
                     limits=c(10000000, 30000000)) + 
  labs(x = NULL,
       y = 'Days of travel')

ggsave(.outputs[1],
       p_filtering,
       width = 8, 
       height = 3,
       units = 'in')

distance_distribution <- distance_distribution[order(bin_numeric)]

scales::percent(distance_distribution[nrow(distance_distribution)]$n_uid_date / sum(distance_distribution$n_uid_date, na.rm=T))

p_distance <- ggplot(data = distance_distribution) + 
  geom_path(aes(x = bin_numeric, y = p_uid_date), size=0.3) + 
  geom_point(aes(x = bin_numeric, y = p_uid_date), size=0.2) + 
  scale_y_continuous(trans="log10",
                     breaks = c(0.000001, 0.0001, 0.01, 0.1),
                     labels = c(expression(10^-6), expression(10^-4), expression(10^-2), expression(10^-1))) + 
  scale_x_continuous(trans="log10",
                     breaks = c(1, 10, 100, 1000, 10000),
                     labels = c(expression(10^0), expression(10^1), expression(10^2), expression(10^3), expression(10^4))) + 
  labs(x = "Total Distance Travelled Daily (km)",
       y = "Proportion of travel days") + 
  theme_classic()

ggsave(.outputs[2],
       p_distance,
       width = 3, 
       height = 3,
       units = 'in')

visit_distribution <- visit_distribution[order(n_visits)]

scales::percent(visit_distribution[1]$n_uid_date / sum(visit_distribution$n_uid_date, na.rm=T))

p_visits <- ggplot(data = visit_distribution) + 
  geom_path(aes(x = n_visits, y = p_uid_date), size=0.3) + 
  geom_point(aes(x = n_visits, y = p_uid_date), size=0.2) + 
  scale_y_continuous(trans="log10",
                     breaks = c(0.000001, 0.0001, 0.01, 0.1, 1),
                     limits = c(0.000001, 1),
                     labels = c(expression(10^-6), expression(10^-4), expression(10^-2), expression(10^-1), expression(10^0))) + 
  scale_x_continuous(trans="log10",
                     breaks = c(1, 2, 5, 10, 25, 50),
                     limits = c(1, 10^2)) + 
  labs(x = "Number of stop points per day",
       y = "Proportion of travel days") + 
  theme_classic()

ggsave(.outputs[3],
       p_visits,
       width = 3, 
       height = 3,
       units = 'in')

