suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2) 
})

if (interactive()){
  .args <- c(
    "/Users/hamishgibbs/Documents/UCL/hbp2/data/documents_20231011/20231010_HAMISHGIBBS_905_01_time_thresh_sensitivity.csv",
    "output/time_thresh_sensitivity.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 1)

time_thresh <- fread(.args[1])

p <- ggplot(time_thresh) + 
  geom_vline(xintercept=300, color='red', linetype='dashed') + 
  geom_point(aes(x = thresh_mins, y = n_uid), color="black") +
  geom_path(aes(x = thresh_mins, y = n_uid), color="black") + 
  theme_classic() + 
  scale_y_continuous(labels=scales::comma, breaks=c(250000, 500000, 750000, 1000000, 1250000),
                     limits=c(0, 1500000)) + 
  scale_x_continuous(breaks=c(0, 60, 120, 300, 600, 900, 1200)) + 
  theme(title = element_text(face="bold")) + 
  labs(y = "Number of mobile devices",
       x = "Minutes observed per day",
       title = "Sensitivity to minutes observed per day threshold")

ggsave(.outputs[1],
       p,
       width = 10, 
       height = 5.5,
       units = 'in')
