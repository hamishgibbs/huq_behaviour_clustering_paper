export_folder_date = "20231011"
export_date = "20231010" 

rule all:
  input:
    "output/filtering_stages.png",
    "output/cluster_diagnostics.png",
    "output/example_network_types.png",
    "output/time_series_long_tail_wt.png"

rule figure_1:
  input:
    "src/figure_1.R",
    f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_total_distance_distribution_censored.csv",
    f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_n_visits_distribution_censored.csv"
  output:
    "output/filtering_stages.png",
    "output/overall_total_distance_distribution.png",
    "output/overall_visit_distribution.png"
  shell:
    "Rscript {input} {output}"
    
rule figure_2:
    input:
      "src/figure_2.R",
      f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_clustering_vars_std.csv",
      f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_clustering_distance_distribution.csv",
      f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_total_distance_distribution_censored.csv",
      f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_clustering_min_dist_from_home_distribution.csv"
    output:
      "output/cluster_diagnostics.png"
    shell:
      "Rscript {input} {output}"
      
rule figure_3:
  input:
    "src/figure_3.R",
    f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_connectivity_edge_counts.csv",
    f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_connectivity_degree_distribution.csv",
    f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_connectivity_dist_km_distribution.csv"
  output:
    "output/example_network_types.png",
    "output/example_networks_sigma.csv",
    "output/network_metrics.png",
    "output/connectivity_degree_distribution.png",
    "output/connectivity_edge_distance_distribution.png",
    "output/network_metrics_table.csv"
  shell:
      "Rscript {input} {output}"
      
rule figure_4:
  input:
    "src/figure_4.R",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_long_tail_perc_of_days_per_uid.csv",
    "data/documents_20231011/20231010_HAMISHGIBBS_905_01_n_cluster_days_per_time_wt.csv"
  output:
    "output/time_series_long_tail_wt.png",
    "output/time_series_long_tail.png",
    "output/overall_time_series_distribution.png",
    "output/long_tailed_days_per_device_days.png"
  shell:
      "Rscript {input} {output}"
