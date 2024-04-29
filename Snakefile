export_folder_date = "20231011"
export_date = "20231010" 

rule all:
  input:
    "output/filtering_stages.png",
    "output/cluster_diagnostics.png",
    "output/example_network_types.png",
    "output/time_series_long_tail_wt.png",
    "output/app_cluster_bias.png"

rule plot_data_cleaning:
  input:
    "src/plot_data_cleaning.R",
    f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_total_distance_distribution_censored.csv",
    f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_n_visits_distribution_censored.csv"
  output:
    "output/filtering_stages.png",
    "output/overall_total_distance_distribution.png",
    "output/overall_visit_distribution.png"
  shell:
    "Rscript {input} {output}"
    
rule plot_cluster_diagnositcs:
    input:
      "src/plot_cluster_diagnositcs.R",
      f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_clustering_vars_std.csv",
      f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_clustering_distance_distribution.csv",
      f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_total_distance_distribution_censored.csv",
      f"data/documents_{export_folder_date}/{export_date}_HAMISHGIBBS_905_01_clustering_min_dist_from_home_distribution.csv"
    output:
      "output/cluster_diagnostics.png"
    shell:
      "Rscript {input} {output}"
      
rule plot_network_connectivity:
  input:
    "src/plot_network_connectivity.R",
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

rule plot_app_clusters:
  input:
    "src/plot_app_clusters.R",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_cluster_per_app_cluster.csv",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_app_cluster_summary.csv",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_visit_distribution.csv",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_total_distance_distribution_total_distance_distribution.csv",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_visits_per_hour.csv",
    "data/documents_20240315/20240313_HAMISHGIBBS_905_01_visits_per_wday.csv"
  output:
    "output/app_cluster_bias.png",
    "output/app_cluster_bias_time.png"
  shell:
    "Rscript {input} {output}"
