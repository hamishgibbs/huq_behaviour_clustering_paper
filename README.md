# huq_behaviour_clustering_paper

Source code and supporting data for the manuscript: *Clustering reveals key behaviours driving human movement network structure. Gibbs et. al. 2023.*

#### Usage

Code for creating main text and supporting figures is located in the `src` directory.

Aggregated data supporting the manucript results is located in the `data` folder. 

The only data not tracked in this repository is the 2011 Output Area Classification which can be downoaded from [this](https://data.cdrc.ac.uk/dataset/output-area-classification-2011) link.

Workflow is managed with [Snakemake](https://snakemake.readthedocs.io/en/stable/). To run the workflow use:

```
cd huq_behaviour_clustering_paper
snakemake -j1
```


