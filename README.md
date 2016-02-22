Network project
====
The data that used for this project came from this [Crop_Survey_Database](https://github.com/sithjaisong/Crop_Survey_Database). You can folk from the link.

The project is created for keep Rscript used for analyze the survey data under SEKP project.

## project root

```
|--chapter3/
           /--R/
                /--do.R
                /--data-processing/
                                  /do.R
                /--functions/
                            /function_audpc.R
                            /function_cluster.network
                            /function_cooc_table.R
                            /function_node_net_stat.R
                            /function_plot_network.R
                            /function_random_graph.R
                            /function_summarySE.R
                            /function_to_pdf
                            /injury_analysis.R
                            /sweep_analysis.R
                            /weed_analysis.R                            
                /--figure/
                          /F.country-network.R
                          /F.dataset.R
                          /F.interaction.R
                          /F.testing.R
                          /F.fullnet.R
             /--table/
/--README            
```
The complete project should have 2 :folder:
1. chapter 3 titled _Different topological features of networks for animal pests and disease co-occurrence in irrigated lowland rice growing areas in South and South East Asia_  this folder can be run with single script (`do.R`).
2. chapter 4 (no exist)
