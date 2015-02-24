
# What it does: Loads raw data which consists of csv files of PerImage and 
# PerObject tables from CellProfiler
# Input: cell_nuclei_features.csv and image_data.csv
# Output: 
# ph_raw_data.RData - stores the csv files in binary form, after
# doing some basic processing to add columns to the PerObject table.
# Contains the variables cell.ftrs and image.data
#source("ph_load_raw_data.R")

# What it does: Loads libraries, installs what is missing
# Input: None
# Output: None
source("ph_load_requered_packages.R")

# What it does: Filters replicates based on cell density. This is done on a 
# per surface basis. Replicates with too few or too many cells are discarded,
# using the 1.5 IQR rule
# Input: ph_raw_data.RData
# Output: 
# Cell_dens_corr.RData - contains cell.dns.f
# cell_density_filter_plot.RData - cell.density, cell.dns.f
# - cell.density - cell count for each replicate
# - cell.dns.f - same as cell.density but after filtering
source("ph_filter_cell_density.R")

# What it does: Pools all the cells from all the replicates of a given surface
# then removes cells that are outliers in terms of area and perimeter. Uses
# the 1.5 IQR rule, first on area and then on perimeter.
# Input: ph_raw_data.RData, Cell_dens_corr.RData
# Output: 
# Cell_area_perim_corr.RData - contains cell.area.f
# area_perimeter_plot.RData - contains cell.area, cell.area.f
# - cell.area - cell area and perimeter for each cell
# - cell.area.f - same as cell.area but after filtering
source("ph_filter_extreme_missegmentataion.R")

# What it does: Pools all the cells from all the replicates of a given surface
# then removes cells that are outliers in terms of the shape. Moutlier is used
# to do the pruning. Moutlier computes the mahalanobis distance of each cell
# fromt the center and removes outlier cells
# Input: ph_raw_data.RData, Cell_area_perim_corr.RData
# Output: 
# mahalanobis_filtration_plot.RData - contains cell.shape.f, cell.shape
# Cell_shape_corr.RData - contains cell.shape.f
# - cell.shape - cell shape features for each cell
# - cell.shape.f - same as cell.shape but after filtering
source("ph_filter_in_mahalanobis.R")

# What it does: For each surface, retains only those replicates that are 
# good quality, that is, those that are reproducible
# Input: ph_raw_data.RData, Cell_shape_corr.RData
# Output: 
# reproducable_surfaces_plot.RData - contains statperfeat,cell.ftrs.reprod,
# and cell.ftrs.f.scaled
# cell.ftrs.reprod - contains statperfeat,cell.ftrs.reprod
# - statperfeat - 
# - cell.ftrs.reprod - 
# - cell.ftrs.f.scaled - 
source("ph_find_reproducible_repeats.R")


# What it does:
# Input: 
# Output:
source("ph_scale_data.R")

# What it does:
# Input: 
# Output:
source("ph_create_ground_truth_data_set.R")

# What it does:
# Input: 
# Output:
source("ph_create_ground_truth_feature.R")

# What it does:
# Input: 
# Output:
source("ph_groundtruth_scaling_and_joining_data.R")

# What it does:
# Input: 
# Output:
#source("ph_find_set_of_features_to_use_for_clustering.R")##can take a lot of hours, better load previous res

# What it does:
# Input: 
# Output:
#source("ph_groud_truth_pca.R")

# What it does:
# Input: 
# Output:
source("ph_find_noncorrelated_surfaces.R")

# What it does:
# Input: 
# Output:
source("ph_calculate_PCA_all_data.R")

# What it does:
# Input: 
# Output:
source("ph_find_optimal_clustering_technique.R")

# What it does:
# Input: 
# Output:
source("ph_cluster_all_data.R")

# What it does:
# Input: 
# Output:
# source("ph_cluster_representative_surfaces.R")

# What it does:
# Input: 
# Output:
# source("ph_find_outliers_from_all_data_set.R")

# What it does:
# Input: 
# Output:
# source("ph_make_plots.R")

# What it does:
# Input: 
# Output:
# source("ph_plot_map_of_ground_truth.R")

# What it does:
# Input: 
# Output:
# source("ph_")

# What it does:
# Input: 
# Output:
source("ph_make_plots.R")
