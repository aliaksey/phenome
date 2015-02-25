
# What it does: Loads raw data which consists of csv files of PerImage and 
# PerObject tables from CellProfiler
# Input: cell_nuclei_features.csv and image_data.csv
# Output: 
# ph_raw_data.RData - stores the csv files in binary form, after
# doing some basic processing to add columns to the PerObject table.
# Contains 
# cell.ftrs - PerObject table
# image.data - PerImage table
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
# the 1.5 IQR rule, first on area and then on perimeter. Note that no scaling
# of cell features is done here because we are computing the outliers for each
# features (area and perimeter) separately
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
# fromt the center and removes outlier cells. Note that feature scaling is done
# prior to the outlier removal. However the output (cell.shape.f) is not scaled
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
# - statperfeat - various statistics per surface, such as no. of cells and no.
#                 of repeats before and after filtering
# - cell.ftrs.reprod - ImageNumber and FeatureIdx of reproducible replicates
# - cell.ftrs.f.scaled - per cell data, same as cell.shape.f except that it is 
#                        scaled
source("ph_find_reproducible_repeats.R")


# What it does: For each of the reproducible replicates, creates a per-replicate
# profile by computing median across all the cells in the replicate (cells have
# been filtered for outliers). All features from the PerObject and PerImage
# table are included. Scaled version of the features are also saved.
# Input: ph_raw_data.RData, Cell_image reprod.RData, Cell_shape_corr.RData
# Output: joined scaled data.RData - contains image.allftrs.scale,image.allftrs
# - image.allftrs - per-replicate profile (by computing median across all 
#                   cells per feature) of reproducible replicates. The row ids 
#                   are same as cell.ftrs.reprod. Here, all features from the
#                   PerImage and PerObject table were included
# - image.allftrs.scale - same as image.allftrs except scaled. Some features
#                         that were NA after scaling were excluded
source("ph_scale_data.R")


# What it does: Loads classification results obtained by running the 5 
# CPA-trained binary classifiers against all the images in the dataset. Then
# it generates a ground truth set by selecting the most enriched images for 
# each class. 
# Input: supervised classyfication result.Rdata, joined scaled data.RData
# supervised classyfication result.Rdata contains brn,mlt,pnc,spn,stk
# - brn - for each image (=replicate) in the dataset, the enrichment score for 
#         the class "branched"
# - mlt - multipolar, pnc - pancake, spn - stretch pancake, stk - stick
# Output: Images for Control ground  Truth all3.Rdata - contains 
#         grnd_trth1,grnd_trth2,grnd_trth3
# - grnd_trth1 - the top 1% percentile of images (=replicates) per class. Here
#                "top" means top-ranked based on enrichment score for the class
# - grnd_trth2 - top 2%
# - grnd_trth3 - top 3%
source("ph_create_ground_truth_data_set.R")


# What it does: same as ph_create_ground_truth_data_set.R but does so on a 
# per surface basis
# Input: 
# Output: Images for Control ground  Truth all3 feature edition.Rdata - 
# contains grnd_trth1.sf,grnd_trth2.sf,grnd_trth3.sf
source("ph_create_ground_truth_feature.R")

# What it does: For each ground truth set, create data matrix by including
# all the features
# !!!!!!!!!! NOT SURE EXACTLY WHAT IS BEING DONE HERE !!!!!!!!!! 
# Input: 
# Images for Control ground  Truth all3.Rdata
# Images for Control ground  Truth all3 feature edition.Rdata
# joined scaled data.RData
# ph_raw_data.RData
# Output: Cell all data & ground truth scaled.RData containing
# feature.cell - takes image.allftrs, which is the per-replicate profiles of 
#                reproducible replicates and computes median across all of them
#                per surface
# feature.cell.scale - scaled version of feature.cell
# feature.cell.scale_log - log version of feature.cell.scale
# image.cell.scale - ?
# grnd.truth.feat.scale - ground truth subset of feature.cell.scale
# grnd.truth.feat.scale_log - ground truth subset of feature.cell.scale_log
# grnd.truth.img.scale - ground truth subset of image.cell.scale
source("ph_groundtruth_scaling_and_joining_data.R")

# What it does: Not being used
# Input: 
# Output:
#source("ph_find_set_of_features_to_use_for_clustering.R")##can take a lot of hours, better load previous res

# What it does: Not being used
# Input: 
# Output:
#source("ph_groud_truth_pca.R")

# What it does:
# Input: Cell all data & ground truth scaled.RData
# Output: non_correlated_surfaces.RData contains
# - non_cor_feat_data
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
