# test_phenopix_cuny.R
#library(usethis)
#use_github()

library(phenopix)
# structureFolder()
# DrawMULTIROI()
# extractVIs()
# extractDateFilename()
# autoFilter()
# FitDoubleLogElmore()
# PhenoExtract()

phenocam_polys <- DrawMULTIROI("/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_source/phenocamdata_RGB_2022/cuny/2022/10/cuny_2022_10_19_115706.jpg",
                               "/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_source/phenocom_rois",
                               nroi = 1, roi.names = c("tree_1"), file.type = ".jpg")
