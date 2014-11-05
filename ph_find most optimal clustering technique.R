rm(list=ls())
load("Cell all data & ground truth scaled.RData")

##selecting cell shape variables creating 3 classes ofv variables: 10 simple; Zernike; and selected by bootstrap
all.names.temp<-names(image.cell.scale)
##selecting only classycal cell shapes
simple.cellshape.name<-all.names.temp[grepl("Cells_AreaShape", all.names.temp)
                                      & !grepl("Zernike", all.names.temp)&
                                             !grepl("Center", all.names.temp)&
                                             !grepl("Neighbors", all.names.temp)]
# selecting only Zernike cell shapes
zernike.cellshape.name<-all.names.temp[grepl("Zernike", all.names.temp)]
# selecting cell names find by model simulation

##find and remove highly correlated features
