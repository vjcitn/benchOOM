#' obtain example of HDF5-backed RangedSummarizedExperiment
#' @examples
#' require("SummarizedExperiment")
#' litgeu = getLitGeu()
#' assay(litgeu[1:5,1:5])
#'
#' @export
getLitGeu = function() {
 data("litgeu")
 assay(litgeu)@seed@file = system.file("hdf5/litgeu.h5", package="benchOOM")
 litgeu
}
