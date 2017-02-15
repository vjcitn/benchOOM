#' obtain example of HDF5-backed RangedSummarizedExperiment
#' @importFrom SummarizedExperiment loadHDF5SummarizedExperiment
#' @examples
#' require("SummarizedExperiment")
#' litgeu = getLitGeu()
#' assay(litgeu[1:5,1:5])
#'
#' @export
getLitGeu = function() {
 loadHDF5SummarizedExperiment(system.file("litgeu", package="benchOOM"))
}
