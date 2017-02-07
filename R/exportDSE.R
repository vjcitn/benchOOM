#' @import SummarizedExperiment
#' @import DelayedArray
#' @import HDF5Array
#' @export
setClass("HDF5RangedSummarizedExperiment", contains="RangedSummarizedExperiment")
#' dump assay of RangedSummarizedExperiment to Delayed HDF5 and return suitable instance
#' @param rse RangedSummarizedExperiment instance
#' @param file character string with path to HDF5 store on disk
#' @param name character string with name of HDF5 dataset
#' @examples
#' \dontrun{
#'  library(geuvPack)
#'  data(geuFPKM)
#'  delgeu = rhconvert( geuFPKM, "geuFPKM.h5", "geuFPKM" )
#'  delgeu
#' }
#'
#' @export
rhconvert = function (rse, file, name, ...) 
{
    assaydat = assay(rse)
    harr = writeHDF5Array(assaydat, file, name, ...)
    harr = DelayedArray(harr)
    rownames(harr) = rownames(rse)
    colnames(harr) = colnames(rse)
    assay(rse) = harr
    new("HDF5RangedSummarizedExperiment", rse)
}
