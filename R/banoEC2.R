#' interact with banovichSE content via hdf5 server
#' @export
banoH5 = function(url="http://170.223.248.164:7248",
          host="assays.hdfgroup.org", ...) {
require(benchOOM)
con = getH5ShContent(url, host)
ds = datasets(con)
ds
}

setMethod("dim", "H5SDatasets", function(x) x@attrs$shape$dim)
setMethod("show", "H5SDatasets", function(object) {
  dims = dim(object)
  cat("An HDF5-served dataset with internal dimensions", dims[1], "x", dims[2], "\n")
  okr = min(5, dims[2])
  okc = min(5, dims[1])
  cat("Northwest", okr, "x", okc, "submatrix:\n")
  print(t(object[1:okr, 1:okc]))  # consider transposing upstream when loaded into server
})

setClass("RestfulSummarizedExperiment",
   contains="SummarizedExperiment", representation(source="H5SDatasets"))


