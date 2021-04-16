#' get an in-memory SummarizedExperiment
#' @return SummarizedExperiment instance
#' @export
get_bano_inmem = function() {
  ca = BiocFileCache::BiocFileCache()
  chk = BiocFileCache::bfcquery(ca, "bano_inmem")
  if (nrow(chk)<1) {
    BiocFileCache::bfcadd(ca, "bano_inmem",
     fpath="https://biocfound-tiledb.s3.ca-central-1.amazonaws.com/banovichSE.rda")
    chk = BiocFileCache::bfcquery(ca, "bano_inmem")
    }
  get(load(ca[[chk$rid]]))  # technical debt for not using RDS
}

#' get an HSDS instance of banovich 450k data
#' @return restful SummarizedExperiment
#' @export
get_bano_hsds = function() {
   ExperimentHub::ExperimentHub()[["EH551"]]
}

#' get an HDF5-backed banovich
