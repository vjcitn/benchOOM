#' propagate information from a RangedSummarizedExperiment to Oracle
#' @import SummarizedExperiment
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @param x RangedSummarizedExperiment
#' @param con connection to oracle database
#' @param featureKeyName character string that is used for joining features to metadata
#' @param sampleKeyName character string that is used for joining samples to feature data
#' @param tablename character string used as a prefix, suffixes _assay, _ranges, _coldata will be appended in the back end
#' @param \dots not used
#' @examples
#' \dontrun{
#'  require(geuvPack)
#'  data(geuFPKM) # following will generate charges,
#'  # assumes that connection has been made to the database
#'  writeOracleTable( geuFPKM, con, "GENEID", "SAMPLE", "geuFPKM" )
#' }
#'
#' @export
setGeneric("writeOracleTable", function(x, con, featureKeyName, sampleKeyName, tablename, ...)
  standardGeneric("writeOracleTable"))
setOldClass("src_oracle")
setMethod("writeOracleTable", c("RangedSummarizedExperiment", "src_oracle"), function(x, con, featureKeyName, sampleKeyName, tablename, ...) {
  dfassay = as.data.frame(assays(x)[[1]])
  featureKey = rownames(x)
  if (is.null(featureKey)) stop("need rownames for this operation")
  dfassay = data.frame(featureKey, dfassay)
  names(dfassay)[1] = featureKeyName
  dfmeta = as.data.frame(rowRanges(x))
  dfmeta = data.frame(featureKey, dfmeta)
  names(dfmeta)[1] = featureKeyName
  sampdata = as.data.frame(colData(x))
  colnames(sampdata) <- substring(colnames(sampdata), 1, 25)
  colnames(sampdata) = gsub("\\.", "_", colnames(sampdata))
  sampdata = data.frame(colnames(x), sampdata)
  names(sampdata)[1] = sampleKeyName
  mycon = con
  assaycheck = dbWriteTable(mycon, paste0(tablename, "_assay"), dfassay)
  rangecheck = dbWriteTable(mycon, paste0(tablename, "_ranges"), dfmeta)
  sampcheck = dbWriteTable(mycon, paste0(tablename, "_coldata"), sampdata)
  if (all(c(assaycheck, rangecheck, sampcheck))) return(con)
  else stop("a write operation failed")
})
