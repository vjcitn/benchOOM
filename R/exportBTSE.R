#' propagate information from a RangedSummarizedExperiment to google BigTable
#' @import SummarizedExperiment
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @param x RangedSummarizedExperiment
#' @param src instance of src_bigrquery
#' @param featureKeyName character string that is used for joining features to metadata
#' @param sampleKeyName character string that is used for joining samples to feature data
#' @param tablename character string used as a prefix, suffixes _assay, _ranges, _coldata will be appended in the back end
#' @param \dots not used
#' @examples
#' \dontrun{
#'  require(geuvPack)
#'  data(geuFPKM) # following will generate charges,
#'  # assumes that 'bq' is an authenticated 'src_bigquery' instance
#'  bq = writeBigTable( geuFPKM, bq, "GENEID", "SAMPLE", "geuFPKM" )
#'  bq
#' }
#'
#' @export
setGeneric("writeBigTable", function(x, src, featureKeyName, sampleKeyName, tablename, ...)
   standardGeneric("writeBigTable"))
setOldClass("src_bigquery")
setMethod("writeBigTable", c("RangedSummarizedExperiment", "src_bigquery"), function(x, src, featureKeyName, sampleKeyName, tablename, ...) {
#
# do we really need two tables?  could join them and just keep field
# names "separate"
#
   dfassay = as.data.frame(assays(x)[[1]])
   featureKey = rownames(x)
   if (is.null(featureKey)) stop("need rownames for this operation")
   dfassay = data.frame(featureKey, dfassay)
   names(dfassay)[1] = featureKeyName
   dfmeta = as.data.frame(rowRanges(x))
   dfmeta = data.frame(featureKey, dfmeta)
   names(dfmeta)[1] = featureKeyName
   sampdata = as.data.frame(colData(x))
   colnames(sampdata) = gsub("\\.", "_", colnames(sampdata))
   sampdata = data.frame(colnames(x), sampdata)
   names(sampdata)[1] = sampleKeyName
   mycon = src$con
   assaycheck = dbWriteTable(mycon, paste0(tablename, "_assay"), dfassay)
   rangecheck = dbWriteTable(mycon, paste0(tablename, "_ranges"), dfmeta)
   sampcheck = dbWriteTable(mycon, paste0(tablename, "_coldata"), sampdata)
   if (all(c(assaycheck, rangecheck, sampcheck))) return(src)
   else stop("a write operation failed")
})

