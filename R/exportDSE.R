#' @import SummarizedExperiment

.writeHDF5Dataset = function (x, file, name, ...) 
{
# literal copy from HDF5Array for generic, adding dots
# please remove after getting some consensus on
# whether/how to define generic
#
    old_dump_file <- getHDF5DumpFile()
    old_dump_name <- getHDF5DumpName()
    setHDF5DumpFile(file)
    on.exit({
        setHDF5DumpFile(old_dump_file)
        setHDF5DumpName(old_dump_name)
    })
    setHDF5DumpName(name)
    dump <- HDF5ArrayDump(dim(x), dimnames(x), type(x))
    write_to_dump(x, dump)
    invisible(as(dump, "HDF5ArraySeed"))
}
#' marker class for delayed assay content
#' @export
setClass("DelayedRangedSummarizedExperiment", contains="RangedSummarizedExperiment")

#' support for HDF5-backed SummarizedExperiment
#' @param x \code{\link[SummarizedExperiment]{SummarizedExperiment-class}} instance
#' @param file character string identifying target file to hold HDF5
#' @param name character string identifying dataset
#' @param \dots passed down
#' @aliases "writeHDF5Dataset-RangedSummarizedExperiment"
#' @export
setGeneric("writeHDF5Dataset", function(x, file, name, ...)
   standardGeneric("writeHDF5Dataset"))
setMethod("writeHDF5Dataset", "ANY", function(x, file, name, ...) {
   .writeHDF5Dataset(x, file, name, ...)
})
setMethod("writeHDF5Dataset", "RangedSummarizedExperiment", function(x, file, name, ...) {
   assaydat = assay(x)
   seed = .writeHDF5Dataset(assaydat, file, name, ...)
   seed = DelayedArray(seed)
   rownames(seed) = rownames(x)
   colnames(seed) = colnames(x)
   assay(x) = seed
   new("DelayedRangedSummarizedExperiment", x) # now the assay data is on disk
})

