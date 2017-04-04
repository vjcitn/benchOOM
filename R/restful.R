#library(rjson)
#library(httr)

dataref = function(serverURL, host) {
paste0(serverURL, "/?host=", host)
}

setClass("H5ShContent", representation(
# "host" content
   serverURL="character",  # includes port
   host="character",
   hrefs="character",
   lastModified="character", created="character",
   root = "character"))

setClass("H5SDatasets", representation(dsuuid="character", attrs="list"), contains="H5ShContent")

setMethod("show", "H5ShContent", function(object) {
 self = object@hrefs["self"]
 cat("H5serv content for 'host'", object@host, "\n")
 cat(" at server", object@serverURL, "\n")
# cat(" use datasets() ...\n")
})

setMethod("show", "H5SDatasets", function(object) {
 cat("H5SDatasets, (1 of ", length(object@dsuuid), "): ", object@dsuuid[1], "\n", sep="")
 cat("derived from :\n")
 callNextMethod()
})

#' extract dataset UUID and attributes from H5ShContent
#' @param object instance of H5ShContent
#' @param \dots not used yet
#' @export
#' @exportMethod datasets
setGeneric("datasets", function(object, ...) standardGeneric("datasets"))
setMethod("datasets", "H5ShContent", function(object,...) {
 target = paste0(.serverURL(object), "/datasets?host=", .host(object))
 lis = fromJSON(readBin(GET(target)$content, w="character"))
 ds = new("H5SDatasets", object, dsuuid=lis$datasets)
 atts = dsAttrs(ds)
 ds@attrs=atts@attrs
 ds
})

# should write (unexported?) methods here
.serverURL = function(x) x@serverURL
.host = function(x) x@host
.root = function(x) x@root
.dsuuid = function(x, which=1) x@dsuuid[which]
setGeneric("dsAttrs", function(x, ...) standardGeneric("dsAttrs"))
setClass("H5DatasetAttributes", representation(attrs="list"), contains="H5SDatasets")
setMethod("show", "H5DatasetAttributes", function(object) {
  cat(class(object), "instance\n")
  callNextMethod()
})
setMethod("dsAttrs", "H5SDatasets", function(x, which=1, ...) {
  target = paste0(.serverURL(x), "/datasets/", .dsuuid(x, which), 
      "/attr2?host=", .host(x))
  acon = GET(target)
  ans = fromJSON(readBin(acon$content, what="character"))
  new("H5DatasetAttributes", attrs=ans, x)
})
  

#' acquire a list of REST content for an HDF5 server query, wrap in H5ShContent ('host' content)
#' @import httr
#' @import rjson
#' @param serverURL a character string giving the URL including port number on which HDF5 server is listening
#' @param host character
#' @examples
#' # this is data served by HDF group
#' tall = getH5ShContent( serverURL = "https://data.hdfgroup.org:7258", host="tall.data.hdfgroup.org")
#' tall
#' tds = datasets(tall)
#' tds
#' tds["0:3", "0:3"]
#' tds[c(1,3),]
#' tds[,c(1,3)]
#' tds[c(1,3),c(1,3)]
#' @export
getH5ShContent = function(serverURL, host) {
 stopifnot(length(serverURL)==1, length(host)==1)
 stopifnot(is(c(serverURL, host), "character"))
 basic = GET( dataref( serverURL, host ) )
 ans = fromJSON( readBin( basic$content, what = "character" ) )
 h = ans$hrefs
 nms = sapply(h, function(x) x[[2]])
 hr = sapply(h, function(x) x[[1]])
 names(hr) = nms
 new("H5ShContent", serverURL=serverURL, host=host, 
         hrefs=hr, lastModified=ans$lastModified,
         created=ans$created, root=ans$root)
}

setMethod("[", c("H5SDatasets", "character", "character"), function (x, i, j, ..., drop = TRUE) 
 {
 target = paste0(.serverURL(x), "/datasets/", .dsuuid(x), "/value?host=", .host(x), "&select=[", i, ",", j, "]")
 val = GET(target)
 ans = fromJSON( readBin( val$content, what="character" ) )
 (do.call(rbind, ans$value))
 })

setMethod("[", c("H5SDatasets", "character", "missing"), function (x, i, j, ..., drop = TRUE) 
 {
 dims = dsAttrs(x)@attrs$shape$dims
 colind = paste0("0:", as.integer(dims[2]))
 target = paste0(.serverURL(x), "/datasets/", .dsuuid(x), "/value?host=", .host(x), 
       "&select=[", i, ",", colind, "]")
 val = GET(target)
 ans = fromJSON( readBin( val$content, what="character" ) )
 (do.call(rbind, ans$value))
 })

setMethod("[", c("H5SDatasets", "missing", "character"), function (x, i, j, ..., drop = TRUE) 
 {
 stopifnot(length(j)==1)
 dims = dsAttrs(x)@attrs$shape$dims
 rowind = paste0("0:", as.integer(dims[1]))
 target = paste0(.serverURL(x), "/datasets/", .dsuuid(x), "/value?host=", .host(x), 
       "&select=[", rowind, ",", j, "]")
 val = GET(target)
 ans = fromJSON( readBin( val$content, what="character" ) )
# there may be a bug, or semantics of retrieving a column are
# different from those of retrieving a row.  a row retrieval comes
# back as an R list, a column retrieval comes back as a vector
# we need a list 
 if (!is.list(ans$value)) ans$value = list(ans$value)
 (do.call(rbind, ans$value))
 })

#trivial tuple generator
tupleGen = function(x) {
 y = x-1
 paste(y,x,1,sep=":")
}

setMethod("[", c("H5SDatasets", "numeric", "missing"), function (x, i, j, ..., drop = TRUE) {
  rowtups = tupleGen(i)
  do.call(rbind, lapply(rowtups, function(r) x[r, ]))
})

setMethod("[", c("H5SDatasets", "missing", "numeric"), function (x, i, j, ..., drop = TRUE) {
  coltups = tupleGen(j)
# retrieval of columns has different semantics
  t(do.call(rbind, lapply(coltups, function(thecol) x[, thecol ])))
})

setMethod("[", c("H5SDatasets", "numeric", "numeric"), function (x, i, j, ..., drop = TRUE) {
#
# some optimization could be done here to retrieve by column
# or row first prior to filtering the other index in R
#
# for now just retrieve rows and then filter in R
#
  rowtups = tupleGen(i)
  rowsel = do.call(rbind, lapply(rowtups, function(r) x[r, ]))
  rowsel[,j,drop=drop]
})
