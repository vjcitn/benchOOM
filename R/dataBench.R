#
# this is rapidly improvised code for benchmarking out-of-memory
# strategies, will write in current folder without checking
# for clobbering
#
#' @import rhdf5
#' @import ff
#' @import microbenchmark
#' @import data.table
#' @import RSQLite
#' @import bigmemory
.h5RoundTrip = function(x, chunkIn=c(1000,10), inLevel=0, intimes=1) {
#system("rm -rf ex_hdf5file.h5")
if (file.exists("ex_hdf5file.h5")) file.remove("ex_hdf5file.h5")
requireNamespace("rhdf5")
h5createFile("ex_hdf5file.h5")
h5createDataset("ex_hdf5file.h5", "x", c(nrow(x),ncol(x)),
   storage.mode = "double", chunk=chunkIn, level=inLevel)
mw = microbenchmark(h5write(x, "ex_hdf5file.h5", name="x"), times=intimes)
mr= microbenchmark(h5read("ex_hdf5file.h5", name="x"), times=intimes)
msel= microbenchmark(ysel <- h5read("ex_hdf5file.h5", name="x", index=list(4001:5000, 1:100)), times=intimes)
stopifnot(all.equal(ysel, x[4001:5000,]))
list(mwrite=mw, ingFull=mr, ing1K=msel, times=intimes, method="hdf5")
}

.ffRoundTrip = function(x, chunkIn=c(5000,10), inLevel=0, intimes=1) {
#system("rm -rf ex_ff.ff")
if (file.exists("ex_ff.ff")) file.remove("ex_ff.ff")
requireNamespace("ff")
dx = dim(x)
mw = microbenchmark({
  xff <- ff(vmode="double", dim=dx, filename="ex_ff.ff")
  xff[,] = x
  }, times=intimes)
mr= microbenchmark({
  suppressWarnings({
   yff <- as.ram(xff)
                  })}, times=intimes)
msel= microbenchmark({
  suppressWarnings({
   yff <- xff[4001:5000,]
                  })}, times=intimes)
stopifnot(all.equal(yff, x[4001:5000,]))
rm(yff)
delete(xff)
rm(xff)
list(mwrite=mw, ingFull=mr, ing1K=msel, times=intimes, method="ff")
}
.bmRoundTrip = function(x, intimes=1) {
#system("rm -rf ex_bm.bm ex_bm.desc")
if (file.exists("ex_bm.bm")) file.remove("ex_bm.bm")
if (file.exists("ex_bm.bm.desc")) file.remove("ex_bm.bm.desc")
requireNamespace("bigmemory")
dx = dim(x)
mw = microbenchmark({
  xbm = big.matrix(dx[1], dx[2], init=NA, backingfile="ex_bm.bm",
   descriptorfile="ex_bm.bm.desc")
  xbm[,] = x
  }, times=intimes)
mr = microbenchmark(xin <- xbm[,], times=intimes)
msel = microbenchmark({xin2 <- xbm[4001:5000,]}, times=intimes)
stopifnot(all.equal(xin2, x[4001:5000,]))
indl = list(3001:4000, 4001:5000)
rm(xbm)
gc()
if (file.exists("ex_bm.bm")) file.remove("ex_bm.bm")
if (file.exists("ex_bm.bm.desc")) file.remove("ex_bm.bm.desc")
list(mwrite=mw, ingFull=mr, ing1K=msel, times=intimes, method="bigmemory")
}


.slRoundTrip = function(x, intimes=1) {
#system("rm -rf ex_sqlite.sqlite")
if (file.exists("ex_sqlite.sqlite")) file.remove("ex_sqlite.sqlite")
Sys.sleep(1)
stopifnot(!file.exists("ex_sqlite.sqlite"))
requireNamespace("RSQLite")
con = dbConnect(SQLite(), "ex_sqlite.sqlite")
mw = microbenchmark({
  dbWriteTable(con, "x", data.frame(ind=1:nrow(x), x), overwrite=TRUE)
  }, times=intimes)
mr= microbenchmark(yff <- dbReadTable(con, "x"), times=intimes)
msel = microbenchmark(
   {tmp <- dbGetQuery(con, "select * from x where ind >= 4001 and ind <= 5000")},
   times=intimes
   )
dbRemoveTable(con, "x")
dbDisconnect(con)
list(mwrite=mw, ingFull=mr, ing1K=msel, times=intimes, method="sqlite")
}

.dtRoundTrip = function(x, intimes=1) {
#system("rm -rf ex_dt.rda")
if (file.exists("ex_dt.rda")) file.remove("ex_dt.rda")
Sys.sleep(1)
requireNamespace("data.table")
mw = microbenchmark({
  dtx = data.table(x)
  save(dtx, file="ex_dt.rda", compress=FALSE)
  }, times=intimes)
mr= microbenchmark(load("ex_dt.rda"), times=intimes)
# at this point dtx is available
msel = microbenchmark( tmp <- dtx[4001:5000,], times=intimes )
oo = as.matrix(tmp)
dimnames(oo) = NULL
stopifnot(all.equal(oo, x[4001:5000,]))
list(mwrite=mw, ingFull=mr, ing1K=msel, times=intimes, method="data.table")
}

getStats = function(times, ..., summstat = mean, rtfun=.h5RoundTrip) {
 a1 = lapply(1:times, function(z) rtfun(...))
 w = lapply(a1, "[[", "mwrite")
 r = lapply(a1, "[[", "ingFull")
 rsel = lapply(a1, "[[", "ing1K")
 ans = list(
   meth=a1[[1]]$method,
   wr=summstat(sapply(w, function(x)x[,"time"]/10^6)),
   ingFull=summstat(sapply(r, function(x)x[,"time"]/10^6)),
   ing1K=summstat(sapply(rsel, function(x)x[,"time"]/10^6))
   )
  data.frame(ans)
}
#' harness for out-of-memory benchmarking
#' @param NR numeric number of rows of matrix to process
#' @param NC numeric number of columns
#' @param times numeric, as used by \code{\link[microbenchmark]{microbenchmark}}, number of times to execute for averaging
#' @param inseed numeric, as used by \code{\link[base]{set.seed}}
#' @param methods a list of functions, each having arguments \code{x} and \code{intimes}, with \code{x}
#'     the matrix being processed and \code{intimes} to be passed to microbenchmark for \code{times}
#' @export
benchOOM = function(NR=5000, NC=100, times=5, inseed=1234,
  methods = list(.h5RoundTrip, .ffRoundTrip, .slRoundTrip, .dtRoundTrip, .bmRoundTrip)) {
nel = NR * NC
set.seed(inseed)
x = array(rnorm(nel), dim=c(NR,NC))
cbind(NR=NR, NC=NC, times=times, do.call(rbind,
    lapply(methods, function(z) getStats(times, x, rtfun=z))))
}
           
#' helper for creating a methodlist
#' @param methods a character vector with tags for available round trip methods
#' export
methodList = function(methods=c("hdf5", "bigm", "sqlite", "ff")) {
   allm = list(hdf5=.h5RoundTrip, bigm=.bmRoundTrip, sqlite=.slRoundTrip, ff=.ffRoundTrip, dt=.dtRoundTrip)
   allm[methods]
   }
