#' @author Shweta Gopaulakrishnan <reshg@channing.harvard.edu>
#' @param serverURL character including the port
#' @param host domain name of the data file on the server
#' @param times benchmarking time nevals
#' @param i character corresponding to row [e.g: "0:3"]
#' @param j character correspoding to coloumn [e,g: "0:3"]
#' @return a m1 with the microbenchmarking results [timings]
#' @title microbenchmark retrieval of banovich assay data from HDF Server. [running on ec2 instance and schion]
#' @examples  
#' \dontrun{
#'    serverURL = "http://54.163.220.201:5000"
#'    host = "assays.hdfgroup.org"
#'    micro_H5bano(serverURL = serverURL,host = host,times=5, i="0:3",j="0:3")
#' }
#' @export
#'
micro_H5bano <- function(serverURL = serverURL, host= host,times=times,i=i,j=j) {
  bano = getH5ShContent(serverURL = serverURL, host=host)
  bds = datasets(bano)
  m1 = microbenchmark(bds[i,j],times = times)
  m1
}

### Note: For the Banovich assays.h5 data:  
#for the ec2 instance : serverURL = http://54.163.220.201:5000, host= "assays.hdfgroup.org"
#for schion : serverURL : http://170.223.248.164:7248, host="assays.hdfgroup.org"

#1. aws instance 
#a. ) 0:3,0:3
#Unit: milliseconds
#expr      min       lq     mean       median       uq      max     neval
#bds[i, j] 57.50294 65.21327 72.14242 66.50131 83.39206 88.10251     5

#b.) 0:64,0:64
#Unit: milliseconds
#expr      min       lq     mean       median       uq      max    neval
#bds[i, j] 94.13976 116.9109 123.8612 124.9422 138.7258 144.5875     5

#2. schion
#a.) 0:3,0:3
#Unit: milliseconds
#expr      min       lq     mean   median       uq      max neval
#bds[i, j] 19.86382 20.07597 20.57503 20.43149 20.89383 21.61005     5

#b.)0:64,0:64
#Unit: milliseconds
#expr      min       lq     mean   median       uq      max neval
#bds[i, j] 66.37483 67.11739 73.53795 71.93652 73.73785 88.52317     5


