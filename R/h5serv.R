#' prototypical query to HDF5 server established by Landmark Bioinfo (Shweta Gopaulakrishnan, reshg@channing.harvard.edu, Vince Carey, stvjc@channing.harvard.edu) 
#' @description There is no guarantee of availability of this service.
#' @import rPython
#' @param dsn internal HDF5 name of dataset
#' @param rows vector of row selection, only min and max are used to define a range, all data in range retrieved
#' @param cols vector of column selection, only min and max are used to define a range, all data in range retrieved
#' @param h5id name of HDF5 resource as identified by server
#' @param endpoint URL for server
#' @note You will need python h5pyd support installed, see \url{https://github.com/HDFGroup/h5pyd}
#' @examples
#' h5serv()
#' @export
h5serv = function(dsn="assay001", rows=1:3, cols=1:5, h5id = "assays.hdfgroup.org", 
                   endpoint = "http://54.163.220.201:5000") {
 require("rPython")
 tf1 = tempfile()
 open(f1 <- file(tf1), "wb")
 writeLines("import h5pyd as h5py\n", tf1)
 close(f1)
 python.load(tf1)
 python.exec("v = h5py.version.version")
 vers = try( python.get("v") )
 if (inherits(vers, "try-error")) stop("can't run version check for h5pyd")
 message(paste("running h5pyd version", vers))
 tf2 = tempfile()
 open(f2 <- file(tf2), "wb")
 rowspec = paste(min(rows)-1, ":", max(rows), sep="")
 colspec = paste(min(cols)-1, ":", max(cols), sep="")
 comm = paste("import h5pyd as h5py\ncurf = h5py.File(\"", h5id, "\", \"r\", endpoint=\"",
   endpoint, "\")\nndat=curf['", dsn, "']\ndat=ndat[", colspec, ",", rowspec, "].tolist()\n", sep="")
 writeLines(comm, tf2)
 close(f2)
 python.load(tf2)
 do.call(cbind, python.get("dat"))
}
