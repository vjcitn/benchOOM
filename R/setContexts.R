#' establish an (ordered) list of hashes or functions that translate symbols
#' @param conlist a list of functions or named vectors; no validity checking
#' @export
setContexts = function( conlist ) {
assign(".AnnoContexts", conlist, .GlobalEnv)
}

#' translate a token using annotation context
#' @param sym a symbol to be resolved
#' @export
resolve = function( sym ) {
 conl = get(".AnnoContexts", .GlobalEnv)
 if (is.null(conl)) return(NA)
 for (i in 1:length(conl)) {
   if (is(conl[[i]], "vector")) {
     stopifnot(is.character(names(conl[[i]])))
     if (sym %in% names(conl[[i]])) return(conl[[i]][sym])
     }
   if (is(conl[[i]], "function")) return(conl[[i]](sym))
   }
 return(NA)
}
 
