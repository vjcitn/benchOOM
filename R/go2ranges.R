#' given a GO term, obtain a GRanges on the basis of a src_organism and GO.db
#' @import GO.db
#' @import Organism.dplyr
#' @param iniGO character string of length one, should match a TERM in GO.db for the selected ontology
#' @param src_o a src_organism instance
#' @param gobasis a character string naming a go-related tbl in src_o
#' @param ont a character string naming an ontology (MF, CC, BP)
#' @param BPini a character string, if "A-L", candidate terms must begin with a-l, otherwise must begin with a complementary letter (m-z, etc.)
#' @note the BPini and ont parameters are for controlling memory consumption, principally for shiny usage
#' @export
go2ranges = function (iniGO = "abscission", src_o, gobasis = "id_go_all", ont = "MF", BPini = "A-L") {
    require(GO.db)
    require(Organism.dplyr)
    stopifnot(inherits(src_o, "src_organism"))
    gok = keys(GO.db)
    godf = AnnotationDbi::select(GO.db, keys = gok, columns = c("GOID", 
        "ONTOLOGY", "TERM"))
    godf = godf[which(godf$ONTOLOGY == ont), ]
    if (ont == "BP") {
        tag = substr(tolower(godf$TERM), 1, 1)
        kp = which(tag %in% letters[1:12])
        if (BPini != "A-L") 
            kp = -kp
        godf = godf[kp, ]
    }
    godf = godf[order(godf$TERM), ]
    ans = godf[godf$TERM == iniGO, ]
    curid = ans$GOID
    jo = inner_join(tbl(src_o, gobasis), tbl(src_o, "ranges_gene"))
    jo %>% filter(goall == curid) %>% collect() %>% 
                GenomicRanges::GRanges()
}
