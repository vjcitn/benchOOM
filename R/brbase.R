
foo = function() {
 ont = "BP"
 BPini = "A-L"
 require(GO.db)
 require(Organism.dplyr)
src = src_organism(dbpath="~/hg19.sqlite")
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
 input = list(gosel="abscission")


 ans = godf[godf$TERM == input$gosel, ]
      curid = ans$GOID
      genes = (src %>% tbl("id_go") %>% filter(go == curid)) %>%
        as.data.frame()
      jo = inner_join(tbl(src, "id_go"), tbl(src, "ranges_gene"))
      res = jo %>% filter(go==curid) %>% collect() %>% GenomicRanges::GRanges()


}
