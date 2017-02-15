#' Use a shiny gadget to query user on a GO category and return entrez ids
#' @import shiny
#' @import miniUI
#' @import DT
#' @import dplyr
#' @import magrittr
#' @import AnnotationDbi
#' @param src instance of src_organism in Organism.dplyr
#' @export
gobrowse = function (src, ont = "BP", BPini = "A-L") 
{
  require(GO.db)
  require(Organism.dplyr)
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
  ui <- miniPage(gadgetTitleBar("Search for a GO category:"), 
                 miniContentPanel(selectInput("gosel", "GO:", godf$TERM, 
                                              selected = godf$TERM[1]), dataTableOutput("tab")))
  server <- function(input, output, session) {
    output$tab <- renderDataTable({
      ans = godf[godf$TERM == input$gosel, ]
      curid = ans$GOID
      genes = (src %>% tbl("id_go") %>% filter(go == curid)) %>% 
        as.data.frame()
      jo = inner_join(tbl(src, "id_go"), tbl(src, "ranges_gene"))
      res = jo %>% filter(go==curid) %>% collect() %>% GenomicRanges::GRanges()
      
    })
    observeEvent(input$done, {
      ans = godf[godf$TERM == input$gosel, ]
      curid = ans$GOID
      genes = (src %>% tbl("id_go") %>% filter(go == curid)) %>% 
        as.data.frame()
      jo = inner_join(tbl(src, "id_go"), tbl(src, "ranges_gene"))
      res = jo %>% filter(go==curid) %>% collect() %>% GenomicRanges::GRanges()
      stopApp(res)
    })
  }
  runGadget(ui, server)
}
