#' Use a shiny gadget to query user on a GO category and return entrez ids
#' @import shiny
#' @import miniUI
#' @import dplyr
#' @importFrom magrittr %>%
#' @param src_o instance of src_organism in Organism.dplyr
#' @export
gobrowse = function (src_o, ont = "BP", BPini = "A-L", iniGO="abscission") 
{
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
  ui <- miniPage(gadgetTitleBar("Search for a GO category:"), 
                 miniContentPanel(selectInput("gosel", "GO:", godf$TERM, 
                                              selected = iniGO), dataTableOutput("tab")))
  server <- function(input, output, session) {
    output$tab <- renderDataTable({
      ans = godf[godf$TERM == input$gosel, ]
      curid = ans$GOID
      genes = (src_o %>% tbl("id_go") %>% filter(go == curid)) %>% 
        as.data.frame()
      jo = inner_join(tbl(src_o, "id_go"), tbl(src_o, "ranges_gene"))
      res = jo %>% filter(go==curid) %>% collect() %>% GenomicRanges::GRanges()
      as.data.frame(res)   
    })
    observeEvent(input$done, {
      ans = godf[godf$TERM == input$gosel, ]
      curid = ans$GOID
      genes = (src_o %>% tbl("id_go") %>% filter(go == curid)) %>% 
        as.data.frame()
      jo = inner_join(tbl(src_o, "id_go"), tbl(src_o, "ranges_gene"))
      res = jo %>% filter(go==curid) %>% collect() %>% GenomicRanges::GRanges()
      stopApp(res)
    })
  }
  runGadget(ui, server)
}
