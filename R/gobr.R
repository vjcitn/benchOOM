#' Use a shiny gadget to query user on a GO category and return entrez ids
#' @import shiny
#' @import miniUI
#' @import DT
#' @import dplyr
#' @import magrittr
#' @import AnnotationDbi
#' @param src instance of src_organism in Organism.dplyr
#' @export
gobrowse <- function(src) {
  require(GO.db)
  require(Organism.dplyr)
  gok = keys(GO.db)
  godf = AnnotationDbi::select(GO.db, keys=gok, columns=c("GOID", "ONTOLOGY", "TERM"))
  godf = godf[order(godf$TERM),]

  ui <- miniPage(
    gadgetTitleBar("Search for a GO category:"),
    miniContentPanel(
      selectInput("gosel", "GO:", godf$TERM[1:10000], selected=godf$TERM[1]),
      dataTableOutput("tab")
    )
  )

  server <- function(input, output, session) {

    # Render the plot
    output$tab <- renderDataTable({
      # Plot the data with x/y vars indicated by the caller.
      ans = godf[ godf$TERM == input$gosel, ]
      curid = ans$GOID
      genes = (src %>% tbl("id_go") %>% filter(go==curid)) %>% as.data.frame()
      genes
    })

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      ans = godf[ godf$TERM == input$gosel, ]
      curid = ans$GOID
      genes = (src %>% tbl("id_go") %>% filter(go==curid)) %>% as.data.frame()
      genes
      stopApp(genes)
    })
  }

  runGadget(ui, server)
}
