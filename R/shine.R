#' @export
shine <- function(port = NULL) {
    require("shiny")
    require("cRomppass")

    shinyApp(
        
        ui = fluidPage(
            titlePanel("CompPASS"),
            sidebarLayout(
                sidebarPanel(
                    fileInput("experimentUploader", "Experiments"),
                    fileInput("statsUploader", "External Stats"),
                    textInput("normFactor", "Normalization Factor", value = "0.98")
                ),
                mainPanel(downloadButton("download.scores", "Download"),
                          dataTableOutput("scores"))
            )
        ),
        
        server = function(input, output) {

            input.experiment <- reactive({
                indf <- input$experimentUploader
                if(is.null(indf)) {
                    return(NULL)
                }

                read.delim(indf[1, "datapath"], stringsAsFactors = FALSE)
            })

            input.stats <- reactive({
                indf <- input$statsUploader
                if(is.null(indf)) {
                    return(NULL)
                }

                read.delim(indf[1, "datapath"], stringsAsFactors = FALSE)
            })
            
            current.scores <- reactive({
                experiment <- input.experiment()
                stats <- input.stats()
                if(is.null(experiment)) {
                    return(NULL)
                }
                comppass(experiment, stats, norm.factor = as.numeric(input$normFactor))
            })
            
            
            output$scores <- renderDataTable({
                current.scores()
            })

            output$download.scores <- downloadHandler(
                filename = function() { "comppass_scores.tsv" },
                content = function(file) {
                    write.table(current.scores(), file,
                                sep = "\t", row.names = FALSE)
                }
            )
        },
        options = list("port" = as.numeric(port))
    )
}
