#' @export
shine <- function(host = NULL, port = NULL) {
    require("shiny")
    require("cRomppass")

    if(! is.null(port)) {
        port <- as.numeric(port)
    }
    
    shinyApp(
        
        ui = fluidPage(
            sidebarLayout(
                sidebarPanel(
                    imageOutput("comppassLogo", height = "115px", width = "361px"),
                    fileInput("experimentUploader", "Experiments"),
                    fileInput("statsUploader", "External Stats"),
                    textInput("normFactor", "Normalization Factor", value = "0.98"),
                    div(id="input_instructions_div",
                        
                        h3("Input Instructions"),
                        downloadButton("download.sample.input", "Download Sample Input"),
                        br(),
                        p("The input file must be a tab-delimited text file with the following five columns:"),
                        withTags(
                            ol(li("Experiment ID"),
                               li("Replicate"),
                               li("Bait"),
                               li("Prey"),
                               li("Spectral Count")
                               )
                        ),
                        
                        h4("Input Field Details"),
                        withTags(dl(
                            dt(dfn("Experiment ID"),
                               dd("A unique ID for a given AP-MS experiment. Replicates will all share the same Experiment ID")
                               ),
                            dt(dfn("Replicate"),
                               dd("An ID for the replicate in a given experiment")
                               ),
                            dt(dfn("Experiment Type"),
                               dd("This field is ignored and is allowed for backward compatability with the Spotlite website.")
                               ),
                            dt(dfn("Bait"),
                               dd("The ID for the bait used in the experiment. This is usually the gene symbol.")
                               ),
                            dt(dfn("Prey"),
                               dd("The ID for the prey found in the experiment. This is also usually the gene symbol. This ID type should match that used for the Bait.")
                               ),
                            dt(dfn("Spectral Count"),
                               dd("The number of spectra found per bait-prey pair in an experimental replicate.")
                               )
                        )),

                        h3("Output Description"),
                        withTags(dl(
                            dt(dfn("Experiment.ID"),
                               dd("The unique ID for a given AP-MS experiment provided by the input")
                               ),
                            dt(dfn("Bait"),
                               dd("The unique ID for the bait in the AP-MS experiment provided by the input")
                               ),
                            dt(dfn("Prey"),
                               dd("The unique ID for the prey in the AP-MS experiment provided by the input")
                               ),
                            dt(dfn("AvePSM"),
                               dd("The mean spectral counts across all the replicates in an experiment provided by the input")
                               ),
                            dt(dfn("Z"),
                               dd("The Z-Score for the prey given all other experiments provided.")
                               ),
                            dt(dfn("WD"),
                               dd("The WD score defined by Sowa et al.")
                               ),
                            dt(dfn("Entropy"),
                               dd("The Shannon Entropy for each spectral count provided by the input")
                               )
                        ))
                    )
                ),
                mainPanel(uiOutput("download.scores.button"),
                          dataTableOutput("scores"))
            ),
            title = "CompPASS"
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

            output$download.scores.button <- renderUI({
                if(!is.null(current.scores())) {
                    return(downloadButton("download.scores", "Download"))
                }
            })

            output$download.scores <- downloadHandler(
                filename = function() {
                    if(is.null(current.scores())) {
                        return("#")
                    }
                    "comppass_scores.tsv"
                },
                content = function(file) {
                    if(is.null(current.scores())) {
                        return(NULL)
                    }
                    write.table(current.scores(), file,
                                sep = "\t", row.names = FALSE)
                }
            )

            output$download.sample.input <- downloadHandler(
                filename = function() { "comppass_test_data.tsv" },
                content = function(file) {
                    data(comppass_test_data)
                    write.table(comppass.test.data, file,
                                sep = "\t", row.names = FALSE)
                }
            )

            output$comppassLogo <- renderImage({
                filename <- normalizePath(system.file("extdata", "CompPASS_reflect_logo_smaller.png",
                                                      package = "cRomppass"))
                list(src = filename, alt = "CompPASS Logo")
            }, deleteFile = FALSE)

        },
        options = list(host = host, port = port)
    )
}
