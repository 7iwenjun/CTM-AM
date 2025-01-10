library(shiny)
library(readxl)
library(base64enc)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .output-title {
        font-size: 18px; 
        font-weight: bold; 
        margin-bottom: 20px; 
      }
      .image-output {
        text-align: center; 
        margin-top: 20px;  
        margin-bottom: 20px; 
      }
      .image-output img {
        width: 100%; 
        height: auto; 
        max-height: 400px; 
        display: block; 
        margin: 0 auto; 
      }
      .table-title {
        font-size: 20px;  
        font-weight: bold; 
        margin-bottom: 10px; 
      }
    "))
  ),
  fluidRow(
    column(8, offset = 2, fileInput("file", "Select Files", accept = c(".xlsx", ".csv"), multiple = TRUE, width = "100%"))
  ),
  hr(),
  fluidRow(
    column(8, offset = 2, uiOutput("title_output")),
    column(8, offset = 2, uiOutput("active_compounds_output"))
  ),
  hr(),
  fluidRow(
    column(8, offset = 2, uiOutput("image_output"))
  ),
  hr(),
  fluidRow(
    column(8, offset = 2, div(class = "table-title", "List of active compounds")),
    column(8, offset = 2, tableOutput("table_output"))
  )
)

server <- function(input, output, session) {
  e_numbers <- reactiveVal()
  
  observeEvent(input$file, {  
    req(input$file)
    file_names <- input$file$name
    e_numbers_current <- sapply(file_names, function(x) {
      regmatches(x, regexpr("E\\d+", x))
    })
    e_numbers(e_numbers_current)
    output$title_output <- renderUI({
      HTML(paste0("<div class='output-title'>", 
                  paste(e_numbers_current, collapse = ", "), 
                  "</div>"))
    })
  })
  
  output$image_output <- renderUI({
    req(e_numbers())
    e_num <- e_numbers()[1]
    
    relative_path <- file.path("..", "figure", "6_Graph_TIC_active_peaks", paste0("TIC_", e_num, ".png"))
    image_path <- normalizePath(relative_path, mustWork = FALSE)
    
    print(paste("Working Directory:", getwd()))  
    print(paste("Relative Path:", relative_path))  
    print(paste("Absolute Path:", image_path))  
    
    if (file.exists(image_path)) {
      img_tag <- paste0('<img src="data:image/png;base64,', base64enc::base64encode(image_path), 
                        '" alt="E" />')
    } else {
      img_tag <- "<div>Image not found</div>"
    }
    
    HTML(paste0("<div class='image-output'>", img_tag, "</div>"))
  })
  
  observeEvent(input$file, {
    req(input$file)
    active_file <- read_excel(input$file$datapath[1])
    if (nrow(active_file) == 0 || all(is.na(active_file))) {
      output$table_output <- renderTable({
        data.frame(Warning = "No active compounds were detected in the herbal extract.")
      })
      return(NULL)
    }
    compound_names <- active_file$name
    mz_values <- active_file$S_value
    rt_values <- active_file$S_retention_time
    efficiency_values <- active_file$Efficiency
    rank_column <- active_file$Rank
    smiles_column <- active_file$smiles
    inchi_column <- active_file$InChI
    smiles_column[is.na(smiles_column) | smiles_column == "N/A"] <- sapply(inchi_column[is.na(smiles_column) | smiles_column == "N/A"], convert_inchi_to_smiles)
    unique_rows <- !duplicated(rank_column)
    min_length <- min(sum(unique_rows), 100)
    filtered_data <- data.frame(
      No. = 1:min_length,
      m.z = mz_values[unique_rows][1:min_length],  
      RT = rt_values[unique_rows][1:min_length],  
      Efficiency = format(round(efficiency_values[unique_rows][1:min_length], 4), nsmall = 4),  
      Compound_name = compound_names[unique_rows][1:min_length]  
    )
    structure_images <- sapply(smiles_column[unique_rows][1:min_length], function(smiles) {  
      if (!is.na(smiles) && smiles != "") {
        return(paste0('<img src="', generate_structure_image(smiles), '" height="100" alt="Structure not found"/>'))
      } else {
        return("NA")
      }
    })
    filtered_data$Structure <- structure_images
    output$table_output <- renderTable({
      filtered_data
    }, striped = TRUE, bordered = TRUE, hover = TRUE, sanitize.text.function = identity)
  })
}

shinyApp(ui = ui, server = server)
