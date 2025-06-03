library(shiny)
library(plotly)
library(bslib)
library(DT)
library(shinyWidgets)

# Replace with your data loading method
metadata <- metadata

# Define your color palette
lund_colors <- c(
  Uro = "#3cb44b",
  GU = "#4363d8",
  BaSq = "#CD2626",
  Mes = "#f58231",
  ScNE = "#A020F0",
  UroA = "#3cb44b",
  UroB = "#8B1A1A",
  UroC = "#006400"
)

# Set levels for Predictions_5classes
if ("subtype_5_class" %in% names(metadata)) {
  metadata$subtype_5_class <- factor(
    metadata$subtype_5_class,
    levels = c("Uro", "GU", "BaSq", "Mes", "ScNE")
  )
}

# Set levels for Predictions_7classes
if ("subtype_7_class" %in% names(metadata)) {
  metadata$subtype_7_class <- factor(
    metadata$subtype_7_class,
    levels = c("UroA", "UroB", "UroC", "GU", "BaSq", "Mes", "ScNE")
  )
}

# Variable categories
var_categories <- list(
  `Clinical Data` = c("clin_prog_tat1_event", "clin_prog_tat1_time", "clin_prog_pat_tat1_event", "clin_prog_pat_tat1_time", "clin_clinprog_fu_tat1_event", "clin_clinprog_fu_tat1_time", "bcg_any_event", "bcg_any_time", "bcg_adequate_event", "bcg_adequate_time", "gem_mmc_event", "gem_mmc_time", "time_to_primary_cystectomy", "progression_event", "time_to_progression", "primary_cystectomy_event_nmi", "all_cystectomy_event_nmi", "time_to_cystectomy_nmi", "recurrence_event", "time_to_recurrence", "primary_recurrence", "palliative", "pdd_turb_returb", "urine_cytology_pre_turb", "tnm", "primary_cystectomy", "neoadj_induction", "preop_chemo_type", "n_chemo_doses", "returb", "pad_returb_t_stage", "prostatic_urethra", "lvi", "t1_invasion_depth", "variant_histology", "bcg_instillations", "adjuvant_mmc", "only_palliative_treatment", "ypt", "ypn", "tma_grade_who16_as"),
  `Cohorts` = c("cohort", "seq_batch", "cohort_group"),
  `Descriptive` = c("age", "eau_risk_category", "eau_score", "hospital_turb", "gender", "smoker", "death_other_cause", "death_bladder_cancer"),
  `EAU Factors` = c("eau_over70", "eau_tumor_status", "eau_n_tumors", "eau_tumor_diam", "eau_stage", "eau_cis", "eau_who73", "eau_var_hist", "eau_prostatic_urethra", "eau_lvi"),
  `Molecular Signatures` = c("proliferation_score", "progression_score", "progression_risk", "mol_grade_2022_score", "mol_grade_2022", "mol_grade_1999_score", "mol_grade_1999", "stromal141_up", "immune141_up", "b_cells", "t_cells", "cd8_t_cells", "nk_cells", "t_cells", "neutrophils", "monocytic_lineage", "macrophages", "m2_macrophage", "myeloid_dc", "endothelial_cells", "fibroblasts", "smooth_muscle"),
  `Pathology` = c("multi_hist"),
  `Sequencing Metrics` = c("dna_ngul", "rna_ngul", "rin"),
  `Sample Sets` = c("set_719", "set_676", "set_hq_572", "set_hq_572_index_uc_533", "set_lq_147", "set_lq_147_index_uc_129", "set_rna_tma", "set_eau_risk", "set_clin_prog_tat1", "set_clin_prog_ta", "set_clin_prog_t1", "set_clin_prog_pat_tat1", "set_clin_prog_pat_ta", "set_clin_prog_pat_t1", "set_clin_clinprog_fu_tat1", "set_clin_clinprog_fu_ta", "set_clin_clinprog_fu_t1", "set_bcg_any", "set_bcg_adequate", "set_gem_mmc"),
  `Subtype Predictions` = c("subtype_5_class", "subtype_7_class", "Uro_score", "UroA_score", "UroB_score", "UroC_score", "GU_score", "BaSq_score", "Mes_score", "ScNE_score"),
  `Tumor Information` = c("stage", "node", "met", "grade", "n_tumors_cat", "tumor_size"),
  `Quality Control` = c("qc_removal", "category", "category_group")
)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  tags$head(
    tags$style(HTML("
      .main-title { font-size: 2em; font-weight: bold; margin-bottom: 10px; }
      .plot-container { background: #fff; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px #ccc; }
      .filter-title { font-size: 1.3em; font-weight: bold; margin-bottom: 10px; }
      .after-plot-text { margin-top: 15px; font-size: 1.1em; }
    "))
  ),
  fluidRow(
    column(
      width = 12,
      div("UROSCANSEQ Explorer", class = "main-title"),
      helpText("Use filters to subset your data. Select variables and plot type below.")
    )
  ),
  sidebarLayout(
    sidebarPanel(width = 5,
                 wellPanel(
                   div("Metadata Filters", class = "filter-title"),
                   actionBttn("add_filter", "Add Filter", style = "jelly", color = "default"),
                   tags$div(id = "filter_ui_container")
                 ),
                 hr(),
                 # Use var_categories for grouped variable selection
                 pickerInput("xvar", "X variable", choices = var_categories, selected = "subtype_5_class", 
                             options = list(`live-search` = TRUE)),
                 pickerInput("yvar", "Y variable (optional)", choices = c("None", var_categories), selected = "None", 
                             options = list(`live-search` = TRUE)),
                 pickerInput("color", "Color by (optional)", choices = c("None", var_categories), selected = "subtype_5_class", 
                             options = list(`live-search` = TRUE)),
                 radioGroupButtons("plot_type", "Plot type",
                                   choices = c("Histogram" = "hist", "Barplot" = "bar", "Boxplot" = "box", "Scatterplot" = "scatter"),
                                   selected = "hist", justified = TRUE
                 ),
                 prettySwitch("sort_x", "Sort/Ranks by X variable", value = FALSE, status = "info"),
                 downloadBttn("download_ids", "Download Sample IDs", style = "gradient", color = "primary")
    ),
    mainPanel(width = 7,
              tabsetPanel(
                tabPanel("Plot",
                         div(class = "plot-container", plotlyOutput("plot")),
                         div(class = "after-plot-text", textOutput("n_samples"))
                ),
                tabPanel("Table",
                         DT::dataTableOutput("table")
                )
              )
    )
  )
)

server <- function(input, output, session) {
  filter_count <- reactiveVal(0)
  filter_ids <- reactiveVal(character(0))
  
  observeEvent(input$add_filter, {
    id <- paste0("filter_", filter_count() + 1)
    insertUI(
      selector = "#filter_ui_container",
      ui = tags$div(
        id = id, # Unique id for the filter row
        fluidRow(
          column(3, pickerInput(paste0(id, "_cat"), "Category", choices = names(var_categories), width = "100%")),
          column(5, uiOutput(paste0(id, "_var_ui"))),
          column(4, uiOutput(paste0(id, "_val_ui"))), # wider column for slider
          column(2, actionBttn(paste0(id, "_remove"), "Remove", style = "simple", color = "danger", size = "sm"))
        )
      ),
      immediate = TRUE
    )
    filter_count(filter_count() + 1)
    filter_ids(c(filter_ids(), id))
  })
  
  # Update variable choices based on category
  observe({
    lapply(filter_ids(), function(id) {
      output[[paste0(id, "_var_ui")]] <- renderUI({
        cat_input <- input[[paste0(id, "_cat")]]
        if (is.null(cat_input)) return(NULL)
        pickerInput(paste0(id, "_var"), "Variable", choices = var_categories[[cat_input]], width = "100%",
                    options = list(`live-search` = TRUE))
      })
    })
  })
  
  # Remove filter UI (now removes the entire filter row)
  observe({
    lapply(filter_ids(), function(id) {
      observeEvent(input[[paste0(id, "_remove")]], {
        removeUI(selector = paste0("#", id))
        filter_ids(setdiff(filter_ids(), id))
      }, ignoreInit = TRUE)
    })
  })
  
  # Dynamically generate value selectors for each filter
  observe({
    lapply(filter_ids(), function(id) {
      output[[paste0(id, "_val_ui")]] <- renderUI({
        var_input <- input[[paste0(id, "_var")]]
        if (is.null(var_input)) return(NULL)
        vals <- metadata[[var_input]]
        if (is.numeric(vals)) {
          rng <- range(vals, na.rm = TRUE)
          sliderInput(
            paste0(id, "_val"), "Range",
            min = rng[1], max = rng[2], value = rng, step = diff(rng)/100,
            width = "100%" # Make slider wide
          )
        } else {
          pickerInput(paste0(id, "_val"), "Value(s)", choices = unique(vals), multiple = TRUE, width = "100%",
                      options = list(`live-search` = TRUE))
        }
      })
    })
  })
  
  # Reactive filtered data
  data_filtered <- reactive({
    df <- metadata
    for (id in filter_ids()) {
      var <- input[[paste0(id, "_var")]]
      val <- input[[paste0(id, "_val")]]
      if (!is.null(var) && !is.null(val)) {
        if (is.numeric(metadata[[var]])) {
          df <- df[df[[var]] >= val[1] & df[[var]] <= val[2], , drop = FALSE]
        } else if (length(val) > 0) {
          df <- df[df[[var]] %in% val, , drop = FALSE]
        }
      }
    }
    df
  })
  
  output$n_samples <- renderText({
    n <- nrow(data_filtered())
    paste("Samples after filtering:", n)
  })
  
  output$plot <- renderPlotly({
    df <- data_filtered()
    x <- input$xvar
    y <- if (input$yvar != "None") input$yvar else NULL
    color <- if (input$color != "None") input$color else NULL
    
    # Sorting logic for ranked plot
    if (input$sort_x && !is.null(x) && is.numeric(df[[x]])) {
      df <- df[order(df[[x]], decreasing = TRUE), , drop = FALSE]
      if (input$plot_type %in% c("bar", "box", "scatter")) {
        df[[x]] <- factor(df[[x]], levels = unique(df[[x]]))
      }
    }
    
    # Use custom colors for Predictions_5classes or Predictions_7classes
    color_map <- if (!is.null(color) && color %in% c("subtype_5_class", "subtype_7_class")) lund_colors else NULL
    
    if (input$plot_type == "hist") {
      plot_ly(
        df, x = ~get(x),
        color = if (!is.null(color)) ~get(color) else NULL,
        colors = color_map,
        type = "histogram"
      )
    } else if (input$plot_type == "bar") {
      plot_ly(
        df, x = ~get(x),
        color = if (!is.null(color)) ~get(color) else NULL,
        colors = color_map,
        type = "bar"
      )
    } else if (input$plot_type == "box") {
      plot_ly(
        df, x = ~get(x), y = ~get(y),
        color = if (!is.null(color)) ~get(color) else NULL,
        colors = color_map,
        type = "box"
      )
    } else if (input$plot_type == "scatter") {
      plot_ly(
        df, x = ~get(x), y = ~get(y),
        color = if (!is.null(color)) ~get(color) else NULL,
        colors = color_map,
        type = "scatter", mode = "markers"
      )
    }
  })
  
  output$table <- DT::renderDataTable({
    data_filtered()
  }, options = list(pageLength = 7))
  
  output$download_ids <- downloadHandler(
    filename = function() {
      paste0("sample_ids_", Sys.Date(), ".txt")
    },
    content = function(file) {
      writeLines(as.character(data_filtered()$sample_id), file)
    }
  )
}

shinyApp(ui, server)