#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


#Necessary Packages
library(shiny)
library(ggplot2)
library(interactions)
library(lme4)
library(lmerTest)
library(texreg)
library(DT)
library(dplyr)
library(rlang)
library(googledrive)
library(googlesheets4)
library(readxl)
library(table1)
library(knitr)
library(kableExtra)

#Load Global Data
rbd <- readRDS(""path/to/your/data.csv"")
variable_info <- read_excel(""path/to/your/data.xsl".xlsx")

# Predictors table
Predictors_vars <- data.frame(
  Variable = c("Wealth", "Education", "Gender", "Geographic Factor", "Boundaries", "Barriers", "ELF", "Village Size", 
               "Distance Variables", "Population Density", "LGPI Indicators", "Information Infrastructure", 
               "Health Infrastructure", "Social Capital"),
  Description = c("Wealth Level", "Education Level", "Gender of respondent", "District", 
                  "Need permission to trade land","The probability that Ethnicity would cause a problem",  
                  "Ethno-Linguistic Fractionalization measure", "Reported Size of the village in Categories (Big, Middle, Small)", 
                  "Distance to main roads (and highway) / health service", "Density of population in the area", 
                  "Indicators from LGPI dataset", "Availability and quality of information infrastructure", 
                  "Availability and quality of health infrastructure", "Indexes of social capital")
)

#Helper Functions
int.plot <- function(myfit,df,xAxisVar,legendVar,panelVar = NA,intClass = "ContCatCat"){
  #Create plot
  if(intClass == "ContCatCat"){
    p <- interact_plot(model = myfit, pred = !!sym(xAxisVar), modx = !!sym(legendVar),
                       mod2 = !!sym(panelVar),interval = TRUE, 
                       mod2.labels = levels(df[[panelVar]]),colors = c("black","grey","#475c6c"))+
      theme_bw()+
      theme(
        text=element_text(size=10),
        axis.text=element_text(size=10),
        plot.title=element_text(size=12),
        legend.text=element_text(size=10),
        legend.position = "right",
        axis.title.x=element_text(size=10))+
      xlab(xAxisVar)+
      ylab(formula(myfit)[[2]])+
      ggtitle(paste(formula(myfit)[[2]],"~",xAxisVar,"x",legendVar, "x",panelVar,sep = " "))
  }else if(intClass == "ContContCat"){
    p <- interact_plot(model = myfit, pred = !!sym(xAxisVar), modx = !!sym(legendVar),
                       mod2 = !!sym(panelVar),interval = TRUE, 
                       mod2.labels = levels(df[[panelVar]]),colors = c("black","grey","#475c6c"))+
      theme_bw()+
      theme(
        text=element_text(size=10),
        axis.text=element_text(size=10),
        plot.title=element_text(size=12),
        legend.text=element_text(size=10),
        legend.position = "right",
        axis.title.x=element_text(size=10))+
      xlab(xAxisVar)+
      ylab(formula(myfit)[[2]])+
      ggtitle(paste(formula(myfit)[[2]],"~",xAxisVar,"x",legendVar, "x",panelVar,sep = " "))
  }else if(intClass == "CatCatCat"){
    p <- cat_plot(myfit, 
                  pred = !!sym(xAxisVar),  # Categorical predictor on the x-axis
                  modx = !!sym(legendVar),  # First categorical moderator (for the legend)
                  mod2 = !!sym(panelVar),   # Second categorical moderator (for facets)
                  mod2.labels = levels(df[[panelVar]]),  # Labels for the facets
                  modx.labels = levels(df[[legendVar]]), # Labels for the legend
                  interval = TRUE,geom = "line",
                  colors = c("black","grey"),
                  dodge.width = 0.3,
                  point.shape = TRUE,
                  facet_grid = TRUE        # Facet grid for the third categorical variable
    ) +
      theme_bw() +
      theme(
        text = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "right",
        axis.title.x = element_text(size = 10)
      ) +
      xlab(xAxisVar) +
      ylab(as.character(formula(myfit)[[2]])) +
      ggtitle(paste(as.character(formula(myfit)[[2]]), "~", xAxisVar, "x", legendVar, "x", panelVar))
    
  }else if(intClass == "CatCat"){
    p <- cat_plot(myfit, 
                  pred = !!sym(xAxisVar),  # Categorical predictor on the x-axis
                  modx = !!sym(legendVar),  # First categorical moderator (for the legend)
                  modx.labels = levels(df[[legendVar]]), # Labels for the legend
                  interval = TRUE,geom = "line",
                  colors = c("black","grey"),
                  dodge.width = 0.3,
                  point.shape = TRUE,
    ) +
      theme_bw() +
      theme(
        text = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "right",
        axis.title.x = element_text(size = 10)
      ) +
      xlab(xAxisVar) +
      ylab(as.character(formula(myfit)[[2]])) +
      ggtitle(paste(as.character(formula(myfit)[[2]]), "~", xAxisVar, "x", legendVar))
  }else{
    p <- interact_plot(model = myfit, pred = !!sym(xAxisVar), modx = !!sym(legendVar),
                       ,interval = TRUE, 
                       colors = c("black","grey"))+
      theme_bw()+
      theme(
        text=element_text(size=10),
        axis.text=element_text(size=10),
        plot.title=element_text(size=12),
        legend.text=element_text(size=10),
        legend.position = "right",
        axis.title.x=element_text(size=10))+
      xlab(xAxisVar)+
      ylab(formula(myfit)[[2]])+
      ggtitle(paste(formula(myfit)[[2]],"~",xAxisVar,"x",legendVar,sep = " "))
  }
  
  #Return the Plot
  print(p)
}



##############################
# GOOGLE DRIVE SET UP ----
##############################

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "ResiliencyDashboard/.secrets"
)

feedback_sheet_id <- googledrive::drive_get("FeedbackSuggestions")$id
byom_sheet_id <- googledrive::drive_get("BYOM")$id
Infvis_sheet_id <- googledrive::drive_get("InfvisComments")$id
stats_sheet_id <- googledrive::drive_get("stats_model_comments")$id

function(input, output, session) {
  
  # HOMEPAGE INFO ----
  total_n <- nrow(rbd)
  caption_text <- sprintf("<b>Dataset Overview (Total Sample Size: %d)</b>", total_n)
  
  # Render the table with `table1`
  output$sample_table <- renderUI({
    sample_table <- table1(~ Surveys, data = rbd, overall = "Full Sample", caption = caption_text)
    HTML(sample_table)  # Convert to HTML for rendering in the UI
  })
  
    #---------------------------------------------------------------------------
    # STATISTICAL MODELS ----
    #---------------------------------------------------------------------------
  #Regression Model Specification
    # Formula-building function
  output$predictors_table <- renderUI({
    # Generate the table using kable with a caption
    table_html <- kable(Predictors_vars, "html", caption = "<b>Predictors Variables</b>") %>%
      kable_styling("striped", full_width = FALSE)  # Add optional styling
    
    HTML(table_html)  # Convert the table to HTML for rendering in UI
  })
  
  output$outcomevar_ui_reg <- renderUI({
    req(input$reg_commind)
    selected_subcategory <- NULL
    
    # Determine the selected subcategory based on the user's selections
    if (input$reg_commind == "Community") {
      selected_subcategory <- input$community_var_reg
    } else if (input$reg_commind == "Individual") {
      selected_subcategory <- input$individual_var_reg
    }
    
    # Filter the variable_info data based on the selected subcategory
    if (!is.null(selected_subcategory)) {
      filtered_data <- variable_info %>%
        filter(Subcategory == selected_subcategory) %>%
        pull(Name)
      
      # If there are no filtered results, display a placeholder message
      if (length(filtered_data) == 0) {
        return(selectInput("third_level_model", "Choose an outcome variable", choices = "No options available"))
      } else {
        # Use the filtered data as choices for the third-level dropdown
        return(selectInput("third_level_model", "Choose an outcome variable", choices = filtered_data))
      }
    }
  })


    
  # Revised selected_outcome_reg to return selected column name directly
  selected_outcome_reg <- reactive({
    req(input$third_level_model)
    input$third_level_model
  })
  
  build_formula <- function(dv, variable_info) {
    # Static predictors that do not require dynamic rounds
    static_predictors <- c("gender", "HighBarrier", "HighBoundary", "ELF_village", 
                           "VillageSize", "GPopDensity", "GdistH", "GdistR", 
                           "info_infrastructure", "Hlth_infrastructure", 
                           "HighBarrier:ELF_village", "(1|Village)")
    
    # Predictors with dynamic rounds (Education, wealth, CtrlAge, District)
    dynamic_vars <- c("Education", "wealth", "CtrlAge", "District")
    
    # Determine round suffix for dynamic variables based on selected outcome variable
    dynamic_predictors <- sapply(dynamic_vars, function(var) {
      round <- variable_info$Round[variable_info$Name == input$third_level_model]
      # round <- variable_info %>% 
      #   filter(Name == var) %>% 
      #   pull(Round) %>% 
      #   as.character()
      # 
      paste0(var, "_", round)  # Append the correct round suffix
    })
    
    # Combine predictors into formula text
    predictors <- c(dynamic_predictors, static_predictors)
    formula_text <- paste(dv, "~", paste(predictors, collapse = " + "))
    
    return(formula_text)
  }
  
  # Regression model and table output function
  mlm.models <- function(df, dv, formula_text) {
    # Run the model
    formula <- as.formula(formula_text)
    MM1 <- tryCatch({
      lmer(formula, data = df)
    }, error = function(e) {
      stop("Error fitting model: ", e$message)
    })
    
    # Extract and format results
    results <- as.data.frame(summary(MM1)$coefficients)
    results <- results %>%
      mutate(Significance = cut(`Pr(>|t|)`, breaks = c(0, 0.001, 0.01, 0.05, 1.1), 
                                labels = c("***", "**", "*", ""), right = FALSE))
    
    # Shiny-friendly table output (use DT for interactivity)
    if (requireNamespace("DT", quietly = TRUE)) {
      DT::datatable(results, 
                    options = list(pageLength = 5),
                    caption = paste("MLM Results:", dv, "(n =", nobs(MM1), ")")) %>%
        formatStyle('Significance', backgroundColor = styleEqual(c("***", "**", "*", ""), c("#79C9C6", "#A3D5D3", "#C1BBB5")))
    } else {
      kable(results, caption = paste("MLM Results:", dv, "(n =", nobs(MM1), ")")) %>%
        column_spec(1, background = ifelse(results$Significance != "", "#79C9C6", "#C1BBB5")) %>%
        kable_styling(latex_options = "HOLD_position", full_width = F)
    }
  }
  
  # Reactive expression to get the selected question's text and answer choices
  selected_question <- reactive({
    req(input$third_level_model)
    variable_info %>%
      filter(Name == input$third_level_model)
  })
  
  observeEvent(input$show_info_reg, {
    # Show question text and answer choices
    output$questionText_reg <- renderText({
      req(selected_question())
      selected_question()$`Question Text`
    })
    
    output$answerChoices_reg <- renderText({
      req(selected_question())
      selected_question()$`Stats/Value`
    })
    
    outcome_variable <- selected_outcome_reg()
    df <- rbd
    
    # Ensure that build_formula function is defined and works correctly
    formula_text <- build_formula(outcome_variable, variable_info)
    
    # Run the regression and render the table
    output$reg_table <- renderTable({
      mlm.models(df, outcome_variable, formula_text)
    })
  })
  
  # Comments
  stats_df <- reactiveVal(read_sheet(ss = stats_sheet_id, 
                                      sheet = "stats_model_comments"))
  
  stats_formula <- reactiveVal()
  # UPDATE SAVED RESULTS
  #----------------------
  observeEvent(input$saveResultsModel,{
    req(input$saveResultsModel)
    #Create Dataframe Entry
    model_data <- data.frame(Variable = input$third_level_model,
                              Person = input$person_model,
                              Notes = input$notes_model)
    #Update dataframe in Google Drive
    sheet_append(
      data = model_data,
      ss = stats_sheet_id,
      sheet = "stats_model_comments"
    )
    #Update the displayed dataframe
    stats_df(rbind(stats_df(),model_data))
  })
  
  
  # DISPLAY SAVED RESULTS
  #----------------------
  output$stats_model_comments <- renderDT({
    datatable(stats_df(),options = list(
      dom = 't',  # Removes search, pagination, show entries
      paging = FALSE,  # Disable pagination
      searching = FALSE,  # Disable searching
      info = FALSE,  # Disable table information
      scrollX = TRUE
    ))
  })
  
    #---------------------------------------------------------------------------
    # INFERENCE & VISUALIZATION ----
    #---------------------------------------------------------------------------
  output$outcomevar_ui <- renderUI({
    req(input$infvis_commind)
    selected_subcategory <- NULL
    
    # Determine the selected subcategory based on the user's selections
    if (input$infvis_commind == "Community") {
      selected_subcategory <- input$community_var
    } else if (input$infvis_commind == "Individual") {
      selected_subcategory <- input$individual_var
    }
    
    # Filter the variable_info data based on the selected subcategory
    if (!is.null(selected_subcategory)) {
      filtered_data <- variable_info %>%
        filter(Subcategory == selected_subcategory) %>%
        pull(Name)
      
      # If there are no filtered results, display a placeholder message
      if (length(filtered_data) == 0) {
        selectInput("third_level", "Choose an outcome variable", choices = "No options available")
      } else {
        # Use the filtered data as choices for the third-level dropdown
        selectInput("third_level", "Choose an outcome variable", choices = filtered_data)
      }
    }
  })
  
  # Reactive expression to get the selected outcome variable data
  selected_outcome_data <- reactive({
    req(input$third_level)
    selected_outcome <- input$third_level
    df <- rbd  
    df[[selected_outcome]]
  })
  
  # Reactive expression to get the selected question's text and answer choices
  selected_question <- reactive({
    req(input$third_level)
    selected_row <- variable_info[variable_info$Name == input$third_level, ]
    selected_row
  })
  
  observeEvent(input$show_info, {
    # Show question text and answer choices
    output$questionText <- renderText({
      req(selected_question())
      selected_question()$`Question Text`
    })
    
    output$answerChoices <- renderText({
      req(selected_question())
      selected_question()$`Stats/Value`
    })
    
    # Render the histogram for the selected binary outcome variable
    output$outcome_histogram <- renderPlot({
      # Ensure hist_data is numeric
      hist_data <- as.numeric(selected_outcome_data())
      
      # Create the histogram with 0 and 1 values
      hist_obj <- hist(hist_data, 
                       main = paste("Histogram of", input$third_level), 
                       xlab = input$third_level, 
                       col = "#79C9C6", 
                       border = "black", 
                       breaks = 2,  # Ensure only two bins (0 and 1)
                       xaxt = "n"   # Remove the default x-axis labels
      )
      
      # Add custom x-axis labels for 0 and 1
      axis(1, at = c(0, 1), labels = c("0", "1"))
      
      # Add counts on top of the bars
      text(x = hist_obj$mids, y = hist_obj$counts, 
           labels = hist_obj$counts, pos = 3, cex = 1, col = "black")
    })
    
    
    # Render the summary table for the selected binary outcome variable
    output$outcome_summary <- renderTable({
      # Create a frequency table for 0 and 1
      outcome_table <- table(selected_outcome_data(), useNA = "ifany")
      
      # Calculate counts and percentages
      df <- data.frame(
        Value = names(outcome_table),
        Count = as.integer(outcome_table),
        Proportion = scales::percent(as.numeric(outcome_table) / sum(outcome_table))  # Format as percentage
      )
      
      colnames(df) <- c("Value", "Count", "Proportion")  # Rename columns for clarity
      df
    })
    
    
    # Perform t-tests with the binary variables: Barrier, ELF-type, HighBoundary, Gender
    ttest_results <- reactive({
      outcome_var <- as.numeric(selected_outcome_data())
      ttest_barrier <- t.test(outcome_var ~ rbd$Barrier)
      ttest_elf <- t.test(outcome_var ~ rbd$ELFType)
      ttest_highboundary <- t.test(outcome_var ~ rbd$HighBoundary)
      ttest_gender <- t.test(outcome_var ~ rbd$gender)
      
      list(
        Barrier = ttest_barrier,
        ELFType = ttest_elf,
        HighBoundary = ttest_highboundary,
        gender = ttest_gender
      )
    })
    
    # Display t-test results as a table
    output$ttest_results <- renderTable({
      ttest_results_list <- ttest_results()
      
      p_values <- c(ttest_results_list$Barrier$p.value, 
                    ttest_results_list$ELFType$p.value, 
                    ttest_results_list$HighBoundary$p.value, 
                    ttest_results_list$gender$p.value)
      
      test_statistics <- c(ttest_results_list$Barrier$statistic, 
                           ttest_results_list$ELFType$statistic, 
                           ttest_results_list$HighBoundary$statistic, 
                           ttest_results_list$gender$statistic)
      
      # Holm adjustment for p-values
      adjusted_p_values <- p.adjust(p_values, method = "holm")
      
      variable_names <- c("Barrier", "ELF Type", "High Boundary", "Gender")
      conclusions <- ifelse(p_values < 0.05,
                            paste("The", variable_names, "is significantly different between the two groups"),
                            paste("The", variable_names, "shows no significant difference between the two groups"))
      
      result_table <- data.frame(
        Comparison = variable_names,
        `Test Statistic` = round(test_statistics, 3),
        `p-value` = round(p_values, 3),
        `Adjusted p-value (Holm)` = round(adjusted_p_values, 3),
        Conclusion = conclusions
      )
      result_table
    })
  })
  
  # Comments
  Infvis_df <- reactiveVal(read_sheet(ss = Infvis_sheet_id, 
                                      sheet = "InfvisComments"))
  
  Infvis_formula <- reactiveVal()
  # UPDATE SAVED RESULTS
  #----------------------
  observeEvent(input$saveResultsInfvis,{
    req(input$saveResultsInfvis)
    #Create Dataframe Entry
    Infvis_data <- data.frame(Variable = input$third_level,
                            Person = input$personInfvis,
                            Notes = input$notesInfvis)
    #Update dataframe in Google Drive
    sheet_append(
      data = Infvis_data,
      ss = Infvis_sheet_id,
      sheet = "InfvisComments"
    )
    #Update the displayed dataframe
    Infvis_df(rbind(Infvis_df(),Infvis_data))
  })


  # DISPLAY SAVED RESULTS
  #----------------------
  output$Infvis_comments <- renderDT({
    datatable(Infvis_df(),options = list(
      dom = 't',  # Removes search, pagination, show entries
      paging = FALSE,  # Disable pagination
      searching = FALSE,  # Disable searching
      info = FALSE,  # Disable table information
      scrollX = TRUE
    ))
  })
  
  # SOCIAL CAPITAL ----
  observeEvent(input$socialcapital, {
    
    # Determine which variable to use based on the selected index
    selected_variable <- switch(input$socialcapitalindex,
                                "Individual Bonding Capital (Reciprocity)" = "ind_bc1",
                                "Individual Bonding Capital (Engagement)" = "ind_bc2",
                                "Village Bonding Capital (Reciprocity)" = "vbc1",
                                "Village Bonding Capital (Engagement)" = "vbc2",
                                "Individual Linking Capital" = "ind_lsc1",
                                "Linking Capital (Local Authorities to Supra-local Authorities)" = "lsc2",
                                "Village Linking Capital" = "vlsc1",
                                "Strong Leaderships" = "vindstrongldr",
                                "Perceived Legitimate Leaderships" = "vpll"
    )
    
    # Render the density plot for the selected variable
    output$socialcapital_table <- renderPlot({
      req(selected_variable()  # Ensure a variable is selected
      )
      # Plot density for the selected variable
      ggplot(rbd, aes_string(x = selected_variable)) +
        geom_density(fill = "#79C9C6", alpha = 0.7) +
        labs(
          title = paste("Density Plot of", input$socialcapitalindex),
          x = input$socialcapitalindex,
          y = "Density"
        ) +
        theme_minimal()
    })
  })
  
  # SPECIAL T TEST ----
  #--------------------------
   observeEvent(input$run_ttest_special, {
    
    # Define the selected leader type based on radio button
    leader_type_special <- if (input$infvis_ttest_special == "Village Head") "VHelected" else "ACelected"
    
    output$ttest_results_special <- renderTable({
      
      # Perform t-tests based on selected leader type
      ttest_results_list_special <- list(
        ELF = if (leader_type_special == "VHelected") {
          t.test(ELF_village ~ VHelected, data = rbd)
        } else {
          t.test(ELF_village ~ ACelected, data = rbd)
        },
        GESI = if (leader_type_special == "VHelected") {
          t.test(GESI_village ~ VHelected, data = rbd)
        } else {
          t.test(GESI_village ~ ACelected, data = rbd)
        },
        VillageSize = if (leader_type_special == "VHelected") {
          t.test(VillageSize ~ VHelected, data = rbd)
        } else {
          t.test(VillageSize ~ ACelected, data = rbd)
        }
      )
      
      # Extract p-values and test statistics
      p_values_special <- sapply(ttest_results_list_special, function(x) x$p.value)
      test_statistics_special <- sapply(ttest_results_list_special, function(x) x$statistic)
      
      # Holm adjustment for p-values
      adjusted_p_values_special <- p.adjust(p_values_special, method = "holm")
      
      variable_names_special <- c("ELF", "GESI", "Village Size")
      conclusions_special <- ifelse(p_values_special < 0.05,
                            paste("The", variable_names_special, "is significantly different between the two groups"),
                            paste("The", variable_names_special, "shows no significant difference between the two groups"))
      
      # Format results into a data frame
      result_table_special <- data.frame(
        Comparison = variable_names_special,
        `Test Statistic` = round(test_statistics_special, 3),
        `p-value` = round(p_values_special, 4),
        `Adjusted p-value (Holm)` = round(adjusted_p_values_special, 4),
        Conclusion = conclusions_special
      )
      
      result_table_special
    })
  })

  
}
