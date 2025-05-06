library(tidyverse)
library(shiny)
library(DT)
library(shinydashboard)
library(shinyjs)
library(shinybusy)            # Adds spinner for when shiny is busy - doesn't include map render wait, for which I have another spinner
library(Buddle)               # for CheckNonNumeric

IntroductoryText.Input = readLines("./TextFiles/IntroductoryText.html")
OriginData = read.csv("./AttributionOrigin.csv")
PopLinkDataDF = read.csv("./LSOA11_LSOA21PopLookups.csv")
HHLinkDataDF = read.csv("./LSOA11_LSOA21HHLookups.csv")


  # Define UI for data upload app ----
  ui <- fluidPage(
    # Need Shinyjs for using tags$span to change colour of fileinput text
    useShinyjs(),
    
    # Set title as seen on webpage tab
    title ="LSOA2011 Attribution",
    
    # Set title at top of visible page----
    titlePanel(p("Attribute LSOA11 Counts, Percents, Proportions or Scores to LSOA21 Geography",style = "color:#3474A7;font-size:80%")),
    
    add_busy_spinner(spin = "pixel",color= 	"red", position = "bottom-left", margins = c(100,100)), 
    
    # Define sidebar layout
    sidebarLayout(
    
      # Introductory text above the sidebar/mainPanel split   
      fluidRow(
        style = "margin: 5px 45px 5px 15px;",
        htmlOutput("IntroductoryText"),
      ),
      
      # Sidebar and main panel within an outer fluidRow
      fluidRow(
        # Sidebar panel for inputs ----
        sidebarPanel(width = 4,
          fluidRow(
            # The user selects a file on their own PC
            div(id="FileSelectDiv", style = "height:90px",
                fileInput("files", tags$span(style="color:#0000ff;","(1) Select file (1st col must be 'LSOA21CD')"), multiple = FALSE, accept = c(".csv"))
            ),
          ),
          fluidRow(
            # The user chooses the data columns to be attributed to 2021 LSOAs
            div(style = "height:5px;"),
            tags$span(style="color:#0000ff; font-weight: bold;","(2) Select columns to be attributed:"),
            div(style = "height:5px;"),
            
            # The user clicks button to select the columns
            actionButton("SelectColButton", label = HTML("Select Columns"), 
                         style="color: #ffffff; background-color: #337ab7; border-color: #2e6da4"),
            div(style = "height:5px;")
          ),
          fluidRow(height = 200,
           # Feedback to user to confirm selected data columns
           htmlOutput("ColumnSelectionIntro"),
           htmlOutput("ColumnSelection"),
           div(style = "height:15px;")
          ),
          fluidRow(
            # Sub-title of section where choose data type, weighting method and whether output to be rounded to integer
            div(style = "height:5px;"),
            tags$span(style="color:#0000ff; font-weight: bold;","(3) Select data type, method and output:")
          ),
          fluidRow(
            # User selects what sort of data is to be attributed (i.e. counts, percents, proportions or scores)
            column(width = 5,
                   #style = "background-color:#bfbfbf;",
                   align = "left",
                   radioButtons("Select.Conversion.Type",
                                width="100%", 
                                h5(HTML("<strong>Data Type</strong>")),
                                choices = list("Counts" = 1,
                                               "Percents (0-100)" = 2,
                                               "Proportions (0-1)" = 3,
                                               "Scores" = 4),
                                selected = 1)
            ),
            # User selects whether attribution to 2021 LSOAs is to use person or household weightings 
            column(width = 4,
                   #style = "background-color:#bfbfbf;",
                   align = "left",
                   radioButtons("Select.Weighting",
                                width="100%", 
                                h5(HTML("<strong>Weight by:</strong>")),
                                choices = list("Persons" = 1,
                                               "Households" = 2),
                                selected = 1)
            ),
            # User selects whether or not output to be rounded to integers (will not apply to proportions or percents, which are 6 and 4dp respectively) 
            column(width = 3,
                   #style = "background-color:#bfbfbf;",
                   align = "left",
                   radioButtons("Select.Rounding",
                                width="100%", 
                                h5(HTML("<strong>Output:</strong>")),
                                choices = list("Integer" = 1,
                                               "Decimal" = 2),
                                selected = 1)
            )
          ),
          fluidRow(
            # The action button to initiate attribution
            actionButton("AttributeButton", label = HTML("Attibute to 2021 LSOAs"), 
                         style="color: #ffffff; background-color: #337ab7; border-color: #2e6da4"),
            div(style = "height:15px;"),
            htmlOutput("ReportForUser")
          )
         ),
      
        # Main panel for displaying file (as table), the attributed data filename, and final information and instructions
        mainPanel(width = 8,
          fluidRow(style="background:#F5F5F5",
            # Selected 2011 data as table ----
            box(width=12, style = "padding: 1px 1px 1px 1px; margin: 1px 1px 1px -15px;
                                      align: left;  height: 500px; overflow-y: scroll;", 
                div(DTOutput("tbl_out"),style="font-size:80%")
            ) # end of box
          ),
          fluidRow(
            # Test display of attributed file name 
            div(style = "height:15px;"),
            column(width = 1,
                   div(style = "height:10px;"),
                   textOutput("OutputFileNameIntro")
            ),
            
            column(width = 7,
                   align = "left",
                   verbatimTextOutput("OutputFileName")
                   ),
            column(width = 4,
                   align = "left",
                   div(style = "height:3px;"),
                   uiOutput("mybutton")
             )
          ),
          fluidRow(
             column(width = 12,
                div(style = "margin-left: 0px;",
                  htmlOutput("OverwriteWarning")
                )
            )
          )
        )
        
      ) # end of outer fluidrow
    )   # end of sidebarLayout
  )     # end of ui

  
# Server ############################################################################################
  
  server <- function(input, output) {
    
    
    output$IntroductoryText = renderUI(HTML(IntroductoryText.Input))
    
    # Define reactive variables
    TriggerOnStart = reactiveVal(1)
    TriggerColumnSelectionProblem = reactiveVal(1)
    TextOut = reactiveVal(NULL)
    Col1Name = reactiveVal(NULL)
    FileLength = reactiveVal(NULL)
    RunningReport = reactiveVal(NULL)
    ProceedCheck = reactiveVal(0)
    ReadyToAttribute = reactiveVal(0)
    DownloadFileName = reactiveVal(NULL)
    DownloadOutput = reactiveVal(NULL)
    ReadyToDownload = reactiveVal(0)

    # Obtain name of filename with 2011 data to be attributed to 2021 geography
    TargetFile <- reactive({
      validate(need(input$files != "", "Select file..."))
      
      if (is.null(input$files)) {
        return(NULL)
        ReadyToDownload(0)
      } else {
        ReadyToDownload(0)
        path_list <- as.list(input$files$datapath)

        df <- read.csv(input$files$datapath)
        #print("# 188: Input file follows:")
        #print(head(df))
        Col1Name(names(df)[1])
        FileLength(nrow(df))
        RunningReport("Follow steps above in sequence")
        
        output$ReportForUser <- renderText({
          RunningReport()
        })
        
        output$OutputFileName <- renderText({
          "Nothing created yet!"
        })
        
        return(df)
      }
    })
    
    # Test whether input file suitable and report (and set ProceedCheck()) 
    observeEvent(TargetFile(),{
      req(!is.na(Col1Name()))
      #print(paste0("# 206 Col1Name() =",Col1Name()))
      #print(paste0("# 207 FileLength() =",FileLength()))
      
      if (Col1Name() != "LSOA11CD" | FileLength() != 32844){
        ProceedCheck(0)
         if (Col1Name() != "LSOA11CD" & FileLength() != 32844) {
          Addon = "<span style = color:#ff0000><b>The selected file not suitable because<br>the 1st column not titled 'LSOA11CD' &<br>it does not contain exactly 32844 rows.</b></span>"
        } else if (Col1Name() != "LSOA11CD") {
          Addon = "<span style = color:#ff0000><b>The selected file not suitable because<br>the 1st column is not titled 'LSOA11CD'.</b></span>"
        } else {
          Addon = "<span style = color:#ff0000><b>The selected file not suitable because<br>it does not contain exactly 32844 rows.</b></span>"
        }
        output$ColumnSelection <- renderText({
          Addon
        })
        
      } else {
        RunningReport("<span style = color:#0B6623><b>OK: Selected file suitable for attribution</b></span>")
        ProceedCheck(1)
        output$ReportForUser <- renderText({
          RunningReport()
        })
        output$ColumnSelection <- renderText({
          "<span style = color:#0B6623><b>Awaiting choice:</b></span>"
        })
        
      }
    })
    
    # Render selected file as table
    output$tbl_out <- renderDT({
      datatable(TargetFile(),selection = list(target = 'column'))
     })
    
    # Text output actions when programs first starts
    observeEvent(TriggerOnStart(),{
      output$ColumnSelectionIntro <- renderText({
        "<span style = color:#000000><strong><em>Selected Columns are:</em></strong></span>"
      })
      output$ColumnSelection <- renderText({
        "Awaiting choice:"
      })
      output$OutputFileNameIntro <- renderText({
        "Output:"
      })
      output$OutputFileName <- renderText({
        "Nothing created yet!"
      })
      
      RunningReport("Follow steps above in sequence")
      output$ReportForUser <- renderText({
        RunningReport()
      })

    })

    # Actions when Select Columns button clicked - print out names of selected columns
    observeEvent(input$SelectColButton,{
       # Actions depend on whether columns have been selected
      #print(paste0("# 265: input$tbl_out_columns_selected = ", input$tbl_out_columns_selected))
      #print(paste0("# 266: ProceedCheck() = ", ProceedCheck()))
      #print("# 270: Input file follows:")
      #print(head(TargetFile()))
      
      TestExtract = TargetFile() %>% select(all_of(input$tbl_out_columns_selected))
      #print("TestExtract follows:")
      #print(head(TestExtract))
      #print((paste0("# 276: ProceedCheck() = ",ProceedCheck())))
      
      
      ReadyToDownload(0)
      
      if (ProceedCheck() == 1) {
        
        if (length(names(TestExtract))>0) {
          
          #Check for non-numeric data and report
           
          NumericTestResult = sum(unlist(CheckNonNumeric(TestExtract)))
          #Check for NA data and report
          NATestResult = nrow(na.omit(TestExtract))
          #print(paste0("# 278: NumericTestResult = ",NumericTestResult))
          #print(paste0("# 279: NATestResult = ",NumericTestResult))
          
          if (NumericTestResult != 0 | NATestResult != 32844){
            output$ReportForUser <- renderText({
              "<span style = color:#ff0000><strong>Data problem: NAs or non-numeric in selected columns!</strong></span>"
            })
            TriggerColumnSelectionProblem(TriggerColumnSelectionProblem()*-1)
            ReadyToAttribute(0)
            ReadyToDownload(0)
          } else {
            # If select - an ugly hack to display names nicely
            TextBit = names(TargetFile())[input$tbl_out_columns_selected]
            TextBitLength = length(TextBit)
            
            #print(paste0("# 274: TextBit = ",TextBit))
            #print(paste0("# 275: TextBitLength = ",TextBitLength))
            
            TextOut(paste0(TextBit[1],"<br>"))
            if (TextBitLength > 1) {
              for (j in 2:TextBitLength){
                TextOut(paste0(TextOut(),TextBit[j],"<br>"))
              }
            }
            TextOut(paste0("<span style = color:#0B6623><b>",TextOut(),"</b></span>"))
            ReadyToAttribute(1)
          }
        } else {
          # If nothing selected - tell user what to do
          TextOut("<span style = color:#ff0000><strong>Nothing selected! Click table to choose.</strong></span>")
          ReadyToAttribute(0)
          ReadyToDownload(0)
        }
        
        #print("# 291: Rendering TextOut()")
        output$ColumnSelection <- renderText({
          # Render text produced above - either list of selected variables or 'Nothing selected'
          TextOut()
        })
        
        } else {
          TextOut("<span style = color:#ff0000><strong>File not suitable - choose another!!</strong></span>")
         ReadyToDownload(0)
          output$ColumnSelection <- renderText({
            TextOut()
          })
          
        }
    })
  
    
    ############################################################################################################
    ############################################################################################################
    
    # Actions when 'Attrubute to 2011' button clicked - this does the actual attribution
    observeEvent(input$AttributeButton,{
      # Temporary output to console
      #print(paste0("#311: files = ",input$files[1]))
      #print(paste0("#312: Selected columns are:",input$tbl_out_columns_selected))
      #print((paste0("#313: Select.Conversion.Type = ",input$Select.Conversion.Type)))
      #print((paste0("#315: Select.Weighting = ",input$Select.Weighting)))
      #print((paste0("#316: Select.Rounding = ",input$Select.Rounding)))
      #print((paste0("#317: ProceedCheck() = ",ProceedCheck())))
      
     # If have selected input$Select.Conversion.Type == 1 & input$Select.Weighting == 1
      if (ProceedCheck() == 1){
        if (ReadyToAttribute() == 1){
          ReadyToDownload(1) 
          
          if (input$Select.Conversion.Type == 1) {                                                  # This is for counts
            #print("# 324: input$Select.Conversion.Type == 1")
            # Extract data from input file - using selected columns (NOTE Col 1 must be 'LSOA11CD')
            InCountData2011 = TargetFile()[,c(1,input$tbl_out_columns_selected)]
            #print("# 328: InCountData2011 follows:")
            #print(head(InCountData2011))
            
            # Define top-end of range for loop (depends on number of data columns being processed)
            Range = ncol(InCountData2011)
            # Loop through all columns and attribute to 2021 LSOAs
            for (i in 2:Range) { 
              # Sets up CountData2011 as 2 columns: LSOA11CD and DataCol[i]
              CountData2011 = InCountData2011[,c(1,i)]
              KeepName = names(CountData2011)[2]
              names(CountData2011)[2] = "Count"
              
              # Run some checks
              #Test3 = CountData2011 %>% filter(is.na(Count)) %>% select(LSOA11CD) %>% pull(.) %>% length(.) # This must be 0 (i.e. no NA)
              #Test4 = CountData2011 %>% filter(Count<0) %>% select(LSOA11CD) %>% pull(.) %>% length(.) # This must be 0 (i.e. no negative numbers)
              #Test5 = CountData2011 %>% mutate(Country = substr(LSOA11CD,1,1)) %>% select(Country) %>% unique(.) # This must be 1 row and must == "E"
  
              # Read in LSOA11 to LSOA21 Populations link data
              if (input$Select.Weighting == 1){
                LinkDataDF = PopLinkDataDF
              } else {
                LinkDataDF = HHLinkDataDF
              }
              
              # Deal with all unchanged, merges and splits - i.e. AllNotComplex
              AllNotComplex = LinkDataDF %>% filter(Category != "X")
              
              if (input$Select.Rounding == 1) {
                Part1 = AllNotComplex %>% left_join(CountData2011, by = "LSOA11CD") %>%
                mutate(NewCount = Prop21of11 * Count) %>% group_by(LSOA21CD) %>% summarise(Count = round(sum(NewCount),0))
              } else {
                Part1 = AllNotComplex %>% left_join(CountData2011, by = "LSOA11CD") %>%
                  mutate(NewCount = Prop21of11 * Count) %>% group_by(LSOA21CD) %>% summarise(Count = sum(NewCount))
              }
              
              # Deal with the Complex set
              AllComplex = LinkDataDF %>% filter(Category == "X")
              
              if (input$Select.Rounding == 1) {
                Part2 = AllComplex %>% left_join(CountData2011, by = "LSOA11CD") %>%
                  mutate(NewCount = Prop21of11 * Count) %>% group_by(LSOA21CD) %>% summarise(Count = round(sum(NewCount),0))
              } else {
                Part2 = AllComplex %>% left_join(CountData2011, by = "LSOA11CD") %>%
                  mutate(NewCount = Prop21of11 * Count) %>% group_by(LSOA21CD) %>% summarise(Count = sum(NewCount))
              }
              
              Output = rbind(Part1,Part2)
              
              # Do some final checks
              #nrow(Output) # This should be 33755
              #sum(Output$Count) # This should be equal to next (unless rounding error)
              #sum(CountData2011$Count)
              
              names(Output)[2] = KeepName
              
              # If all tests passed then can either create or left_join to FinalOutput (cbind would probably work, but just in case order changes use left_join)
              if (i == 2){
                FinalOutput = OriginData %>% left_join(Output, by="LSOA21CD")
              } else {
                FinalOutput = FinalOutput %>% left_join(Output, by = "LSOA21CD")
              }
              
            } # end of loop through all columns of data to be converted
            
             DownloadOutput(FinalOutput)
             #print("# 392: Count data > DownloadOutput() follows:")
             #print(paste0(head(DownloadOutput())))
             
  
            if (input$Select.Weighting == 1 & input$Select.Rounding == 1){
              #print("# 397: when > input$Select.Weighting == 1 & input$Select.Rounding == 1")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>population-based</b></span><span style = color:#0B6623> weighting (</b></span><span style = color:#ff0000><b>integer output</b></span><span style = color:#0B6623>)</b><br>
                            <b>Attribution of 2011 </b></span><span style = color:#ff0000><b>count data</b></span><span style = color:#0B6623> to 2021 LSOAs successful</b><br></span>
                            <span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"__CountsAttributedToLSOA21(Pop_Int).csv")
              DownloadFileName(OutputFileName)
            } else if (input$Select.Weighting == 1 & input$Select.Rounding == 2){
              #print("# 404: when > input$Select.Weighting == 1 & input$Select.Rounding == 2")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>population-based</b></span><span style = color:#0B6623> weighting (</b></span><span style = color:#ff0000><b>decimal output</b></span><span style = color:#0B6623>)</b><br>
                            <b>Attribution of 2011 </b></span><span style = color:#ff0000><b>count data</b></span><span style = color:#0B6623> to 2021 LSOAs successful</b><br></span>
                            <span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"__CountsAttributedToLSOA21(Pop_Decimal).csv")
              DownloadFileName(OutputFileName)
            } else if (input$Select.Weighting == 2 & input$Select.Rounding == 1){
              #print("# 411: when > input$Select.Weighting == 2 & input$Select.Rounding == 1")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>household-based</b></span><span style = color:#0B6623> weighting (</b></span><span style = color:#ff0000><b>integer output</b></span><span style = color:#0B6623>)</b><br>
                            <b>Attribution of 2011 </b></span><span style = color:#ff0000><b>count data</b></span><span style = color:#0B6623> to 2021 LSOAs successful</b><br></span>
                            <span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"__CountsAttributedToLSOA21(HH_Int).csv")
              DownloadFileName(OutputFileName)
            } else if (input$Select.Weighting == 2 & input$Select.Rounding == 2){
              #print("# 418: when > input$Select.Weighting == 2 & input$Select.Rounding == 2")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>household-based</b></span><span style = color:#0B6623> weighting (</b></span><span style = color:#ff0000><b>decimal output</b></span><span style = color:#0B6623>)</b><br>
                            <b>Attribution of 2011 </b></span><span style = color:#ff0000><b>count data</b></span><span style = color:#0B6623> to 2021 LSOAs successful</b><br></span>
                            <span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"__CountsAttributedToLSOA21(HH_Decimal).csv")
              DownloadFileName(OutputFileName)
            }
            output$ReportForUser <- renderText({
              RunningReport()
            })
            
            output$OutputFileName <- renderText({
              OutputFileName
            })
            
            
            output$mybutton <- renderUI({
              #print("# 435")
              #print(paste0("ProceedCheck() = ",ProceedCheck()))
              #print(paste0("ReadyToAttribute() = ",ReadyToAttribute()))
              #print(paste0("DownloadFileName() = ",DownloadFileName()))
              #print(paste0("ReadyToDownload() = ",ReadyToDownload()))
              
              if (ReadyToDownload() == 1 & ProceedCheck() == 1) {
                downloadButton("downloadData", "Download", style="color: #ffffff; background-color: #228B22; border-color: #ff0000")
              } else {
                actionButton("dummybutton","Download",icon = icon("download"))
              }
            })
            
          } 
          
          else if (input$Select.Conversion.Type == 2 | input$Select.Conversion.Type == 3) {          # This is for Percents (0 - 100) & Proportions (0 - 1)
            #print("# 451: input$Select.Conversion.Type == 2 | input$Select.Conversion.Type == 3")
            # Read in the percent data (0-100)
            InPercentData2011 = TargetFile()[,c(1,input$tbl_out_columns_selected)]
            
            #print("# 455: InPercentData2011 follows:")
            #print(head(InPercentData2011))
            DataColumns = ncol(InPercentData2011)
            ExtraCol = DataColumns + 1
            #print(paste0("459: ExtraCol =", ExtraCol))
            
            # Read in LSOA11 to LSOA21 Pops/HHs (and weightings) as required and convert to counts
            if (input$Select.Weighting == 1){
              LinkDataDF = PopLinkDataDF
             } else {
              LinkDataDF = HHLinkDataDF
             }
            LSOA11Totals = LinkDataDF %>% select(LSOA11CD,LSOA11Denom) %>% group_by(LSOA11CD) %>% summarise(LSOA11Denom = first(LSOA11Denom)) %>% arrange(LSOA11CD)
            LSOA21Totals = LinkDataDF %>% select(LSOA21CD,LSOA21Denom) %>% group_by(LSOA21CD) %>% summarise(LSOA21Denom = first(LSOA21Denom)) %>% arrange(LSOA21CD)
                                                                                                                                                                                                            
            # Convert to counts
            InCountData2011 = InPercentData2011 %>% left_join(LSOA11Totals, by ="LSOA11CD")
            Mult = InCountData2011 %>% select(all_of(ExtraCol)) %>% pull(.)
            
            if (input$Select.Conversion.Type == 2) {
              if (DataColumns == 2) {
                CopyName = names(InCountData2011)[2]
                InCountData2011 = InCountData2011 %>% mutate(Product = (.[[2]] * .[[3]])/100) %>% select(all_of(c(1,4)))
                names(InCountData2011)[2] = CopyName
              } else {
                InCountData2011 = InCountData2011 %>% mutate((InCountData2011[,c(2:DataColumns)] * Mult)/100) %>% select(all_of(c(1:DataColumns)))
              }
            } else {
              if (DataColumns == 2) {
                CopyName = names(InCountData2011)[2]
                InCountData2011 = InCountData2011 %>% mutate(Product = .[[2]] * .[[3]]) %>% select(all_of(c(1,4)))
                names(InCountData2011)[2] = CopyName
              } else {
                InCountData2011 = InCountData2011 %>% mutate(InCountData2011[,c(2:DataColumns)] * Mult) %>% select(all_of(c(1:DataColumns)))
              }
            }
            
            # Extract data from input file - using selected columns (NOTE Col 1 must be 'LSOA11CD')
            
            # Define top-end of range for loop (depends on number of data columns being processed)
            Range = ncol(InCountData2011)
            # Loop through all columns and attribute to 2021 LSOAs
            for (i in 2:Range) { 
              # Sets up CountData2011 as 2 columns: LSOA11CD and DataCol[i]
              CountData2011 = InCountData2011[,c(1,i)]
              KeepName = names(CountData2011)[2]
              names(CountData2011)[2] = "Count"
              
              # Run some checks
              #Test3 = CountData2011 %>% filter(is.na(Count)) %>% select(LSOA11CD) %>% pull(.) %>% length(.) # This must be 0 (i.e. no NA)
              #Test4 = CountData2011 %>% filter(Count<0) %>% select(LSOA11CD) %>% pull(.) %>% length(.) # This must be 0 (i.e. no negative numbers)
              #Test5 = CountData2011 %>% mutate(Country = substr(LSOA11CD,1,1)) %>% select(Country) %>% unique(.) # This must be 1 row and must == "E"
               
              # Deal with all unchanged, merges and splits - i.e. AllNotComplex
              AllNotComplex = LinkDataDF %>% filter(Category != "X")
              
              Part1 = AllNotComplex %>% left_join(CountData2011, by = "LSOA11CD") %>%
                                      mutate(NewCount = Prop21of11 * Count) %>% group_by(LSOA21CD) %>% summarise(Count = sum(NewCount))

              # Deal with the Complex set
              AllComplex = LinkDataDF %>% filter(Category == "X")
              
              Part2 = AllComplex %>% left_join(CountData2011, by = "LSOA11CD") %>%
                                    mutate(NewCount = Prop21of11 * Count) %>% group_by(LSOA21CD) %>% summarise(Count = sum(NewCount))
            
              Output = rbind(Part1,Part2)
              
              # Do some final checks
              #print(paste0("# 522: nrow(Output) = ", nrow(Output))) # This should be 33755
              #print(paste0("# 523: sum(Output$Count) = ",sum(Output$Count))) # This should be equal to next (unless rounding error)
              #print(paste0("# 527: sum(CountData2011$Count) = ",sum(CountData2011$Count)))
              
              # Set back to percents
              Output = Output %>% left_join(LSOA21Totals, by="LSOA21CD")
              if (input$Select.Conversion.Type == 2) {
                Output[,2] = round((Output[,2] / Output[,3]) * 100,4)  # Note that this calculates percents to 4 decimal places
               } else {
                Output[,2] = round((Output[,2] / Output[,3]),6)  # Note that this calculates proportions to 6 decimal places
              }
              Output = Output %>% select(LSOA21CD,Count) %>% arrange(LSOA21CD)
              
              
              names(Output)[2] = KeepName
              
              # If all tests passed then can either create or left_join to FinalOutput (cbind would probably work, but just in case order changes use left_join)
              if (i == 2){
                FinalOutput = OriginData %>% left_join(Output, by="LSOA21CD")
              } else {
                FinalOutput = FinalOutput %>% left_join(Output, by = "LSOA21CD")
              }
              
            } # end of loop through all columns of data to be converted
            
            DownloadOutput(FinalOutput)
            #print("# 549: Percent/Proportion data > DownloadOutput() follows:")
            #print(paste0(head(DownloadOutput())))
            
            
            if (input$Select.Conversion.Type == 2 & input$Select.Weighting == 1){
              #print("# 554: when > input$Select.Conversion.Type == 2 & input$Select.Weighting == 1")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>population-based</b></span><span style = color:#0B6623><b> weighting<br>
                            Attribution of 2011 </b></span><span style = color:#ff0000><b>percent</b></span><span style = color:#0B6623><b> data to 2021 LSOAs successful</b><br>
                            <span style = color:#0B6623><b>[Note that output will be decimal (4dp) even if integer selected.]</b></span><br>
                            </span><span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"_PercentsAttributedToLSOA21(Pop).csv")
              DownloadFileName(OutputFileName)
            } else if (input$Select.Conversion.Type == 2 & input$Select.Weighting == 2){
              #print("# 562: when > input$Select.Conversion.Type == 2 & input$Select.Weighting == 2")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>household-based</b></span><span style = color:#0B6623><b> weighting<br>
                            Attribution of 2011 </b></span><span style = color:#ff0000><b>percent</b></span><span style = color:#0B6623><b> data to 2021 LSOAs successful</b><br>
                           <span style = color:#0B6623><b>[Note that output will be decimal (4dp) even if integer selected.]</b></span><br>
                            </span><span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"_PercentsAttributedToLSOA21(HH).csv")
              DownloadFileName(OutputFileName)
            } else if (input$Select.Conversion.Type == 3 & input$Select.Weighting == 1){
              #print("# 570: when > input$Select.Conversion.Type == 3 & input$Select.Weighting == 1")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>population-based</b></span><span style = color:#0B6623><b> weighting<br>
                            Attribution of 2011 </b></span><span style = color:#ff0000><b>proportion</b></span><span style = color:#0B6623><b> data to 2021 LSOAs successful</b><br>
                           <span style = color:#0B6623><b>[Note that output will be decimal (6dp) even if integer selected.]</b></span><br>
                            </span><span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"_ProportionsAttributedToLSOA21(Pop).csv")
              DownloadFileName(OutputFileName)
            } else if (input$Select.Conversion.Type == 3 & input$Select.Weighting == 2){
              #print("# 578: when > > input$Select.Conversion.Type == 3 & input$Select.Weighting == 1")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>household-based</b></span><span style = color:#0B6623><b> weighting<br>
                            Attribution of 2011 </b></span><span style = color:#ff0000><b>proportion</b></span><span style = color:#0B6623><b> data to 2021 LSOAs successful</b><br>
                           <span style = color:#0B6623><b>[Note that output will be decimal (6dp) even if integer selected.]</b></span><br>
                            </span><span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"_ProportionsAttributedToLSOA21(HH).csv")
              DownloadFileName(OutputFileName)
            }
              
            output$ReportForUser <- renderText({
              RunningReport()
            })
            
            output$OutputFileName <- renderText({
              OutputFileName
            })
            
            output$mybutton <- renderUI({
                if (ReadyToDownload() == 1 & ProceedCheck() == 1) {
                  downloadButton("downloadData", "Download", style="color: #ffffff; background-color: #228B22; border-color: #ff0000")
                } else {
                  actionButton("dummybutton","Download",icon = icon("download"))
                }
            })
            
          } 
          
          else if (input$Select.Conversion.Type == 4) {                                                    # This is the section for scores (e.g. IMD)
            
            # Extract data from input file - using selected columns (NOTE Col 1 must be 'LSOA11CD')
            InScoreData2011 = TargetFile()[,c(1,input$tbl_out_columns_selected)]
 
            # Define top-end of range for loop (depends on number of data columns being processed)
            Range = ncol(InScoreData2011)
            # Loop through all columns and attribute to 2021 LSOAs
            for (i in 2:Range) { 
              # Sets up ScoreData2011 as 2 columns: LSOA11CD and DataCol[i]
              ScoreData2011 = InScoreData2011[,c(1,i)]
              KeepName = names(ScoreData2011)[2]
              names(ScoreData2011)[2] = "Score"
              
              # Run some checks
              #Test3 = ScoreData2011 %>% filter(is.na(Score)) %>% select(LSOA11CD) %>% pull(.) %>% length(.) # This must be 0 (i.e. no NA)
              #Test5 = ScoreData2011 %>% mutate(Country = substr(LSOA11CD,1,1)) %>% select(Country) %>% unique(.) # This must be 1 row and must == "E"
              
              # Read in LSOA11 to LSOA21 Populations link data
              if (input$Select.Weighting == 1){
                LinkDataDF = PopLinkDataDF
              } else {
                LinkDataDF = HHLinkDataDF
              }
              
              # Deal with all unchanged, merges and splits - i.e. AllNotComplex
              AllNotComplex = LinkDataDF %>% filter(Category != "X")
              
              Part1 = AllNotComplex %>% left_join(ScoreData2011, by = "LSOA11CD") %>%
                mutate(NewScore = Prop11of21 * Score) %>% group_by(LSOA21CD) %>% summarise(Score = sum(NewScore))
              
              # Deal with the Complex set
              AllComplex = LinkDataDF %>% filter(Category == "X")
              
              # Note Rounding isn't applied to scores
              Part2 = AllComplex %>% left_join(ScoreData2011, by = "LSOA11CD") %>%
                mutate(NewScore = Prop11of21 * Score) %>% group_by(LSOA21CD) %>% summarise(Score = sum(NewScore))

              Output = rbind(Part1,Part2)
              
              # Do some final checks
              #nrow(Output) # This should be 33755
              #sum(Output$Score) # This should be equal to next (unless rounding error)
              #sum(ScoreData2011$Score)
              
              names(Output)[2] = KeepName
              
              # If all tests passed then can either create or left_join to FinalOutput (cbind would probably work, but just in case order changes use left_join)
              if (i == 2){
                FinalOutput = OriginData %>% left_join(Output, by="LSOA21CD")
              } else {
                FinalOutput = FinalOutput %>% left_join(Output, by = "LSOA21CD")
              }
              
            } # end of loop through all columns of data to be converted
            
            DownloadOutput(FinalOutput)
            #print("# 662: Score data > DownloadOutput() follows:")
            #print(paste0(head(DownloadOutput())))
            
            
            if (input$Select.Weighting == 1 & input$Select.Rounding == 1){
              #print("# 667: when > input$Select.Weighting == 1 & input$Select.Rounding == 1")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>population-based</b></span><span style = color:#0B6623><b> weighting (</b></span><span style = color:#ff0000><b>integer output</b></span><span style = color:#0B6623>)</b></span><br>
                            <span style = color:#ff0000><b>[Are you really sure you want interger output for scores?]</b></span><br>
                            <span style = color:#0B6623><b>Attribution of 2011 </b></span><span style = color:#ff0000><b>score</b></span><span style = color:#0B6623><b> data to 2021 LSOAs successful</b></span><br>
                            <span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"__ScoresAttributedToLSOA21(Pop_Int).csv")
              DownloadFileName(OutputFileName)
            } else if (input$Select.Weighting == 1 & input$Select.Rounding == 2){
              #print("# 675: when > input$Select.Weighting == 1 & input$Select.Rounding == 2")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>population-based</b></span><span style = color:#0B6623><b> weighting (</b></span><span style = color:#ff0000><b>decimal output</b></span><span style = color:#0B6623>)</b></span><br>
                            <span style = color:#0B6623><b>Attribution of 2011 </b></span><span style = color:#ff0000><b>score</b></span><span style = color:#0B6623><b> data to 2021 LSOAs successful</b></span><br>
                            <span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"__ScoresAttributedToLSOA21(Pop_Decimal).csv")
              DownloadFileName(OutputFileName)
            } else if (input$Select.Weighting == 2 & input$Select.Rounding == 1){
              #print("# 682: when > input$Select.Weighting == 2 & input$Select.Rounding == 1")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>household-based</b></span><span style = color:#0B6623><b> weighting (</b></span><span style = color:#ff0000><b>integer output</b></span><span style = color:#0B6623>)</b></span><br>
                            <span style = color:#ff0000><b>[Are you really sure you want interger output for scores?]</b></span><br>
                            <span style = color:#0B6623><b>Attribution of 2011 </b></span><span style = color:#ff0000><b>score</b></span><span style = color:#0B6623><b> data to 2021 LSOAs successful</b></span><br>
                            <span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"__ScoresAttributedToLSOA21(HH_Int).csv")
              DownloadFileName(OutputFileName)
            } else if (input$Select.Weighting == 2 & input$Select.Rounding == 2){
              #print("# 690: when > input$Select.Weighting == 2 & input$Select.Rounding == 2")
              RunningReport("<span style = color:#0B6623><b>Using </b></span><span style = color:#ff0000><b>household-based</b></span><span style = color:#0B6623><b> weighting (</b></span><span style = color:#ff0000><b>decimal output</b></span><span style = color:#0B6623>)</b></span><br>
                            <span style = color:#0B6623><b>Attribution of 2011 </b></span><span style = color:#ff0000><b>score</b></span><span style = color:#0B6623><b> data to 2021 LSOAs successful</b></span><br>
                            <span style = color:#00008B><b>Double check the settings are correct for selected data<br>OTHERWISE THE RESULTS WILL BE INCORRECT!!</b></span>")
              OutputFileName = paste0(str_sub(input$files[1],1,-5),"__ScoresAttributedToLSOA21(HH_Decimal).csv")
              DownloadFileName(OutputFileName)
            }
            

            output$ReportForUser <- renderText({
              RunningReport()
            })
            
            output$OutputFileName <- renderText({
              OutputFileName
            })
            
 
            output$mybutton <- renderUI({
                if (ReadyToDownload() == 1 & ProceedCheck() == 1) {
                  downloadButton("downloadData", "Download", style="color: #ffffff; background-color: #228B22; border-color: #ff0000")
               } else {
                  actionButton("dummybutton","Download",icon = icon("download"))
                }
            })
            
          }
          else {  # end of if input$Select.Conversion.Type == 1 & input$Select.Weighting == 1
            RunningReport("<span style = color:#ff0000><br><b>Selected options not available yet!</b></span>")
            output$ReportForUser <- renderText({
              RunningReport()
            })
          }
        } else {
          RunningReport("<span style = color:#ff0000><b>You must select valid columns</b></span>")
          output$ReportForUser <- renderText({
            RunningReport()
          })
          
        }
        } else {
          RunningReport("<span style = color:#ff0000><b>File not selected or not suitable for attribiution!</b></span>")
          output$ReportForUser <- renderText({
            RunningReport()
          })
        }
    })
    
    # Respond to change in columns selected
    observeEvent(input$tbl_out_columns_selected,{
      req(ProceedCheck() == 1 & ReadyToAttribute() == 1)
      #print("741: Column choice changed!!!!!!!!!!!!!!!!!!!!!!!!!")
      ReadyToDownload(0)
      
      output$ColumnSelection <- renderText({
        "<span style = color:#0B6623><b>Awaiting choice:</b></span>"
      })
      
      RunningReport("Settings have changed - click 'Attribute' button for new output")
      
      output$ReportForUser <- renderText({
        RunningReport()
      })
      
      output$OutputFileName <- renderText({
        "You need to click the 'Attribute to 2021 LSOAs' button"
      })
      
      ReadyToAttribute(0)
      
    })
    
    # Watch whether setting change
    ChangeSettings.Listen = reactive({
      req(ProceedCheck() == 1 & ReadyToAttribute() == 1)
      list(input$Select.Conversion.Type,input$Select.Weighting,input$Select.Rounding)
    })
    
    # Respond to change in settings
    observeEvent(ChangeSettings.Listen(),{
      RunningReport("Settings have changed - click 'Attribute' button for new output")
      ReadyToDownload(0)
      
      output$ReportForUser <- renderText({
        RunningReport()
      })
      
      output$OutputFileName <- renderText({
        "You need to click the 'Attribute to 2021 LSOAs' button"
      })
    })
    
    
    # Download handler button
    observeEvent(input$dummybutton,{
      showModal(modalDialog(size = "s", easyClose = TRUE,
        title = "Download not yet available!",
        tags$p("Selection or settings have changed or the input data are invalid - you must ensure everything is as required and then click the 'Attribute to 2021 LSOAs' button."),
        tags$p("Then you will be able to download the output file listed in the dialog box at the foot of the page.")
      ))
    })
    
    # Download handler
    output$downloadData <- downloadHandler(
       filename = function() {
        DownloadFileName()
        },
       content = function(con) {
        write.csv(DownloadOutput(), con, row.names=FALSE)
        }
    )
    
    observeEvent(TriggerColumnSelectionProblem(),{
      req(ProceedCheck() == 1)
      output$ColumnSelection <- renderText({
        "<span style = color:#ff0000><strong>Data problem: NAs or non-numeric in selected columns!</strong></span>"
      })
      
    })
    
    output$OverwriteWarning <- renderText({
      "<span style = color:#ff0000; ><b>Note: If a file already exists in the local download directory a sequential number will be added to ensure the downloaded file is unique!</b></span>"
    })
    
    

 }
  
  # Create Shiny app ----
  shinyApp(ui, server)