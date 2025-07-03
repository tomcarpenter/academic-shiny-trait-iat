library(shiny)
library(lavaan)
library(tidySEM)
library(magrittr)


# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel(h1("Latent Trait Analysis for Implicit Association Tests")),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            h4("Instructions"), 
            "This is where you upload your data. You should upload a CSV file with one column for each IAT and one column for the self-report total.",
            tags$br(),tags$br(),
            "Columns should be named 'iat1', 'iat2', 'iat3', etc. Data in these columns should be D-scores ready for analysis. You should have one column for your self-report measure, entitled 'selfreport'. You need a minimum of three IATs and one self-report measure to run this app.",
            tags$br(), tags$br(),
            "Failure to name columns correctly will invalidate the analysis. For an example of a correctly named/formatted dataset, see:", tags$a(href="https://osf.io/5y8w9/", "https://osf.io/5y8w9/"), " or ",tags$a(href="https://osf.io/83dza/", "https://osf.io/83dza/."),
            
            tags$hr(),
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Number of IATs in file
            numericInput("num.iat", "Number of IATs in Analysis (Allowed Values: 3-7)", value=3, min=3, max=7, step=1),
            
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Inspect Uploaded Data? ",
                         choices = c("View Top Rows"  = "head",
                                     "View All" = "all",
                                     "None"="none"),
                         selected = "head"),
            
            # Input: VIEW FULL MODEL 1 OUTPUT ----
            radioButtons("mod1full", "View Full Output for Model 1? ",
                         choices = c("Do Not Display"  = "no",
                                     "View All" = "yes"),
                         selected = "no"),
            
            radioButtons("mod2full", "View Full Output for Model 2? ",
                         choices = c("Do Not Display"  = "no",
                                     "View All" = "yes"),
                         selected = "no")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            h2("Overview"),
            "This application extracts a trait factor from several IATs (Implicit Association Tests; min = 3, max = 7) given to participants over time or in a repeated-measures fashion, allowing you to estimate and model individual differences in implicit bias free of measurement error, which is generally quite large for IATs. Specifically, this application estimates this via confirmatory factor analysis (CFA), as documented in our pre-print (LINK). We assume the reader has knowledge of CFA / SEM; if not, please consider starting with ",tags$a(href="http://davidakenny.net/cm/causalm.htm", "David Kenny's website"),", ",tags$a(href="https://www.guilford.com/books/Confirmatory-Factor-Analysis-for-Applied-Research/Timothy-Brown/9781462515363", "Brown’s (2015) text")," on CFA, and ",tags$a(href="https://www.guilford.com/books/Principles-and-Practice-of-Structural-Equation-Modeling/Rex-Kline/9781462523344", "Kline’s (2015) text."),
            tags$br(),tags$br(),

            "Results summary:",
            verbatimTextOutput("summ"),
            tags$br(),tags$br(),
                
            h2("Model 1 - Confirmatory Factor Analysis"),
            "The first analysis is a simple confirmatory factor analysis, run in lavaan, using FIML to handle missing data (mirroring Mplus). This estimates the degree to which each IAT is (and is not) explained by the trait factor.",
            
            tags$br(),
            h3("Model Equation:"),
            verbatimTextOutput("modeleq1"),
            
            h3("Results:"),
            
            "The proportion of variance in each IAT explained by the trait factor (squared loadings):",
            verbatimTextOutput("mod1.trait"),
            
            "The proportion of variance in each IAT *not* explained by the trait factor (residuals):",
            verbatimTextOutput("mod1.nontrait"),
            
            "The reliability of the composite, were you to use a sum-score approach (omega):",
            verbatimTextOutput("mod1.omega"),
            
            "Model fit for the CFA:", 
            verbatimTextOutput("mod1.fit"),
            
            "Note: Model fit cannot be intepreted if you have only three IATs. For more information regarding model fit and CFA in general, see ",tags$a(href="http://davidakenny.net/cm/causalm.htm", "David Kenny's website."),
            tags$br(),tags$br(),
            "Model plot:",
            plotOutput("mod1.plot"),
            tags$br(), tags$br(),
            "Full Output:",
            verbatimTextOutput("summary.mod1"),
            
            
            h2("Model 2 - Correlation with Self-Report"),
            "The second analysis re-implements the model above but estimating the correlation between the trait factor and the self-report measure.",
            tags$br(),
            h3("Model Equation:"),
            verbatimTextOutput("modeleq2"),
            h3("Results:"),
            "Latent correlation between trait IAT factor and the observed self-report measure:",
            verbatimTextOutput("mod2.cor"),

            "z-test statistic for correlation:",
            verbatimTextOutput("mod2.z"),
            
            "p-value for correlation:",
            verbatimTextOutput("mod2.p"),
            
            "Model fit:", 
            verbatimTextOutput("mod2.fit"),
            "(note: poor fit may indicate that the self-report measure has residuals associations with individual IATs outside of the trait factor. You should manually run and inspect the model in SEM mannually in SEM software).", 
            
            tags$br(),tags$br(),
            "Model plot:",
            plotOutput("mod2.plot"),
            
            tags$br(), tags$br(),
            "Full Output:",
            verbatimTextOutput("summary.mod2"),
            
            h2("Data for Inspection:"),
            
            # Output: Data file ----
            tableOutput("contents"),
            
            # WHERE YOUR FOOTER GOES
            hr(),
            "Note: A minimum of three IATs is required to properly estimate a confirmatory factor analysis (CFA); we do not include code for more than seven IATs as we do not anticipate need for this many (see our preprint). Users who wish to run larger models can run them manually in lavaan or Mplus."
            
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = TRUE)
        
        if(input$disp == "head") {
            return(head(df))
        }
        if(input$disp == "all") {
            return(df)
        }
        if(input$disp == "none") {
            return(NULL)
        }
        
    })
    
    
    writeMod1 <- reactive({
        if (input$num.iat == 3){
            return('iat =~ iat1 + iat2 + iat3')
        }
        if (input$num.iat == 4){
            return('iat =~ iat1 + iat2 + iat3 + iat4')
        }
        if (input$num.iat == 5){
            return('iat =~ iat1 + iat2 + iat3 + iat4 + iat5')
        }
        if (input$num.iat == 6){
            return('iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6')
        }
        if (input$num.iat == 7){
            return('iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6 + iat7')
        }
    })
    
    output$modeleq1 <- renderText(writeMod1())
    
    runMod1 <- reactive({
        req(input$file1)
        dat <- read.csv(input$file1$datapath,header = TRUE)
        runmod <- function(num.iat=input$num.iat){
            
            if (num.iat == 3){
                lav.mod <- 'iat =~ iat1 + iat2 + iat3'
            }
            
            if (num.iat == 4){
                lav.mod <- 'iat =~ iat1 + iat2 + iat3 + iat4'
            }
            
            if (num.iat == 5){
                lav.mod <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5'
            }
            
            if (num.iat == 6){
                lav.mod <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6'
            }
            
            if (num.iat == 7){
                lav.mod <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6 + iat7'
            }
            
            # dat <- read.csv(input$file1$datapath,header = TRUE)
            
            fit <- cfa(lav.mod, data=dat, missing="fiml", estimator = "MLR")
            out <- fitmeasures(fit) 
            
            #fit stats - DISPLAY
            sum.chisq <- out['chisq.scaled']
            sum.df <- out['df.scaled']
            sum.p <- out['pvalue.scaled']
            sum.cfi <- out['cfi.scaled']
            sum.tli <- out['tli.scaled']
            sum.rmsea <- out['rmsea.scaled']
            sum.srmr <- out['srmr_mplus']
            
            params <- parameterestimates(fit, standardized = TRUE)
            
            #loadings
            load1 <- params[1,'std.all']
            load2 <- params[2,'std.all']
            load3 <- params[3,'std.all']
            load4 <- NA
            load5 <- NA
            load6 <- NA
            load7 <- NA
            
            # proportion non-trait variance
            nontrait1 <- 1-params[1,'std.all']^2
            nontrait2 <- 1-params[2,'std.all']^2
            nontrait3 <- 1-params[3,'std.all']^2
            nontrait4 <- NA
            nontrait5 <- NA
            nontrait6 <- NA
            nontrait7 <- NA
            
            if (num.iat == 4){
                nontrait4 <- 1-params[4,'std.all']^2
                load4 <- params[4,'std.all']
            }
            
            if (num.iat == 5){
                nontrait4 <- 1-params[4,'std.all']^2
                nontrait5 <- 1-params[5,'std.all']^2
                
                load4 <- params[4,'std.all']
                load5 <- params[5,'std.all']
            }
            
            if (num.iat == 6){
                nontrait4 <- 1-params[4,'std.all']^2
                nontrait5 <- 1-params[5,'std.all']^2
                nontrait6 <- 1-params[6,'std.all']^2
                
                load4 <- params[4,'std.all']
                load5 <- params[5,'std.all']
                load6 <- params[6,'std.all']
            }
            
            if (num.iat == 7){
                nontrait4 <- 1-params[4,'std.all']^2
                nontrait5 <- 1-params[5,'std.all']^2
                nontrait6 <- 1-params[6,'std.all']^2
                nontrait7 <- 1-params[7,'std.all']^2
                
                load4 <- params[4,'std.all']
                load5 <- params[5,'std.all']
                load6 <- params[6,'std.all']
                load7 <- params[7,'std.all']
            }
            
            nontrait <- c(iat1 = nontrait1, iat2 = nontrait2, iat3 = nontrait3,
                          iat4=nontrait4, iat5=nontrait5, iat6=nontrait6, iat7=nontrait7)
            trait <- c(iat1 = load1^2, iat2 = load2^2, iat3 = load3^2,
                       iat4=load4^2, iat5=load5^2, iat6=load6^2, iat7=load7^2)
            loadings <- c(iat1 = load1, iat2 = load2, iat3 = load3,
                          iat4=load4, iat5=load5, iat6=load6, iat7=load7)
            
            trait <- trait[complete.cases(trait)]
            nontrait <- nontrait[complete.cases(nontrait)]
            loadings <- loadings[complete.cases(loadings)]
            
            
            # reliability if scored as sum score - DISPLAY
            omega <- sum(loadings)^2 / (sum(loadings)^2 + sum(nontrait))
            
            plot <- prepare_graph(fit) %>%
                edit_edges({label =round(parameterestimates(fit, standardized = TRUE)$std.all[1:nrow(get_edges(fit))], 3) }) %>%
                edit_nodes({label = gsub("\\n.*","",get_nodes(fit)$label)})%>%
                plot()
            
            return(list(
                sum.chisq=sum.chisq, sum.df=sum.df, sum.p=sum.p, sum.cfi=sum.cfi, sum.tli=sum.tli, sum.rmsea=sum.rmsea,
                trait=trait, nontrait=nontrait, loadings=loadings, omega=omega, plot=plot
            ))
            
        }
        runmod()
    })

    sumMod1 <- reactive({
        req(input$file1)
        dat <- read.csv(input$file1$datapath,header = TRUE)
        summod <- function(num.iat=input$num.iat){
            
            if (num.iat == 3){
                lav.mod <- 'iat =~ iat1 + iat2 + iat3'
            }
            
            if (num.iat == 4){
                lav.mod <- 'iat =~ iat1 + iat2 + iat3 + iat4'
            }
            
            if (num.iat == 5){
                lav.mod <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5'
            }
            
            if (num.iat == 6){
                lav.mod <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6'
            }
            
            if (num.iat == 7){
                lav.mod <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6 + iat7'
            }
            
            fit <- cfa(lav.mod, data=dat, missing="fiml", estimator = "MLR")
            return(summary(fit, fit.measures=TRUE, standardized=TRUE))
        }
        summod()
    })
    
    output$mod1.trait <- renderPrint(runMod1()$trait)
    output$mod1.nontrait <- renderPrint(runMod1()$nontrait)
    output$mod1.omega <- renderText(runMod1()$omega)
    output$mod1.fit <- renderText(paste0("Chi-squared = ", round(runMod1()$sum.chisq, 3), ", df = ", runMod1()$sum.df, ", p = ", 
                                         round(runMod1()$sum.p, 3), ". CFI = ", round(runMod1()$sum.cfi,3), ", TLI = ", round(runMod1()$sum.tli, 3),
                                         ", RMSEA = ", round(runMod1()$sum.rmsea, 3)))
    output$mod1.plot <- renderPlot(runMod1()$plot)
    output$summary.mod1 <- renderPrint({
            if(input$mod1full == "yes") {
                return(sumMod1())
            } else {
                return("Select option at left to view full lavaan output")
            }
        })
    
    
    writeMod2 <- reactive({
        if (input$num.iat == 3){
            return('iat =~ iat1 + iat2 + iat3\niat ~~ selfreport')
        }
        if (input$num.iat == 4){
            return('iat =~ iat1 + iat2 + iat3 + iat4\niat ~~ selfreport')
        }
        if (input$num.iat == 5){
            return('iat =~ iat1 + iat2 + iat3 + iat4 + iat5\niat ~~ selfreport')
        }
        if (input$num.iat == 6){
            return('iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6\niat ~~ selfreport')
        }
        if (input$num.iat == 7){
            return('iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6 + iat7\niat ~~ selfreport')
        }
    })
    
    output$modeleq2 <- renderText(writeMod2())
    
    runMod2 <- reactive({
        req(input$file1)
        dat <- read.csv(input$file1$datapath,header = TRUE)
        runmod2 <- function(num.iat=input$num.iat){
            if (num.iat == 3){
                lav.mod2 <- 'iat =~ iat1 + iat2 + iat3
                 iat ~~ selfreport'
            }
            
            if (num.iat == 4){
                lav.mod2 <- 'iat =~ iat1 + iat2 + iat3 + iat4
                     iat ~~ selfreport'
            }
            
            if (num.iat == 5){
                lav.mod2 <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5
                         iat ~~ selfreport'
            }
            
            if (num.iat == 6){
                lav.mod2 <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6
                         iat ~~ selfreport'
            }
            
            if (num.iat == 7){
                lav.mod2 <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6 + iat7
                         iat ~~ selfreport'
            }
            
            fit2 <- cfa(lav.mod2, data=dat, missing="fiml", estimator = "MLR")
            
            params2 <- parameterestimates(fit2, standardized = TRUE)
            
            results <- params2[(params2$lhs=="iat" & params2$rhs=="selfreport"),]
            
            #latent correlation
            cor <- results$std.all  
            
            #latent se
            se <- results$se
            
            # latent test stat
            z <- results$z
            
            # latent p-value
            p <- results$pvalue
        
            
            out <- fitmeasures(fit2) 
            #fit stats - DISPLAY
            sum.chisq <- out['chisq.scaled']
            sum.df <- out['df.scaled']
            sum.p <- out['pvalue.scaled']
            sum.cfi <- out['cfi.scaled']
            sum.tli <- out['tli.scaled']
            sum.rmsea <- out['rmsea.scaled']
            sum.srmr <- out['srmr_mplus'] #unsure if correct
            
            plot <- prepare_graph(fit2) %>%
                edit_edges({label =round(parameterestimates(fit2, standardized = TRUE)$std.all[1:nrow(get_edges(fit2))], 3) }) %>%
                edit_nodes({label = gsub("\\n.*","",get_nodes(fit2)$label)})%>%
                plot()
            
            return(list(cor=cor, se=se, z=z, p=p,
                        sum.chisq=sum.chisq, sum.df=sum.df, sum.p=sum.p, sum.cfi=sum.cfi, 
                        sum.tli=sum.tli, sum.rmsea=sum.rmsea, plot=plot))
        }
        runmod2()  
    })
    
    sumMod2 <- reactive({
        req(input$file1)
        dat <- read.csv(input$file1$datapath,header = TRUE)
        summod2 <- function(num.iat=input$num.iat){
            if (num.iat == 3){
                lav.mod2 <- 'iat =~ iat1 + iat2 + iat3
                 iat ~~ selfreport'
            }
            
            if (num.iat == 4){
                lav.mod2 <- 'iat =~ iat1 + iat2 + iat3 + iat4
                     iat ~~ selfreport'
            }
            
            if (num.iat == 5){
                lav.mod2 <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5
                         iat ~~ selfreport'
            }
            
            if (num.iat == 6){
                lav.mod2 <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6
                         iat ~~ selfreport'
            }
            
            if (num.iat == 7){
                lav.mod2 <- 'iat =~ iat1 + iat2 + iat3 + iat4 + iat5 + iat6 + iat7
                         iat ~~ selfreport'
            }
            
            fit2 <- cfa(lav.mod2, data=dat, missing="fiml", estimator = "MLR")
            return(summary(fit2, standardized=TRUE, fit.measures=TRUE))
        }
        summod2()  
    })
    
    output$mod2.cor <- renderText(runMod2()$cor)
    output$mod2.cor2 <- renderText(runMod2()$cor)
    output$mod2.z <- renderText(runMod2()$z)
    output$mod2.p <- renderText(runMod2()$p)
    output$mod2.fit <- renderText(paste0("Chi-squared = ", round(runMod2()$sum.chisq, 3), ", df = ", runMod2()$sum.df, ", p = ", 
                                         round(runMod2()$sum.p, 3), ". CFI = ", round(runMod2()$sum.cfi,3), ", TLI = ", round(runMod2()$sum.tli, 3),
                                         ", RMSEA = ", round(runMod2()$sum.rmsea, 3)))
    output$mod2.plot <- renderPlot(runMod2()$plot)
    output$summary.mod2 <- renderPrint({
        if(input$mod2full == "yes") {
            return(sumMod2())
        } else {
            return("Select option at left to view full lavaan output")
        }
    })
    
    
    meancor <- reactive({
        req(input$file1)
        dat <- read.csv(input$file1$datapath,header = TRUE)
        rawcors <- function(){
        cors <- dat %>% dplyr::select(selfreport, starts_with("iat")) %>% cor(., use="pairwise.complete.obs") %>% .[-1,"selfreport"]
        mean <- mean(cors)
        return(list(cors=cors, mean=mean))
        }
        rawcors()
    })
    
    output$summ <- renderText(paste0("The proportion of variance in the IATs explained by the individual-
    difference factor was, on average, ",round(mean(runMod1()$trait),2 ),". Conversely, the proportion 
    of variance in the IATs not explained by the individual-difference 
    factor was ",round(mean(runMod1()$nontrait),2 ),". With respect to individual differences, the 
    individual IAT scores thus contain ",round(100*mean(runMod1()$nontrait),0),"% measurement error (on average).
                                     
    The latent correlation between the trait factor and the self-report
    measure was ", round(runMod2()$cor,3), ". By comparison, the observed IAT scores correlate
    with the self-report measure (on average) at r = ",round(meancor()$mean,3),". Consequently,
    the latent factor explains ",round(runMod2()$cor^2/meancor()$mean^2, 2)," times more variance than do the 
    raw scores.
                                     
    Details regarding the latent variable models used to generate this 
    output, as well as additional analyses such as significance tests,
    are given below. Raw correlations can be estimated in your analysis 
    package of choice.")) 
    
    
}


#https://community.rstudio.com/t/failing-to-deploy-shinyapp-depending-on-bioconductor-packages/6970/2

# Create Shiny app ----
shinyApp(ui, server)