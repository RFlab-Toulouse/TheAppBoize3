library(shiny)
library(shinyjs)
shinyUI(fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("La Boize - Omics Data Analysis"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      wellPanel( 
        conditionalPanel(condition ="input.confirmdatabutton==0" ,
                         radioButtons("analysis","",c("new analysis","previous analysis"),inline=T),
                         conditionalPanel( condition="input.analysis=='previous analysis' ",     
                                           fileInput("modelfile",label=h4("previous analysis"),accept=".RData")
                         ), 
                         conditionalPanel(condition="input.analysis=='new analysis' ",
                                          fluidRow(
                                            column(12,br(),radioButtons("filetype", "Extention of the file",c("csv" = "csv", "xlsx" = "xlsx"),inline = TRUE))
                                          ),
                                          fluidRow(
                                            column(12,conditionalPanel(condition ="input.help",
                                                                       helpText("Learning file is obligatory to continue")
                                            ),
                                            fileInput("learningfile", 
                                                      label = h4("learning File"),
                                                      accept =  c("text/csv",
                                                                  "application/vnd.ms-excel",
                                                                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                                                  ".xls",".xlsx"))
                                            )
                                            ,
                                            column(12,
                                                   fileInput("validationfile", label = h4("validation File "),accept =  c("text/csv","application/vnd.ms-excel","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xls",".xlsx")) 
                                            )
                                          ),
                                          fluidRow(
                                            conditionalPanel(condition ="input.filetype=='csv' ",column(6,textInput('dec', 'character for decimal point',value = "." ))),
                                            column(6,textInput("NAstring", label = "characters for missing values",value = "NA"))
                                          ),   
                                          fluidRow(
                                            conditionalPanel(condition ="input.filetype=='csv' ",
                                                             radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),inline = TRUE )),
                                            conditionalPanel(condition ="input.filetype=='xlsx' ",
                                                             column(6,numericInput("skipn",label = "number of lines to skip",value = 0)),
                                                             column(6,numericInput("sheetn",label = "sheet",value = 1)))
                                          ),hr(),
                                          checkboxInput("transpose","Transpose the table",FALSE),
                                          checkboxInput("zeroegalNA","consider 0 as NA",FALSE)
                         )
                         ,
                         actionButton("confirmdatabutton","Confirm data", 
                                      style = "background-color: #63BFBF;
                                  color: white;
                                  border-color: #63BFBF;"),
                         
                         conditionalPanel(condition ="input.help",
                                          helpText("Data has to be confirm to continue"))
        ),
        conditionalPanel(condition ="input.confirmdatabutton!=0",
                         h4("Learning data"),
                         # useConfetti(),
                         # tags$head(
                         #   # Ajouter la bibliothèque JS pour les confettis
                         #   tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.4.0/dist/confetti.browser.min.js"),
                         #   
                         #   # Fonction JavaScript pour lancer les feux d'artifice
                         #   tags$script(HTML("
                         #            function triggerFireworks() {
                         #              const duration = 1.5 * 1000;
                         #              const animationEnd = Date.now() + duration;
                         #              const defaults = { startVelocity: 30, spread: 360, ticks: 60, zIndex: 1000 };
                         #      
                         #              function randomInRange(min, max) {
                         #                return Math.random() * (max - min) + min;
                         #              }
                         #      
                         #              const interval = setInterval(function() {
                         #                const timeLeft = animationEnd - Date.now();
                         #                if (timeLeft <= 0) {
                         #                  return clearInterval(interval);
                         #                }
                         #      
                         #                const particleCount = 50 * (timeLeft / duration);
                         #                confetti(Object.assign({}, defaults, {
                         #                  particleCount,
                         #                  origin: { x: randomInRange(0.1, 0.9), y: Math.random() - 0.2 }
                         #                }));
                         #              }, 250);
                         #            }
                         #      
                         #            // Gérer les messages envoyés depuis Shiny
                         #            Shiny.addCustomMessageHandler('evaljs', function(message) {
                         #              eval(message.code);
                         #            });
                         #          "))
                         # ),
                         textOutput("namefilelearn",inline=T), tags$head(tags$style("#namefilelearn{color: grey;font-size: 15px;font-style: italic;}")),
                         br(),
                         textOutput("dim1learn",inline=T), "lines (individuals)",
                         br(),
                         textOutput("dim2learn",inline=T), "columns (variables)",
                         br()
        ),
        conditionalPanel(condition ="input.confirmdatabutton!=0 & output.fileUploadedval",
                         h4("Validation data"),
                         textOutput("namefileval",inline=T), tags$head(tags$style("#namefileval{color: grey;font-size: 15px;font-style: italic;}")),
                         br(),
                         textOutput("dim1val",inline=T), "lines (individuals)",br(),
                         textOutput("dim2val",inline=T), "columns (variables)",
                         br()
        ), 
        conditionalPanel(condition ="input.confirmdatabutton!=0",
                         hr(),
                         fluidRow(
                           column(3,checkboxInput("invers", "inverse" , value = FALSE)),
                           column(8,
                                  p(textOutput("positif",inline=T),HTML( '&#x21D2;'), "case ",br(),
                                    textOutput("negatif",inline=T),HTML( '&#x21D2;'), "control",align="center")
                           )
                         ),
                         hr(),
                         radioButtons("paramdownplot","Download images as",choices=list("png"="png","jpg"="jpg","pdf"="pdf"),selected="png"),
                         radioButtons("paramdowntable","Download datasets as",choices=list("csv"="csv","xlsx"="xlsx"),selected="csv"),
                         hr(),
                         downloadButton("savestate","Save settings RData",class = "dlButton"),
                         hr(),
                         downloadButton("savestatetable","Save settings table and main results",class = "dlButton")
                         
        ),
        hr(),
        checkboxInput("help","show help",FALSE)
      ),width=3      
    ) ,
    
    mainPanel(
      conditionalPanel(condition ="!output.fileUploaded & !output.modelUploaded",
                       h3("The purpose of this application is to provide a user-friendly tool to build a prediction model from omics datas.",
                          align="center"),
                       h4("Check the box 'show help' for any further informations."),br(),br(),br(),
                       
                       
                       fluidRow(column(6,imageOutput("image1")),column(2,imageOutput("image2"))),
                       br(),
                       h4("This application is developped in the 12th team of I2MC for internal used.",align="center")
                       
      ),           
      conditionalPanel(condition ="output.fileUploaded || output.modelUploaded",
                       tabsetPanel(id = "data",              
                                   tabPanel("Learning Data",
                                            br(),
                                            conditionalPanel(condition ="input.help",
                                                             fluidRow(
                                                               column(5,br(),helpText("To verify if the import parameters are correct : the first column has to be the names of the individual, 
                                       the second the groups. Others are the datas."), 
                                                                      helpText("Non attributes values have to appears empty, "),
                                                                      helpText()
                                                               ),
                                                               column(7,imageOutput("image3",width = "100%")))
                                            ),
                                            DT::dataTableOutput("JDDlearn")%>% withSpinner(color="#0dc5c1",type = 1),
                                            p(downloadButton("downloaddataJDDlearn","Download dataset"),align="center")
                                   ),
                                   tabPanel("Validation Data",
                                            conditionalPanel(condition ="output.fileUploadedval",
                                                             br(),
                                                             DT::dataTableOutput("JDDval")%>% withSpinner(color="#0dc5c1",type = 1),
                                                             p(downloadButton("downloaddataJDDval","Download dataset"),align="center")
                                            )
                                   ),
                                   # Sélection of  variables by % of values and structure of NA's  
                                   tabPanel("Select Data", 
                                            conditionalPanel(condition ="input.help",
                                                             helpText(" Select variables to extract variables from the learning dataset according to the number or the structure of Non-Attribute values (missing values)")
                                            ),  
                                            fluidRow(
                                              column(7, numericInput("prctvalues","Percentage minimum of values" , 0, min = 0, max = 100, step = 5),
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("")),br(),
                                                     checkboxInput("NAstructure", "Select variables with a NA's structure " , value = FALSE),
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("The structure test is a proportion test of the Non Attributes values between the 2 groups."))
                                              ),
                                              column(5,radioButtons("selectmethod","Methods of selection ",c("selection on all samples"="nogroup","each group has more than x% of values "="bothgroups","at least one group has more than x% of more"="onegroup")),
                                                     conditionalPanel(condition ="input.help",helpText("3 ways of selection : select variables which got at least x% of values in all samples, "),
                                                                      helpText("select variables which which have more than x% in the two groups"),
                                                                      helpText("select variables which have at leat one group whith more than x% of values"))
                                              )
                                              
                                            ),p(downloadButton('downloaddataselect', 'Download data selected'),align="center"),
                                            hr(),
                                            
                                            fluidRow(
                                              column(7,textOutput("nvarselect",inline=T), "selected variables" ,
                                                     plotOutput("heatmapNA",width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1) ,
                                                     p(downloadButton("downloadplotheatmapNA","Download plot"),downloadButton('downloaddataheatmapNA', 'Download raw data'),align="center")
                                              ),
                                              column(5,br(),
                                                     conditionalPanel(condition ="input.help",helpText("The 3 curves present the number of variables selected according to the three possible options and the % of Na's selected"))
                                                     ,
                                                     plotOutput("plotNA",width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                     p(downloadButton("downloadplotNA","Download plot"),downloadButton('downloaddataplotNA', 'Download raw data'),align="center")
                                              )
                                            ),
                                            hr(),
                                            fluidRow(
                                              column(6,  
                                                     conditionalPanel(condition ="input.NAstructure==true",
                                                                      conditionalPanel(condition ="input.help",helpText("Consider the NA in the group with less values as real 0 (replace by 0) the NA in the group with more values are raplace by the solution chosen later")),
                                                                      numericInput("thresholdNAstructure","pvalue for the structure test" , 0.05, min = 0, max = 1, step = 0.005))
                                              ),
                                              conditionalPanel(condition ="input.NAstructure==true",
                                                               column(6,radioButtons("structdata", "search structure in : ",c("all dataset" = "alldata","selected dataset" = "selecteddata"))))
                                            ), 
                                            hr(),
                                            fluidRow(
                                              conditionalPanel(condition ="input.NAstructure==true", 
                                                               column(9,textOutput("nstructuredfeatures",inline=T),"structured features",
                                                                      plotOutput("heatmapNAstructure" ,width = "95%",height = 600)%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton("downloadstructur","Download plot"),downloadButton('downloaddatastructur', 'Download raw data'),align="center")),
                                                               column(3,br(),br(),
                                                                      numericInput("maxvaluesgroupmin","The group with the minimum number of values has at most x% of values",value = 25,min = 0,max = 100,step = 5),
                                                                      numericInput("minvaluesgroupmax","The group with the maximum number of values has at least y% of values",value = 75,min = 0,max = 100,step = 5))
                                              )
                                            )
                                   ),
                                   tabPanel("Transform Data",
                                            conditionalPanel(condition ="input.help",
                                                             helpText("")),
                                            fluidRow(
                                              column(5,radioButtons("rempNA", "Replacing NA (Not Attributes) by:",
                                                                    c("zero" = "z","mean of the cohort" = "moy",
                                                                      "mean by group"="moygr","PCA estimation" = "pca","Random forest estimation /!\\" = "missforest")),
                                                     helpText("/!\\ process can be long"),
                                                     
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("Random Forest can "))),
                                              column(3,
                                                     checkboxInput("log","transform data in log",FALSE),
                                                     checkboxInput("standardization","standardization dataset",FALSE),
                                                     conditionalPanel(condition ="input.help",helpText("dividing the columns quadratic mean")),
                                                     checkboxInput("arcsin","arcsine transformation",FALSE),
                                                     conditionalPanel(condition ="input.help",helpText("each column is rescaled between 1 and 0, and arcsin transformation is applying"))
                                              ),
                                              column(4,
                                                     conditionalPanel(condition="input.log",
                                                                      radioButtons("logtype",label = NULL,c("ln"="logn","log 10"="log10","log2"="log2"),inline = TRUE)))
                                            ),p(downloadButton('downloaddatatransform', 'Download transform data '),align="center"),
                                            hr(),
                                            
                                            fluidRow(
                                              column(5,plotOutput("plotheatmaptransformdata" ,width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                     p(downloadButton("downloadplotheatmap","Download plot"),
                                                       downloadButton('downloaddataheatmap', 'Download raw data'),align="center")),  
                                              
                                              column(7,conditionalPanel(condition ="input.help",
                                                                        helpText("The mds (MultiDimensionnal Scaling) calcul the distances between the individuals (rows) and represented it on a plan as well as possible."),
                                                                        helpText("The aim of this graphic is to vizualized if the selection and transform parameters separate well the 2 groups.")),
                                                     plotOutput("plotmds",height=500,width = "100%")%>% withSpinner(color="#0dc5c1",type = 1),
                                                     p(downloadButton("downloadplotmds","Download plot"),
                                                       downloadButton('downloaddatamds', 'Download raw data'),align="center"))),
                                            plotOutput("plothist",height=500,width = "100%")%>% withSpinner(color="#0dc5c1",type = 1),
                                            p(downloadButton("downloadplothist","Download plot"),
                                              downloadButton('downloaddatahist', 'Download raw data'),align="center")
                                   ),
                                   # selection variables basé sur 20% out of bag avec un ceraint nombre d'iteration : 
                                   tabPanel("Statistics",
                                            # CSS pour améliorer l'apparence
                                            tags$style(HTML("
                                                            .progress-bar-animated {
                                                              animation: progress-bar-stripes 1s linear infinite;
                                                            }
                                                            
                                                            @keyframes progress-bar-stripes {
                                                              0% { background-position: 1rem 0; }
                                                              100% { background-position: 0 0; }
                                                            }
                                                            
                                                            .alert-success {
                                                              border-color: #28a745;
                                                              background-color: #d4edda;
                                                            }
                                                            
                                                            .alert-info {
                                                              border-color: #17a2b8;
                                                              background-color: #d1ecf1;
                                                            }
                                                            
                                                            .alert-warning {
                                                              border-color: #ffc107;
                                                              background-color: #fff3cd;
                                                            }
                                                            
                                                            .wellPanel {
                                                              background-color: #f8f9fa;
                                                              border: 1px solid #dee2e6;
                                                              border-radius: 0.25rem;
                                                              padding: 1rem;
                                                            }
                                                            
                                                            .progress {
                                                              background-color: #e9ecef;
                                                              border-radius: 0.25rem;
                                                              overflow: hidden;
                                                            }
                                                            
                                                            .progress-bar {
                                                              transition: width 0.3s ease;
                                                              font-weight: bold;
                                                            }
                                                            
                                                            .alert h4 {
                                                              margin-bottom: 0.5rem;
                                                            }
                                                            
                                                            .alert p {
                                                              margin-bottom: 0;
                                                            }
                                                      ")),
                                            
                                            # JavaScript pour synchroniser use_elbow_method avec seuil_method
                                            tags$script(HTML("
                                                      $(document).ready(function() {
                                                        // Synchroniser use_elbow_method avec seuil_method
                                                        $('#seuil_method').on('change', function() {
                                                          var isElbow = $(this).val() === 'elbow';
                                                          $('#use_elbow_method').prop('checked', isElbow);
                                                        });
                                                        
                                                        // Initialiser au chargement
                                                        var isElbow = $('#seuil_method').val() === 'elbow';
                                                        $('#use_elbow_method').prop('checked', isElbow);
                                                      });
                                                    ")),    
                                            conditionalPanel(condition ="input.help",
                                                             helpText("Peptide selection using statistical tests or bootstrapping (without replacement) with cross-validation")
                                            ),
                                            
                                            # Choix de méthode d'analyse
                                            fluidRow(
                                              column(12,
                                                     radioButtons("stats_method", "Méthode d'analyse:",
                                                                  choices = list(
                                                                    "Analyse standard" = "standard",
                                                                    "Bootstrap avec validation croisée" = "bootstrap"
                                                                  ),
                                                                  selected = "standard", inline = TRUE
                                                     )
                                              )
                                            ),
                                            hr(),
                                            
                                            # Section analyse standard (votre code existant)
                                            conditionalPanel(condition = "input.stats_method == 'standard'",
                                                             fluidRow(
                                                               column(6,
                                                                      radioButtons("test", "Tests",
                                                                                   c( "No test"="notest",
                                                                                      "Wilcoxon Test" = "Wtest",
                                                                                      "Student Test" = "Ttest")),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Le test de Wilcoxon (Mann-Whitney-Wilcoxon) est non paramétrique.")),
                                                                      checkboxInput("SFtest","Shapiro and Fisher Tests",T)
                                                               ),
                                                               column(6,br(),
                                                                      numericInput("thresholdFC", "Seuil Fold change" , 0, min =0, max = 5, step = 0.5),
                                                                      numericInput("thresholdpv", "Seuil p-value" , 0.05, min =0, max = 1, step = 0.01),
                                                                      checkboxInput("adjustpv", "Ajustement p-value" , value = FALSE)
                                                               )
                                                             )
                                            ),
                                            # Section bootstrap simplifiée
                                            conditionalPanel(condition = "input.stats_method == 'bootstrap'",
                                                             #wellPanel(
                                                             # h4("Paramètres Bootstrap"),
                                                             fluidRow(
                                                               column(3,
                                                                      numericInput("n_iterations", "Number of iterations:", 
                                                                                   value = 100, min = 10, max = 500, step = 10),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Plus d'itérations = plus de robustesse"))
                                                               ),
                                                               column(3,
                                                                      numericInput("sample_percentage", "% of patients per iteration:", 
                                                                                   value = 80, min = 50, max = 90, step = 5),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Percentage of samples at each iteration"))
                                                               )
                                                               #,
                                                               # column(4,
                                                               #        numericInput("cv_folds", "Folds validation croisée:", 
                                                               #                     value = 5, min = 3, max = 10, step = 1),
                                                               #        conditionalPanel(condition ="input.help",
                                                               #                         helpText("Nombre de groupes pour stratification"))
                                                               # )
                                                             ),
                                                             fluidRow(
                                                               column(6,
                                                                      radioButtons("bootstrap_test", "Tests",
                                                                                   choices = list("No test"="notestOOB",
                                                                                                  "Wilcoxon Test" = "WtestOOB", 
                                                                                                  "Student Test" = "TtestOOB"),
                                                                                   selected = "notestOOB"),
                                                                      numericInput("bootstrap_thresholdFCOOB",
                                                                                   "Fold change threshold",
                                                                                   0, min =0, max = 5, step = 0.5),
                                                                      numericInput("bootstrap_thresholdpvOOB",
                                                                                   "p-value threshold", 0.05, min =0, max = 1, step = 0.01),
                                                                      checkboxInput("bootstrap_adjustpvOOB", 
                                                                                    "Adjust p-value",
                                                                                    value = FALSE)
                                                               ),
                                                               column(6,
                                                                      # Threshold selection method
                                                                      h5("Threshold selection method"),
                                                                      radioButtons("seuil_method", 
                                                                                   "Choose the method:",
                                                                                   choices = list(
                                                                                     "Elbow method (default)" = "elbow",
                                                                                     "GMM (Gaussian Mixture Model)" = "gmm",
                                                                                     "Inflection point(second derivative method)" = "inflection",
                                                                                     # "Confidence interval" = "intervalle_confiance",
                                                                                     "Manual threshold" = "manual"
                                                                                   ),
                                                                                   selected = "elbow"
                                                                      ),
                                                                      
                                                                      # Manual threshold (if selected)
                                                                      conditionalPanel(condition = "input.seuil_method == 'manual'",
                                                                                       numericInput("stability_threshold", 
                                                                                                    "Stability threshold (%):", 
                                                                                                    value = 10, min = 5, max = 100, step = 5)
                                                                      ),
                                                                      
                                                                      # Explanation of the elbow method
                                                                      conditionalPanel(condition = "input.seuil_method == 'elbow' && input.help",
                                                                                       div(class = "alert alert-info", style = "margin-top: 10px;",
                                                                                           HTML("<strong> Elbow method:</strong><br/>
                                                                                  Automatically finds the optimal inflection point by:<br/>
                                                                                  1. Normalizing the coordinates<br/>
                                                                                  2. Calculating the perpendicular distances to the diagonal<br/>
                                                                                  3. Selecting the point with the maximum distance<br/>
                                                                                  4. Using this point as the selection threshold")
                                                                                       )
                                                                      ),
                                                                      
                                                                      # Explanation of the GMM method
                                                                      conditionalPanel(condition = "input.seuil_method == 'gmm' && input.help",
                                                                                       div(class = "alert alert-info", style = "margin-top: 10px;",
                                                                                           HTML("<strong> GMM method:</strong><br/>
                                                                                        Uses a Gaussian mixture model to estimate the data distribution.<br/>
                                                                                        The threshold is determined by analyzing the Gaussian components and their variance.")
                                                                                       )
                                                                      ),
                                                                      
                                                                      # Explanation of the inflection point method
                                                                      conditionalPanel(condition = "input.seuil_method == 'inflection' && input.help",
                                                                                       div(class = "alert alert-info", style = "margin-top: 10px;",
                                                                                           HTML("<strong> Inflection point method:</strong><br/>
                                                                                  Identifies the point where the curve changes concavity, indicating a significant change in the data.")
                                                                                       )
                                                                      ),
                                                                      
                                                                      # Explanation of the confidence interval
                                                                      conditionalPanel(condition = "input.seuil_method == 'intervalle_confiance' && input.help",
                                                                                       div(class = "alert alert-info", style = "margin-top: 10px;",
                                                                                           HTML("<strong> Confidence interval:</strong><br/>
                                                                      Defines an interval around the data mean, allowing to select the variables that fall within this interval.")
                                                                                       )
                                                                      )
                                                               )
                                                             ),
                                                             fluidRow(
                                                               conditionalPanel(condition = "input.help",
                                                                               div(class = "alert alert-info", style = "margin-top: 10px;",
                                                                                   HTML("<strong> recalculation of frequencies :</strong><br/>
                                                                                  Once the frequencies have been calculated, only changes to the parameters below 
                                                                                  can automatically restart the frequency recalculation:<br/>"),
                                                                                   HTML("• Fold change threshold<br/>"),
                                                                                   HTML(" • p-value threshold<br/>"),
                                                                                   HTML("• Tests")
                                                                                   
                                                                               )
                                                               ),
                                                             )
                                                             
                                                             # Bouton de lancement
                                                             # ,actionButton("run_bootstrap", " Lancer l'Analyse Bootstrap", 
                                                             #              style = "background-color: #63BFBF; color: white; border-color: #63BFBF; margin-top: 15px;")
                                                             # )
                                            ),
                                            
                                            # Variable réactive pour use_elbow_method (compatibilité avec le serveur)
                                            conditionalPanel(condition = "false",
                                                             checkboxInput("use_elbow_method", "", value = TRUE)
                                            ),
                                            # BARRE DE PROGRESSION
                                            conditionalPanel(condition = "input.stats_method == 'bootstrap' && output.progress_visible",
                                                             uiOutput("iteration_progress")
                                            ),
                                            
                                            # Indicateur de cache
                                            # conditionalPanel(condition = "input.stats_method == 'bootstrap'",
                                            #                  div(class = "alert alert-info", style = "margin-top: 10px;",
                                            #                      HTML("<strong>💡 Optimisation :</strong><br/>
                                            #                      Les fréquences bootstrap sont mises en cache pour éviter les recalculs inutiles.<br/>
                                            #                      Le cache est automatiquement nettoyé quand les paramètres changent."),
                                            #                      br(),
                                            #                      actionButton("clear_cache", "🗑️ Nettoyer le cache", 
                                            #                                   style = "background-color: #dc3545; color: white; border-color: #dc3545; margin-top: 10px;"),
                                            #                      actionButton("debug_cache", " Debug Cache", 
                                            #                                   style = "background-color: #17a2b8; color: white; border-color: #17a2b8; margin-top: 10px;")
                                            #                  )
                                            # ),
                                            
                                            br(),
                                            p(downloadButton('downloaddatastatistics', 'Download statistics'),
                                              downloadButton('downloadddatadiff', 'Download differently expressed variables'),
                                              align="center"),
                                            hr(),
                                            
                                            # Résultats analyse standard (existant)
                                            conditionalPanel(condition= "input.stats_method == 'standard' && (input.test== 'Wtest' || input.test== 'Ttest')",
                                                             fluidRow(
                                                               column(6,
                                                                      textOutput("nvarselect2",inline=T), "variables sélectionnées",
                                                                      plotOutput("volcanoplot_standard" ,width = 500,height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton("downloadvolcanoplot_standard","Download plot"),
                                                                        downloadButton('downloaddatavolcanoplot_standard', 'Download raw data'),align="center")
                                                               ),
                                                               column(6,
                                                                      textOutput("nbdiff",inline=T), "différentiellement exprimées",
                                                                      plotOutput("barplottest_standard" ,width = 600,height = 500)%>% withSpinner(color="#0dc5c1",type = 1),   
                                                                      p(downloadButton("downloadbarplottest_standard","Download plot"),
                                                                        downloadButton('downloaddatabarplottest_standard', 'Download raw data'),align="center")
                                                               )
                                                             )
                                            ),
                                            
                                            # RÉSULTATS BOOTSTRAP
                                            conditionalPanel(condition = "input.stats_method == 'bootstrap' && output.bootstrap_completed",
                                                             # Résumé des résultats
                                                             fluidRow(
                                                               column(6,
                                                                      div(class = "alert alert-success", 
                                                                          id = "alert_success",
                                                                          style = "margin-bottom: 20px; display: none;",
                                                                          h4(class = "alert-heading", "Bootstrap Analysis Completed !"),
                                                                          p("The peptide stability analysis has been completed successfully.")
                                                                      ),
                                                               ),
                                                               
                                                               column(6, 
                                                                      # Alerte selon la méthode utilisée
                                                                      conditionalPanel(condition = "input.seuil_method == 'elbow' && input.help",
                                                                                       div(class = "alert alert-info", 
                                                                                           HTML("<strong> Threshold determined by the elbow method</strong><br/>
                                                                                                           The optimal threshold was calculated automatically using the maximum 
                                                                                                                 distance to the diagonal method."))
                                                                      ),
                                                                      
                                                                      conditionalPanel(condition = "input.seuil_method == 'manual' && input.help",
                                                                                       div(class = "alert alert-warning", 
                                                                                           HTML("<strong>Manually defined threshold</strong><br/>
                                                                                                         Define your threshold."))
                                                                      ),
                                                                      
                                                                      conditionalPanel(condition = "input.seuil_method == 'gmm' && input.help",
                                                                                       div(class = "alert alert-info", 
                                                                                           HTML("<strong>Threshold determined by Gaussian Mixture Model (GMM)</strong><br/>
                                                                                                          The optimal threshold was calculated automatically using a 2-component Gaussian mixture model."))
                                                                      ),
                                                                      conditionalPanel(condition = "input.seuil_method == 'inflection' && input.help",
                                                                                       div(class = "alert alert-info", 
                                                                                           HTML("<strong>Threshold determined by the inflection point method</strong><br/>
                                                                                                          The optimal threshold was calculated automatically using the second derivative method."))
                                                                      )
                                                               )
                                                             ),
                                                             
                                                             fluidRow(
                                                               column(6,
                                                                      textOutput("nvarselect2OOB", inline=T), "variables sélectionnées",
                                                                      plotOutput("volcanoplot_bootstrap" ,width = 500,height = 500) %>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton("downloadvolcanoplot_bootstrap", "Download plot"),
                                                                        downloadButton('downloaddatavolcanoplot_bootstrap', 'Download raw data'),align="center")
                                                               ),
                                                               column(6,
                                                                      textOutput("nbdiffOOB",inline=T), "différentiellement exprimées",
                                                                      plotOutput("barplottest_bootstrap", width = 600, height = 500) %>% withSpinner(color="#0dc5c1",type = 1),   
                                                                      p(downloadButton("downloadbarplottest_bootstrap","Download plot"),
                                                                        downloadButton('downloaddatabarplottest_bootstrap', 'Download raw data'),align="center")
                                                               )
                                                             ),
                                                             
                                                             # Graphiques principaux
                                                             fluidRow(
                                                               column(7,
                                                                      h4("Peptide Stability"),
                                                                      plotOutput("stability_plot", height = 600) %>% withSpinner(color="#0dc5c1",type = 1),
                                                                      # plotlyOutput("stability_plot", height = 600) %>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton("download_stability_plot", "Download plot"),
                                                                        downloadButton('download_stability_data', 'Download data'),align="center")
                                                               ),
                                                               column(5,
                                                                      h4("Detailed statistics"),
                                                                      wellPanel(
                                                                        verbatimTextOutput("elbow_stats")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                        style = "background-color: #f8f9fa; 
                                                                                 border: 1px solid #dee2e6; 
                                                                                  height: 400px; 
                                                                                  overflow-y: auto;"
                                                                      )
                                                               )
                                                             ),
                                                             
                                                             # Graphiques de la méthode du coude (les deux graphiques)
                                                             conditionalPanel(condition = "input.seuil_method == 'elbow'",
                                                                              fluidRow(
                                                                                column(6,
                                                                                       h4(" Elbow Method - Sorted Frequencies"),
                                                                                       plotOutput("elbow_frequency_plot", height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                                                       p(downloadButton("download_elbow_frequency_plot","Download plot"),align="center")
                                                                                ),
                                                                                column(6,
                                                                                       h4(" Distances to Reference Line"),
                                                                                       plotOutput("elbow_distance_plot", height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                                                       p(downloadButton("download_elbow_distance_plot","Download plot"),align="center")
                                                                                )
                                                                              )
                                                             ),
                                                             conditionalPanel( condition = "input.seuil_method == 'gmm'",
                                                                               # Graphique GMM (modèle de mélange gaussien)
                                                                               fluidRow(
                                                                                 column(8,
                                                                                        h4(" Threshold Detection - Gaussian Mixture Model (GMM)"),
                                                                                        plotOutput("gmm_threshold_plot", height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                                                        p(downloadButton("download_gmm_threshold_plot","Download plot"),align="center")
                                                                                 ),
                                                                                 column(4,
                                                                                        h4(" GMM Statistics"),
                                                                                        verbatimTextOutput("gmm_stats")%>% withSpinner(color="#0dc5c1",type = 1)
                                                                                 )
                                                                               )
                                                             ),
                                                             
                                                             # Table des peptides stables
                                                             fluidRow(
                                                               column(6,
                                                                      h4(" Selected Stable Peptides"),
                                                                      
                                                                      dataTableOutput("stable_peptides_table")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      br(),
                                                                      p(downloadButton('download_stable_peptides', 
                                                                                       'Download stable peptides'),align="center")
                                                               ),
                                                               column(6,
                                                                      h4("Peptide Frequencies"),
                                                                      dataTableOutput("table_peptides") %>% withSpinner(color="#0dc5c1", type = 1),
                                                                      br(),
                                                                      p(downloadButton('table_peptides_all_freqencies', 
                                                                                       'Download the peptide frequency table'),align="center")
                                                               )
                                                             ),
                                                             fluidRow(
                                                               plotlyOutput("plot_selected_peptideBar", width = "100%", height = 800) %>% withSpinner(color="#0dc5c1",type = 1),
                                                               br(),
                                                               p(downloadButton("download_plot_selected_peptideBar", label = "Download plot"),
                                                                 downloadButton("saveTableselectedpeptides", label = "Downlaod data") ,align="center")
                                                             )
                                                             
                                            ),
                                            
                                            # Message d'attente pour bootstrap
                                            conditionalPanel(condition = "input.stats_method == 'bootstrap' && !output.bootstrap_completed",
                                                             div(class = "text-center", style = "margin-top: 50px;",
                                                                 conditionalPanel(condition = "input.run_bootstrap == 0",
                                                                                  h4(" Configure parameters and launch analysis", style = "color: #6c757d;"),
                                                                                  icon("flask", class = "fa-3x", style = "color: #63BFBF; margin-top: 20px;")
                                                                 ),
                                                                 conditionalPanel(condition = "input.run_bootstrap > 0 && output.progress_visible",
                                                                                  h4(" Analysis in progress...", style = "color: #007bff;"),
                                                                                  p("Follow the progress above", style = "color: #666;")
                                                                 )
                                                             )
                                            ),
                                            
                                            # Section Shapiro Fisher (existant)
                                            conditionalPanel(condition ="input.SFtest==true && input.stats_method == 'standard'",
                                                             column(6,
                                                                    conditionalPanel(condition ="input.help",
                                                                                     helpText("Results of Shapiro and Fisher tests")),
                                                                    plotOutput("plottestSF")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                    p(downloadButton("downloadplottestSF","Download plot"),
                                                                      downloadButton('downloaddatatestSF', 'Download raw data'),align="center")
                                                             )
                                            )
                                   ),
                                   # tabPanel("Statistics",
                                   #          conditionalPanel(condition ="input.help",
                                   #                           helpText("")),
                                   #          fluidRow(
                                   #            column(6,
                                   #                   radioButtons("test", "Tests",c( "No test"="notest","Wilcoxon Test" = "Wtest","Student Test" = "Ttest")),
                                   #                   conditionalPanel(condition ="input.help",
                                   #                                    helpText("The test will select the differently expressed variables. The willcoxon test (Mann-Whitney-Willcoxon test) is a non parametric test. The student test is parametrics (the group has to be normally distribute and  the variance equal (or effective superior to 30))")),
                                   #                   checkboxInput("SFtest","Shapiro and Fisher Tests",F),
                                   #                   conditionalPanel(condition ="input.help",helpText("The shapiro test is a test of normallity. The F test is a test of equality of variance."))
                                   #            ),
                                   #            column(6,br(),
                                   #                   numericInput("thresholdFC","choise of the Fold change threshold" , 0, min =0, max = 5, step = 0.5),
                                   #                   conditionalPanel(condition ="input.help",helpText("Fold Change is a mean of a groups divided by the mean of the other group. Mesure of the difference between the means of the 2 groups")),
                                   #                   numericInput("thresholdpv","choise of the p-value threshold %" , 0.05, min =0, max = 1, step = 0.01),
                                   #                   checkboxInput("adjustpv", "adjust p-value " , value = FALSE),
                                   #                   conditionalPanel(condition ="input.help", helpText("Benjamini & Hochberg correction"))
                                   #            )
                                   #          ),br(),
                                   #          p(downloadButton('downloaddatastatistics', 'Download statistics'),downloadButton('downloadddatadiff', 'Download differently expressed variables'),align="center"),
                                   #          hr(),
                                   #          conditionalPanel(condition= "input.test== 'Wtest' || input.test== 'Ttest'",
                                   #                           fluidRow(
                                   #                             column(6,
                                   #                                    textOutput("nvarselect2",inline=T), "selected variables",
                                   #                                    plotOutput("volcanoplot" ,width = 500,height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                   #                                    p(downloadButton("downloadvolcanoplot","Download plot"),downloadButton('downloaddatavolcanoplot', 'Download raw data'),align="center")
                                   #                             ),
                                   #                             column(6,
                                   #                                    textOutput("nbdiff",inline=T), "differently expressed",
                                   #                                    plotOutput("barplottest" ,width = 400,height = 500)%>% withSpinner(color="#0dc5c1",type = 1),   
                                   #                                    p(downloadButton("downloadbarplottest","Download plot"),downloadButton('downloaddatabarplottest', 'Download raw data'),align="center")
                                   #                             )
                                   #                           )
                                   #          )
                                   #          ,
                                   #          conditionalPanel(condition ="input.SFtest==true  ",
                                   #                           column(6,conditionalPanel(condition ="input.help",
                                   #                                                     helpText("Barplot presents the Results of shapiro and Fisher test")),
                                   #                                  plotOutput("plottestSF")%>% withSpinner(color="#0dc5c1",type = 1),
                                   #                                  p(downloadButton("downloadplottestSF","Download plot"),downloadButton('downloaddatatestSF', 'Download raw data'),align="center"))
                                   #                           
                                   #          )
                                   # ),
                                   tabPanel("Model",
                                            fluidRow(
                                              column(4,
                                                     radioButtons("model", "Type of model to adjust", 
                                                                  c("No model" = "nomodel","Random Forest"="randomforest","Support Vector Machine" = "svm"))),
                                              column(6,
                                                     numericInput("thresholdmodel","Threshold model" ,0, min = -1, max = 1, step = 0.05),
                                                     conditionalPanel(condition ="input.help", helpText("The threshold of the score is used for the validation")),
                                                     fluidRow(
                                                       column(7,checkboxInput("fs","features selection by cross validation /!\\ ",F))
                                                       # ,
                                                       # column(5,radioButtons("fstype",label=NULL,choice=c("Learning"="learn","Validation" = "val"),inline=T))
                                                     ),
                                                     helpText("/!\\ process can be long")
                                              )
                                            ),
                                            conditionalPanel(condition ="output.fileUploadedval & input.model!='nomodel'  ",
                                                             checkboxInput("adjustval","Adjust model on validation data",F)
                                            )
                                            ,
                                            hr(),
                                            conditionalPanel(condition ="input.model!='nomodel'  ",
                                                             fluidRow(
                                                               column(4,
                                                                      textOutput('nbselectmodel',inline=T),'selected variables', 
                                                                      
                                                                      h3("model learning")
                                                               ),
                                                               column(4,br(),downloadButton('downloaddatalearning', 'Download learning data')),
                                                               column(4, radioButtons("plotscoremodel", "",c( "boxplot"="boxplot","points" = "points")))
                                                             ),
                                                             fluidRow(
                                                               column(6,
                                                                      plotOutput("plotmodeldecouvroc")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton("downloadplotdecouvroc","Download plot"),
                                                                        downloadButton('downloaddatadecouvroc', 'Download raw data'),align="center")
                                                               ),
                                                               column(4,
                                                                      plotOutput("plotmodeldecouvbp")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton("downloadplotmodeldecouvbp","Download plot"),
                                                                        downloadButton('downloaddatamodeldecouvbp', 'Download raw data'),align="center")
                                                               ),
                                                               column(2,
                                                                      conditionalPanel(condition="input.plotscoremodel=='points'",checkboxInput("shownames1","show indivuals names",value=FALSE)),
                                                                      br(),
                                                                      tableOutput("tabmodeldecouv"),
                                                                      "Sensibility = ",textOutput("sensibilitydecouv",inline=T), 
                                                                      br(),
                                                                      "Specificity = ",textOutput("specificitydecouv",inline=T),
                                                                      br(),hr(),br(),
                                                                      tableOutput("youndendecouv")
                                                                      
                                                               )
                                                             ),
                                                             hr(),
                                                             conditionalPanel(condition ="input.adjustval==true  ",
                                                                              fluidRow(div(
                                                                                column(6,h3("model validation")), 
                                                                                column(6,br(),downloadButton('downloaddatavalidation', 'Download validation data')))
                                                                              ), 
                                                                              fluidRow(
                                                                                column(6,plotOutput("plotmodelvalroc")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                                       p(downloadButton("downloadplotvalroc","Download plot"),
                                                                                         downloadButton('downloaddatavalroc', 'Download raw data'),align="center")
                                                                                ),
                                                                                column(4,plotOutput("plotmodelvalbp")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                                       p(downloadButton("downloadplotmodelvalbp","Download plot"),
                                                                                         downloadButton('downloaddatamodelvalbp', 'Download raw data'),align="center")
                                                                                ),
                                                                                column(2,
                                                                                       #conditionalPanel(condition="input.plotscoremodel=='points'",checkboxInput("shownames2","show indivuals names",value=FALSE)),
                                                                                       tableOutput("tabmodelval"),
                                                                                       "Sensibility = ",textOutput("sensibilityval",inline=T), 
                                                                                       br(),
                                                                                       "Specificity = ",textOutput("specificityval",inline=T),
                                                                                       br(),hr(),br(),
                                                                                       tableOutput("youndenval")
                                                                                       
                                                                                )
                                                                              )
                                                             )
                                            )),
                                   tabPanel("Details of the model", 
                                            h3("Summary of the model"),
                                            verbatimTextOutput("summarymodel"),
                                            plotOutput("plotimportance"),
                                            p(downloadButton("downloadplotimportance","Download plot"),
                                              downloadButton('downloaddataplotimportance', 'Download raw data'),align="center")
                                   ),
                                   tabPanel("Test parameters",
                                            fluidRow(
                                              column(6,
                                                     h4("Selection Parameters"),
                                                     sliderInput("prctvaluestest", "Percent of values accepted",min = 0, max = 100, value = c(50,90),width="60%"),
                                                     checkboxGroupInput("selectmethodtest","Methods of selection ",c("selection on all samples"="nogroup","each group has more than x% of values "="bothgroups",
                                                                                                                     "at least one group has more than x% of more"="onegroup"),selected ="bothgroups" )
                                              ),
                                              column(6,
                                                     checkboxGroupInput("NAstructuretest", "Select variables with a NA's structure " , choices = list("TRUE /!\\"=TRUE,"FALSE"=FALSE),selected ="FALSE"),
                                                     helpText("/!\\ process can be long"),
                                                     conditionalPanel(condition ="output.testNAstructure ",
                                                                      fluidRow(
                                                                        column(6,
                                                                               numericInput("thresholdNAstructuretest","pvalue for the structure test" , 0.05, min = 0, max = 1, step = 0.005),
                                                                               radioButtons("structdatatest", "search structure in",c("all dataset" = "alldata","selected dataset" = "selecteddata"))
                                                                        ),
                                                                        column(6,
                                                                               numericInput("maxvaluesgroupmintest","The group with the minimum number of values has at most x% of values",value = 25,min = 0,max = 100,step = 5),
                                                                               numericInput("minvaluesgroupmaxtest","The group with the maximum number of values has at least y% of values",value = 75,min = 0,max = 100,step = 5)
                                                                        )
                                                                      )
                                                     )
                                              )
                                            ),
                                            #textOutput("testNAstructure"),
                                            #hr(),
                                            fluidRow(
                                              column(6,h3("Transform Parameters")),
                                              column(6,h3("Statistics Parameters"))
                                            ),
                                            fluidRow(
                                              column(3,
                                                     checkboxGroupInput("rempNAtest", "Replacing NA (Not Attributes) by",
                                                                        c("zero" = "z","mean of the cohort" = "moy",
                                                                          "mean by group"="moygr","PCA estimation" = "pca","Random forest estimation /!\\" = "missforest"),selected = "moygr")
                                              ),
                                              column(3,
                                                     #br(),br(),
                                                     checkboxGroupInput("logtest","transform data in log",choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected = "FALSE"),
                                                     radioButtons("logtypetest",label = NULL,c("ln"="logn","log 10"="log10","log2"="log2"),inline = TRUE),
                                                     checkboxGroupInput("standardizationtest","standardization dataset",choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected = "FALSE"),
                                                     checkboxGroupInput("arcsintest","arcsine transformation",choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected ="FALSE")
                                              ),
                                              #),
                                              #hr(),
                                              #fluidRow(
                                              column(3,
                                                     checkboxGroupInput("testtest", "Tests",
                                                                        c( "No test"="notest",
                                                                           "Wilcoxon Test" = "Wtest",
                                                                           "Student Test" = "Ttest"),
                                                                        selected = "Wtest"),
                                                     checkboxGroupInput("adjustpvtest", 
                                                                        "adjust p-value " , 
                                                                        choices = list("TRUE"=TRUE,"FALSE"=FALSE),
                                                                        inline = TRUE,
                                                                        selected = "FALSE")
                                              ),
                                              column(3,
                                                     numericInput("thresholdFCtest",
                                                                  "choise of the Fold change threshold" , 
                                                                  0, min =0, max = 5, step = 0.5),
                                                     numericInput("thresholdpvtest",
                                                                  "choise of the p-value threshold %" ,
                                                                  0.05, min =0, max = 1, step = 0.01)
                                              )
                                            ),
                                            #hr(),
                                            h3("Model Parameters"),
                                            fluidRow(
                                              column(3,
                                                     checkboxGroupInput("modeltest", "Type of model to adjust", c("No model" = "nomodel","Random Forest"="randomforest","Support Vector Machine" = "svm"),selected = "svm")
                                              ),
                                              column(4,
                                                     #numericInput("thresholdmodeltest","threshold model" ,0, min = -1, max = 1, step = 0.05),
                                                     checkboxGroupInput("fstest","features selection by cross validation",choices = list("TRUE /!\\"=TRUE,"FALSE"=FALSE),inline = TRUE,selected ="FALSE"),
                                                     helpText("/!\\ process can be long")
                                              ),
                                              column(5,
                                                     p(actionButton("tunetest",h4("Test all models"),width=200),align="center")
                                              )
                                            ),
                                            dataTableOutput("tabtestparameters") %>% withSpinner(color="#0dc5c1",type = 1),
                                            p(downloadButton("downloadtabtestparameters","Download dataset"),align="center")
                                   )
                       )
      )
    )
  )
))
