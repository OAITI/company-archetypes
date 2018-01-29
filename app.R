library(shiny)
library(shinythemes)
library(shinycssloaders)
library(psych)
library(png)
library(ggplot2)
library(RColorBrewer)

addResourcePath(prefix = "images", directoryPath = "images/")

theme_set(theme_bw(base_size = 20))

# Read archetype blurb
blurb <- read.csv("data/archetype_description.csv")

profiles <- read.csv("data/archetype_labels.csv")
rownames(profiles) <- profiles$Label
profiles$Label <- NULL
p <- principal(profiles, nfactors=2, scores = TRUE)
plotscores <- as.matrix(profiles) %*% p$loadings[,1:2]

fac1 <- diff(range(plotscores[,1]))
scores1 <- as.hexmode(round(plotscores[,1] * (255 / fac1)))
fac2 <- diff(range(plotscores[,2]))
scores2 <- as.hexmode(round(plotscores[,2] * (255 / fac2)))
thecolors <- paste0("#", scores1, "00", scores2)

## Load images
img1 <- readPNG("images/png/lightbulb.png")
img2 <- readPNG("images/png/forecasters.png")
img3 <- readPNG("images/png/key.png")
img4 <- readPNG("images/png/detective.png")
img5 <- readPNG("images/png/newbie2.png")
img6 <- readPNG("images/png/robotic-arm.png")
img7 <- readPNG("images/png/datadec2.png")
all_imgs <- list(img1, img2, img3, img4, img5, img6, img7)
names(all_imgs) <- rownames(profiles)
file_imgs <- as.list(file.path("images", "png", list.files("images/png")[c(5, 3, 4, 2, 6, 7, 1)]))
names(file_imgs) <- rownames(profiles)

mydf <- as.data.frame(plotscores)
mydf$Label <- factor(rownames(plotscores), levels = rownames(plotscores)[c(5, 1, 6, 4, 2, 7, 3)])

myplot <- ggplot(mydf, aes(RC1,RC2)) + geom_point(aes(color=Label), size=6) + 
    labs(color="Archetypes", title = "Archetype Map") + 
    scale_color_manual(values = thecolors[c(c(5, 1, 6, 4, 2, 7, 3))]) +
    geom_text(size = 6, aes(x = RC1, y = RC2, label = rownames(plotscores)), nudge_y = 0.08) +
    xlim(c(-0.1, 3)) +
    ylim(c(-0.1, 2.1)) +
    xlab(expression(paste("Low Implementation ", symbol("\254"), symbol("\276"), symbol("\256"), " High Implementation"))) +
    ylab(expression(paste("Low Innovation ", symbol("\254"), symbol("\276"), symbol("\256"), " High Innovation"))) +
    theme(axis.title.x = element_text(color = "#c50026"),
          axis.title.y = element_text(color = "#1500a8"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "off",
          plot.margin=unit(c(0,0,0,0),"mm")) +
    coord_fixed()

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                includeCSS("css/styles.css"),
                
                titlePanel("Data Science Archetypes"),
                
                sidebarLayout(sidebarPanel(
                    a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
                    h4("About"),
                    HTML("Ever wondered if you could define the <i>type</i> of a data science organization by its activities? This app tells you the \"data mindset\" of your organization. To get the archetype, simply answer some questions about the role of various aspects of data science within your organization. To begin, click on <b>Take the Quiz!</b> button below."),
                    hr(),
                    actionButton("goButton", "Take the Quiz!",icon=icon("play-circle"))#,
                    #textOutput("debug")
                ),
                
                mainPanel(
                    tabsetPanel(id = "tabs1",
                                
                                tabPanel(
                                    "Quiz",
                                    uiOutput("quiz_ui")
                                ),
                                
                                tabPanel(
                                    "Statistics",
                                    uiOutput("stat_ui")
                                )
                    )
                )
))

server <- shinyServer(function(input, output, session) {
    
    ## Reactive Values to hold User Data
    user <- reactiveValues(user = NULL,question=NULL,if_finish_quiz=FALSE,if_finish_all=FALSE,user_response=numeric(),user_vector=NULL, pred_label = NULL)
    
    ## Reactive Values to hold input data
    data <- reactiveValues(q_v_d=NULL,l_v_d=NULL,l_v_d_mean=NULL)
    
    ## Reactive Values to hold Debug
    #debug <- reactiveValues(debug=NULL)
    
    ## Create a new Temporary User if the Take Quiz is pressed
    observeEvent(input$goButton,{
        ## Load Questions Var data
        data$q_v_d <- read.csv("data/question_vars.csv",check.names=FALSE,stringsAsFactors=FALSE)
        
        ## Load Label Var data and preprocess
        data$l_v_d <- read.csv("data/labels_vars.csv",check.names=FALSE,stringsAsFactors=FALSE)
        l_v_d_split <- split(data$l_v_d[,-(1:4)],data$l_v_d$Label)
        data$l_v_d_mean <- do.call(rbind,lapply(l_v_d_split,function(x) apply(x,2,mean)))
        
        user$user <- tempfile(pattern = "User", tmpdir = "")
        user$question <- 1
        user$if_finish_quiz <- FALSE
        user$user_response <- numeric()
        updateTabsetPanel(session, "tabs1",selected = "Quiz")
    })
    
    ## Render UI for asking questions and displaying results
    output$quiz_ui<-renderUI ({  
        ## If start and ask questions
        if(!is.null(user$user)&!user$if_finish_quiz)
        {
            return(list(
                ## Question UI
                HTML(paste0("<h3>What is the level of effort devoted to <b>",tolower(data$q_v_d$Question[user$question]), "</b> in your organization?</h3>")),
                #tags$style("#q_choice {font-size:20px;}"),
                radioButtons("q_choice", label="", selected = .5,inline = FALSE,width="400px", choices = c("High" = 1,"High-Medium" = .75,"Medium" = .5,"Low-Medium" = .25,"Low" = 0)),
                actionButton("ansButton", label = "Submit")
            ))
        } else if (is.null(user$user)) {
            list(
                h4("Find out which of these archetypes your organization fits in!"),
                withSpinner(tableOutput("desc")),
                hr(),
                withSpinner(plotOutput("descplot", width = 700, height = 700)),
                HTML("This map shows the positions of the main archetypes created using the five constructs shown below.<br><br>
                     <b>Creation/Innovation:</b>	Researching and creating new products to solve challenges <br>
                     <b>Automation:</b>	Focus on doing faster <br>
                     <b>Prediction:</b>	Knowing/understanding the future <br>
                     <b>Insight:</b>	Better understanding business processes and clients <br>
                     <b>Action:</b>	Implementing analytical models and modifying decisions based on outputs <br>
                     ")
            )
        }
    })
    
    output$desc <- renderTable({
        return(blurb[,1:2])
    })
    output$descplot <- renderPlot({
        ggplot(mydf, aes(RC1,RC2)) + geom_point(aes(color=Label), size=0) + 
            labs(color="Archetypes", title = "Archetype Map") + 
            scale_color_manual(values = thecolors[c(c(5, 1, 6, 4, 2, 7, 3))]) +
            geom_text(size = 6, aes(x = RC1, y = RC2, label = rownames(plotscores)), nudge_y = -0.01) +
            xlim(c(-0.1, 3)) +
            ylim(c(-0.1, 2.1)) +
            xlab(expression(paste("Low Implementation ", symbol("\254"), "    ", symbol("\256"), " High Implementation"))) +
            ylab(expression(paste("Low Innovation ", symbol("\254"),"    ", symbol("\256"), " High Innovation"))) +
            theme(axis.title.x = element_text(color = "#c50026"),
                  axis.title.y = element_text(color = "#1500a8"),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.grid = element_blank(),
                  legend.position = "off",
                  plot.margin=unit(c(0,0,0,0),"mm")) +
            coord_fixed() + 
            annotation_raster(all_imgs[[1]], xmin=mydf$RC1[1]-0.1, xmax=mydf$RC1[1]+0.1, ymin = mydf$RC2[1]+0.05, ymax = mydf$RC2[1]+0.24) +
            annotation_raster(all_imgs[[2]], xmin=mydf$RC1[2]-0.11, xmax=mydf$RC1[2]+0.15, ymin = mydf$RC2[2]+0.02, ymax = mydf$RC2[2]+0.25) +
            annotation_raster(all_imgs[[3]], xmin=mydf$RC1[3]-0.1, xmax=mydf$RC1[3]+0.14, ymin = mydf$RC2[3]+0.03, ymax = mydf$RC2[3]+0.25) +
            annotation_raster(all_imgs[[4]], xmin=mydf$RC1[4]-0.12, xmax=mydf$RC1[4]+0.15, ymin = mydf$RC2[4]+0.03, ymax = mydf$RC2[4]+0.25) +
            annotation_raster(all_imgs[[5]], xmin=mydf$RC1[5]-0.1, xmax=mydf$RC1[5]+0.1, ymin = mydf$RC2[5]+0.05, ymax = mydf$RC2[5]+0.25) +
            annotation_raster(all_imgs[[6]], xmin=mydf$RC1[6]-0.1, xmax=mydf$RC1[6]+0.13, ymin = mydf$RC2[6]+0.05, ymax = mydf$RC2[6]+0.27) +
            annotation_raster(all_imgs[[7]], xmin=mydf$RC1[7]-0.15, xmax=mydf$RC1[7]+0.16, ymin = mydf$RC2[7]+0.02, ymax = mydf$RC2[7]+0.28) 
    })
    
    ## Render Statistics UI for display result and ask user info
    output$stat_ui<-renderUI ({  
        ## If Finish and show result
        if(!is.null(user$user) & user$if_finish_quiz & !user$if_finish_all)
        {
            ## Calculate Nearest Label
            user$user_vector <- apply(user$user_response * data$q_v_d[,-1],2,sum)/apply(data$q_v_d[,-1],2,sum)
            user$pc_vector <- user$user_vector %*% p$loadings[,1:2]
            user$pred_label <- names(which.min(as.matrix(dist(rbind(user$pc_vector,plotscores)))[1,-1]))
            
            return(list(
                ## Stat UI
                h2("Results",align="center"),
                HTML(paste0("<h3>"," Your archetype is represented by <b>'",user$pred_label,"'</b>!  <img src='", file_imgs[[user$pred_label]], "' width='90'></h3>")),
                br(),
                h4(blurb$Description[blurb$Archetype==user$pred_label]),
                h4(blurb$Details[blurb$Archetype==user$pred_label]),
                hr(),
                withSpinner(plotOutput("mdsplot", width = 700, height = 700)),
                hr(),
                strong(h2("Feedback",align="center")),
                HTML("<h4>Please give us some feedback by selecting the archetype that you think fits your organization. We shall use this information to make the quiz more efficient.</h4>"),
                br(),
                textInput("name_in", "Name", value = "Name", width = NULL, placeholder = NULL),
                textInput("org_in", "Organization", value = "Organization", width = NULL, placeholder = NULL),
                radioButtons("act_label", label="Select Actual Label", choices =rownames(data$l_v_d_mean) , selected = user$pred_label,inline = FALSE, width = NULL),
                actionButton("finButton", label = "Submit")
            ))
        }
        if(!is.null(user$user) & user$if_finish_quiz & user$if_finish_all)
        {
            return(list(
                ## Stat UI
                h1("Thank you!",align="center")
            ))
        }
    })
    
    output$mdsplot <- renderPlot({
        myplot +
            #geom_point(data=as.data.frame(plotscores_new), size=4) +  
            annotation_raster(all_imgs[[user$pred_label]], xmin=user$pc_vector[1,1]-0.15, xmax=user$pc_vector[1,1]+0.15, ymin = user$pc_vector[1,2]-0.15, ymax = user$pc_vector[1,2]+0.15)
        
    })
    
    ## Submit the answer and process values
    observeEvent(input$ansButton,{
        user$user_response[user$question] <- as.numeric(input$q_choice)
        if(user$question==nrow(data$q_v_d))
        {
            user$if_finish_quiz <- TRUE
            updateTabsetPanel(session, "tabs1",selected = "Statistics")
        }
        user$question <- user$question + 1
    })
    
    ## Submit the Details and process
    observeEvent(input$finButton,{
        write.table(cbind(data.frame(input$act_label,input$name_in,input$org_in,Sys.time()),t(data.frame(user$user_vector))),file="data/labels_vars.csv",sep=",",append=TRUE,col.names=FALSE,row.names=FALSE)
        user$if_finish_all <- TRUE
    })
    
    ## Debug code line
    #output$debug <- renderText({(dim(debug$debug))})
})

shinyApp(ui = ui, server = server)
