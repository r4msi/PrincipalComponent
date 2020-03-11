
library(shiny)
library(dashboardthemes)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(dplyr)
library(shinycssloaders)


###############


lb <- shinyDashboardLogoDIY(
  
  boldText = "",
  mainText = "",
  textSize = 20,
  badgeText = "PRINCIPAL COMPONENT",
  badgeTextColor = "white",
  badgeTextSize = 2,
  badgeBackColor = "#ad40e0",
  badgeBorderRadius = 3
  
  
)



load("ccaa.Rdata")

ccaa_location <- data.frame(location = factor(c("South", "North", "North", "ExtraPeninsular", "ExtraPeninsular", "North", "Center", "Center", "North", "ExtraPeninsular", "Center", "South", "South", "Center", "ExtraPeninsular", "South", "North", "North", "North" )))


# PCA
pc <- prcomp(ccaa,scale. = T)

# Eigenvectors applied to observations.
res <- pc$x*(-1) # changing the direction.
x <- res[,1]
y <- res[,2]
z <- res[,3]

# Loadings/Eigenvectors
ev <- pc$rotation*-1 # Changing the direction.



# 
# 
# ######################################################## theme
theme_grey_blue <- shinyDashboardThemeDIY(

    ### general
    appFontFamily = "Arial"
    ,appFontColor = "rgb(255,255,255)"
    ,primaryFontColor = "rgb(255,255,255)"
    ,infoFontColor = "rgb(255,255,255)"
    ,successFontColor = "rgb(255,255,255)"
    ,warningFontColor = "rgb(255,255,255)"
    ,dangerFontColor = "rgb(255,255,255)"
    ,bodyBackColor = cssGradientThreeColors(
        direction = "down",
        colorStart = "rgb(0,0,0)",
        colorMiddle = "rgb(35, 35, 35)",
        colorEnd = "rgb(52, 62, 80)",
        colorStartPos = 0,
        colorMiddlePos = 70,
        colorEndPos = 100
    )

    ### header
    ,logoBackColor = "rgb(70,80,90)"

    ,headerButtonBackColor = "rgb(70,80,90)"
    ,headerButtonIconColor = "rgb(25,35,45)"
    ,headerButtonBackColorHover = "rgb(40,50,60)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"

    ,headerBackColor = "rgb(70,80,90)"
    ,headerBoxShadowColor = ""
    ,headerBoxShadowSize = "0px 0px 0px"

    ### sidebar
    ,sidebarBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "rgb(0, 0, 0)"
        ,colorMiddle = "rgb(35, 36, 35)"
        ,colorEnd = "rgb(52, 62, 80)"
        ,colorStartPos = 0
        ,colorMiddlePos = 70
        ,colorEndPos = 100
    )

    ,sidebarShadowRadius = ""
    ,sidebarPadding = 5
    ,sidebarShadowColor = "0px 0px 0px"

    ,sidebarMenuBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "rgb(52,62,70)"
        ,colorMiddle = "rgb(52, 62, 80)"
        ,colorEnd = "rgb(89, 98, 112)"
        ,colorStartPos = 0
        ,colorMiddlePos = 70
        ,colorEndPos = 100
    )
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 10

    ,sidebarUserTextColor = "rgb(128,177,221)"

    ,sidebarSearchBackColor = "rgb(40,70,115)"
    ,sidebarSearchIconColor = "rgb(50,115,145)"
    ,sidebarSearchBorderColor = "rgb(30,60,105)"

    ,sidebarTabTextColor = "rgb(128,177,221)"
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none"
    ,sidebarTabBorderColor = "none"
    ,sidebarTabBorderWidth = 0

    ,sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgb(0, 255, 255)"
        ,colorMiddle = "rgb(65,95,145)"
        ,colorEnd = "rgb(68,84,137)"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorSelected = "rgb(255,255,255)"
    ,sidebarTabRadiusSelected = "30px"

    ,sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgb(52, 62, 80)"
        ,colorMiddle = "rgb(65,95,145)"
        ,colorEnd = "rgb(0,255,255)"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorHover = "rgb(255,255,255)"
    ,sidebarTabBorderStyleHover = "none"
    ,sidebarTabBorderColorHover = "none"
    ,sidebarTabBorderWidthHover = 0
    ,sidebarTabRadiusHover = "30px"

    ### boxes
    ,boxBackColor = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgb(70,75,125)"
        ,colorMiddle = "rgb(65,79,129)"
        ,colorEnd = "rgb(55,70,120)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,boxBorderRadius = 15
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = ""
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(49,56,107)"
    ,boxPrimaryColor = "rgb(141,192,241)"
    ,boxInfoColor = "rgb(20,100,160)"
    ,boxSuccessColor = "rgb(64,186,170)"
    ,boxWarningColor = "rgb(255,217,144)"
    ,boxDangerColor = "rgb(249,144,144)"

    ,tabBoxTabColor = "rgb(80,95,155)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(128,177,221)"
    ,tabBoxTabTextColorSelected = "rgb(255,255,255)"
    ,tabBoxBackColor = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgb(70,75,125)"
        ,colorMiddle = "rgb(65,79,129)"
        ,colorEnd = "rgb(55,70,120)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,tabBoxHighlightColor = "rgb(80,95,155)"
    ,tabBoxBorderRadius = 15

    ### inputs
    ,buttonBackColor = "rgb(230,230,230)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(50,50,50)"
    ,buttonBorderRadius = 5

    ,buttonBackColorHover = "rgb(180,180,180)"
    ,buttonTextColorHover = "rgb(50,50,50)"
    ,buttonBorderColorHover = "rgb(50,50,50)"

    ,textboxBackColor = "rgb(68,80,90)"
    ,textboxBorderColor = "rgb(76,90,103)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(80,90,100)"
    ,textboxBorderColorSelect = "rgb(255,255,255)"

    ### tables
    ,tableBackColor = "transparent"
    ,tableBorderColor = "rgb(80,95,155)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1

)

###########DATA


 


############################################ Header
header <- dashboardHeader(
    title = lb
)

############################################## sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        HTML(paste0(
            "<br>",
            "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/R_logo.svg/724px-R_logo.svg.png' height = '50' width = '60'></a>",
            "<br>"
            
            
        )),
        menuItem("Autonomous Communities",
                 icon = icon("tasks"),
                 tabName = "cc"
   
                 
        )
        ,menuItem("Math PDF Explanation"
                 ,icon = icon("download", lib = "font-awesome")
                 ,downloadButton(
                   outputId = "data",
                   label = "Download"
                 )
                  
                 
        )
    
        
        
        ,HTML(paste0(
            
            "<table style='margin-left:auto; margin-right:auto;'>",
            "<tr>",
            "<td style='padding: 5px;'><a href='https://github.com/r4msi' target='_blank'><i class='fab fa-github'></i></i></i></a></td>",
            "<td style='padding: 5px;'><a href='https://www.linkedin.com/in/manuel-alda-mart%C3%ADn-mora-566ba419b/' target='_blank'><i class='fab fa-linkedin-in'></i></i></a></td>",
            "</table>",
            "<br>")
        )
        
    )
    
    
)

####################################################### body
body <- dashboardBody(
    
    theme_grey_blue
    ,withMathJax()
    ,tabItems(
        tabItem(tabName = "cc"
                ,widgetUserBox(
                    title = "3D PCA"
                    ,src='https://upload.wikimedia.org/wikipedia/commons/3/37/Plotly-logo-01-square.png'
                    ,color = "maroon"
                    ,width = 8
                    ,footer_padding = F
                    ,footer = plotlyOutput("pl",height = 600) %>% withSpinner(color="#0dc5c1")
                    
 
                )
                
                ,carousel(width = 4
                    ,id = "mycarousel",
                    carouselItem(
                      caption = "Item 1",
                      valueBox(
                        "PrinCompAnalysis"
                        ,"The more data the better! This is 
                        totally true when talking about observations, but...is it always the case with columns? 
                        Sometimes when two variables are highly correlated, they are providing the same information 
                        to your models. Here is where dimensionality reduction techniques come into play. ", icon = icon("robot", lib = "font-awesome"),
                        color = "blue",width = 12
                      )
                    )
                    ,carouselItem(
                      caption = "Item 2",
                      valueBox(
                        "Linear relation:"
                        ,"\\(\\ CP_1 =  v_{11}*x_1 + v_{12}*x_2 + ... + a_{1m}*x_m\\)
                        \\(\\ A * v_{(matricial product)}=v*\\lambda_{(escalarproduct)}\\) (Eigenvalues/vectors)
                        Extracted from the covariance matrix S"
                        ,width = 12
                      )
                    )
                   
                    
                )
                
        )
    )
)


####################################################### ui

ui <- dashboardPage(header, sidebar, body, title = "PCA")


####################################################### server
server <- function(input, output) {
    
    output$pl <- renderPlotly ({
      
   
      plv <- plot_ly() %>%
        add_trace(x=x, y=y, z=z,
                  type="scatter3d",
                  mode="markers",
                  color=ccaa_location$location,
                  text = rownames(ccaa)
        )
      



      for (i in 1:nrow(ev)) {
        x <- c(0, ev[i,1])*4 # Creating a vector the origin is 0, and direction vij.
        y <- c(0, ev[i,2])*4 # Multiplied * 4 because of the standarizarion that us PrComp function.
        z <- c(0, ev[i,3])*4
        plv <- plv %>% add_trace(x=x, y=y, z=z,
                                 type="scatter3d", mode="lines",
                                 line = list(width=8),
                                 opacity = 1, name = names(ccaa)[i])
      }

      plv <- plv %>%
        layout(
          title = "Principal Component: 82.75%",
          scene = list(
            xaxis = list(title = "PC1 (Age under 19) 40%",
                         backgroundcolor="rgb(0, 0,0)",
                         gridcolor="rgb(255,255,255)",
                         showbackground=TRUE,
                         zerolinecolor="rgb(152, 78, 165)"
            ),

            yaxis = list(title = "PC2 (Political Party) 25.8%",
                         backgroundcolor="rgb(0, 0,0)",
                         gridcolor="rgb(255,255,255)",
                         showbackground=TRUE,
                         zerolinecolor="rgb(152, 78, 165)"
            ),
            zaxis = list(title = "PC3 (Left_Wing) 17%",
                         backgroundcolor="rgb(0, 0,0)",
                         gridcolor="rgb(255,255,255)",
                         showbackground=TRUE,
                         zerolinecolor="rgb(152, 78, 165)"

            )
          ))
      
       
      
    })
    
    
    output$data <- downloadHandler(
      
      filename = function() {
        paste("pcca", "pdf", sep=".")
      },
      content = function(files) {
        file.copy(from = "CPw.pdf", files)
      }
      
    )
    
    output$likeBox <- renderValueBox({
      valueBox(
        paste0(5 + input$count, "%"), "Would you use PCA?", icon = icon("list"),
        color = "purple"
      )
    })
    
    
    
}

######################################################## app
shinyApp(ui = ui, server = server)
