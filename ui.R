library(shiny)
library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)


#Le header 
pageHeader <- dashboardHeader(
    title = "Basic dashboard",
    dropdownMenuOutput("messageMenu"),
    dropdownMenuOutput("notificationMenu"),
    dropdownMenu(type = "tasks", 
                 badgeStatus = "success",
                 taskItem(value = 90, color = "green",
                          "Documentation"
                 ),
                 taskItem(value = 17, color = "aqua",
                          "Project X"
                 ),
                 taskItem(value = 75, color = "yellow",
                          "Server deployment"
                 ),
                 taskItem(value = 80, color = "red",
                          "Overall project"
                 )
    )
)


# La barre latérale 
pageSiderbar <- dashboardSidebar(
    sidebarMenu(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        # Custom CSS to hide the default logout panel
        tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
        # The dynamically-generated user panel
        uiOutput("userpanel"),
        menuItem(tabName = "home", text = "Home", icon = icon("home")),
        menuItem(tabName = "another", text = "Another Tab", icon = icon("heart")),
        menuItem("Source code", icon = icon("file-code-o"), 
                 href = "https://github.com/rstudio/shinydashboard/",
                 badgeLabel = "link", badgeColor = "blue"),
        menuItem("Widgets", icon = icon("th"), tabName = "widgets",
                 badgeLabel = "new", badgeColor = "green")
    )
)


# Le corps de la page
pageBody <- dashboardBody(
    fluidRow(
        box(title = "Histogram",
            plotOutput("plot1"), 
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            background = "maroon"
            ),
        
        box(
            "Box content here", br(), "More box content",
            sliderInput("slider", "Slider input:", 1, 100, 50),
            textInput("text", "Text input:")
        )
    ),
    fluidRow(
        # A static valueBox
        valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
        
        # Dynamic valueBoxes
        valueBoxOutput("progressBox"),
        
        valueBoxOutput("approvalBox")
    ),
    fluidRow(
        # Clicking this will increment the progress amount
        box(width = 4, actionButton("count", "Increment progress"))
    ),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
)
    



#===============================================================================
# Le tableau de bord 
dashboardPage(pageHeader, pageSiderbar, pageBody)

#===============================================================================

