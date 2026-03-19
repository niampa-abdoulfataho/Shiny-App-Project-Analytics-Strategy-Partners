#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

"==========================================================================="
"======         Données pour le message ===================================="
"==========================================================================="
messageData <- reactive(
   data.frame(
   from    = c("Alice", "Bob"),
   message = c("Rapport prêt", "Réunion à 14h ?"),
   time    = c("Il y a 5 min", "Il y a 1h"),
   stringsAsFactors = FALSE
))

"=============================================================================="

# Define server logic required to draw a histogram
function(input, output, session) {
        set.seed(122)
        histdata <- rnorm(500)
        output$plot1 <- renderPlot({
            data <- histdata[seq_len(input$slider)]
            hist(data)
        })
        
        # Message pour afficher dans le header
        messageData <- reactive(
           data.frame(
              from    = c("Saïdou YAMEOGO", "A. Fatah NIAMPA"),
              message = c("Rapport prêt", "Réunion à 14h ?"),
              time    = c("Il y a 5 min", "Il y a 1h"),
              stringsAsFactors = FALSE
           ))
        # Le contenu de menu message
        output$messageMenu <- renderMenu({
           # Code to generate each of the messageItems here, in a list. This assumes
           # that messageData is a data frame with two columns, 'from' and 'message'.
           msgs <- lapply(seq_len(nrow(messageData())), function(i) {
              messageItem(
                 from    = messageData()$from[i],
                 message = messageData()$message[i],
                 time = messageData()$message[i]
              )
           })
           
           # This is equivalent to calling:
           dropdownMenu(type = "messages", .list = msgs)
        })
        
        # Message pour les notifications dans le header
        notificationData <- reactive(
           data.frame(
              text    = c(
                 "5 new users today", 
                 "12 items delivered", 
                 "Server load at 86%"
                 ),
              icon = c("users", "truck", "exclamation-triangle"),
              status = c("primary", "success","warning"),
              stringsAsFactors = FALSE
           ))
        
        # Le contenu de menu notification 
        output$notificationMenu <- renderMenu({
           # Code to generate each of the messageItems here, in a list. This assumes
           # that messageData is a data frame with two columns, 'from' and 'message'.
           msgs <- lapply(seq_len(nrow(notificationData())), function(i) {
              notificationItem(
                 text    = notificationData()$text[i],
                 icon = icon(notificationData()$icon[i]),
                 status = notificationData()$status[i]
              )
           })
           
           # This is equivalent to calling:
           dropdownMenu(type = "notifications", .list = msgs)
        })
        
        #connexion deconnexion
        output$userpanel <- renderUI({
           # session$user is non-NULL only in authenticated sessions
           if (!is.null(session$user)) {
              sidebarUserPanel(
                 span("Logged in as ", session$user),
                 subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
           }
        })
}
