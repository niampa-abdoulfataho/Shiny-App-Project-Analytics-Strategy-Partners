# ============================================================
# app.R — Point d'entrée de l'application
# ============================================================

source("modules/global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
