# ============================================================
# auth.R — Configuration shinymanager
# ============================================================
# shinymanager utilise une fonction personnalisée pour vérifier
# les credentials depuis PostgreSQL (au lieu de son SQLite interne)
# ============================================================

#' Fonction de vérification des credentials
#' Appelée par shinymanager à chaque tentative de connexion
#'
#' @param user  email saisi
#' @param password mot de passe saisi
#' @return liste avec result (TRUE/FALSE) + user_info
check_credentials_db <- function(user, password) {

  # Chercher l'utilisateur en BD
  user_data <- tryCatch(
    db_verify_password(user, password),
    error = function(e) NULL
  )

  # Échec : utilisateur non trouvé ou mot de passe incorrect
  if (is.null(user_data) || nrow(user_data) == 0) {
    return(list(
      result   = FALSE,
      user_info = data.frame()
    ))
  }

  # Succès : retourner les infos utilisateur
  list(
    result = TRUE,
    user_info = data.frame(
      user          = user_data$email,
      nom_structure = user_data$nom_structure,
      role          = user_data$role,
      utilisateur_id = user_data$id,
      stringsAsFactors = FALSE
    )
  )
}

#' UI de connexion personnalisée
#' Remplace la page de login par défaut de shinymanager
auth_ui <- function(id = "auth") {
  tags$div(
    style = "
      min-height:100vh; background:#0f1117;
      display:flex; align-items:center; justify-content:center;
    ",
    tags$div(
      style = "
        background:#13151f; border:1px solid #1e2232;
        border-radius:16px; padding:40px 48px;
        width:400px; position:relative; overflow:hidden;
      ",

      # Ligne dorée en haut
      tags$div(style = "
        position:absolute; top:0; left:0; right:0; height:3px;
        background:linear-gradient(90deg,#f0c040,#e07b20,#f0c040);
      "),

      # Logo + titre
      tags$div(style = "text-align:center; margin-bottom:32px;",
        tags$img(
          src    = "https://cdn-icons-png.flaticon.com/512/1534/1534938.png",
          height = "52px",
          style  = "margin-bottom:12px;"
        ),
        tags$h2(
          "GestioProjets",
          style = "
            font-family:'Playfair Display',serif;
            color:#f0c040; font-size:22px;
            font-weight:700; margin:0; letter-spacing:1px;
          "
        ),
        tags$p(
          "Système de suivi des projets",
          style = "color:#3a3f52; font-size:12px; margin-top:4px;"
        )
      ),

      # Champs
      tags$div(style = "margin-bottom:16px;",
        tags$label("Email", style = "
          color:#6b7280; font-size:11px; font-weight:600;
          text-transform:uppercase; letter-spacing:.7px;
        "),
        tags$input(
          id = "user_id", type = "email",
          placeholder = "votre@email.bf",
          style = "
            width:100%; background:#0a0c14; color:#d4d8e8;
            border:1px solid #1e2232; border-radius:7px;
            padding:10px 14px; font-size:13px; margin-top:4px;
            box-sizing:border-box; outline:none;
          "
        )
      ),
      tags$div(style = "margin-bottom:24px;",
        tags$label("Mot de passe", style = "
          color:#6b7280; font-size:11px; font-weight:600;
          text-transform:uppercase; letter-spacing:.7px;
        "),
        tags$input(
          id = "password", type = "password",
          placeholder = "••••••••",
          style = "
            width:100%; background:#0a0c14; color:#d4d8e8;
            border:1px solid #1e2232; border-radius:7px;
            padding:10px 14px; font-size:13px; margin-top:4px;
            box-sizing:border-box; outline:none;
          "
        )
      ),

      # Bouton connexion
      tags$button(
        "Se connecter",
        id    = "btn_login",
        style = "
          width:100%;
          background:linear-gradient(135deg,#f0c040,#e07b20);
          color:#0f1117; border:none; border-radius:8px;
          font-weight:700; font-size:14px; letter-spacing:.5px;
          padding:12px; cursor:pointer; text-transform:uppercase;
        "
      ),

      # Message d'erreur
      uiOutput("login_error"),

      tags$div(
        style = "text-align:center; margin-top:24px;",
        tags$span(
          "GestioProjets v1.0 · Burkina Faso",
          style = "color:#2e3347; font-size:10px;"
        )
      )
    )
  )
}
