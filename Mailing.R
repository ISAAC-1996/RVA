##############################################################
################ MAIL POUR LA CAMPAGNE VA CHIER #############
library(mailR)
library(httr)
library(ospharm2)
library(tidyverse)

glob <- list(
  log = TRUE,
  ymdhms = "%Y-%m-%d_%H-%M-%S",
  month = NULL, # If need to force something != than prev month
  customer_to = "biama@ospharea.com",
  customer_cc = "adouchin@ospharea.com",
  ospharm_from = "Coopérative OSPHARM<solutions@ospharea.com>",
  ospharm_cc = "datastat@ospharea.com")
# Exécution de la requête SQL pour récupérer les informations des clients

Infos <- executer_sql("SELECT a.cip, mail
FROM vuProdAdhpha a
LEFT JOIN ospharea_adherents ON finess_adhpha = finess
WHERE dextinct_adhpha IS NULL
AND a.n_auto_adhpha NOT IN (
    SELECT n_auto_adhpha
    FROM os_completudepha WHERE periode_completudepha = 202502 AND moisok_completudepha IN (0,8))")

# Liste des administrateurs pour le rapport final
admins <- c("biama@ospharea.com")

# Initialisation des compteurs et logs
emails_envoyes <- 0
emails_echoues <- 0
log_erreurs <- list()

# Fonction pour envoyer un email personnalisé
envoyer_email <- function(destinataire, cip) {
  # Génération du contenu HTML avec le CIP unique
  html_content <- paste0('
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <!-- Retire ou commente la ligne viewport pour éviter la responsivité -->
  <!-- <meta name="viewport" content="width=device-width, initial-scale=1.0"> -->
  <title>Campagne Mars Bleu</title>
  <!-- Script Kaspersky conservé (même si déconseillé dans un email) -->
  <script type="text/javascript" src="https://me.kis.v2.scr.kaspersky-labs.com/FD126C42-EBFA-4E12-B309-BB3FDD723AC1/main.js?attr=MFKeeqGWPM_maQywHYKQLrM1PvuSURbrjq9GHyk2eT1DTuoPI3bw9TO7b-jpKX2kbM80bUs82OFEYNhwGE6y5s3wmGtTZbshorljv918MT-XyluWnYEp-5xa2YZxyp4hwoOoyA6VPDjPqH7CNpcMw04u0zDmcK9aDWlTXYHQp00UJN4koV7vv4T8IrtXCQH-FzZoEas1hF3tM_ebhzaZNsqM7yLSbVo6yb2iDbBlaxuVC_toWMYfk0AeAzWQ9mwm7LZHG1Jv9iurlDCJ0NFcXh6tblGLrxoB6HRU6hDa4PFr9fd3SYOMwQp9zW4sYPht" charset="UTF-8"></script>
  <link rel="stylesheet" crossorigin="anonymous" href="https://me.kis.v2.scr.kaspersky-labs.com/E3E8934C-235A-4B0E-825A-35A08381A191/abn/main.css?attr=aHR0cHM6Ly9vc3BoYXJlYS1teS5zaGFyZXBvaW50LmNvbS9wZXJzb25hbC9sam91cmRhaW5fb3NwaGFyZWFfY29tL19sYXlvdXRzLzE1L2Rvd25sb2FkLmFzcHg_U291cmNlVXJsPSUyZnBlcnNvbmFsJTJmbGpvdXJkYWluX29zcGhhcmVhX2NvbSUyZkRvY3VtZW50cyUyZkZpY2hpZXJzJTIwZGUlMjBjb252ZXJzYXRpb24lMjBNaWNyb3NvZnQlMjBUZWFtcyUyZmVtYWlsJTIwMS5odG1s"/>
</head>

<body style="margin:0; padding:0; background-color:#f8f8f8; font-family:Arial, sans-serif;">
 
<!-- TABLE PRINCIPALE (Largeur fixe) -->
<table width="800" border="0" cellpadding="0" cellspacing="0" align="center" 
       style="margin:20px auto; border:3px solid #6cb33f; border-radius:10px; background-color:#ffffff;">
  
  <tr>
    <td style="padding:0; text-align:center; border-radius:10px 10px 0 0;">
      <!-- L&apos;image occupe la largeur de la table, arrondie sur le haut -->
      <img src="https://isaac-1996.github.io/RVA/ttt.png" 
           alt="OSPHARM & Ruban Mars Bleu" 
           style="display:block; width:100%; border-radius:10px 10px 0 0;" />
    </td>
  </tr>
  
  <tr>
    <td>
      <hr style="border:0; border-top:1px solid #dadada; margin:20px auto; width:80%;" />
    </td>
  </tr>
  
  <tr>
    <td style="padding:0 20px; text-align:center;">
      <p style="font-size:16px; color:#333; margin:10px 0;">
        Vous êtes pharmacien titulaire et avez décidé de relayer et d&apos;animer votre officine autour de la <br>
        <br>
        <strong>CAMPAGNE MARS BLEU</strong>
      </p>
      <p style="font-size:16px; color:#333; margin:10px 0;">
        Faites-le nous savoir en moins d&apos;1 minute via le lien ci-dessous :
      </p>
      <p>
        <a href="https://campagne.ospharm.org/?campagne=vachier&cip=',cip,'"
           style="font-size:18px; font-weight:bold; color:#0072c6; text-decoration:none;">
          https://campagne.ospharm.org/MarsBleu
        </a>
      </p>
      <p style="font-size:16px; color:#333; margin:10px 0;">
        OSPHARM s&apos;engage à vous adresser les résultats de notre <strong>étude nationale</strong>, <br>
        accompagnée d&apos;une <strong><em>analyse sur la mise en place de l&apos;honoraire RKD personnalisée à votre officine</em><strong>
      </p>
      <p style="font-size:16px; color:#333; margin:10px 0;">
        Rendez-vous <strong>mi-avril...</strong><br>
        Nous savons pouvoir compter sur vous !
      </p>
      <p style="font-size:14px; color:#666; margin-top:20px; text-align:center;">
        <strong>PS :</strong> n&apos;hésitez pas à nous partager vos photos de mise en avant sur 
        <a href="mailto:solutions@ospharea.com" style="color:#0072c6;">solutions@ospharea.com</a><br>
        Elles nous seront précieuses pour la réalisation d&apos;un patchwork illustrant cette étude.
      </p>
    </td>
  </tr>
  
  <!-- TROISIÈME SECTION : Bas de page / Footer -->
  <tr>
    <td style="padding:0;">
      <table width="100%" border="0" cellpadding="0" cellspacing="0" style="margin-top:30px;">
        <tr>
          <!-- Logo gauche -->
          <td width="20%" align="left" style="padding:15px;">
            <img src="https://isaac-1996.github.io/Localisation/00_SHAPE_OSPHARM.png"
                 alt="OSPHARM"
                 style="width:60px; display:block;" />
          </td>
          <!-- Texte central + QR code -->
          <td align="right" style="padding:5px; font-size:16px; font-weight:bold; color:#333;">
            Retrouvez toutes nos solutions
          </td>
          <td>
            <img src="https://isaac-1996.github.io/Localisation/frame_1.png"
                 alt="QR Code"
                 style="width:70px; margin-top:10px;" />
          </td>
          <!-- Logo droit -->
          <td width="20%" align="right" style="padding:15px;">
            <img src="https://isaac-1996.github.io/Localisation/00_SHAPE_OSPHARM.png"
                 alt="OSPHARM"
                 style="width:60px; display:block;" />
          </td>
        </tr>
      </table>
    </td>
  </tr>

</table>
</body>
</html>
')
  
  # Envoi du mail avec MailJet
  tryCatch({
    send.mail(from = glob$ospharm_from,
              to = destinataire,
              subject = "Etude de l'impact des campagnes de santé publique sur les nouvelles missions en pharmacie",
              body = html_content,
              smtp = list(host.name = "in-v3.mailjet.com",
                          port = 587,
                          user.name = Sys.getenv("mailjet_user"),
                          passwd = Sys.getenv("mailjet_pass"),
                          tls = TRUE),
              authenticate = TRUE,
              html = TRUE,
              send = TRUE)
    
    print(paste("Mail envoyé à:", destinataire))
    assign("emails_envoyes", emails_envoyes + 1, envir = .GlobalEnv)
  }, error = function(e) {
    print(paste("Échec d'envoi à:", destinataire, "Erreur:", e$message))
    assign("emails_echoues", emails_echoues + 1, envir = .GlobalEnv)
    log_erreurs <<- append(log_erreurs, paste("Échec pour:", destinataire, "Erreur:", e$message))
  })
}

# Boucle pour envoyer les emails personnalisés
for (i in 1:nrow(Infos)) {
  envoyer_email(Infos$mail[i], Infos$cip[i])
}

print("Tous les emails ont été envoyés ! 🚀")