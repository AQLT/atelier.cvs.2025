packages_a_installer <- c("rjd3x13","rjd3workspace", "rjd3report", "ggdemetra3", "qpdf")
packages_a_installer <- packages_a_installer[! packages_a_installer %in% installed.packages()[,"Package"]]
if (length(packages_a_installer) > 0) {
  install.packages(packages_a_installer, repos = c("https://aqlt.r-universe.dev", "https://cloud.r-project.org"))
}
library(rjd3workspace)
library(rjd3report)

# On charge le workspace
# Remplacer "R-macronia/macronia_08_2025.xml" par le chemin vers votre workspace
jws <- jws_open(
  # À MODIFIER SI BESOIN
  "R-macronia/macronia_08_2025.xml"
) 
ctxt <- get_context(jws)
jws_compute(jws)
all_jmod <- jread_workspace(jws,compute = TRUE)
all_jmod <- lapply(all_jmod, function(sap) {
  # On enlève les noms des MP dans les SaItem
  names(sap) <- gsub(".*\n", "", names(sap))
  sap
})
replace_existing_file <- TRUE

# Nombre maximal de caractères pour le nom des fichiers
nchar_f <- 50

# Dossier où l'on veut exporter les résultats
dir_exp <- "R-macronia/graphs" # A MODIFIER si besoin

# Si le dossier n'existe pas on le crée
if (!dir.exists(dir_exp))
  dir.create(dir_exp, recursive = TRUE)

for (sap in names(all_jmod)) {
  # Pour chaque saprocessing :
  if (!dir.exists(file.path(dir_exp, sap)))
    dir.create(file.path(dir_exp, sap), recursive = TRUE)
  for(series in names(all_jmod[[sap]])) {
    # Pour chaque série :
    
    # On crée le nom du fichier
    file <- file.path(dir_exp, sap, paste0(
      substr(series, 1, nchar_f), # on ne prend que les nchar_f premiers caractères pour les noms des fichiers
      ".pdf"))
    # Si le fichier existe déjà (et que replace_existing_file == FALSE) on ne fait pas l'export
    if (!replace_existing_file && file.exists(file))
      next;
    # On affice le nom du saprocessing et de la série
    print(sprintf("%s : %s", sap, series))
    # On définit les dimensions de l'images
    pdf(file, width = 29.7*0.393701/1.5, height = 21*0.393701/1.5)
    # On trace le tableau de bord grâce à rjd3report
    dashboard <- rjd3report::simple_dashboard2(all_jmod[[sap]][[series]], context = ctxt)
    if(is.null(dashboard)) {
      plot.new()
      legend("topleft", legend = c(series, "No model"),
             bty = "n", text.font =  2, inset = c(0),
             cex = 0.95,
             xpd = TRUE)
    } else {
      plot(dashboard, main = paste(sap, series, sep= " - "))
    }
    dev.off()
  }
}


# Ensuite on va créer un cahier avec tous les pdfs
for (sap in names(all_jmod)) {
  qpdf::pdf_combine(input = sprintf("%s/%s/%s.pdf", dir_exp, sap, substr(names(all_jmod[[sap]]), 1, nchar_f)),
                    output = sprintf("%s/%s.pdf", dir_exp, sap))
}
# On combine tous les pdfs pour produire un cahier complet
qpdf::pdf_combine(input = sprintf("%s/%s.pdf", dir_exp, names(all_jmod)),
                  output = sprintf("%s/%s.pdf", dir_exp, "cahier_complet"))

