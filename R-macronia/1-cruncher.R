packages_a_installer <- c("rjd3workspace", "rjd3report", "ggdemetra3", "rjwsacruncher", "readr", "XLConnect")
packages_a_installer <- packages_a_installer[! packages_a_installer %in% installed.packages()[,"Package"]]
if (length(packages_a_installer) > 0) {
  install.packages(packages_a_installer, repos = c("https://aqlt.r-universe.dev", "https://cloud.r-project.org"))
}
library(rjwsacruncher)
create_param_file(
  # dossier où le fichier de paramètres sera créé
  # Attention : modifier si besoin, si ce dossier n'existe pas vous aurez une erreur
  dir_file_param = "R-macronia", 
  policy = "lastoutliers", # politique de rafraichissement
  csv_layout = "vtable" # format d'export des fichiers CSV
)
# Il faut spécifier où est le JWSACruncher
options(cruncher_bin_directory = "/Applications/jwsacruncher/jwsacruncher-3.5.1/bin")
getOption("cruncher_bin_directory")

# Plutôt que de lancer le cruncher directement sur le workspace on va faire une copie
library(rjd3workspace)
jws <- jws_open(
  # Chemin vers le workspace, A MODIFIER POUR POINTER VERS VOTRE WORKSPACE
  "data/macronia.xml"
)
new_file_workspace <- sprintf(
  "R-macronia/macronia_%s.xml", # A MODIFIER
  format(Sys.time(), # je récupère la date du jour
         "%m_%Y") # je la mets sous le format MM_YYYY
)
# Si plus simple mettre directement le nom du fichier :
new_file_workspace <- "R-macronia/macronia_08_2025.xml" # MODIFIER

# Ce code permet de créer la copie du workspace
rjd3workspace::save_workspace(jws, new_file_workspace, replace = TRUE)

# ATTENTION : SI VOUS UTILISEZ DIRECTEMENT LE WORKSPACE macronia.xml AUCUNE DONNÉE NE SERA EXPORTÉE :
# LE CRUNCHER VA EN EFFET CHERCHER LES DONNÉES LÀ OÙ J'AI CRÉÉ LE PROGRAMME
cruncher(workspace = new_file_workspace,
         param_file_path = "R-macronia/parameters.param", # A MODIFIER AVEC VOTRE CHEMIN
         log_file = "log.txt"
)


##################################
### Récupération des résultats ###
##################################
### Cette partie est optionnelle : elle permet juste d'avoir des codes exemples pour importer des données
# Méthode 1 : récupérer les données
library(readr)
series_sa <- read_delim(
  # On spécifie ici le chemin vers le fichier
  # Par défaut, il sera dans le dossier associé à votre workspace, dans un sous-dossier output
  # et ensuite vous aurez un dossier par SAProcessing
  file.path(
    gsub(".xml", "", new_file_workspace), 
    "Output", 
    "SAProcessing-1", # On ne prend que les fichiers associés au premier SAProcessing, a modifier éventuellement
    "series_sa.csv"),
  delim = ";", escape_double = FALSE, col_names = TRUE,
  # Si sous JDemetra+ les données importées depuis un fichier Excel avec des accents,
  # il peut être nécessaire de spécifier l'encodage
  locale = locale(encoding = "WINDOWS-1252", decimal_mark = ","),
  trim_ws = TRUE
)
series_sa <- as.data.frame(series_sa)

# Exemple d'export en Excel :
# On crée un fichier "series_y.xlsx" à partir du tableau series_y
XLConnect::writeWorksheetToFile("series_y.xlsx", series_sa, sheet = "y")
# Autre façon de lire les données :
series_sa2 <- read.csv2(
  # On spécifie ici le chemin vers le fichier
  # Par défaut, il sera dans le dossier associé à votre workspace, dans un sous-dossier output
  # et ensuite vous aurez un dossier par SAProcessing
  file.path(
    gsub(".xml", "", new_file_workspace), 
    "Output", 
    "SAProcessing-1", # On ne prend que les fichiers associés au premier SAProcessing, a modifier éventuellement
    "series_sa.csv"),
  fileEncoding = "WINDOWS-1252"
)


# Methode 2 : utiliser rjd3workspace pour la v3
jws <- jws_open(new_file_workspace)
all_jmod <- rjd3workspace::.jread_workspace(jws)
all_jmod <- lapply(all_jmod, function(sap) {
  # On enlève les noms des MP dans les SaItem
  names(sap) <- gsub(".*\n", "", names(sap))
  sap
})

# Permet d'avoir une liste avec les séries désaisonnalisées de chaque SAProcessing
all_sa<- lapply(all_jmod, function(mp) {
  do.call(ts.union, lapply(mp, ggdemetra3::seasonaladj))
})
all_y<- lapply(all_jmod, function(mp) {
  do.call(ts.union, lapply(mp, ggdemetra3::raw))
})
