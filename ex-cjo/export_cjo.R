library(RJDemetra)
library(rJava)
#Export Excel
library(XLConnect)

#' Fonction permettant d'exporter un objet ts vers un fichier Excel
#' 
#' @param x objet à exporter.
#' @param file chemin vers le fichier Excel.
#' @param sheet nom de la feuille dans laquelle exporter les données.
#' @param format format de la date dans Excel (par défaut "dd/mm/yyyy").
ts2xls <- function(x, file, sheet="Feuille 1", format = "dd/mm/yyyy"){
  wb <- loadWorkbook(file, create = TRUE)
  createSheet(wb, sheet)
  if(is.mts(x)){
    col <- c("date", colnames(x))
  }else{
    col <- c("date", "x")
  }
  # Le titre
  writeWorksheet(wb,matrix(col,nrow = 1),
                 sheet = sheet,startCol = 1,startRow =1,
                 header = FALSE)

  # Petit trick pour que la colonne date soit au format date d'Excel
  csDate <- getOrCreateCellStyle(wb, name = "date")
  setDataFormat(csDate, format = format)
  date <- as.Date(format(zoo::as.Date((time(x))), "%d/%m/%Y"),
                  "%d/%m/%Y")
  writeWorksheet(wb,date,sheet = sheet,
                 startCol = 1,startRow = 2,
                 header = FALSE)
  setCellStyle(wb, sheet = sheet, row = seq_along(date)+1,
               col = 1,
               cellstyle = csDate)
  # Fin colonne date

  # Autres colonnes
  writeWorksheet(wb,x,sheet = sheet,startCol = 2,startRow = 2,
                 header = FALSE)
  setColumnWidth(wb, sheet, column = seq_along(col), width = -1)
  saveWorkbook(wb, file)
}

#' Fonction permettant de compléter les variables d'un workspace
#' 
#' @param liste_var ensemble des variables à ajouter dans le workspace.
#' @param workspace workspace dans lequel ajouter les variables.
complete_variables <- function(liste_var, workspace){
  if(!is.mts(liste_var))
    stop("liste_var doit être de type mts")
  context_dictionary <- .jcall(workspace,"Lec/tstoolkit/algorithm/ProcessingContext;", "getContext")
  ts_variable_managers <- context_dictionary$getTsVariableManagers()
  ts_variables <- .jnew("ec/tstoolkit/timeseries/regression/TsVariables")
  jd_r_variables <- ts_variable_managers$get("r")
  if (is.null(jd_r_variables)) {
    ts_variable_managers$set("r",
                             .jnew("ec/tstoolkit/timeseries/regression/TsVariables"))
    jd_r_variables <- ts_variable_managers$get("r")
  }
  jd_var_names <- jd_r_variables$getNames()

  model_var_names <- colnames(liste_var)

  for (i in seq_along(model_var_names)) {
    name <- model_var_names[i]
    dictionary_var <- jd_r_variables$get(name)
    tsvar <- .jnew("ec/tstoolkit/timeseries/regression/TsVariable",
                   name, RJDemetra:::ts_r2jd(liste_var[, i]))
    if (is.null(dictionary_var)) {
      jd_r_variables$set(name, tsvar)
    } else {
      warning(sprintf("La variable %s existe déjà", name))
    }
  }
}

# Pour la création des regresseurs, voir le TP R associé.
# Par exemple :
library(rjd3toolkit)

jours_macronia <- list(
  special_day("NEWYEAR"),
  special_day("EASTERMONDAY"), # Lundi de Pâques
  fixed_day(4, 4), # Jour de l'indépendance de la Macronia
  special_day("MAYDAY"), # 1er mai
  special_day("ASCENSION"), # Jour de l'Ascension
  special_day("WHITMONDAY"), # Lundi de Pentecôte
  special_day("ASSUMPTION"), # Assomption de Marie
  special_day("ALLSAINTSDAY"), # Toussaint
  special_day("CHRISTMAS") # Noël
)
# # Manque Début ramadan, Korité (Aïd al-Fitr), Tabaski (Aïd el-Kébir), Tamkharite, Magal de Touba (non officiel), Maouloud et jours décrétés
# # On récupère ces jours construisant un fichier Excel
# jours_mobiles <- readxl::read_excel("ex-cjo/DateFetesMusulmanes_Macronia.xlsx") |>
#   as.data.frame()
# jours_mobiles <- jours_mobiles[,-1] #on enlève l'année
# jours_mobiles <- lapply(jours_mobiles, function(x){
#   na.omit(as.character(format(x, "%Y-%m-%d")))
#   })
# cat(unlist(lapply(names(jours_mobiles), function(day){
#   c(
#     sprintf("# %s", day),
#     sprintf('single_day("%s")', jours_mobiles[[day]])
#   )
# })),
#   sep= ",\n")
jours_macronia <- c(
  jours_macronia,
  list(
    # DEBUT_RAMADAN,
    single_day("1990-03-28"),
    single_day("1991-03-18"),
    single_day("1992-03-06"),
    single_day("1993-02-23"),
    single_day("1994-02-12"),
    single_day("1995-02-14"),
    single_day("1996-01-22"),
    single_day("1997-01-11"),
    single_day("1997-12-31"),
    single_day("1998-12-20"),
    single_day("1999-12-10"),
    single_day("2000-11-28"),
    single_day("2001-11-17"),
    single_day("2002-11-07"),
    single_day("2003-10-27"),
    single_day("2004-10-15"),
    single_day("2005-10-05"),
    single_day("2006-09-24"),
    single_day("2007-09-14"),
    single_day("2008-09-02"),
    single_day("2009-08-22"),
    single_day("2010-08-12"),
    single_day("2011-08-01"),
    single_day("2012-07-20"),
    single_day("2013-07-10"),
    single_day("2014-06-29"),
    single_day("2015-07-18"),
    single_day("2016-06-07"),
    single_day("2017-05-27"),
    single_day("2018-05-17"),
    single_day("2019-05-06"),
    single_day("2020-04-24"),
    single_day("2021-04-14"),
    single_day("2022-04-03"),
    single_day("2023-03-23"),
    single_day("2024-03-12"),
    # KORITE,
    single_day("1990-04-27"),
    single_day("1991-04-16"),
    single_day("1992-04-04"),
    single_day("1993-03-25"),
    single_day("1994-03-14"),
    single_day("1995-03-03"),
    single_day("1996-02-09"),
    single_day("1997-02-09"),
    single_day("1998-01-29"),
    single_day("1999-01-18"),
    single_day("2000-01-08"),
    single_day("2000-12-27"),
    single_day("2001-12-16"),
    single_day("2002-12-06"),
    single_day("2003-11-25"),
    single_day("2004-11-13"),
    single_day("2005-11-03"),
    single_day("2006-10-23"),
    single_day("2007-10-13"),
    single_day("2008-10-01"),
    single_day("2009-09-20"),
    single_day("2010-09-09"),
    single_day("2011-08-29"),
    single_day("2012-08-17"),
    single_day("2013-08-07"),
    single_day("2014-07-27"),
    single_day("2015-07-16"),
    single_day("2016-07-05"),
    single_day("2017-06-24"),
    single_day("2018-06-14"),
    single_day("2019-06-03"),
    single_day("2020-05-22"),
    single_day("2021-05-12"),
    single_day("2022-05-01"),
    single_day("2023-04-20"),
    single_day("2024-04-10"),
    # TABASKI,
    single_day("1990-06-24"),
    single_day("1991-06-23"),
    single_day("1992-06-11"),
    single_day("1993-06-01"),
    single_day("1994-05-21"),
    single_day("1995-05-10"),
    single_day("1996-04-29"),
    single_day("1997-04-19"),
    single_day("1998-04-08"),
    single_day("1999-03-28"),
    single_day("2000-03-17"),
    single_day("2001-03-06"),
    single_day("2002-02-23"),
    single_day("2003-02-13"),
    single_day("2004-02-02"),
    single_day("2005-01-21"),
    single_day("2006-01-11"),
    single_day("2006-12-31"),
    single_day("2007-12-21"),
    single_day("2008-12-09"),
    single_day("2009-11-11"),
    single_day("2010-11-18"),
    single_day("2011-11-07"),
    single_day("2012-10-26"),
    single_day("2013-10-16"),
    single_day("2014-10-05"),
    single_day("2015-09-25"),
    single_day("2016-09-13"),
    single_day("2017-09-02"),
    single_day("2018-08-23"),
    single_day("2019-08-12"),
    single_day("2020-07-31"),
    single_day("2021-07-21"),
    single_day("2022-06-10"),
    single_day("2023-06-29"),
    single_day("2024-06-18"),
    # TAMKHARITE,
    single_day("1990-07-04"),
    single_day("1991-07-22"),
    single_day("1992-07-11"),
    single_day("1993-06-30"),
    single_day("1994-06-19"),
    single_day("1995-06-09"),
    single_day("1996-05-28"),
    single_day("1997-05-18"),
    single_day("1998-05-07"),
    single_day("1999-04-26"),
    single_day("2000-04-15"),
    single_day("2001-04-04"),
    single_day("2002-03-24"),
    single_day("2003-03-14"),
    single_day("2004-03-02"),
    single_day("2005-02-19"),
    single_day("2006-02-09"),
    single_day("2007-01-29"),
    single_day("2008-01-19"),
    single_day("2009-01-07"),
    single_day("2009-12-27"),
    single_day("2010-12-17"),
    single_day("2011-12-06"),
    single_day("2012-11-24"),
    single_day("2013-11-14"),
    single_day("2014-11-03"),
    single_day("2015-11-30"),
    single_day("2016-10-12"),
    single_day("2017-10-01"),
    single_day("2018-09-21"),
    single_day("2019-09-10"),
    single_day("2020-08-29"),
    single_day("2021-08-19"),
    single_day("2022-08-08"),
    single_day("2023-07-28"),
    single_day("2024-07-17"),
    # MAOULOUD,
    single_day("1990-10-02"),
    single_day("1991-09-21"),
    single_day("1992-09-10"),
    single_day("1993-08-30"),
    single_day("1994-08-19"),
    single_day("1995-08-09"),
    single_day("1996-07-28"),
    single_day("1997-07-18"),
    single_day("1998-07-07"),
    single_day("1999-06-26"),
    single_day("2000-06-15"),
    single_day("2001-06-04"),
    single_day("2002-05-24"),
    single_day("2003-05-14"),
    single_day("2004-05-02"),
    single_day("2005-04-21"),
    single_day("2006-04-11"),
    single_day("2007-03-31"),
    single_day("2008-03-20"),
    single_day("2009-03-09"),
    single_day("2010-02-26"),
    single_day("2011-02-16"),
    single_day("2012-02-05"),
    single_day("2013-01-24"),
    single_day("2014-01-14"),
    single_day("2015-01-03"),
    single_day("2015-11-23"),
    single_day("2016-12-12"),
    single_day("2018-12-01"),
    single_day("2018-11-21"),
    single_day("2019-11-18"),
    single_day("2020-10-29"),
    single_day("2021-10-19"),
    single_day("2022-10-08"),
    single_day("2023-09-27"),
    single_day("2024-09-15"),
    # MAGAL,
    single_day("1990-09-09"),
    single_day("1991-08-29"),
    single_day("1992-09-18"),
    single_day("1993-10-07"),
    single_day("1994-07-27"),
    single_day("1995-07-17"),
    single_day("1996-07-05"),
    single_day("1997-06-25"),
    single_day("1998-06-14"),
    single_day("1999-06-03"),
    single_day("2000-05-23"),
    single_day("2001-05-12"),
    single_day("2002-05-01"),
    single_day("2003-04-21"),
    single_day("2004-04-09"),
    single_day("2005-03-29"),
    single_day("2006-03-19"),
    single_day("2007-03-08"),
    single_day("2008-02-26"),
    single_day("2009-02-14"),
    single_day("2010-02-03"),
    single_day("2011-01-03"),
    single_day("2012-01-13"),
    single_day("2013-01-01"),
    single_day("2013-12-22"),
    single_day("2014-12-11"),
    single_day("2015-12-01"),
    single_day("2016-11-19"),
    single_day("2017-11-08"),
    single_day("2018-10-29"),
    single_day("2019-10-18"),
    single_day("2020-10-06"),
    single_day("2021-09-26"),
    single_day("2022-09-15"),
    single_day("2023-09-04"),
    single_day("2024-08-23")
  ))

CAL <- national_calendar(jours_macronia)
frequency <- 4
start <- c(1990,1)
end = c(2030, 1)
length = (end[1] - start[1]) * frequency + end[2] - start[2]


gen_calendrier <- function(cal, frequency, start = c(1990, 1), end = c(2030, 1)) {
  length = (end[1] - start[1]) * frequency + end[2] - start[2]
  ly <- rjd3toolkit::lp_variable(frequency = frequency, start = start,
                                 length = length)
  # N'hésitez pas à ajouter les votre !
  TD7 <- calendar_td(cal, frequency = frequency, start = start, length = length,
                     groups = c(1, 2, 3, 4, 5, 6, 0))
  TD4 <- calendar_td(cal, frequency = frequency, start = start, length = length,
                     groups = c(1, 1, 1, 1, 2, 3, 0))
  TD3 <- calendar_td(cal, frequency = frequency, start = start, length = length,
                     groups = c(1, 1, 1, 1, 1, 2, 0))
  TD3c <- calendar_td(cal, frequency = frequency, start = start, length = length,
                      groups = c(1, 1, 1, 1, 2, 2, 0))
  TD2 <- calendar_td(cal, frequency = frequency, start = start, length = length,
                     groups = c(1, 1, 1, 1, 1, 0, 0))
  TD2c <- calendar_td(cal, frequency = frequency, start = start, length = length,
                      groups = c(1, 1, 1, 1, 1, 1, 0))

  reg_jo <- ts(cbind(TD2, TD2c, TD3, TD3c, TD4, TD7),
               start = start, frequency = frequency)
  reg_jo <- ts.intersect(reg_jo,
                         ly)
  colnames(reg_jo) <- c(
    "TD2_semaine",
    "TD2c_lundi_samedi",
    sprintf("TD3_%s", c("semaine", "samedi")),
    sprintf("TD3c_%s", c("lundi_jeudi", "vendredi_samedi")),
    sprintf("TD4_%s", c("lundi_jeudi", "vendredi", "samedi")),
    sprintf("TD7_%s", c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi")),
    "leap_year")
  reg_jo
}
regresseurs_JO_trim <- gen_calendrier(CAL, frequency = 4)
regresseurs_JO_mens <- gen_calendrier(CAL, frequency = 12)

colnames(regresseurs_JO_trim) <- paste0(colnames(regresseurs_JO_trim), "_trim")
colnames(regresseurs_JO_mens) <- paste0(colnames(regresseurs_JO_mens), "_mens")

############################################################
### Création d'un workspace en V2 et ajout des variables ###
############################################################

jws <- new_workspace()
# regresseurs_JO est l'objet mts qui contient tous vos régresseurs
# Il doit donc déjà être créé (voir code ci-dessus) !
complete_variables(regresseurs_JO_mens, jws)
complete_variables(regresseurs_JO_trim, jws)
save_workspace(jws,"wk_CJO.xml")

# On peut également exporter les régresseurs vers un fichier Excel
ts2xls(regresseurs_JO_mens, file = "reg_cjo.xlsx", sheet = "Reg mensuels")
ts2xls(regresseurs_JO_trim, file = "reg_cjo.xlsx", sheet = "Reg trim")

#############################################################################
### Création d'un workspace en V2 et ajout des variables et du calendrier ###
#############################################################################
# On va créer deux groupes de variables, on peut donc simplifier les noms
colnames(regresseurs_JO_mens) <-
  colnames(regresseurs_JO_trim) <-
  gsub("_mens", "", colnames(regresseurs_JO_mens))
ctxt <- rjd3toolkit::modelling_context(
  # on appelle "CAL" le calendrier
  calendars = list(CAL = CAL),
  # on crée un groupe de variables "cjo_mens" contenant les régresseurs mensuels
  # et un groupe de variables "cjo_trim" contenant les régresseurs trimestriels
  variables = list(cjo_mens = regresseurs_JO_mens,
                   cjo_trim = regresseurs_JO_trim)
)

jws <- rjd3workspace::jws_new(ctxt)
# On peut également ajouter les calendriers et les variables avec les fonctions :
# rjd3workspace:::add_variables()
# rjd3workspace:::add_calendar()
# Pour modifier un workspace existant :
# rjd3workspace::set_context()
rjd3workspace::save_workspace(jws, "wk_CJO_v3.xml")
