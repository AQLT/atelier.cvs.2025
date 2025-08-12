library(tempdisagg)
library(zoo)
ipi_idbank <- c(BE = "010768260", C1 = "010768266", C2 = "010768268", C3 = "010768270", 
                C4 = "010768272", C5 = "010768274", DE = "010768310")
cna_courant_idbank <- c(BE = "011785442", C1 = "011785471", C2 = "011785474", C3 = "011785477", 
                        C4 = "011785480", C5 = "011785483", DE = "011785486")
ipi <- AQLTools::lectureBDM(ipi_idbank)
colnames(ipi) <- names(ipi_idbank)
ipi_trim <- rjd3toolkit::aggregate(ipi, nfreq = 4)
cna_courant <- AQLTools::lectureBDM(cna_courant_idbank)
cna_courant <- window(cna_courant, start =start(ipi)[1])
colnames(cna_courant) <- names(cna_courant_idbank)

qna <- do.call(cbind, lapply(colnames(cna_courant), function(s){
  predict(
    # On utilise predict pour extraire la série calée
    tempdisagg::td(cna_courant[,s] ~ 0 + ipi_trim[,s],
                   to = "quarterly",
                   method = "denton-cholette",
                   criterion = "proportional")
  )
}))
colnames(qna) <- colnames(cna_courant)
imae <- do.call(cbind, lapply(colnames(qna), function(s){
  predict(
    # On utilise predict pour extraire la série calée
    tempdisagg::td(qna[,s] ~ 0 + ipi[,s],
                   to = "monthly",
                   method = "denton-cholette",
                   criterion = "proportional")
  )
}))
colnames(imae) <- colnames(qna)

library(XLConnect)
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
ts2xls(ipi, file = "data/macronia.xlsx", sheet = "IPI")
ts2xls(imae, file = "data/macronia.xlsx", sheet = "IMAE")
ts2xls(qna, file = "data/macronia.xlsx", sheet = "CNT")
ts2xls(cna_courant, file = "data/macronia.xlsx", sheet = "CNA")


xls2csv <- function(sheet) {
  tmp <- readWorksheetFromFile("data/macronia.xlsx", sheet = "IPI")
  tmp[,1] <- gsub(" .*", "", tmp[,1])
  write.table(
    tmp, 
    sprintf("data/macronia_%s.csv", tolower(sheet)), 
    row.names = FALSE, sep = ";")
}
xls2csv("IPI")
xls2csv("IMAE")
xls2csv("CNT")


