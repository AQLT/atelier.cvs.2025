packages_a_installer <- c("rjd3x13","rjd3workspace", "rjd3report", "ggdemetra3")
packages_a_installer <- packages_a_installer[! packages_a_installer %in% installed.packages()[,"Package"]]
if (length(packages_a_installer) > 0) {
  install.packages(packages_a_installer, repos = c("https://aqlt.r-universe.dev", "https://cloud.r-project.org"))
}
library(rjd3workspace)
library(zoo)

# On charge le workspace
# Remplacer "R-macronia/macronia_07_2025.xml" par le chemin vers votre workspace
jws <- jws_open("R-macronia/macronia_07_2025.xml")
ctxt <- get_context(jws)
jws_compute(jws)
all_jmod <- jread_workspace(jws,compute = TRUE)
all_jmod <- lapply(all_jmod, function(sap) {
  # On enlève les noms des MP dans les SaItem
  names(sap) <- gsub(".*\n", "", names(sap))
  sap
})
all_sa <- lapply(all_jmod, function(sap) {
  do.call(ts.union, lapply(sap, ggdemetra3::seasonaladj))
})
sa_cnt <- all_sa$CNT

be_indirecte <- ts(rowSums(sa_cnt[,c("C1", "C2", "C3", "C4", "C5", "DE")]), 
                   start = start(sa_cnt),
                   frequency = frequency(sa_cnt))
plotly:::ggplotly(
  forecast::autoplot(sa_cnt[,"BE"], series = "CVS directe") +
    forecast::autolayer(be_indirecte, series = "CVS indirecte")
)
