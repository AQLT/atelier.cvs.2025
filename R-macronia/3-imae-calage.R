packages_a_installer <- c("rjd3x13","rjd3workspace", "rjd3report", "ggdemetra3")
packages_a_installer <- packages_a_installer[! packages_a_installer %in% installed.packages()[,"Package"]]
if (length(packages_a_installer) > 0) {
  install.packages(packages_a_installer, repos = c("https://aqlt.r-universe.dev", "https://cloud.r-project.org"))
}
library(rjd3workspace)
library(rjd3report)

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
sa_imae <- all_sa$IMAE
sa_cnt <- all_sa$CNT

sa_imae_cale <- do.call(cbind, lapply(colnames(sa_imae), function(s){
  predict(
    # On utilise predict pour extraire la série calée
    tempdisagg::td(
      sa_cnt[,s] ~ 0 + sa_imae[,s],
      to = "quarterly",
      # method = "denton-cholette",
      # Pour reproduire les résultats de l'outil Ecotrim
      method= "chow-lin-minrss-ecotrim", 
      criterion = "proportional")
  )
}))
colnames(sa_imae_cale) <- colnames(sa_imae)
plotly:::ggplotly(forecast::autoplot(sa_imae_cale[,1], series = "Serie calee") +
                    forecast::autolayer(sa_imae[,1], series = "IMAE"))
