library(rjd3toolkit)
FR <- national_calendar(list(
	special_day("NEWYEAR"),
	special_day("EASTERMONDAY"), # Lundi de Pâques
	special_day("MAYDAY"), # 1er mai
	special_day("ASCENSION"), # Jour de l'Ascension
	fixed_day(5, 8),
	special_day("WHITMONDAY"), # Lundi de Pentecôte
	fixed_day(7, 14),
	special_day("ASSUMPTION"), # Assomption
	special_day("ALLSAINTSDAY"), # Toussaint
	special_day("ARMISTICE")
))

gen_calendrier <- function(cal, frequency, start = c(1960, 1), end = c(2030, 1)) {
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
regresseurs_JO_trim <- gen_calendrier(FR, frequency = 4)
regresseurs_JO_mens <- gen_calendrier(FR, frequency = 12)
regresseurs_JO_bimens <- gen_calendrier(FR, frequency = 6)

# colnames(regresseurs_JO_trim) <- paste0(colnames(regresseurs_JO_trim), "_trim")
# colnames(regresseurs_JO_mens) <- paste0(colnames(regresseurs_JO_mens), "_mens")
AQLTools::ts2xls(regresseurs_JO_trim, "data/regcjo.xlsx",sheet = "trim")
AQLTools::ts2xls(regresseurs_JO_mens, "data/regcjo.xlsx",sheet = "mens")
AQLTools::ts2xls(regresseurs_JO_bimens, "data/regcjo.xlsx",sheet = "bimens")
zoo::as.Date(time(regresseurs_JO_bimens))
as.Date(format(zoo::as.Date(zoo::as.yearmon(time(regresseurs_JO_bimens))), "%d/%m/%Y"),
		"%d/%m/%Y")
as.Date(format(zoo::as.Date(zoo::as.yearmon(time(regresseurs_JO_bimens))), "%d/%m/%Y"),
		"%d/%m/%Y")

# AQLTools::ts2xls(RJDemetra::ipi_c_eu, "data/ipi_eu.xlsx",sheet = "bimens")
