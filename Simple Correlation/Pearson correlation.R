#Chowdhury Rafatul Kabir
#B.Sc, M.S. in Forestry (SUST)
#To check simple pearson correlation between climate variables and RWI. Also saving the r and p value in a csv sheet.
#last updated 02/07/2025

setwd("C:/Users/C Rafatul Kabir/Desktop/hydnocarpus kurzii manuscript/new analysis/Dendro climate growth study/Plots")
library(readxl)
Temp_max_det_correl <- read_excel("C:/Users/C Rafatul Kabir/Desktop/hydnocarpus kurzii manuscript/new analysis/Dendro climate growth study/Plots/precip2.xlsx")
# Convert both columns to numeric (in case they're character or factor)
#x <- cor.test(
  #as.numeric(Temp_max_det_correl$`Std Chronology of Rw`),
  #as.numeric(Temp_max_det_correl$Jan)
#)

library(dplyr)

# Convert all columns except 'Year' (or any non-numeric) to numeric
Temp_max_det_correl <- Temp_max_det_correl %>%
  mutate(across(where(is.character), ~as.numeric(.)))

Temp_max_det_correl <- Temp_max_det_correl %>%
  mutate(across(-Year, ~as.numeric(.)))



pyma <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$pyMay)
pyj <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$pyJun)
pyju <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$pyJul)
pyau <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$pyAug)
pys <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$pySep)
pyo <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$pyOct)
pyn <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$pyNov)
pyd <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$pyDec)
j <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$Jan)
f <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$Feb)
m <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$March)
a <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$April)
ma <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$May)
j <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$June)
ju <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$July)
au <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$Aug)
s <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$Sep)
o <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$Oct)
n <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$Nov)
mnAn <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$`MA`)
pm <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$`PM`)
ms <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$`M`)
ptms <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$`PoM`)
ds <- cor.test(Temp_max_det_correl$`Std Chronology of Rw`,Temp_max_det_correl$`DS`)


df <- c(pyma,pyj,pyju,pyau,pys,pyo,pyn,pyd,j,f,m,a,ma,j,ju,au,s,o,n,pm,ms,ptms,ds,mnAn)





#r and p value




pyma[["estimate"]][["cor"]]

pyma[["p.value"]]


pyj[["estimate"]][["cor"]]

pyj[["p.value"]]

pyju[["estimate"]][["cor"]]

pyju[["p.value"]]

pyau[["estimate"]][["cor"]]

pyau[["p.value"]]

pys[["estimate"]][["cor"]]

pys[["p.value"]]

pyo[["estimate"]][["cor"]]

pyo[["p.value"]]

pyn[["estimate"]][["cor"]]

pyn[["p.value"]]

pyd[["estimate"]][["cor"]]

pyd[["p.value"]]

j[["estimate"]][["cor"]]

j[["p.value"]]

f[["estimate"]][["cor"]]

f[["p.value"]]

m[["estimate"]][["cor"]]

m[["p.value"]]

a[["estimate"]][["cor"]]

a[["p.value"]]

ma[["estimate"]][["cor"]]

ma[["p.value"]]

j[["estimate"]][["cor"]]

j[["p.value"]]

ju[["estimate"]][["cor"]]

ju[["p.value"]]

au[["estimate"]][["cor"]]

au[["p.value"]]

s[["estimate"]][["cor"]]

s[["p.value"]]

o[["estimate"]][["cor"]]

o[["p.value"]]

n[["estimate"]][["cor"]]

n[["p.value"]]

mnAn[["estimate"]][["cor"]]

mnAn[["p.value"]]

pm[["estimate"]][["cor"]]

pm[["p.value"]]

ms[["estimate"]][["cor"]]

ms[["p.value"]]

ptms[["estimate"]][["cor"]]

ptms[["p.value"]]

ds[["estimate"]][["cor"]]

ds[["p.value"]]



####################saving in excel#############

# Load required library
library(writexl)

# Combine all correlation results into a dataframe
cor_results <- data.frame(
  Variable = c("pyma", "pyj", "pyju", "pyau", "pys", "pyo", "pyn", "pyd",
               "j", "f", "m", "a", "ma", "j", "ju", "au", "s", "o", "n", 
               "mnAn", "pm", "ms", "ptms", "ds"),
  Correlation = c(
    pyma[["estimate"]][["cor"]], pyj[["estimate"]][["cor"]],
    pyju[["estimate"]][["cor"]], pyau[["estimate"]][["cor"]],
    pys[["estimate"]][["cor"]], pyo[["estimate"]][["cor"]],
    pyn[["estimate"]][["cor"]], pyd[["estimate"]][["cor"]],
    j[["estimate"]][["cor"]], f[["estimate"]][["cor"]],
    m[["estimate"]][["cor"]], a[["estimate"]][["cor"]],
    ma[["estimate"]][["cor"]], j[["estimate"]][["cor"]],
    ju[["estimate"]][["cor"]],
    au[["estimate"]][["cor"]], s[["estimate"]][["cor"]],
    o[["estimate"]][["cor"]], n[["estimate"]][["cor"]],
    mnAn[["estimate"]][["cor"]], pm[["estimate"]][["cor"]],
    ms[["estimate"]][["cor"]], ptms[["estimate"]][["cor"]],
    ds[["estimate"]][["cor"]]
  ),
  P_value = c(
    pyma[["p.value"]], pyj[["p.value"]],
    pyju[["p.value"]], pyau[["p.value"]],
    pys[["p.value"]], pyo[["p.value"]],
    pyn[["p.value"]], pyd[["p.value"]],
    j[["p.value"]], f[["p.value"]],
    m[["p.value"]], a[["p.value"]],
    ma[["p.value"]], j[["p.value"]], ju[["p.value"]],
    au[["p.value"]], s[["p.value"]],
    o[["p.value"]], n[["p.value"]],
    mnAn[["p.value"]], pm[["p.value"]],
    ms[["p.value"]], ptms[["p.value"]],
    ds[["p.value"]]
  )
)

# Write to Excel file
write_xlsx(cor_results, path = "precipexp_rp.xlsx")

