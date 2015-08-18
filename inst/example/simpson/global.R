library(shiny)
library(dcr)

ucb <- as.data.frame(UCBAdmissions)
ucb <- within(ucb, {Admit <- as.numeric(Admit == "Admitted")})

mydcr <- dcr(ucb)
adbygender <- dcrchart("barChart", "adgender", "Gender", reduceMean("Admit", "Freq"), 350, 250,
                     gap = 25, elasticY = TRUE, yAxis = y_axis(ticks = 6),
                     yAxisLabel = "Admission Rate")
adbydept <- dcrchart("barChart", "addept", "Dept", reduceMean("Admit", "Freq"), 350, 250,
                   yAxis = y_axis(ticks = 6), gap = 5, yAxisLabel = "Admission Rate",
                    y = x_linear(c(0, 0.82)))
ctbygender <- dcrchart("pieChart", "ctgender", use_dimension("adgender"), reduceSum("Freq"), 350, 250, innerRadius = 40, radius = 100)
ctbydept <- dcrchart("pieChart", "ctdept", use_dimension("addept"), reduceSum("Freq"), 350, 250, innerRadius = 40, radius = 100)

mydcr + adbygender + adbydept + ctbygender + ctbydept
