library(dcr)
## bar plot
data <- read.csv("inst/example/data/morley.csv")
data <- within(data, {dist <- Speed * Run / 1000})
mydcr <- dcr(data)
barchart <- dcrchart("barChart", "xx", "Run", reduceMean("dist"), width = 500, height = 480,
                     yAxisLabel = "My y axis label" )
linchart <- dcrchart("lineChart", "test", "Run", reduceSum("dist"), width = 200,  height = 150,
                     xAxis = x_axis(ticks = 3), yAxis = y_axis(ticks = 5))
mydcr + linchart
mydcr + linchart + barchart

###
data <- data.frame(name = c("apple", "orange", "banana", "grapefruit", "grapefruit"), cnt = c(10, 15, 12, 2, 4))
mydcr <- dcr(data)
mychart <- dcrchart("barChart", "test", "name", reduceSum("cnt"), width = 768, height = 380,
                    x = x_ordinal(sort(unique(data$name), decreasing=TRUE)), xUnits = dc_code("dc.units.ordinal"))
mydcr + mychart


## data table
mydcr <- dcr(mtcars)

mydcr + dcrchart("barChart", "aa", "cyl", reduceSum("am"), 500, 300)  +
  dcrchart("dataTable", "bb", "cyl", dc_code(""), group = dc_code("function(d) {return d.am}"), 800, 500, columns = c("cyl", "gear"), size = 200)


## geo json
data <- read.csv("inst/example/data/vc.csv", stringsAsFactors = F)
mydcr <- dcr(data)
bar <- dcrchart("barChart", "myb", "State", reduceSum("Deals"), 990, 500)
geochart <- dcrchart("geoChoroplethChart", "mychart", "State", reduceSum("Deals"), 990, 500,
                     colors = dc_code('d3.scale.quantize().range(["#E2F2FF", "#C4E4FF", "#9ED2FF", "#81C5FF", "#6BBAFF", "#51AEFF", "#36A2FF", "#1E96FF", "#0089FF", "#0061B5"])'),
                     colorDomain = c(0, 20),
                     colorCalculator = dc_code("function (d) { return d ? chartmychart.colors()(d) : '#ccc'; }"),
                     overlayGeoJson = geojson("geojson/us-states.json", "state", "function(d) {return d.properties.name;}"))
d <- mydcr + geochart + bar
d


###########
library(dcr)
example <- data.frame(state = rep(c("MA", "CT", "NY"), each = 100),
                      x = c(rnorm(100, 1), rnorm(100, 2), rnorm(100, 3)),
                      y = sample(c("A", "B", "C"), 300, replace = TRUE),
                      stringsAsFactors = FALSE)
mydcr <- dcr(example)
bar <- dcrchart("barChart", "myb", "y", reduceMean("x"), 495, 250)
geochart <- dcrchart("geoChoroplethChart", "geo", "state", reduceMean("x"), 480, 250,
                     overlayGeoJson = geojson("geojson/us-states.json", "state", "function(d) {return d.properties.name;}"),
                     colorCalculator = dc_code("function (d) { return d ? chartgeo.colors()(d) : '#ccc'; }"),
                     colors = dc_code('d3.scale.quantize().range(["#E2F2FF", "#C4E4FF", "#9ED2FF", "#81C5FF", "#6BBAFF", "#51AEFF", "#36A2FF", "#1E96FF", "#0089FF", "#0061B5"])'),
                     colorDomain = c(0, 4),
                     projection = dc_code("d3.geo.albersUsa().scale(500).translate([240, 125])"),
                     title = simple_fun("d.key + ':' + d.value"))
mydcr + bar + geochart

## legend
dcr(mtcars) + dcrchart("pieChart", "bb", "gear", reduceCount(), 400, 400, legend = dc_legend(x = 180, y = 180), innerRadius = 100)

##reduce mean with weights
data <- data.frame(x = factor(c(1, 1, 2, 2)), y = 1:4, weight = c(1, 2, 1, 2))
dcr(data) + dcrchart("barChart", "aaa", "x", reduceMean("y", "weight"), 200, 200) +
  dcrchart("barChart", "aa", "x", reduceMean("y"), 200, 200)

## test label_keyvalue function
data(mtcars)
mtcars$carbf <- factor(mtcars$carb)
carbf_levels <- levels(mtcars$carbf)
mtcars$carbf <- as.numeric(mtcars$carbf)
mydcr <- dcr(mtcars)
mydcr + dcrchart("pieChart", "aa", "cyl", reduceCount(), 200, 200, label = label_keyvalue("aa"))
mydcr + dcrchart("pieChart", "aa", "cyl", reduceSum("gear"), 200, 200, label = label_keyvalue("aa"))

mydcr + dcrchart("pieChart", "aa", "cyl", reduceSum("gear"), 200, 200,
                 label = label_keyvalue("aa", string1 = "cyl:", string2 = ", value:", string3 = "", valuemap = NULL))

mydcr + dcrchart("pieChart", "aa", "carb", reduceCount(), 200, 200, label = label_keyvalue("aa"))
mydcr + dcrchart("pieChart", "aa", "carbf", reduceSum("gear"), 200, 200,
                 label = label_keyvalue("aa", keymap = carbf_levels),
                 title = label_keyvalue("aa", string2 = ":", string3 = "", keymap = carbf_levels, valuemap = NULL))




