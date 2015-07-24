## bar plot
data <- read.csv("inst/example/data/morley.csv")
data <- within(data, {dist <- Speed * Run / 1000})
mydcr <- dcr(data)
barchart <- dcrchart("barChart", "xx", "Run", reduceMean("dist"), width = 500, height = 480,
                     yAxisLabel = "My y axis label" )
linchart <- dcrchart("lineChart", "test", "Run", reduceSum("dist"), width = 600,  height = 500)
mydcr + barchart
mydcr + linchart + barchart



## line chart
linechart <- dcrchart("lineChart", "yy", Run, reduceSum(dist)) + width(768) +
  height(480) + x_lin(c(0, 20)) + interpolate("step-before") + renderArea("TRUE") +
  renderDataPoints(TRUE) + clipPadding(10) + yAxisLabel("This is the Y Axis!") +
  brushOn(FALSE)
dcr + linechart + barchart

## composite chart
data2 <- read.csv("inst/example/data/morley2.csv")
data <- within(data, {y1 <- Speed * Run / 1000; y2 <- NA})
data2 <- within(data2, {y1 <- NA; y2 <- Speed * Run / 1000})
data3 <- rbind(data, data2)
dcr <- mmdcr(data)
chart1 <- dcrchart("lineChart", "", "", reduceSum(y1))
chart2 <- dcrchart("lineChart", "", "", reduceSum(y2))
chart3 <- composite("aa", "Run", chart1, chart2) + width(768) + height(480) +
  x_lin(c(0, 20)) + brushOn(FALSE)
dcr + chart3

## example
library(lubridate)
data <- read.csv("inst/example/data/ndx.csv")
data <- within(data, {
  Loss_Gain <- ifelse(open > close, "Loss", "Gain")
  date <- as.Date(date, "%m/%d/%Y")
  datem <- floor_date(date, "month")
  quarter <- quarters(date)
  weekday <- paste0(wday(date), ".", weekdays(date, TRUE))
  fluctuation <- round((close - open)/open*100)
  year <- year(date)
  absGain <- close - open
  fluctuation <- abs(close - open)
  Index <- (open + close)/2
})
mydcr <- dcr(data)

mydcr + dcrchart("lineChart", "hello", "datem", reduceSum("volume"), 1000, 400, rangeChart = dc_code("chartbbb"))+
dcrchart("barChart", "bbb", "datem", reduceSum("volume"), 900, 250,
         round=dc_code("d3.time.month.round"), alwaysUseRounding=TRUE,
         xUnits=dc_code("d3.time.months"),
         margins = list(top=0, right=50, bottom=20, left = 40),
         centerBar=TRUE, gap=1)

chart1 <- dcrchart("pieChart", "chart1", "Loss_Gain", reduceCount(), width = 180, height = 180, radius = 80)

chart1 <- chart1 + label_pct(chart1@chartname) + renderLabel(TRUE)
mydcr + chart1

chart2 <- dcrchart("pieChart", "chart2", "quarter", reduceSum(volume)) +
  height(180) + radius(80) + innerRadius(30)
dcr + chart1 + chart2

chart3 <- dcrchart("rowChart", "chart3", "weekday", reduceCount()) +
  margins(top = 20, left = 10, right = 10, bottom = 20) +
  width(180) + height(180) + label_strip(1) +
  ordinalColors(c('#3182bd', '#6baed6', '#9ecae1', '#c6dbef', '#dadaeb')) +
  elasticX(TRUE) + x.ticks(4)
dcr + chart3

chart4 <- dcrchart("barChart", "chart4", "fluctuation", reduceCount()) +
  height(180) + width(420) + margins(top = 10, right = 50, bottom = 30, left = 40) +
  elasticY(TRUE) + centerBar(TRUE) + gap(1) + x_lin(c(-25, 25)) +
  renderHorizontalGridLines(TRUE) + x.ticks(5) + y.ticks(5) + x.tickFormat_pct() +
  brushOn(FALSE)
dcr + chart4

chart5 <- dcrchart("bubbleChart",
                   "chart5",
                   "year",
                   reducefun(absGain = fsum("absGain"), fluctuation = fsum("fluctuation"),
                                    Index = fsum("Index"), avgIndex = fratio("Index", "count"),
                                    pctGain = fratio("absGain * 100", "avgIndex"),
                                    pctfluctuation = fratio("fluctuation * 100", "avgIndex")),
                   width = 990,
                   height = 250,
                   colorAccessor = pluck_key("absGain"),
                   keyAccessor = pluck_key("absGain"),
                   radiusValueAccessor = pluck_key("pctfluctuation"),
                   x = x_linear(c(-2500, 2500)))
# +
#   width(990) + height(250) + colorAccessor(pluck("value.absGain")) + keyAccessor(pluck("value.absGain")) +
#   valueAccessor(pluck("value.pctGain")) +
#   radiusValueAccessor(pluck("value.pctfluctuation")) + x_lin(c(-2500, 2500)) + y_lin(c(-100, 100)) +
#   r_lin(c(0, 4000)) +  elasticY(TRUE)
mydcr + chart5

dcr + chart1 + chart2 + chart3 + chart4 + chart5


chart <- dcrchart("lineChart", "chart", "year",
                  reduce(reducefun(open = fsum("open"), close = fsum("close"),
                                   avgopen = fratio("open", "count"), avgclose = fratio("close", "count")))) +
  x_lin(c(1980, 2015)) +  valueAccessor(pluck("value.avgopen")) +
  stack(pluck("value.avgclose"))+
  x.ticks(5) + brushOn(FALSE) + margins(left = 200, right = 50, top = 50, bottom = 50)+
  width(800) + height(500) + renderArea(TRUE) + yAxisPadding(1.5) + xAxisPadding(1.5)
dcr + chart + chart1

mr <- reduce(reducefun(open = fsum("open"), close = fsum("close/2"),
                 avgopen = fratio("open", "count"), avgclose = fratio("close", "count")))
chart1 <- dcrchart("lineChart", "chart", "year", reduceCount())
chart2 <- dcrchart("lineChart", "chart", "year", mr) + valueAccessor(pluck("value.count"))
chart0 <- composite("chart0", "year", chart1, chart2) + x_lin(c(1985, 2012)) + x.ticks(4) +
  width(800) + height(500) + brushOn(FALSE) + margins(left = 100, top = 100, right = 100, bottom = 100) + elasticY(TRUE)
dcr + chart0

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

##stackable table
mydcr <- dcr(mtcars)
mydcr + dcrchart("lineChart", "aa", "cyl", reduceSum("am"), 500, 300, stack = dc_code("aaGroup "),
                  renderArea = TRUE, legend = dc_legend(x = 350, y=0),stack = dc_code("aaGroup,"))


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

## legend
dcr(mtcars) + dcrchart("pieChart", "bb", "gear", reduceCount(), 400, 400, legend = dc_legend(x = 180, y = 180), innerRadius = 100)
