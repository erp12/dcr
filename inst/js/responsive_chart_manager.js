function dcr_setResponsiveSizes() {
	for (var key in dcr_responsive_chart_widths) {
		if (dcr_responsive_chart_widths.hasOwnProperty(key)) {
			chrt = eval("chart" + key);
			new_chrt_width = window.innerWidth * dcr_responsive_chart_widths[key];
			chrt.width(new_chrt_width);
			chrt.filters()
			has_something_changed = true
		}
	}
	for (var key in dcr_responsive_chart_heights) {
		if (dcr_responsive_chart_heights.hasOwnProperty(key)) {
			chrt = eval("chart" + key);
			new_chrt_height = window.innerHeight * dcr_responsive_chart_heights[key];
			chrt.height(new_chrt_height);
			has_something_changed = true
		}
	}
}

function dcr_reload() {
	dcr_store = {};
	
	all_charts = dc.chartRegistry.list();
	all_filters = [];
	for (var key in all_charts) {
		all_filters[key] = all_charts[key].filters();
	}
	location.reload();
}

var wait;
window.onresize = function(){
  clearTimeout(wait);
  wait = setTimeout(dcr_reload, 200);
};