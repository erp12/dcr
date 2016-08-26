function dcr_onResize(should_reload = true) {
	has_something_changed = false;

	for (var key in dcr_responsive_chart_widths) {
		if (dcr_responsive_chart_widths.hasOwnProperty(key)) {
			chrt = eval("chart" + key);
			new_chrt_width = window.innerWidth * dcr_responsive_chart_widths[key];
			chrt.width(new_chrt_width);
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
	if (should_reload && has_something_changed) {location.reload();}
}

var wait;
window.onresize = function(){
  clearTimeout(wait);
  wait = setTimeout(dcr_onResize, 150);
};