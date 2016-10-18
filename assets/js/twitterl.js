// requires jQuery library

$(document).ready(function() {
	$(".twitterl textarea").text("");
	$(".twitterl_counter").attr("style", "color: #cccccc");

	$(".twitterl textarea").keyup(function() {
		var limit = 140 - this.value.length;
		var counter = $(".twitterl_counter");

		counter.text(limit);
		if(limit <= 0) {
		    counter.attr("style", "color: #d40d12");
		} else if(limit < 20) {
		    counter.attr("style", "color: #5c0002");
		} else {
		    counter.attr("style", "color: #cccccc");
		};
	    });
    });