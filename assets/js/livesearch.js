$(document).ready(function() {
	$("#search_input").attr("value", "start typing...");
	$("#search_input").click(function() {
		if(this.clicked)
		    return;

		this.clicked = true;
		this.value = "";
	    });

	$("#options_button").click(function(e) {
		e.preventDefault();
		
		$("#options_button").hide(200);
		$("#options_panel").show(200);
	    });

	$("#search_input").keyup(function() {
		var val = $("#search_input").attr("value");
		if(val.length > 2) {
		    var options = {'application': $("#options_panel input[name=option][value=apps]")[0].checked,
				   'module': $("#options_panel input[name=option][value=mods]")[0].checked,
				   'function': $("#options_panel input[name=option][value=funs]")[0].checked};

		    $.post("/doc_search?keyword=" + val, {options: JSON.stringify(options)}, 
			   function(json0) {
			       var results = eval("(" + json0 + ")");
			       
			       results.sort(function(a, b) {
				       return (a.module < b.module) ? -1 : ((a.module > b.module) ? 1 : 0);
				   });
			       results.sort(function(a, b) {
				       return (a.weight < b.weight) ? 1 : ((a.weight > b.weight) ? -1 : 0);
				   });
			       results.sort(function(a, b) {
				       return (a.application < b.application) ? -1 : ((a.application > b.application) ? 1 : 0);
				   });
			       
			       $("#search_results").hide(200);
			       $("#functions_results").text(" ");
			       $("#modules_results").text(" ");
			       $("#applications_results").text(" ");
			       
			       for(var i=0; i<results.length; i++) {
				   if(!results[i].type)
				       continue;
				   
				   var result = results[i];
				   
				   switch(result.type) {
				   case "function":
				       var arity = result.anchor.split("-")[1];

				       $("#functions_results").append("<li><a href='http://www.erlang.org/doc/man/" +
								      result.anchor + "'>" + result.module + ":<b>" +
								      result.signature + "</b>/" +
								      arity + "</a></li>");
				       break;
				   case "module":
				       $("#modules_results").append("<li><a href='http://www.erlang.org/doc/man/" +
								    result.anchor + "'><b>" + result.signature +
								    "</b>.erl</a></li>");
				       break;
				   case "application":
				       $("#applications_results").append("<li><a href='http://www.erlang.org/doc/man/" +
									 result.anchor +"'><b>" + result.signature +
									 "</b></a></li>");
				   };
			       };
			       
			       $("#search_results").show(200);
			   });
		}
	    });
    });