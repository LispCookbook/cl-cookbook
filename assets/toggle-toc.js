TOCVisible = false;
window.onload = function(e){
    if (window.location.pathname == "/cl-cookbook/"){
	document.getElementById("toc-btn").style.display = "none";
    }else if ($(document).width() <= 576){
	document.getElementById("toc-btn").style.display = "block";
    }else{
	document.getElementById("toc-btn").style.display = "none";
    }
}

toggleTOC = function(){
    toc = document.getElementById("toc-container");
    toc_title = document.getElementById("toc-title");
    if (!TOCVisible){
	toc.classList.remove("toc-close");
	toc.classList.add("toc-open");
	TOCVisible = true;
    }
    else{
	toc.classList.remove("toc-open");
	toc.classList.add("toc-close");
	TOCVisible = false;
    }
}

