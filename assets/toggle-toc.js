TOCVisible = false;
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

