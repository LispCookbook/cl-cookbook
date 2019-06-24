$(document).ready( function () {
    TOCVisible = false;

    function toggleTOCButton(e){
        if (window.location.pathname == "/cl-cookbook/"){
            document.getElementById("toc-btn").style.display = "none";
        }else if ($(document).width() <= 576){
            document.getElementById("toc-btn").style.display = "block";
        }else{
            document.getElementById("toc-btn").style.display = "none";
        }
    }

    setInterval(toggleTOCButton, 200);

    toggleTOC = function(){
        toc = document.getElementById("toc-container");
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

    $('#toc-container').click(function(e) {
        if (($(document).width() <= 576) && ($(e.target).is('a'))){
            console.log("yes!");
            toggleTOC();
        }
    })
    $('#content-container').click(function(e) {
	if (($(e.target).is('#toc-btn'))
	    || (($(document).width() <= 576) && TOCVisible)){
            toggleTOC();
        }
    })
})
