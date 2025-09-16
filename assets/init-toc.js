document.addEventListener("DOMContentLoaded", function () {
  var $ = window.jQuery;
  var base =
    (document.documentElement.getAttribute("data-baseurl") || "") + "/";
  var onIndexPage = window.location.pathname === base;

  if (!onIndexPage && $ && $.fn && $.fn.toc && document.getElementById("toc")) {
    $("#toc").toc({
    content: "#content", // will ignore the first h1 with the site+page title.
    headings: "h1,h2,h3,h4",
  });
  }

  if ($) {
    $("#two-cols + ul").css({ "column-count": "2" });
    $("#contributors + ul").css({ "column-count": "4" });
  }
});
