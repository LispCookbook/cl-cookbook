document.addEventListener("DOMContentLoaded", function () {
  var $ = window.jQuery;
  var base =
    (document.documentElement.getAttribute("data-baseurl") || "") + "/";

  // don't show the TOC on the index page.
  var onIndexPage = window.location.pathname === base;

  // for safety, check jQuery is loaded and #toc exists.
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
