// Don't write the TOC on the index.
if (window.location.pathname !== "{{ site.baseurl }}/") {
  $("#toc").toc({
    content: "#content", // will ignore the first h1 with the site+page title.
    headings: "h1,h2,h3,h4",
  });
}

$("#two-cols + ul").css({ "column-count": "2" });
$("#contributors + ul").css({ "column-count": "4" });
