(function () {
  function patch(x) {
    document.getElementsByTagName("head")[0].appendChild(x);
  }
  document.title = "Functora Currency Converter";
  var x0 = document.createElement("meta");
  x0.name = "viewport";
  x0.content = "width=device-width, initial-scale=1";
  patch(x0);
  var x1 = document.createElement("link");
  x1.rel = "apple-touch-icon";
  x1.sizes = "180x180";
  x1.href = "static/apple-touch-icon.png";
  patch(x1);
  var x2 = document.createElement("link");
  x2.rel = "icon";
  x2.type = "image/png";
  x2.sizes = "32x32";
  x2.href = "static/favicon-32x32.png";
  patch(x2);
  var x3 = document.createElement("link");
  x3.rel = "icon";
  x3.type = "image/png";
  x3.sizes = "16x16";
  x3.href = "static/favicon-16x16.png";
  patch(x3);
  var x4 = document.createElement("link");
  x4.rel = "manifest";
  x4.href = "static/site.webmanifest";
  patch(x4);
})();
