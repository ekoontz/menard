// Create a new directed graph
var g = new dagreD3.graphlib.Graph().setGraph({});

g.setNode("det",   { shape: "circle" });
g.setNode("noun",  { shape: "circle" });
g.setNode("noun2", { shape: "circle" });
g.setNode("NP",    { shape: "circle" });
g.setNode("S",     { shape: "circle" });
g.setNode("VP",    { shape: "circle" });
g.setNode("verb",  { shape: "circle" });

g.setEdge("S",     "noun", {});
g.setEdge("S",     "VP", {});
g.setEdge("VP",    "verb", {});
g.setEdge("VP",    "NP", {});
g.setEdge("NP",    "det", {});
g.setEdge("NP",    "noun2", {});

var svg = d3.select("#tree1"),
    inner = svg.select("g");

// Set up zoom support
var zoom = d3.behavior.zoom().on("zoom", function() {
      inner.attr("transform", "translate(" + d3.event.translate + ")" +
                                  "scale(" + d3.event.scale + ")");
    });
svg.call(zoom);

// Create the renderer
var render = new dagreD3.render();

// Run the renderer. This is what draws the final graph.
render(inner, g);

// Center the graph
var initialScale = 1;

zoom
  .translate([(svg.attr("width") - g.graph().width * initialScale) / 2, 20])
  .scale(initialScale)
  .event(svg);

svg.attr('height', g.graph().height * initialScale + 40);

