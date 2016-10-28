var digraph = "" +
    "digraph { " +
"    node [rx=5 ry=5 labelStyle=\"font: 300 14px 'Helvetica Neue', Helvetica\"] " +
"    edge [labelStyle=\"font: 300 14px 'Helvetica Neue', Helvetica\"] " +
"    A [labelType=\"html\" " +
"       label=\"A <span style='font-size:32px'>Big</span> <span style='color:red;'>HTML</span> Source!\"]; " +
"    C; " +
"    E [label=\"Bold Red Sink\" style=\"fill: #f77; font-weight: bold\"]; " +
"    A -> B -> C; " +
"    B -> D [label=\"A blue label\" labelStyle=\"fill: #55f; font-weight: bold;\"]; " +
"    D -> E [label=\"A thick red edge\" style=\"stroke: #f77; stroke-width: 2px;\" arrowheadStyle=\"fill: #f77\"]; " +
"    C -> E; " +
"    A -> D [labelType=\"html\" label=\"A multi-rank <span style='color:blue;'>HTML</span> edge!\"]; " +
"    }";

var tree3;
try {
    tree3 = graphlibDot.read(digraph);
} catch (e) {
    console.error("could not read input graph:" + e);
}

tree3.graph().transition = function(selection) {
    return selection.transition().duration(500);
};
// Create and configure the renderer
var render = dagreD3.render();

// Render the graph into svg g
d3.select("#tree3").call(render, tree3);

