var syntax;
try {
    syntax = graphlibDot.read(depth_first_1);
} catch (e) {
    console.error("could not read input graph:" + e);
}

syntax.graph().transition = function(selection) {
    return selection.transition().duration(500);
};

// Create and configure the renderer
var render = dagreD3.render();

// Render the graph into svg g
var svg = d3.select("#depth_first").call(render, syntax);

