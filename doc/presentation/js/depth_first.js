var graph = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    she_sees_the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                  </table></div>"]; 

    she [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍👱‍♀️</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>she</i></td>
                    </tr>
                  </table></div>"]; 

    sees_the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                  </table></div>"]; 

    sees [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>sees</i></td>
                    </tr>
                  </table></div>"]; 

    the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the cat</i></td>
                    </tr>
                  </table></div>"]; 

    she_sees_the_cat  -> she [label="arg" labelStyle="fill: #55f; font-weight: bold;"];
    she_sees_the_cat  -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold;"];
    sees_the_cat -> sees [label="f()" labelStyle="fill: #55f; font-weight: bold;"];
    sees_the_cat -> the_cat  [label="arg" labelStyle="fill: #55f; font-weight: bold;"];
}

`

var syntax;
try {
    syntax = graphlibDot.read(graph);
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

