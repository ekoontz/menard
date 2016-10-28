var digraph = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    she_sees_the_cat [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td> 👁 (👱‍♀️,🐱)</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>she sees the cat</i></td>
                    </tr>
                  </table></div>"]; 

    she [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍👱‍♀️</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>she</i></td>
                    </tr>
                  </table></div>"]; 

    sees_the_cat [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ ,🐱)</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>sees the cat</i></td>
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
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the cat</i></td>
                    </tr>
                  </table></div>"]; 

    the [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td></td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the</i></td>
                    </tr>
                  </table></div>"]; 

    cat [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td>🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>cat</i></td>
                    </tr>
                  </table></div>"]; 


    
    she_sees_the_cat  -> she;
    she_sees_the_cat  -> sees_the_cat;
    sees_the_cat -> sees;
    sees_the_cat -> the_cat;
    the_cat -> the;
    the_cat -> cat;
}

`

var syntax;
try {
    syntax = graphlibDot.read(digraph);
} catch (e) {
    console.error("could not read input graph:" + e);
}

syntax.graph().transition = function(selection) {
    return selection.transition().duration(500);
};

// Create and configure the renderer
var render = dagreD3.render();

// Render the graph into svg g
d3.select("#she_sees_the_cat").call(render, syntax);
