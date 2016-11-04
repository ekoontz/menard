var phrase_structure1 = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    parent [label="NP"];
    child1 [label="article"];
    child2 [label="noun"];
    parent -> child1;
    parent -> child2;
}
`;

var phrase_structure2 = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    parent [label="S"];
    child1 [label="NP"];
    child2 [label="VP"];
    parent -> child1;
    parent -> child2;
}
`;

var phrase_structure3 = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    parent [label="S"];
    child1 [label="NP"];
    child2 [label="VP"];
    parent -> child1;
    parent -> child2;
    child3 [label="article"];
    child4 [label="noun"];
    child1 -> child3;
    child1 -> child4;
}
`;

var s_to_np_vp = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    parent [label="S"];
    child1 [label="NP"];
    child2 [label="VP"];
    parent -> child1;
    parent -> child2;
}
`;

var vp_is_head = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    parent [labelType="html" label="<div class='parent'>S</div>"];
    child1 [label="NP"];
    child2 [labelType="html" label="<div class='head'>VP</div>"];
    parent -> child1;
    parent -> child2;
}
`;

var phrase_structure5 = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    s [label="S"];
    np [label="NP"];
    vp [labelType="html" label="<div class='parent'>VP</div>"];
    sleeps [labelType="html" label="<div class='head'>sleeps</div>"];
    s -> np;
    s -> vp;
    vp -> sleeps;
}
`;

var phrase_structure6 = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    s [label="S"];
    np [labelType="html" label="<div class='parent'>NP</div>"];
    vp [label="VP"];
    nbar [labelType="html" label="<div class='head'>N'</div>"];
    
    sleeps [label="sleeps"];

    s -> np;
    s -> vp;
    vp -> sleeps;
    det [label="Det"];
    np -> det;
    np -> nbar;
}
`;

var phrase_structure7 = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    s [label="S"];
    np [label="NP"];
    vp [label="VP"];
    nbar [labelType="html" label="<div class='parent'>N'</div>"];
    
    sleeps [label="sleeps"];

    s -> np;
    s -> vp;
    vp -> sleeps;
    det [label="Det"];
    np -> det;
    np -> nbar;
    adj [labelType="html" label="<div class='comp'>Adjective</div>"];
    nbar -> adj;
    cat [labelType="html" label="<div class='head'>cat</div>"];
    nbar -> cat;

}
`;

var nbar_finished = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    s [label="S"];
    np [label="NP"];
    vp [label="VP"];
    nbar [labelType="html" label="<div class='parent'>N'</div>"];
    
    sleeps [label="sleeps"];

    s -> np;
    s -> vp;
    vp -> sleeps;
    det [label="Det"];
    np -> det;
    np -> nbar;
    adj [labelType="html" label="<div class='comp'>black</div>"];
    nbar -> adj;
    cat [labelType="html" label="<div class='head'>cat</div>"];
    nbar -> cat;

}
`;

var phrase_structure8 = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    s [label="S"];
    np [labelType="html" label="<div class='parent'>NP</div>"];
    vp [label="VP"];
    nbar [labelType="html" label="<div class='head'>N'</div>"];    
    sleeps [label="sleeps"];

    s -> np;
    s -> vp;
    vp -> sleeps;
    det [labelType="html" label="<div class='comp'>Det</div>"];
    np -> det;
    np -> nbar;

    black [label="black"];
    nbar -> black;

    cat [label="cat"];
    nbar -> cat;
}
`;

var phrase_structure9 = `
digraph {
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];
    s [label="S"];
    np [labelType="html" label="<div class='parent'>NP</div>"];
    vp [label="VP"];
    nbar [labelType="html" label="<div class='head'>N'</div>"];    
    sleeps [label="sleeps"];

    s -> np;
    s -> vp;
    vp -> sleeps;
    det [labelType="html" label="<div class='comp'>a</div>"];
    np -> det;
    np -> nbar;

    black [label="black"];
    nbar -> black;

    cat [label="cat"];
    nbar -> cat;


}
`;

var depth_first_1 = `

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

    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
}

`;

var depth_first_2 = `

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
       label="<div class='avm hide'>
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

    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> sees         [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> the_cat      [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
}

`;

var depth_first_3 = `

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

    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> sees         [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> the_cat      [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
}

`;

var depth_first_4 = `

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
    
    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> sees         [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> the_cat      [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    the [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td></td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the</i></td>
                    </tr>
                  </table></div>"]; 

    cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td>🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>cat</i></td>
                    </tr>
                  </table></div>"]; 

    the_cat           -> the       [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    the_cat           -> cat       [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    
}

`;

var depth_first_5 = `

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
    
    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> sees         [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> the_cat      [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    the [labelType="html"
       label="<div class='avm hide'>
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

    the_cat           -> the       [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    the_cat           -> cat       [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    
}

`;

var depth_first_6 = `

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
    
    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> sees         [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> the_cat      [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    the [labelType="html"
       label="<div class='avm'>
                 <table>
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

    the_cat           -> the       [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    the_cat           -> cat       [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    
}

`;

var depth_first_7 = `

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
    
    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> sees         [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> the_cat      [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    the [labelType="html"
       label="<div class='avm'>
                 <table>
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

    the_cat           -> the       [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    the_cat           -> cat       [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    
}

`;

var depth_first_8 = `

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
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , 🐱)</td>
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
    
    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> sees         [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> the_cat      [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    the [labelType="html"
       label="<div class='avm'>
                 <table>
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

    the_cat           -> the       [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    the_cat           -> cat       [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    
}

`;

var instead1 = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 18px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    s [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>Sentence</td>
                    </tr>
                  </table></div>"]; 

    np [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>Noun Phrase</td>
                    </tr>
                    <tr>
                      <th>num</th><td><div class='ref'>1</div></td>
                    </tr>
                  </table></div>"]; 

    vp [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>Verb Phrase</td>
                    </tr>
                    <tr>
                      <th>num</th><td><div class='ref'>1</div></td>
                    </tr>
                  </table></div>"]; 
    
    s -> np [label="comp" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    s -> vp [label="head" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
}

`;

var instead2 = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 18px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    np [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>Noun Phrase</td>
                    </tr>
                  </table></div>"]; 

    det [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>Determiner</td>
                    </tr>
                    <tr>
                      <th>num</th><td><div class='ref'>1</div></td>
                    </tr>
                  </table></div>"]; 

    n [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>Noun</td>
                    </tr>
                    <tr>
                      <th>num</th><td><div class='ref'>1</div></td>
                    </tr>
                  </table></div>"]; 
    
    np -> det [label="comp" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    np -> n [label="head" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
}

`;

var parents_apply_heads = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 18px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    s [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>S &rarr; NP VP</td>
                    </tr>
                    <tr>
                      <th>args</th><td>[ ]</td>
                    </tr>
                  </table></div>"]; 

    np [labelType="html"
	 label="<div style='margin-bottom:1em' class='ref'>1</div>"];

    vp [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>VP</td>
                    </tr>
                    <tr>
                      <th>args</th><td>[</td><td><div class='ref'>1</div></td><td>]</td>
                    </tr>
                  </table></div>"]; 
    
    s -> np [label="comp" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    s -> vp [label="head" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
}

`;

var parents_apply_heads_2 = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 18px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    vp [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>VP &rarr; V VP</td>
                    </tr>
                    <tr>
                      <th>args</th><td>[</td><td><div class='ref'>1</div></td><td>]</td>
                    </tr>
                  </table></div>"]; 

    v [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>V</td>
                    </tr>
                    <tr>
                      <th>args</th><td>[</td><td><div class='ref'>1</div></td><td>,</td><td><div class='ref'>2</div></td><td>]</td>
                    </tr>
                  </table></div>"]; 

    np [labelType="html"
	 label="<div style='margin-bottom:1em' class='ref'>2</div>"];
    
    vp -> v [label="head" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    vp -> np [label="comp" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
}

`;

var parents_semantics = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 18px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    vp [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>semantics</th><td><div class='ref'>1</div></td>
                    </tr>
                  </table></div>"]; 

    v [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>semantics</th><td><div class='ref'>1</div></td>
                    </tr>
                  </table></div>"]; 

    np [labelType="html"
	 label="<div style='margin-bottom:1em'> </div>"];
    
    vp -> v [label="head" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    vp -> np [label="comp" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
}

`;

var add_sleeps = `
digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 18px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    s [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>rule</th><td>S &rarr; NP VP</td>
                    </tr>
                    <tr>
                      <th>args</th><td>[ ]</td>
                    </tr>
                    <tr>
                      <th>semantics</th>
                      <td style='vertical-align:middle'><div class='ref'>3</div></th>
                      <td>
	                <table>
		          <tr>
		            <th>predicate</th>
		            <td>sleeps</th>
		          </tr>
		          <tr>
		            <th>subject</th>
		            <td><div class='ref'>2</div></td>
		            <td><table><tr><th>animate</th><td>true</td></tr></table></table>
		          </tr>
	                </table>

                      </td>
                    </tr>

                  </table></div>"]; 

    np [labelType="html"
	 label="
<table>
  <tr>
    <td style='vertical-align:middle'>
      <div class='ref'>1</div>
    </td>
    <td>
      <div class='avm'>
        <table>
          <tr><th>semantics</th><td><div class='ref'>2</div></td></tr>
          <tr><th>number</th><td>singular</td></tr>
        </table>
      </div>
    </td>
  </tr>
</table>
"];

    vp [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>orthography</th><td>sleeps</td>
                    </tr>
                    <tr><th>number</th><td>singular</td></tr>
                    <tr>
                      <th>args</th>
                      <td>[</td>
                      <td><div class='ref'>1</div></td>
                      <td>]</td>
                    </tr>

 	            <tr>
                      <th>semantics</th>
                      <td><div class='ref'>3</div></td>
	            </tr>


                  </table></div>"]; 
    
    s -> np [label="comp" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    s -> vp [label="head" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
}

`;

var she_sees_the_cat = `

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
    
    she_sees_the_cat  -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    she_sees_the_cat  -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    sees_the_cat      -> sees [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    sees_the_cat      -> the_cat [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    the_cat           -> the [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
    the_cat           -> cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:100%"];
}

`;
