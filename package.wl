(* ::Package:: *)

Cut0[a__]:=
Module[{b=a,c},
c=ResourceFunction["SetPartitions"][b];
c
]


Cut[a__,ini__]:=
Module[ {d=a,
n=a//Flatten//ToString//StringCount[#,ini//ToString]&,
nnnn=a//Flatten//ToString//StringCount[#,"l["]&,b,c
},
c=If[EvenQ[n],l[(nnnn+2)/2],ini];

d/.{ini->c}
]


Cut2[a__,ini__]:=
Module[ {d=a,
n=a//Flatten//ToString//StringCount[#,ini//ToString]&,
nnnn=a//Flatten//ToString//StringCount[#,"l["]&,b,c
},
c=If[OddQ[n],l[nnnn+1],ini];

d/.{ini->c}
]


Cut1[a__]:=
Module[{b=a},
For[i = 0, i < 30, i++, b=Cut[b,in[i]]];
b
]


Cut2[a__,ini__]:=
Module[ {d=a,
n=a//Flatten//ToString//StringCount[#,ini//ToString]&,
nnnn=a//Flatten//ToString//StringCount[#,"l["]&,b,c
},
c=If[OddQ[n],l[nnnn+1],ini];

d/.{ini->c}
]


Cut3[a__]:=
Module[{b=a},
For[i = 0, i < 30, i++, b=Cut[b,in[i]]];
b
]


Sew1[a__]:=
Module[{b=a,c=Length[a],d},
d=If[c>1,
For[i = 0, i < c-1, i++, b[[i+2]]=sewGraphs[(b[[i+1]]//treesToLoops)[[1]],(b[[i+2]]//treesToLoops)[[1]]]];
b[[c]]//doOrderedPlot[#,k/@Range[4]]&,
b[[1]]//doOrderedPlot[#,k/@Range[4]]&
]

]


Sew2[a__]:=
Module[{b=a,c=Length[a],d,e},
Table[e[i]=(b[[i]]//treesToLoops)[[1]],{i,1,c}];
d=If[c>1,
For[i = 0, i < c-1, i++, e[i+2]=sewGraphs[e[i+1],e[i+2]]];
b[[c]],
b[[1]]
]

]


Sew3[a__]:=
Module[{b=a,c=Length[a],d,e},
Table[e[i]=(b[[i]]//treesToLoops)[[1]],{i,1,c}];
d=If[c>1,
For[i = 0, i < c-1, i++, e[i+2]=sewGraphs[e[i+1],e[i+2]]];
e[c]//doOrderedPlot[#,k/@Range[4]]&,
e[1]//doOrderedPlot[#,k/@Range[4]]&
]

]


Sew4[a__]:=
Module[{b=a,c=Length[a],d,e},
Table[e[i]=(b[[i]]//treesToLoops)[[1]],{i,1,c}];
d=If[c>1,
For[i = 0, i < c-1, i++, e[i+2]=sewGraphs[e[i+1],e[i+2]]];
e[c],
e[1]
]

]


SewTest1[a__]:=
Module[{b=a,c=Length[a]},
Map[Sew3,Table[b[[i]],{i,1,c}]]
]


SewTest2[a__]:=
Module[{b=a,c=Length[a],d={},e=SewTest1[a]},
For[i = 0, i < c-1, i++, If[e[[i+1]]===e[[1]],d=Append[d,i+1]]];
d
]


SewTest3[a__]:=
Module[{b=a,f=a//Cut4,c=Length[a],d={},e=SewTest1[a]},
For[i = 0, i < c-1, i++, If[e[[i+1]]===e[[1]],d=Append[d,b[[i+1]]]]];
d
]


SewTest4[a__]:=
Module[{b=a,f=a//Cut4,c=Length[a],d={},e=SewTest1[a]},
For[i = 0, i < c-1, i++, If[e[[i+1]]===e[[1]],d=Append[d,f[[i+1]]]]];
d
]


SelectSew1[a__]:=
Module[{b=a},
Map[b[[#]]&,SewTest2[b]]
]


SelectSew2[a__]:=
Module[{b=a,c},
c=b//SelectSew1;
c
]


Cut4[a__]:=
Module[{b=a,c=Length[a//SelectSew1],d,m,n,e},

e=(b//SelectSew1);

For[m = 0, m < c, m++,
e[[m+1]]=Map[Cut3,(b//SelectSew1)[[m+1]]]
];
e
]


SewSew[a__]:=
Module[{b=a,c},
c=b/.{Atree->neckl};
c
]


SewSew1[a__]:=
Module[{b=a//SewSew,c},
c=vertexFormGraph[b];
c
]


UncutOff[a__]:=
Module[{b=a,c},
c=Delete[b,1];
c
]


SelectSew3[a__]:=
Module[{b=a,c,d},
c=b//Cut4;
d=c//UncutOff;
d
]


SelectSew4[a__]:=
Module[{b=a,c,d=(a//SelectSew3)//Length,e},
c=b//SelectSew3;
e=c;
For[m = 0, m < d, m++,
e[[m+1]]=Map[SewSew1,(c)[[m+1]]]
];
e
]


SelectSew5[a__]:=
Module[{b=a,c,d=(a//SelectSew3)//Length,e},
c=b//SelectSew3;
e=c//SewSew;


e
]


CutlegSubstituion[a__]:=
Module[{b=a,c},
c=b//SelectSew5;
c
]



sewGraphs1[a__,b__] :=
    Module[ {na = Length[getConnectingEdges[a]],
    bConEdges = getConnectingEdges[b]
    },
        vertexFormGraph[
        Join[a, b/.
        Map[
        #->in[#[[1]]+na]&,
        bConEdges]
        ]
        ]
    ]





SewGraphs2[a__,b__]:=
Module[{c=a,d=b,e,f},
e=Join[c,d];
f=vertexFormGraph[e];
f
]


SewAll1[a__]:=
Module[{b=a,c=a//Length,e,f},
e=Join[Table[b[[i]],{i,1,c}]];
f=vertexFormGraph[e//Flatten];
f
]


SewAll2[a__]:=
Module[{b=a,c},
c=Map[SewAll1,b];
c
]


SewAll3[a__]:=
Module[{b=a,c,d},
c=Map[SewAll1,b];
d=Map[doOrderedPlot[#,k/@Range[4]]&,c];
d
]


getAllCutsgraphs[a__]:=
Module[{b=a},
c=b//Cut0;
d=c//SelectSew5;
e=d//SewAll3;
e]


getAllCutsgraphs[a__]:=
Module[{b=a},
c=b//Cut0;
d=c//SelectSew5;
e=d//SewAll2;
e]
