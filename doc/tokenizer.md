White House Fund Raiser

0      1       2      3        4
 White | House   Fund   Raiser

 White   House | Fund   Raiser

 White   House   Fund | Raiser

[0 1][1 4]
[0 2][2 4]
[0 3][3 4]

0      1       2      3        4
 White | House   Fund | Raiser

[0 1][1 3][3 4]


The bus stop

0    1     2      3
 The | Bus   Stop
 The   Bus | Stop


[0 1][1 3]
[0 2][2 3]







split
generate-pairs
merge

split + generate pairs:

0       1        2        3

a the   | white  | house
b the   | white    house
c the     white  | house

d white | house  | press
e white | house    press
f white   house  | press

g house | press  | corps
h house | press    corps
i house   press  | corps

j press | corps  | dinner
k press | corps    dinner
l press   corps  | dinner

merge:

m merge(a,d) = the | white | house | press
n merge(a,e) = the | white | house   press
o merge(a,f) = nil

p merge(b,d) = nil
q merge(b,e) = nil
r merge(b,f) = the | white house | press

s merge(c,d) = the   white | house | press
t merge(c,e) = the   white | house   press
u merge(c,f) = the   white   house | press

v merge(m,g) = the | white | house | press | corps
w merge(m,h) = the | white | house | press   corps
x merge(m,i) = nil

y merge(n,g) = nil
z merge(n,h) = the | white | house   press   corps
A merge(n,i) = the | white | house   press | corps

B merge(r,g) = merge(the | white house | press, house | press | corps) =
             = nil
C merge(r,h) = merge(the | white house | press, house | press   corps) =
               the | white   house | press corps   
D merge(r,i) = merge(the | white house | press, house press |   corps) =
             = nil
E merge(v,j) = merge(the | white | house | press | corps, press | corps  | dinner) =
             = the | white | house | press | corps  | dinner



The | White | House | Press | Corps | Dinner 31
The | White | House | Press | Corps   Dinner 30
The | White | House | Press   Corps | Dinner 29
The | White | House | Press   Corps   Dinner 28
The | White | House   Press | Corps | Dinner 27
The | White | House   Press | Corps   Dinner 26
The | White | House   Press   Corps | Dinner 25
The | White | House   Press   Corps   Dinner 24
The | White   House | Press | Corps | Dinner 23
The | White   House | Press | Corps   Dinner 22
The | White   House | Press   Corps | Dinner 21
The | White   House | Press   Corps   Dinner 20
The | White   House   Press | Corps | Dinner 19
The | White   House   Press | Corps   Dinner 18
The | White   House   Press   Corps | Dinner 17
The | White   House   Press   Corps   Dinner 16
The   White | House | Press | Corps | Dinner 15
The   White | House | Press | Corps   Dinner 14
The   White | House | Press   Corps | Dinner 13
The   White | House | Press   Corps   Dinner 12
The   White | House   Press | Corps | Dinner 11
The   White | House   Press | Corps   Dinner 10
The   White | House   Press   Corps | Dinner 09
The   White | House   Press   Corps   Dinner 08
The   White   House | Press | Corps | Dinner 07
The   White   House | Press | Corps   Dinner 06
The   White   House | Press   Corps | Dinner 05
The   White   House | Press   Corps   Dinner 04
The   White   House   Press | Corps | Dinner 03
The   White   House   Press | Corps   Dinner 02
The   White   House   Press   Corps | Dinner 01
The   White   House   Press   Corps   Dinner 00

Start with:
 - Bit pattern:  21 =>  [   1      0      1      0      1      ]
 - Words:               [The, White, House, Press, Corps, Dinner]
 - current_string := ""
 - tokens = []
 - next-word    := "The"

Iteration 0:
 - current_bit   := 1
 - token-in-progress := token-in-progress + "The" => "The"
 - next_word    := "White"

Iteration 1:
 - current_bit   := 0
 - tokens        := append(tokens,[token-in-progress]) => ["The"]
 - token-in-progress := next_word => "White"
 - next_word    := "House"

Iteration 2:
 - current_bit   := 1
 - token-in-progress := token-in-progress + next_word => "White House"
 - next_word    := "Press"

Iteration 3:
 - current_bit   := 0
 - tokens        := append(tokens,[token-in-progress]) => ["The","White House"]
 - token-in-progress := next_word => "Press"
 - next_word    := "Corps"

Iteration 4:
 - current_bit   := 1
 - token-in-progress := token-in-progress + next_word => "Press Corps"
 - next_word    := "Dinner"

Iteration 5:
 - current_bit   := nil => 0
 - tokens        := append(tokens,[token-in-progress]) => ["The","White House","Press Corps"]
 - token-in-progress := next_word => "Dinner"
 - next_word    := nil

Iteration 6:
 - current_bit   := nil => 0
 - tokens        := append(tokens,[token-in-progress]) => ["The","White House","Press Corps","Dinner"]
 - token-in-progress := next_word => nil
 - next_word    := nil

Iteration 7:
 - current_bit   := nil => 0
 - token-in-progress is nil, so return tokens: ["The","White House","Press Corps","Dinner"]
 



