# MacroCAAL
The project aims to design and implement a [Haskell](https://www.haskell.org) program to generate the [CAAL code](http://caal.cs.aau.dk/) corresponding to a high-level CCS process specification.\
Compared to the classical definition of CCS processes, the language gives the possibility of converting imperative commands, communications with value passing, specification of parametric process constants defined by mathematical induction.

### Syntax

Here is the grammar for MacroCAAL where tokens with no semantic values are enclosed between quotes, e.g. `"("`, whereas tokens with semantic values are capitalized, e.g. `INT`. 
As usual the operator `*` means zero or more occurrences, `+` means one or more occurrences, and `?` means at most one.


    Program ::= Statement+ EOF
    
    Statement ::= Assignment ";"
                | SetDeclaration ";"
                | DefDeclaration ";"

    Assignment ::= ProcName "=" Process

    ProcName ::= ProcIdentifier | ProcIdentifier "{" AExpr "}"

    ProcIdentifier ::= [A-Z][a-z,A-Z,0-9]*

    Identifier ::= [a-z][a-z,A-Z,0-9]*

    Process ::= Composition | Composition "+" Process

    Composition :: = Action | Action "|" Composition

    Action ::= Command | PrefixAction | ReProcess

    Command ::= "skip" 
              | Identifier ":=" AExpr
              | Identifier ":=" BExpr
              | "if" BExpr "then" "{" Command "}" else "{" Command "}"
              | "while" BExpr "do" "{" Command "}"
              | "inc" Identifier
              | "dec" Identifier
              | Command "," Command  

    PrefixAction ::= "'" Identifier "." Action
                    |"'"Identifier "<" Identifier ">" "." Action
                    |"'"Identifier "<" INT ">" "." Action
                    |Identifier "." Action
                    |Identifier"("Identifier")" "." Action
    
    Reprocess ::= 0 | ProcName | "(" Process ")" 
                | Reprocess "\" ProcIddentifier
                | Reprocess "\" "{" (Identifier ",")* "}"
                | Reprocess "[" (Identifier/Identifier ",")* "]""
    
    SetDeclaration ::= "set" ProcIdentifier "=" "{" (Identifier ",")* "}"

    DefDeclaration ::= "def" Identifier RangeCmp
                     | "def" ProcIdentifier RangeCmp
                     | "def" Identifier Range
                     | "def" ProcIdentifier Range
                     | "def" Identifier RangeBool
    
    RangeCmp = "<" INT | "<" INT INT | "<=" INT | "<=" INT INT
    
    Range = "[" (INT";")* "]"

    RangeBool = "[" (BOOL";")* "]"

    AExpr = INT | Identifier | ProcIdentifier | "(" AExpr ")"
          | "-" AExpr | Aexpr "*" AExpr | Aexpr "+" AExpr | Aexpr "-" AExpr 
    
    CstAExpr = INT | "(" CstAExpr ")"
             | "-" CstAExpr | CstAexpr "*" CstAExpr | CstAexpr "+" CstAExpr | CstAexpr "-" CstAExpr

    BExpr = BOOL | Identifier | "(" BExpr ")"
          | "!" BExpr | "not" BExpr
          | BExpr "&&" BExpr | Bexpr "and" BExpr 
          | BExpr "||" BExpr | BExpr "||" BExpr
          | Identifier "==" AExpr

### Variables
There are two types of variables: variables for parametric processes and those used in commands or input-output actions.\
**Variables for parametric processes**\
Their names begin with a capital letter and do not translate into any process in CAAL. Variables can only take fixed values of a certain domain, which are specified with the following operators:
- def N <= 3, N can take values ranging from 0 to 3 inclusive (except if < is used).
- def N <= 4 8, N can take values from 4 to 8 inclusive (excluded if < is used).
- def N = [1,2,3], N can take the values listed in the list.  
```
def N<=3;
def M < 5 8;

```
**Variables**\
Their names begin with a lower case letter. The variables are like value servers, some generated processes are dedicated, and through channels, the values can be read or written. Again, the admissible values are specified in the same way as for parametric process variables.
Therefore the following code :
```
def b2=[true,false];
def k=[1,2];

```
 is transformed into :
```
B2 = b2wtrue .B2TRUE  + b2wfalse .B2FALSE ;
B2TRUE = 'b2rtrue .B2TRUE  + B2 ;
B2FALSE = 'b2rfalse .B2FALSE  + B2 ;
K = kw1 .K-1  + kw2 .K-2 ;
K-1 = 'kr1 .K-1  + K  + kInc .K-2 ;
K-2 = 'kr2 .K-2  + K  + kDec .K-1 ;

```
A process is created with the variable name, and through input actions, the value of the variable can be written, then as many processes as the values that variables can assume. In these processes, the value can be read with a compulsion and modified using the increase and decrease channels that follow the order given in the definition.\
In addition to the variable name, new names are used for new processes, which cannot be used for other processes in the future and whose conflict management is left to the user.
The same applies to the names of reading, write, increment and decrement channels.\
More examples are available in the file /test/testVariable.txt e /test/testVariableFail.txt .

### Parametric Processes

The usefulness of parametric processes derives mainly from the avoidance of repeatedly writing processes defined by mathematical induction.\
For simplicity, the expression defining the process parameter can contain at most one variable.\
When translated into CAAL syntax, the value obtained is concatenated with the process name.
The following code is translated 
```
def N<=3;
A{N+1} = a.A{N} + b.0;
A{0} = b.0;
```
into:
```
A1 = a .A0  + b .0 ;
A2 = a .A1  + b .0 ;
A3 = a .A2  + b .0 ;
A4 = a .A3  + b .0 ;
A0 = b .0 ;
```

More examples are available in the file /test/testParProcess.txt e /test/testParProcessFail.txt .

### Commands
Below is the translation of each command.

**Skip**
```
SKIP = skip;
```
```
PROG0 = tau .'done .0 ;
SKIP = PROG0 ;
```

**Assignment**
```
def x <= 3;
ASSINGINT = x:=3;
def b = [true, false];
ASSIGNBOOL =  b := true;
```
```
X = xw0 .X-0  + xw1 .X-1  + xw2 .X-2  + xw3 .X-3 ;
X-0 = 'xr0 .X-0  + X  + xInc .X-1 ;
X-1 = 'xr1 .X-1  + X  + xInc .X-2  + xDec .X-0 ;
X-2 = 'xr2 .X-2  + X  + xInc .X-3  + xDec .X-1 ;
X-3 = 'xr3 .X-3  + X  + xDec .X-2 ;
PROG1 = 'xw3 .'done .0 ;
ASSINGINT = PROG1 ;
B = bwtrue .BTRUE  + bwfalse .BFALSE ;
BTRUE = 'brtrue .BTRUE  + B ;
BFALSE = 'brfalse .BFALSE  + B ;
PROG2 = 'bwTrue .'done .0 ;
ASSIGNBOOL = PROG2 ;
```

**If command**
```
IF = if (x == 3 && b) then {x :=2} else {skip};
```
```
PROG3 = xr3 .brtrue .PROG5  + brfalse .PROG6  + xr0 .PROG6  + xr1 .PROG6  + xr2 .PROG6 ;
PROG5 = 'xw2 .'done .0 ;
PROG6 = tau .'done .0 ;
IF = PROG3 ;
```

**While command**
```
WHILE =  while (! x== 0) do {dec x};

```
```
PROG7 = xr1 .PROG8  + xr2 .PROG8  + xr3 .PROG8  + xr0 .'done .0 ;
PROG8 = 'xDec .PROG7 ;
WHILE = PROG7 ;
```

**Inc comman**
```
INC = inc x;

```
```
PROG11 = 'xInc .'done .0 ;
INC = PROG11 ;
```

**Dec command**
```
DEC = dec x;

```
```
PROG12 = 'xDec .'done .0 ;
DEC = PROG12 ;
```

**Sequntial composition**
```
SEC = inc x,  if (x == 3 && b) then {x :=2} else {skip}, b:=false;
```
```
PROG13 = 'xInc .PROG14 ;
PROG14 = xr3 .brtrue .PROG16  + brfalse .PROG17  + xr0 .PROG17  + xr1 .PROG17  + xr2 .PROG17 ;
PROG16 = 'xw2 .PROG15 ;
PROG17 = tau .PROG15 ;
PROG15 = 'bwFalse .'done .0 ;
SEC = PROG13 ;
```
When processes are generated to represent commands, new process names called PROGN are used, with N incremental value. In addition, coaction on the output channel "done" is produced when a sequence of commands is completed.

### Value Passing
Input actions take an identifier as a parameter and therefore receive any value associated with the variable.\
Output actions could send an integer value or a parameter that takes on a particular value if it was previously associated with the input; otherwise, it remains equal to the parameter name.

```
A = a(x).('b<x>.A + 'c<0>.0);

```
```
A = x0 .( 'b0 .A  + 'c0 .0  ) + x1 .( 'b1 .A  + 'c0 .0  ) + x2 .( 'b2 .A  + 'c0 .0  );
```

### Restriction
When the name of a variable is restricted, all the channel names used to manage the variable are also automatically included in the restriction. They are not necessarily needed by the user who can decide to delete them, which is more accessible than inserting them.
```
def c <=3;
A = (a.b.0 | B )\{c};
```
```
A = ( a .b .0  | B  )\{ cr0, cw0, cr1, cw1, cr2, cw2, cr3, cw3, c, cInc, cDec};
```

****
## Project structure

**Modules MC_Lexer and MC_Parser** 

The MC_lexer and MC_Parser modules are implemented using [Parsec](https://wiki.haskell.org/Parsec), monadic parser combinator library for Haskell. \
In MC_Lexer.hs all the tokens are described using Text.Parsec.Token.
Once the tokens are generated, the sequence of tokens is scanned by the parser. Parsec makes it possible to describe the parser simply; in fact, the structure of the MC_Praser.hs file reflects almost precisely the grammar structure above.

**Module Ast**
This module describes the abstract syntax tree, which represents both programs written in the MacroCAAL language and those written in CAAL. It was unnecessary to identify another abstract syntax tree since CAAL can be expressed as a subset of MacroCAAL.  

**Module Semant** 




****
## Requirement to build the code


compile:
```
ghc --make -o macroCAAL -main-is MC_writeast  MC_writeast.hs


```
test:

```
./macroCAAL /test/test.txt 

```

****
### Future developments
* Implementation of a web interface to use the tool via a browser. 
* Extension of comparison operations for Boolean expressions.
* Modelling of the parallel composition of commands to wait for the termination of both commands.
