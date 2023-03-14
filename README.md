### JavaWay
15-411 repo for team JavaWay!

version: 0.0.1

# C0 Compiler Pipeline overview 
This project is divided into three phases: the frontend, the middle end, and the backend. 
The frontend contains: 
* lexer and parser
* elaborator
* static semantics
The middle end contains:
* trans (into IR tree)
* cogen (into Assem, i.e., 3-addr abstract assembly)
* liveness analysis
The back end contains: 
* register allocation
* x86 assembly generation

# Frontend
## Lexing and Parsing

## Elaborater
intermediate representation: JavaWay/compiler/lab3/lib/elaborater/aste.ml
featuring: 
* turning for loop into while loop
* turning Ast.Block, which is a list of stmt, into recursive Seq form (check out aste.Seq)
* remove all syntactic sugars such as post operators, -n to 0-n, 
* elaborating logical AND and logical OR to ternary operator form

## Static Semantics
code in: JavaWay/compiler/lab3/lib/type/statsem.ml
This phase of the compiler checks the static semantic of the program. The input of this phase is aste.t, and will raise error if certain part of the program violates static semantic rules. Program that passes the static semantic phase will not have undefined behavior during evaluation, and will not get stuck in the dynamic semantic. 
### Type checking and variable initialization
Type checking is done differently at different synatic levels
* global: mostly check whether function and typedef namespaces do not overlap; functions aren't redefined, and are always declared with the same signature; header files can only contains typedefs and function declarations.  
* statements: for each statement, check whether it will return the intended type, and collects all variales that got initialized (variable is initialized only in assign and non-empty declare (i.e., declare with assigned value). 
* expression: synthesize or check types based on their operators. 

### Return Checking
Check whether a function's definition properly returns. Note: functions with void return type are ignored. 

# Middle End
## IR Tree: 
Intermediate representation: Javaway/compiler/lab3/lib/trans/tree.ml</br>
IR tree differs from elaborated Ast fundamentally. </br>
At global level, typedefs and function declarations are removed, leaving function definition as the only parts of the original program that has semantic meanings (now called *fspace*). The program is just a list of *fspace*s. </br>
Other distinctions: 
* *If* conditional only accepts comparisons, which are in the form of $e_1 ? e_2$; the branches, which are statements before, now becomes labels that lead to statements where the branch codes are transformed. 
* *While* loop is transformed into if conditionals with back jump
* Pure expressions are distinguished from Effectful expression at the syntactic level.
The trans function 
$$tr(e) = <\hat{e}, \check{e}>$$

## trans: 
code in: Javaway/compiler/lab3/lib/trans/trans.ml</br>
Trans takes in a elaborated ast and turn it into an IR tree. 

## assem (aka. 3-address abstract assembly)
Three address abstract assembly differs from IR tree in the following ways: 

Intermediate representation: Javaway/compiler/lab3/lib/codegen/assem.ml


## cogen: 
code: Javaway/compiler/lab3/lib/codegen/cogen.ml


# Register Allocation design 
## Liveness analysis
For each line of the abstract assembly, live_analysis will analyze the following
data:
  def: the temp or register that is defined on this line. None if nothing applicable
  uses: the set of temps or registers that are used on this line
  livein: the set of temps or registers that are live before this line of code
  liveout: the set of temps or registers that are live after this line of code 
The liveness info of each line is gathered from back to front, using update rules
introduced in lecture notes and recitation handouts. 
* Note: to avoid precoloring conflicts, for each line involving a DIV operater,
 we add %eax and %edx to uses. 

## Build interference graph
The vertex is an abstract type that can be either a temp or a register
The interference graph adopts an adjacency list representation, and is implemented
as a Map from vertex to a vertex set.
The edges are collected in liveness_analysis. After each line's liveout is updated
the edges are collected according to the rules in the lecture notes. 
# Note: for mv operation, if the src is also a temp or a register (i.e., a vertex)
the def will not interfere with it. 

* Simplicial elimination ordering to determine vertex ordering
After the graph is built, we use Maximum Capacity Search to determine a coloring 
order that are guaranteed to produce a simplicial elimination ordering on Chordal
interference graph (see theorem in lecture note). It is also guaranteed that on 
Chordal graph such ordering will produce the optimal coloring. 

**Note**: the weights are initialized such each vertex is assigned the number of 
neighbor vertices that are already registers.

* Greedy graph coloring
We define color simply as the int type
We first do precoloring by coloring all the registers that are already in the 
graph; then for each uncolored vertex in the simplicial elimination ordering, we
find the minimum unused color in its neighborhood and to color this vertex with
it.


# Backend
## register allocation
After building an interference graph and produces a coloring (i.e. map from vertex
to color), we group the vertex by colors. For a group of vertices of the same color
we first determine if there is a register among them. If there is, we will assign 
that the rest of the group (temps) that register. 
**Note**: when one group cannot contain more than two registers because the precoloring
ensures all registers are assigned unique colors.
For groups that do not have a register, we will first check for any unused 
registers. If there are none, we will spill them onto the stack. 

## x86 generation
We iterate through the input 3-addr abstract assembly looking for lines like this:
  dst <- op (lhs, rhs)
and translate into this format:
  dst <- lhs
  dst <- op (dst, rhs)

- cogen function 
we uses a top-down approach. 
