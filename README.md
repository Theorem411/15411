# JavaWay
15-411 repo for team JavaWay!

version: 0.0.1

******************************
* Register Allocation design *
******************************
- Liveness:
For each line of the abstract assembly, live_analysis will analyze the following
data:
  def: the temp or register that is defined on this line. None if nothing applicable
  uses: the set of temps or registers that are used on this line
  livein: the set of temps or registers that are live before this line of code
  liveout: the set of temps or registers that are live after this line of code 
The liveness info of each line is gathered from back to front, using update rules
introduced in lecture notes and recitation handouts. 
# Note: to avoid precoloring conflicts, for each line involving a DIV operater,
 we add %eax and %edx to uses. 

- Build interference graph
The vertex is an abstract type that can be either a temp or a register
The interference graph adopts an adjacency list representation, and is implemented
as a Map from vertex to a vertex set.
The edges are collected in liveness_analysis. After each line's liveout is updated
the edges are collected according to the rules in the lecture notes. 
# Note: for mv operation, if the src is also a temp or a register (i.e., a vertex)
the def will not interfere with it. 

- Simplicial elimination ordering to determine vertex ordering
After the graph is built, we use Maximum Capacity Search to determine a coloring 
order that are guaranteed to produce a simplicial elimination ordering on Chordal
interference graph (see theorem in lecture note). It is also guaranteed that on 
Chordal graph such ordering will produce the optimal coloring. 

# Note: the weights are initialized such each vertex is assigned the number of 
neighbor vertices that are already registers.

- Greedy graph coloring
We define color simply as the int type
We first do precoloring by coloring all the registers that are already in the 
graph; then for each uncolored vertex in the simplicial elimination ordering, we
find the minimum unused color in its neighborhood and to color this vertex with
it.

**********************************************
* translating abstract L1 to x86-64 assembly *
**********************************************
- register assignment
After building an interference graph and produces a coloring (i.e. map from vertex
to color), we group the vertex by colors. For a group of vertices of the same color
we first determine if there is a register among them. If there is, we will assign 
that the rest of the group (temps) that register. 
# Note: when one group cannot contain more than two registers because the precoloring
ensures all registers are assigned unique colors.
For groups that do not have a register, we will first check for any unused 
registers. If there are none, we will spill them onto the stack. 

- convert 3-addr to 2-addr
We iterate through the input 3-addr abstract assembly looking for lines like this:
  dst <- op (lhs, rhs)
and translate into this format:
  dst <- lhs
  dst <- op (dst, rhs)

- cogen function 
we uses a top-down approach. 
