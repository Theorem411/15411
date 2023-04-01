 dump_parsing... 
typedef fpt <--- int;
fpt fadd (fpt x,fpt y);
fpt fsub (fpt x,fpt y);
fpt fmul (fpt x,fpt y);
fpt fdiv (fpt x,fpt y);
bool fless (fpt x,fpt y);
fpt itof (int n);
int ftoi (fpt x);
typedef dub <--- pointer type: (struct dub*);
dub dadd (dub x,dub y);
dub dsub (dub x,dub y);
dub dmul (dub x,dub y);
dub ddiv (dub x,dub y);
bool dless (dub x,dub y);
dub itod (int n);
int dtoi (dub x);
void print_fpt (fpt x);
void print_dub (dub x);
void print_int (int n);
void print_hex (int n);


------------------------------------------

struct bar [int hi]
int main () {
 int x = 5;
pointer type: (struct bar*) m;
WHILE ((x < 10)) {
 x++
 }

if ((x >= 10)) 
then [
m = null;]
ELSE[
m = alloc(struct bar);]
m->hi = (0 / 0);
return 1;
 }
;



typedef int <--- fpt;
fadd(x,y,): (fpt, fpt, ) -> fpt


fsub(x,y,): (fpt, fpt, ) -> fpt


fmul(x,y,): (fpt, fpt, ) -> fpt


fdiv(x,y,): (fpt, fpt, ) -> fpt


fless(x,y,): (fpt, fpt, ) -> bool


itof(n,): (int, ) -> fpt


ftoi(x,): (fpt, ) -> int


typedef pointer type: (struct dub*) <--- dub;
dadd(x,y,): (dub, dub, ) -> dub


dsub(x,y,): (dub, dub, ) -> dub


dmul(x,y,): (dub, dub, ) -> dub


ddiv(x,y,): (dub, dub, ) -> dub


dless(x,y,): (dub, dub, ) -> bool


itod(n,): (int, ) -> dub


dtoi(x,): (dub, ) -> int


print_fpt(x,): (fpt, ) -> void


print_dub(x,): (dub, ) -> void


print_int(n,): (int, ) -> void


print_hex(n,): (int, ) -> void





------------------------------------------



main(): () -> int


struct bar {hi:int}
main(): () -> int =
Seq(Declarig(int x=5; [Declarig(pointer type: (struct bar*) m; [Seq(while((x < 10)) {
Seq(Seq(x = (x + 1);
;nop;)
;nop;)}
;Seq(if ((x >= 10)) {
Seq(m = null;
;nop;)
}
else {
Seq(m = alloc(struct bar);
;nop;)
}
;Seq(m->hi = (0 / 0);
;Seq(return 1;
;nop;))))])])
;nop;)





------------------------------------------



typedef int <--- fpt;
fadd(x,y,): (fpt, fpt, ) -> fpt


fsub(x,y,): (fpt, fpt, ) -> fpt


fmul(x,y,): (fpt, fpt, ) -> fpt


fdiv(x,y,): (fpt, fpt, ) -> fpt


fless(x,y,): (fpt, fpt, ) -> bool


itof(n,): (int, ) -> fpt


ftoi(x,): (fpt, ) -> int


typedef pointer type: (struct dub*) <--- dub;
dadd(x,y,): (dub, dub, ) -> dub


dsub(x,y,): (dub, dub, ) -> dub


dmul(x,y,): (dub, dub, ) -> dub


ddiv(x,y,): (dub, dub, ) -> dub


dless(x,y,): (dub, dub, ) -> bool


itod(n,): (int, ) -> dub


dtoi(x,): (dub, ) -> int


print_fpt(x,): (fpt, ) -> void


print_dub(x,): (dub, ) -> void


print_int(n,): (int, ) -> void


print_hex(n,): (int, ) -> void





------------------------------------------



main(): () -> int


struct bar {hi:int}
main(): () -> int =
Seq(Declarig(int x=5; [Declarig(pointer type: (struct bar*) m; [Seq(while((x < 10)) {
Seq(Seq(x = (x + 1);
;nop;)
;nop;)}
;Seq(if ((x >= 10)) {
Seq(m = null;
;nop;)
}
else {
Seq(m = alloc(struct bar);
;nop;)
}
;Seq(m->hi = (0 / 0);
;Seq(return 1;
;nop;))))])])
;nop;)





------------------------------------------



_c0_main() =
decl x=5;
decl m;
while((x <(4) 10)) {
x = (lhs=x + rhs=1);
nop;
nop;}
if ((x >=(4) 10)) {
m = NULL;
nop;
}
else {
m = alloc(4);
nop;
}
(m[+0])p = (0 / 0)
return 1 of size [4]
nop;
nop;



func _c0_main():
._c0_main()
%t1  <--  5
goto .L2
-------[goto .L2]---------
.L2
if (%t1 <(small) 10).L3 else .L4
-------[t:.L3|f:.L4]---------
.L3
%t1  <--  %t1 + 1
goto .L2
-------[goto .L2]---------
.L4
if (%t1 >=(small) 10).L5 else .L6
-------[t:.L5|f:.L6]---------
.L5
%t2  <--  null
goto .L7
-------[goto .L7]---------
.L6
%t3 <-- alloc (4)
%t2  <--  %t3
goto .L7
-------[goto .L7]---------
.L7
%t4  <--  (%t2, 0)
%t5  <--  (0 / 0)
(%t2, 0) <-- %t5
return 1
-------[ret]---------
.L8
return
-------[ret]---------
_c0_main(): 

._c0_main():
%t1 <-s- $5
jump.L2

.L2

.L2:
%t6 <-s- %t1
%t7 <-s- $10
cmps %t6, %t7
Jge .L4
jump.L3

if(.L3|.L4)

.L3:
%t8 <-s- %t1
%t9 <-s- $1
%t1 <-s- %t8 + %t9
jump.L2

.L2

.L4:
%t10 <-s- %t1
%t11 <-s- $10
cmps %t10, %t11
Jl .L6
jump.L5

if(.L5|.L6)

.L5:
%t2 <-l- $0
jump.L7

.L7

.L6:
RDI <-s- $4
call ____calloc_javaway(RDIs|)
%t3 <-l- EAX
%t2 <-l- %t3
jump.L7

.L7

.L7:
%t12 <-l- %t2
cmpl %t12, $0
Je .L9
%t13 <-l- %t12 + $0
%t4 <-s- (%t13)
%t14 <-s- $0
%t15 <-s- $0
%t5 <-- %t14 / %t15
%t16 <-l- %t2
cmpl %t16, $0
Je .L9
%t17 <-l- %t16 + $0
%t18 <-s- %t5
(%t17) <-s- %t18
EAX <-s- $1
ret

ret

.L8:
ret

ret

---Blocks---

_c0_main(){
[
FunLbl _c0_main():
|
  0:%t1 <-s- $5
  1:jump.L2
|
jump -> .L2
]

[
BlockLbl(.L2):
|
  2:%t6 <-s- %t1
  3:%t7 <-s- $10
  4:cmps %t6, %t7
  5:Jge .L4
  6:jump.L3
|
jump -> .L3 or .L4
]

[
BlockLbl(.L3):
|
  7:%t8 <-s- %t1
  8:%t9 <-s- $1
  9:%t1 <-s- %t8 + %t9
  10:jump.L2
|
jump -> .L2
]

[
BlockLbl(.L4):
|
  11:%t10 <-s- %t1
  12:%t11 <-s- $10
  13:cmps %t10, %t11
  14:Jl .L6
  15:jump.L5
|
jump -> .L5 or .L6
]

[
BlockLbl(.L5):
|
  16:%t2 <-l- $0
  17:jump.L7
|
jump -> .L7
]

[
BlockLbl(.L6):
|
  18:RDI <-s- $4
  19:call ____calloc_javaway(RDIs|)
  20:%t3 <-l- EAX
  21:%t2 <-l- %t3
  22:jump.L7
|
jump -> .L7
]

[
BlockLbl(.L7):
|
  23:%t12 <-l- %t2
  24:cmpl %t12, $0
  25:Je .L9
  26:%t13 <-l- %t12 + $0
  27:%t4 <-s- (%t13)
  28:%t14 <-s- $0
  29:%t15 <-s- $0
  30:%t5 <-- %t14 / %t15
  31:%t16 <-l- %t2
  32:cmpl %t16, $0
  33:Je .L9
  34:%t17 <-l- %t16 + $0
  35:%t18 <-s- %t5
  36:(%t17) <-s- %t18
  37:EAX <-s- $1
  38:ret
|
ret
]

[
BlockLbl(.L8):
|
  39:ret
|
ret
]

}


