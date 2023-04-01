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

int prepare_for_trouble (int a,int b,int c,int d,int e,int f,int g,int h,int i,int j,int k,int l,int m,int n,int o,int p,int q,int r,int s,int t,int u,int v,int w,int x,int y,int z);
int make_it_double (int x);
void blast_off_at_the_speed_of (int light);
void surrender_now_or_prepare_to_fight (bool surrender,bool fight);
void meowth_thats_right ();
int prepare_for_trouble (int a,int b,int c,int d,int e,int f,int g,int h,int i,int j,int k,int l,int m,int n,int o,int p,int q,int r,int s,int t,int u,int v,int w,int x,int y,int z) {
 int light = make_it_double((((((((((((((((((((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j) + k) + l) + m) + n) + o) + p) + q) + r) + s) + t) + u) + v) + w) + x) + y) + z));
blast_off_at_the_speed_of(light)
return light;
 }
;
int make_it_double (int x) {
 return (2 * x);
 }
;
void blast_off_at_the_speed_of (int light) {
 Assert((light > 0))
surrender_now_or_prepare_to_fight(false,true)
 }
;
void surrender_now_or_prepare_to_fight (bool surrender,bool fight) {
 Assert((surrender || fight))
 }
;
void meowth_thats_right () {
 Assert(true)
 }
;
int main () {
 int light = prepare_for_trouble(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26);
blast_off_at_the_speed_of(light)
meowth_thats_right()
return light;
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


prepare_for_trouble(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,): (int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, ) -> int


make_it_double(x,): (int, ) -> int


blast_off_at_the_speed_of(light,): (int, ) -> void


surrender_now_or_prepare_to_fight(surrender,fight,): (bool, bool, ) -> void


meowth_thats_right(): () -> void


prepare_for_trouble(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,): (int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, ) -> int =
Seq(Declarig(int light=(make_it_double ((((((((((((((((((((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j) + k) + l) + m) + n) + o) + p) + q) + r) + s) + t) + u) + v) + w) + x) + y) + z), )); [Seq(blast_off_at_the_speed_of(light, );
;Seq(return light;
;nop;))])
;nop;)


make_it_double(x,): (int, ) -> int =
Seq(Seq(return (2 * x);
;nop;)
;nop;)


blast_off_at_the_speed_of(light,): (int, ) -> void =
Seq(Seq(if ((light > 0)) {
nop;
}
else {
__assert_fail;
}
;Seq(surrender_now_or_prepare_to_fight(false, true, );
;nop;))
;nop;)


surrender_now_or_prepare_to_fight(surrender,fight,): (bool, bool, ) -> void =
Seq(Seq(if ((surrender ? true : fight)) {
nop;
}
else {
__assert_fail;
}
;nop;)
;nop;)


meowth_thats_right(): () -> void =
Seq(Seq(if (true) {
nop;
}
else {
__assert_fail;
}
;nop;)
;nop;)


main(): () -> int =
Seq(Declarig(int light=(prepare_for_trouble (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, )); [Seq(blast_off_at_the_speed_of(light, );
;Seq(meowth_thats_right();
;Seq(return light;
;nop;)))])
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


prepare_for_trouble(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,): (int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, ) -> int


make_it_double(x,): (int, ) -> int


blast_off_at_the_speed_of(light,): (int, ) -> void


surrender_now_or_prepare_to_fight(surrender,fight,): (bool, bool, ) -> void


meowth_thats_right(): () -> void


prepare_for_trouble(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,): (int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, ) -> int =
Seq(Declarig(int light=(make_it_double ((((((((((((((((((((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j) + k) + l) + m) + n) + o) + p) + q) + r) + s) + t) + u) + v) + w) + x) + y) + z), )); [Seq(blast_off_at_the_speed_of(light, );
;Seq(return light;
;nop;))])
;nop;)


make_it_double(x,): (int, ) -> int =
Seq(Seq(return (2 * x);
;nop;)
;nop;)


blast_off_at_the_speed_of(light,): (int, ) -> void =
Seq(Seq(if ((light > 0)) {
nop;
}
else {
__assert_fail;
}
;Seq(surrender_now_or_prepare_to_fight(false, true, );
;nop;))
;nop;)


surrender_now_or_prepare_to_fight(surrender,fight,): (bool, bool, ) -> void =
Seq(Seq(if ((surrender ? true : fight)) {
nop;
}
else {
__assert_fail;
}
;nop;)
;nop;)


meowth_thats_right(): () -> void =
Seq(Seq(if (true) {
nop;
}
else {
__assert_fail;
}
;nop;)
;nop;)


main(): () -> int =
Seq(Declarig(int light=(prepare_for_trouble (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, )); [Seq(blast_off_at_the_speed_of(light, );
;Seq(meowth_thats_right();
;Seq(return light;
;nop;)))])
;nop;)





------------------------------------------



_c0_prepare_for_trouble(a:4,b:4,c:4,d:4,e:4,f:4,g:4,h:4,i:4,j:4,k:4,l:4,m:4,n:4,o:4,p:4,q:4,r:4,s:4,t:4,u:4,v:4,w:4,x:4,y:4,z:4,) =
decl light=(_c0_make_it_double ((lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=(lhs=a + rhs=b) + rhs=c) + rhs=d) + rhs=e) + rhs=f) + rhs=g) + rhs=h) + rhs=i) + rhs=j) + rhs=k) + rhs=l) + rhs=m) + rhs=n) + rhs=o) + rhs=p) + rhs=q) + rhs=r) + rhs=s) + rhs=t) + rhs=u) + rhs=v) + rhs=w) + rhs=x) + rhs=y) + rhs=z) of size [4]));
_c0_blast_off_at_the_speed_of(light[4]);
return light of size [4]
nop;
nop;

_c0_make_it_double(x:4,) =
return (lhs=2 * rhs=x) of size [4]
nop;
nop;

_c0_blast_off_at_the_speed_of(light:4,) =
if ((light >(4) 0)) {
nop;
}
else {
__assert_fail;
}
_c0_surrender_now_or_prepare_to_fight(false[4], true[4]);
nop;
nop;

_c0_surrender_now_or_prepare_to_fight(surrender:4,fight:4,) =
if ((surrender ? true : fight)) {
nop;
}
else {
__assert_fail;
}
nop;
nop;

_c0_meowth_thats_right() =
if (true) {
nop;
}
else {
__assert_fail;
}
nop;
nop;

_c0_main() =
decl light=(_c0_prepare_for_trouble (1 of size [4], 2 of size [4], 3 of size [4], 4 of size [4], 5 of size [4], 6 of size [4], 7 of size [4], 8 of size [4], 9 of size [4], 10 of size [4], 11 of size [4], 12 of size [4], 13 of size [4], 14 of size [4], 15 of size [4], 16 of size [4], 17 of size [4], 18 of size [4], 19 of size [4], 20 of size [4], 21 of size [4], 22 of size [4], 23 of size [4], 24 of size [4], 25 of size [4], 26 of size [4]));
_c0_blast_off_at_the_speed_of(light[4]);
_c0_meowth_thats_right();
return light of size [4]
nop;
nop;



func _c0_prepare_for_trouble(%t1[4], %t2[4], %t3[4], %t4[4], %t5[4], %t6[4], %t7[4], %t8[4], %t9[4], %t10[4], %t11[4], %t12[4], %t13[4], %t14[4], %t15[4], %t16[4], %t17[4], %t18[4], %t19[4], %t20[4], %t21[4], %t22[4], %t23[4], %t24[4], %t25[4], %t26[4]):
._c0_prepare_for_trouble(%t1, %t2, %t3, %t4, %t5, %t6, %t7, %t8, %t9, %t10, %t11, %t12, %t13, %t14, %t15, %t16, %t17, %t18, %t19, %t20, %t21, %t22, %t23, %t24, %t25, %t26)
%t28  <--  _c0_make_it_double(%t1 + %t2 + %t3 + %t4 + %t5 + %t6 + %t7 + %t8 + %t9 + %t10 + %t11 + %t12 + %t13 + %t14 + %t15 + %t16 + %t17 + %t18 + %t19 + %t20 + %t21 + %t22 + %t23 + %t24 + %t25 + %t26, )
%t27  <--  %t28
void <-- _c0_blast_off_at_the_speed_of(%t27, )
return %t27
-------[ret]---------
.L2
return
-------[ret]---------

func _c0_make_it_double(%t29[4]):
._c0_make_it_double(%t29)
return 2 * %t29
-------[ret]---------
.L3
return
-------[ret]---------

func _c0_blast_off_at_the_speed_of(%t30[4]):
._c0_blast_off_at_the_speed_of(%t30)
if (%t30 >(small) 0).L4 else .L5
-------[t:.L4|f:.L5]---------
.L4
goto .L6
-------[goto .L6]---------
.L5
assertfail
goto .L6
-------[goto .L6]---------
.L6
void <-- _c0_surrender_now_or_prepare_to_fight(0, 1, )
return
-------[ret]---------

func _c0_surrender_now_or_prepare_to_fight(%t31[4], %t32[4]):
._c0_surrender_now_or_prepare_to_fight(%t31, %t32)
%t34  <--  %t31
if (%t34 !=(small) 0).L10 else .L11
-------[t:.L10|f:.L11]---------
.L10
%t33  <--  1
goto .L12
-------[goto .L12]---------
.L11
%t33  <--  %t32
goto .L12
-------[goto .L12]---------
.L12
if (%t33 !=(small) 0).L7 else .L8
-------[t:.L7|f:.L8]---------
.L7
goto .L9
-------[goto .L9]---------
.L8
assertfail
goto .L9
-------[goto .L9]---------
.L9
return
-------[ret]---------

func _c0_meowth_thats_right():
._c0_meowth_thats_right()
return
-------[ret]---------

func _c0_main():
._c0_main()
%t36  <--  _c0_prepare_for_trouble(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, )
%t35  <--  %t36
void <-- _c0_blast_off_at_the_speed_of(%t35, )
void <-- _c0_meowth_thats_right()
return %t35
-------[ret]---------
.L13
return
-------[ret]---------
_c0_prepare_for_trouble(%t1s%t2s%t3s%t4s%t5s%t6s%t7s%t8s%t9s%t10s%t11s%t12s%t13s%t14s%t15s%t16s%t17s%t18s%t19s%t20s%t21s%t22s%t23s%t24s%t25s%t26s): 

._c0_prepare_for_trouble(%t1, %t2, %t3, %t4, %t5, %t6, %t7, %t8, %t9, %t10, %t11, %t12, %t13, %t14, %t15, %t16, %t17, %t18, %t19, %t20, %t21, %t22, %t23, %t24, %t25, %t26):
%t86 <-s- %t1
%t87 <-s- %t2
%t84 <-s- %t86 + %t87
%t85 <-s- %t3
%t82 <-s- %t84 + %t85
%t83 <-s- %t4
%t80 <-s- %t82 + %t83
%t81 <-s- %t5
%t78 <-s- %t80 + %t81
%t79 <-s- %t6
%t76 <-s- %t78 + %t79
%t77 <-s- %t7
%t74 <-s- %t76 + %t77
%t75 <-s- %t8
%t72 <-s- %t74 + %t75
%t73 <-s- %t9
%t70 <-s- %t72 + %t73
%t71 <-s- %t10
%t68 <-s- %t70 + %t71
%t69 <-s- %t11
%t66 <-s- %t68 + %t69
%t67 <-s- %t12
%t64 <-s- %t66 + %t67
%t65 <-s- %t13
%t62 <-s- %t64 + %t65
%t63 <-s- %t14
%t60 <-s- %t62 + %t63
%t61 <-s- %t15
%t58 <-s- %t60 + %t61
%t59 <-s- %t16
%t56 <-s- %t58 + %t59
%t57 <-s- %t17
%t54 <-s- %t56 + %t57
%t55 <-s- %t18
%t52 <-s- %t54 + %t55
%t53 <-s- %t19
%t50 <-s- %t52 + %t53
%t51 <-s- %t20
%t48 <-s- %t50 + %t51
%t49 <-s- %t21
%t46 <-s- %t48 + %t49
%t47 <-s- %t22
%t44 <-s- %t46 + %t47
%t45 <-s- %t23
%t42 <-s- %t44 + %t45
%t43 <-s- %t24
%t40 <-s- %t42 + %t43
%t41 <-s- %t25
%t38 <-s- %t40 + %t41
%t39 <-s- %t26
%t37 <-s- %t38 + %t39
EDI <-s- %t37
call _c0_make_it_double(EDIs|)
%t28 <-s- EAX
%t27 <-s- %t28
%t88 <-s- %t27
EDI <-s- %t88
call _c0_blast_off_at_the_speed_of(EDIs|)
EAX <-s- %t27
ret

ret

.L2:
ret

ret
_c0_make_it_double(%t29s): 

._c0_make_it_double(%t29):
%t89 <-s- $2
%t90 <-s- %t29
EAX <-s- %t89 * %t90
ret

ret

.L3:
ret

ret
_c0_blast_off_at_the_speed_of(%t30s): 

._c0_blast_off_at_the_speed_of(%t30):
%t91 <-s- %t30
%t92 <-s- $0
cmps %t91, %t92
Jle .L5
jump.L4

if(.L4|.L5)

.L4:
jump.L6

.L6

.L5:
call __assert_fail
jump.L6

.L6

.L6:
%t93 <-s- $0
%t94 <-s- $1
EDI <-s- %t93
ESI <-s- %t94
call _c0_surrender_now_or_prepare_to_fight(EDIs, ESIs|)
ret

ret
_c0_surrender_now_or_prepare_to_fight(%t31s%t32s): 

._c0_surrender_now_or_prepare_to_fight(%t31, %t32):
%t34 <-s- %t31
%t95 <-s- %t34
%t96 <-s- $0
cmps %t95, %t96
Je .L11
jump.L10

if(.L10|.L11)

.L10:
%t33 <-s- $1
jump.L12

.L12

.L11:
%t33 <-s- %t32
jump.L12

.L12

.L12:
%t97 <-s- %t33
%t98 <-s- $0
cmps %t97, %t98
Je .L8
jump.L7

if(.L7|.L8)

.L7:
jump.L9

.L9

.L8:
call __assert_fail
jump.L9

.L9

.L9:
ret

ret
_c0_meowth_thats_right(): 

._c0_meowth_thats_right():
ret

ret
_c0_main(): 

._c0_main():
%t99 <-s- $1
%t100 <-s- $2
%t101 <-s- $3
%t102 <-s- $4
%t103 <-s- $5
%t104 <-s- $6
%t105 <-s- $7
%t106 <-s- $8
%t107 <-s- $9
%t108 <-s- $10
%t109 <-s- $11
%t110 <-s- $12
%t111 <-s- $13
%t112 <-s- $14
%t113 <-s- $15
%t114 <-s- $16
%t115 <-s- $17
%t116 <-s- $18
%t117 <-s- $19
%t118 <-s- $20
%t119 <-s- $21
%t120 <-s- $22
%t121 <-s- $23
%t122 <-s- $24
%t123 <-s- $25
%t124 <-s- $26
EDI <-s- %t99
ESI <-s- %t100
EDX <-s- %t101
ECX <-s- %t102
R8D <-s- %t103
R9D <-s- %t104
call _c0_prepare_for_trouble(EDIs, ESIs, EDXs, ECXs, R8Ds, R9Ds|%t105s, %t106s, %t107s, %t108s, %t109s, %t110s, %t111s, %t112s, %t113s, %t114s, %t115s, %t116s, %t117s, %t118s, %t119s, %t120s, %t121s, %t122s, %t123s, %t124s)
%t36 <-s- EAX
%t35 <-s- %t36
%t125 <-s- %t35
EDI <-s- %t125
call _c0_blast_off_at_the_speed_of(EDIs|)
call _c0_meowth_thats_right(|)
EAX <-s- %t35
ret

ret

.L13:
ret

ret

---Blocks---

_c0_prepare_for_trouble(%t1,%t2,%t3,%t4,%t5,%t6,%t7,%t8,%t9,%t10,%t11,%t12,%t13,%t14,%t15,%t16,%t17,%t18,%t19,%t20,%t21,%t22,%t23,%t24,%t25,%t26){
[
FunLbl _c0_prepare_for_trouble(%t1,%t2,%t3,%t4,%t5,%t6,%t7,%t8,%t9,%t10,%t11,%t12,%t13,%t14,%t15,%t16,%t17,%t18,%t19,%t20,%t21,%t22,%t23,%t24,%t25,%t26):
|
  0:%t86 <-s- %t1
  1:%t87 <-s- %t2
  2:%t84 <-s- %t86 + %t87
  3:%t85 <-s- %t3
  4:%t82 <-s- %t84 + %t85
  5:%t83 <-s- %t4
  6:%t80 <-s- %t82 + %t83
  7:%t81 <-s- %t5
  8:%t78 <-s- %t80 + %t81
  9:%t79 <-s- %t6
  10:%t76 <-s- %t78 + %t79
  11:%t77 <-s- %t7
  12:%t74 <-s- %t76 + %t77
  13:%t75 <-s- %t8
  14:%t72 <-s- %t74 + %t75
  15:%t73 <-s- %t9
  16:%t70 <-s- %t72 + %t73
  17:%t71 <-s- %t10
  18:%t68 <-s- %t70 + %t71
  19:%t69 <-s- %t11
  20:%t66 <-s- %t68 + %t69
  21:%t67 <-s- %t12
  22:%t64 <-s- %t66 + %t67
  23:%t65 <-s- %t13
  24:%t62 <-s- %t64 + %t65
  25:%t63 <-s- %t14
  26:%t60 <-s- %t62 + %t63
  27:%t61 <-s- %t15
  28:%t58 <-s- %t60 + %t61
  29:%t59 <-s- %t16
  30:%t56 <-s- %t58 + %t59
  31:%t57 <-s- %t17
  32:%t54 <-s- %t56 + %t57
  33:%t55 <-s- %t18
  34:%t52 <-s- %t54 + %t55
  35:%t53 <-s- %t19
  36:%t50 <-s- %t52 + %t53
  37:%t51 <-s- %t20
  38:%t48 <-s- %t50 + %t51
  39:%t49 <-s- %t21
  40:%t46 <-s- %t48 + %t49
  41:%t47 <-s- %t22
  42:%t44 <-s- %t46 + %t47
  43:%t45 <-s- %t23
  44:%t42 <-s- %t44 + %t45
  45:%t43 <-s- %t24
  46:%t40 <-s- %t42 + %t43
  47:%t41 <-s- %t25
  48:%t38 <-s- %t40 + %t41
  49:%t39 <-s- %t26
  50:%t37 <-s- %t38 + %t39
  51:EDI <-s- %t37
  52:call _c0_make_it_double(EDIs|)
  53:%t28 <-s- EAX
  54:%t27 <-s- %t28
  55:%t88 <-s- %t27
  56:EDI <-s- %t88
  57:call _c0_blast_off_at_the_speed_of(EDIs|)
  58:EAX <-s- %t27
  59:ret
|
ret
]

[
BlockLbl(.L2):
|
  60:ret
|
ret
]

}

;


_c0_make_it_double(%t29){
[
FunLbl _c0_make_it_double(%t29):
|
  61:%t89 <-s- $2
  62:%t90 <-s- %t29
  63:EAX <-s- %t89 * %t90
  64:ret
|
ret
]

[
BlockLbl(.L3):
|
  65:ret
|
ret
]

}

;


_c0_blast_off_at_the_speed_of(%t30){
[
FunLbl _c0_blast_off_at_the_speed_of(%t30):
|
  66:%t91 <-s- %t30
  67:%t92 <-s- $0
  68:cmps %t91, %t92
  69:Jle .L5
  70:jump.L4
|
jump -> .L4 or .L5
]

[
BlockLbl(.L4):
|
  71:jump.L6
|
jump -> .L6
]

[
BlockLbl(.L5):
|
  72:call __assert_fail
  73:jump.L6
|
jump -> .L6
]

[
BlockLbl(.L6):
|
  74:%t93 <-s- $0
  75:%t94 <-s- $1
  76:EDI <-s- %t93
  77:ESI <-s- %t94
  78:call _c0_surrender_now_or_prepare_to_fight(EDIs, ESIs|)
  79:ret
|
ret
]

}

;


_c0_surrender_now_or_prepare_to_fight(%t31,%t32){
[
FunLbl _c0_surrender_now_or_prepare_to_fight(%t31,%t32):
|
  80:%t34 <-s- %t31
  81:%t95 <-s- %t34
  82:%t96 <-s- $0
  83:cmps %t95, %t96
  84:Je .L11
  85:jump.L10
|
jump -> .L10 or .L11
]

[
BlockLbl(.L10):
|
  86:%t33 <-s- $1
  87:jump.L12
|
jump -> .L12
]

[
BlockLbl(.L11):
|
  88:%t33 <-s- %t32
  89:jump.L12
|
jump -> .L12
]

[
BlockLbl(.L12):
|
  90:%t97 <-s- %t33
  91:%t98 <-s- $0
  92:cmps %t97, %t98
  93:Je .L8
  94:jump.L7
|
jump -> .L7 or .L8
]

[
BlockLbl(.L7):
|
  95:jump.L9
|
jump -> .L9
]

[
BlockLbl(.L8):
|
  96:call __assert_fail
  97:jump.L9
|
jump -> .L9
]

[
BlockLbl(.L9):
|
  98:ret
|
ret
]

}

;


_c0_meowth_thats_right(){
[
FunLbl _c0_meowth_thats_right():
|
  99:ret
|
ret
]

}

;


_c0_main(){
[
FunLbl _c0_main():
|
  100:%t99 <-s- $1
  101:%t100 <-s- $2
  102:%t101 <-s- $3
  103:%t102 <-s- $4
  104:%t103 <-s- $5
  105:%t104 <-s- $6
  106:%t105 <-s- $7
  107:%t106 <-s- $8
  108:%t107 <-s- $9
  109:%t108 <-s- $10
  110:%t109 <-s- $11
  111:%t110 <-s- $12
  112:%t111 <-s- $13
  113:%t112 <-s- $14
  114:%t113 <-s- $15
  115:%t114 <-s- $16
  116:%t115 <-s- $17
  117:%t116 <-s- $18
  118:%t117 <-s- $19
  119:%t118 <-s- $20
  120:%t119 <-s- $21
  121:%t120 <-s- $22
  122:%t121 <-s- $23
  123:%t122 <-s- $24
  124:%t123 <-s- $25
  125:%t124 <-s- $26
  126:EDI <-s- %t99
  127:ESI <-s- %t100
  128:EDX <-s- %t101
  129:ECX <-s- %t102
  130:R8D <-s- %t103
  131:R9D <-s- %t104
  132:call _c0_prepare_for_trouble(EDIs, ESIs, EDXs, ECXs, R8Ds, R9Ds|%t105s, %t106s, %t107s, %t108s, %t109s, %t110s, %t111s, %t112s, %t113s, %t114s, %t115s, %t116s, %t117s, %t118s, %t119s, %t120s, %t121s, %t122s, %t123s, %t124s)
  133:%t36 <-s- EAX
  134:%t35 <-s- %t36
  135:%t125 <-s- %t35
  136:EDI <-s- %t125
  137:call _c0_blast_off_at_the_speed_of(EDIs|)
  138:call _c0_meowth_thats_right(|)
  139:EAX <-s- %t35
  140:ret
|
ret
]

[
BlockLbl(.L13):
|
  141:ret
|
ret
]

}


