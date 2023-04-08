open Core
module AS = Assem_l4
module R = Register

let alloc_fname ~(unsafe : bool) =
  if unsafe then "_unsafe_calloc" else "____calloc_javaway"
;;

(* alloc_fname:
        cmp     %edi, 0
        jne     .L1
        mov     %edi <- 1
.L1:
        mov     %eax <- %edi
        cdqe
        movq    %rsi <- %rax
        mov     %edi <- 1
        call    calloc
        cmp     %rax, 0
        je     memoryfail
        ret *)
let get_alloc_function memErrLabel ~(unsafe : bool) =
  let name = alloc_fname ~unsafe in
  if not unsafe
  then (
    let l1 = Label.create () in
    [ X86.Directive (sprintf ".globl %s" name)
    ; X86.Directive (sprintf ".type\t%s, @function" name)
    ; X86.FunName name
    ; X86.UnCommand { op = X86.Pushq; src = X86.Reg { reg = R.EBX; size = 8 } }
    ; X86.Cmp
        { lhs = X86.Reg { reg = R.EDI; size = 4 }; rhs = Imm Int64.zero; size = X86.L }
    ; X86.Jump { label = l1; op = Some AS.Jne }
    ; X86.BinCommand
        { op = X86.Mov
        ; dest = X86.Reg { reg = R.EDI; size = 4 }
        ; src = Imm Int64.one
        ; size = X86.L
        }
    ; X86.Lbl l1
    ; X86.BinCommand
        { op = X86.Mov
        ; dest = X86.Reg { reg = R.EAX; size = 4 }
        ; src = X86.Reg { reg = R.EDI; size = 4 }
        ; size = X86.L
        }
    ; X86.ZeroCommand { op = X86.Cqde }
    ; X86.BinCommand
        { op = X86.Mov
        ; src = X86.Reg { reg = R.EAX; size = 8 }
        ; dest = X86.Reg { reg = R.ESI; size = 8 }
        ; size = X86.Q
        }
    ; X86.BinCommand
        { op = X86.Mov
        ; dest = X86.Reg { reg = R.EDI; size = 4 }
        ; src = Imm Int64.one
        ; size = X86.L
        }
    ; X86.Call "calloc@plt"
    ; X86.Cmp
        { lhs = X86.Reg { reg = R.EAX; size = 8 }; rhs = Imm Int64.zero; size = X86.Q }
    ; X86.Jump { label = memErrLabel; op = Some AS.Je }
    ; X86.UnCommand { op = X86.Popq; src = X86.Reg { reg = R.EBX; size = 8 } }
    ; X86.Ret
    ])
  else
    [ X86.Directive (sprintf ".globl %s" name)
    ; X86.Directive (sprintf ".type\t%s, @function" name)
    ; X86.FunName name
    ; X86.BinCommand
        { op = X86.Movsl
        ; src = X86.Reg { reg = R.EDI; size = 4 }
        ; dest = X86.Reg { reg = R.ESI; size = 8 }
        ; size = X86.Q
        }
    ; X86.BinCommand
        { op = X86.Mov
        ; dest = X86.Reg { reg = R.EDI; size = 4 }
        ; src = Imm Int64.one
        ; size = X86.L
        }
    ; X86.JumpToF "calloc"
    ]
;;

let alloc_array_fname ~(unsafe : bool) =
  if unsafe then "_unsafe_allocarray_javaway" else "____allocarray_javaway"
;;

(* c0_alloc_array:
____allocarray_javaway:
        - pushq   %rbx
        - testl   %esi, %esi
        - js      .L8
        - movl    %esi, %ebx
        - movslq  %esi, %rcx
        testq   %rdi, %rdi
        je      .L3333
        movl    $1073741816, %eax
        xorl    %edx, %edx
        divq    %rdi
        cmpq    %rcx, %rax
        jb      .L6
    .L3333:
        imulq   %rcx, %rdi
        leaq    8(%rdi), %rsi
        movl    $1, %edi
        call    calloc
        movl    %ebx, (%rax)
        addq    $8, %rax
        popq    %rbx
        ret
.L1:
        comment "not negative"
        jumpto memory fail
.L3:
        comment "too large"
        jumpto memory fail*)

let get_arrayalloc_function memErrLabel ~(unsafe : bool) =
  let l1, l2, l3 = Label.create (), Label.create (), Label.create () in
  let name = alloc_array_fname ~unsafe in
  if not unsafe
  then
    [ X86.Directive (sprintf ".globl %s" name)
    ; X86.Directive (sprintf ".type\t%s, @function" name)
    ; X86.FunName name
    ; X86.UnCommand { op = X86.Pushq; src = X86.Reg { reg = R.EBX; size = 8 } }
    ; X86.Test
        { rhs = X86.Reg { reg = R.ESI; size = 4 }
        ; lhs = X86.Reg { reg = R.ESI; size = 4 }
        ; size = X86.L
        }
    ; X86.Jump { label = l1; op = Some AS.Js }
    ; X86.BinCommand
        { op = X86.Mov
        ; src = X86.Reg { reg = R.ESI; size = 4 }
        ; dest = X86.Reg { reg = R.EBX; size = 4 }
        ; size = X86.L
        }
    ; X86.BinCommand
        { op = X86.Movsl
        ; src = X86.Reg { reg = R.ESI; size = 4 }
        ; dest = X86.Reg { reg = R.ECX; size = 8 }
        ; size = X86.Q
        }
    ; X86.Test
        { rhs = X86.Reg { reg = R.EDI; size = 8 }
        ; lhs = X86.Reg { reg = R.EDI; size = 8 }
        ; size = X86.Q
        }
    ; X86.Jump { label = l2; op = Some AS.Je }
    ; X86.BinCommand
        { op = X86.Mov
        ; dest = X86.Reg { reg = R.EAX; size = 4 }
        ; src = Imm (Int64.of_int_exn 1073741816)
        ; size = X86.L
        }
    ; X86.BinCommand
        { op = X86.Xor
        ; dest = X86.Reg { reg = R.EDX; size = 4 }
        ; src = X86.Reg { reg = R.EDX; size = 4 }
        ; size = X86.L
        }
    ; X86.UnCommand { op = X86.Div; src = X86.Reg { reg = R.EDI; size = 8 } }
    ; X86.Cmp
        { rhs = X86.Reg { reg = R.ECX; size = 8 }
        ; lhs = X86.Reg { reg = R.EAX; size = 8 }
        ; size = X86.Q
        }
    ; X86.Jump { label = l3; op = Some AS.Jb }
    ; X86.Lbl l2 (* .L2 *)
    ; X86.BinCommand
        { op = X86.IMul
        ; dest = X86.Reg { reg = R.EDI; size = 8 }
        ; src = X86.Reg { reg = R.ECX; size = 8 }
        ; size = X86.Q
        }
    ; X86.Lea
        { dest = X86.Reg { reg = R.ESI; size = 8 }
        ; src =
            X86.Mem
              { disp = Some 8
              ; base_reg = { reg = R.EDI; size = 8 }
              ; idx_reg = None
              ; scale = None
              }
        ; size = X86.Q
        }
    ; X86.BinCommand
        { op = X86.Mov
        ; dest = X86.Reg { reg = R.EDI; size = 4 }
        ; src = Imm Int64.one
        ; size = X86.L
        }
    ; X86.Call "calloc@plt"
    ; X86.MovTo
        { dest = X86.Reg { reg = R.EAX; size = 8 }
        ; src = X86.Reg { reg = R.EBX; size = 4 }
        ; size = X86.L
        }
    ; X86.BinCommand
        { op = X86.Add
        ; dest = X86.Reg { reg = R.EAX; size = 8 }
        ; src = Imm (Int64.of_int_exn 8)
        ; size = X86.Q
        }
    ; X86.UnCommand { op = X86.Popq; src = X86.Reg { reg = R.EBX; size = 8 } }
    ; X86.Ret
    ; X86.Lbl l1 (* .L1 *)
    ; X86.Comment "alloc size must be non-negative"
    ; X86.Jump { label = memErrLabel; op = None }
    ; X86.Lbl l3 (* .L3 *)
    ; X86.Comment "too large"
    ; X86.Jump { label = memErrLabel; op = None }
    ]
  else
    [ X86.Directive (sprintf ".globl %s" name)
    ; X86.Directive (sprintf ".type\t%s, @function" name)
    ; X86.FunName name
    ; X86.BinCommand
        { op = X86.IMul
        ; dest = X86.Reg { reg = R.ESI; size = 4 }
        ; src = X86.Reg { reg = R.EDI; size = 4 }
        ; size = X86.L
        }
    ; X86.BinCommand
        { op = X86.Mov
        ; dest = X86.Reg { reg = R.EDI; size = 4 }
        ; src = Imm Int64.one
        ; size = X86.L
        }
    ; X86.BinCommand
        { op = X86.Movsl
        ; src = X86.Reg { reg = R.ESI; size = 4 }
        ; dest = X86.Reg { reg = R.ESI; size = 8 }
        ; size = X86.Q
        }
    ; X86.JumpToF "calloc"
    ]
;;
