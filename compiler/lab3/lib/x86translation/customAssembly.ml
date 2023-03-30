open Core
module AS = Assem_l4
module R = Register

let alloc_fname = "____calloc_javaway"

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
let get_alloc_function memErrLabel =
  let l1 = Label.create () in
  [ X86.Directive (sprintf ".globl %s" alloc_fname)
  ; X86.Directive (sprintf ".type\t%s, @function" alloc_fname)
  ; X86.FunName alloc_fname
  ; X86.Cmp
      { lhs = X86.Reg { reg = R.EDI; size = 4 }; rhs = Imm Int32.zero; size = X86.L }
  ; X86.Jump { label = l1; op = Some AS.Jne }
  ; X86.BinCommand
      { op = X86.Mov
      ; dest = X86.Reg { reg = R.EDI; size = 4 }
      ; src = Imm Int32.one
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
      ; src = Imm Int32.one
      ; size = X86.L
      }
  ; X86.Call "calloc"
  ; X86.Cmp
      { lhs = X86.Reg { reg = R.EAX; size = 8 }; rhs = Imm Int32.zero; size = X86.Q }
  ; X86.Jump { label = memErrLabel; op = Some AS.Je }
  ; X86.Ret
  ]
;;

let alloc_array_fname = "____allocarray_javaway"
(* c0_alloc_array:
        movsxl	%esi, %r10
        movq	%r10, %r11
        pushq	%rbx
        pushq	%r15
        movl	%esi, %r15d
        movq	%rdi, %rbx
        testl	%r10d, %r10d
        js	.L8
        testq	%rdi, %rdi
        je	.L7
        movl	$1073741816, %eax
        xorl	%edx, %edx
        div	%rdi
        cmpq	%r10, %rax
        jb	.L6
    .L7:
        imulq	%r10, %rbx
        movl	$1, %edi
        leaq	8(%rbx), %rsi
        call	calloc
        movl	%r15d, (%rax)
        addq	$8, %rax
        popq	%r15
        popq	%rbx
        ret
.L1:
        comment "not negative"
        jumpto memory fail
.L3:
        comment "too large"
        jumpto memory fail*)

let get_arrayalloc_function memErrLabel =
  let l1, l2, l3 = Label.create (), Label.create (), Label.create () in
  [ X86.Directive (sprintf ".globl %s" alloc_array_fname)
  ; X86.Directive (sprintf ".type\t%s, @function" alloc_array_fname)
  ; X86.FunName alloc_array_fname
  ; X86.BinCommand
      { op = X86.Movsx
      ; dest = X86.Reg { reg = R.R10D; size = 8 }
      ; src = X86.Reg { reg = R.ESI; size = 4 }
      ; size = X86.L
      }
  ; X86.BinCommand
      { op = X86.Mov
      ; dest = X86.Reg { reg = R.R11D; size = 8 }
      ; src = X86.Reg { reg = R.R10D; size = 8 }
      ; size = X86.Q
      }
  ; X86.UnCommand { op = X86.Pushq; src = X86.Reg { reg = R.RBX; size = 8 } }
  ; X86.UnCommand { op = X86.Pushq; src = X86.Reg { reg = R.R15D; size = 8 } }
  ; X86.BinCommand
      { op = X86.Mov
      ; dest = X86.Reg { reg = R.R15D; size = 4 }
      ; src = X86.Reg { reg = R.ESI; size = 4 }
      ; size = X86.L
      }
  ; X86.BinCommand
      { op = X86.Mov
      ; dest = X86.Reg { reg = R.RBX; size = 8 }
      ; src = X86.Reg { reg = R.RDI; size = 8 }
      ; size = X86.Q
      }
  ; X86.Test
      { rhs = X86.Reg { reg = R.R10D; size = 4 }
      ; lhs = X86.Reg { reg = R.R10D; size = 4 }
      ; size = X86.L
      }
  ; X86.Jump { label = l1; op = Some AS.Js }
  ; X86.Test
      { rhs = X86.Reg { reg = R.EDI; size = 8 }
      ; lhs = X86.Reg { reg = R.EDI; size = 8 }
      ; size = X86.Q
      }
  ; X86.Jump { label = l2; op = Some AS.Je }
  ; X86.BinCommand
      { op = X86.Mov
      ; dest = X86.Reg { reg = R.EAX; size = 4 }
      ; src = Imm (Int32.of_int_exn 1073741816)
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
      { rhs = X86.Reg { reg = R.EAX; size = 8 }
      ; lhs = X86.Reg { reg = R.R10D; size = 8 }
      ; size = X86.Q
      }
  ; X86.Jump { label = l3; op = Some AS.Jb }
  ; X86.Lbl l2 (* .L2 *)
  ; X86.BinCommand
      { op = X86.IMul
      ; dest = X86.Reg { reg = R.EBX; size = 8 }
      ; src = X86.Reg { reg = R.R10D; size = 8 }
      ; size = X86.Q
      }
  ; X86.BinCommand
      { op = X86.Mov
      ; dest = X86.Reg { reg = R.EDI; size = 4 }
      ; src = Imm Int32.one
      ; size = X86.L
      }
  ; X86.Lea
      { dest = X86.Reg { reg = R.ESI; size = 8 }
      ; src =
          X86.Mem
            { disp = Some 8
            ; base_reg = { reg = R.EBX; size = 8 }
            ; idx_reg = None
            ; scale = None
            }
      ; size = X86.Q
      }
  ; X86.Call "calloc"
  ; X86.MovTo
      { dest = X86.Reg { reg = R.EAX; size = 8 }
      ; src = X86.Reg { reg = R.R15D; size = 4 }
      ; size = X86.L
      }
  ; X86.BinCommand
      { op = X86.Add
      ; dest = X86.Reg { reg = R.EAX; size = 8 }
      ; src = Imm (Int32.of_int_exn 8)
      ; size = X86.Q
      }
  ; X86.UnCommand { op = X86.Popq; src = X86.Reg { reg = R.R15D; size = 8 } }
  ; X86.UnCommand { op = X86.Popq; src = X86.Reg { reg = R.RBX; size = 8 } }
  ; X86.Ret
  ; X86.Lbl l1 (* .L1 *)
  ; X86.Comment "alloc size must be non-negative"
  ; X86.Jump { label = memErrLabel; op = None }
  ; X86.Lbl l3 (* .L3 *)
  ; X86.Comment "too large"
  ; X86.Jump { label = memErrLabel; op = None }
  ]
;;
