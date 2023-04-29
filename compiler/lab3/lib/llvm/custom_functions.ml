open Core
module AS = Assem_l4

let mac = false

let get_efkt_name = function
  | AS.Div -> "____JAVAWAY_div"
  | AS.Mod -> "____JAVAWAY_rem"
  | AS.ShiftL -> "____JAVAWAY_shl"
  | AS.ShiftR -> "____JAVAWAY_shr"
;;

let get_efkt_name_ops = function
  | "check_null" -> "_____JAVAWAY_failifnull"
  | "check_array" -> "_____JAVAWAY_failifoutbounds"
  | "alloc_array" -> CustomAssembly.alloc_array_fname ~unsafe:false
  | "alloc" -> CustomAssembly.alloc_fname ~unsafe:false
  | s -> failwith "get_efkt_name_ops got sth strange:" ^ s
;;

let format_mod () =
  "\n; Safe division function\ndefine i32 @"
  ^ get_efkt_name AS.Mod
  ^ "(i32 %a, i32 %b) {\n\
     entry:\n\
    \  ; Check if b is 0\n\
    \  %is_zero = icmp eq i32 %b, 0\n\n\
    \  ; Check if a is INT_MIN and b is -1\n\
    \  %is_int_min = icmp eq i32 %a, -2147483648\n\
    \  %is_minus_one = icmp eq i32 %b, -1\n\
    \  %is_int_min_div_minus_one = and i1 %is_int_min, %is_minus_one\n\n\
    \  ; Combine the two checks\n\
    \  %invalid_division = or i1 %is_zero, %is_int_min_div_minus_one\n\n\
    \  ; If either check is true, call raise(8)\n\
    \  br i1 %invalid_division, label %call_raise, label %continue\n\n\
     call_raise:\n\
    \  call void @raise(i32 8)\n\
    \  unreachable\n\n\
     continue:\n\
     %result = srem i32 %a, %b\n\
     ret i32 %result\n\
     }"
;;

let format_div () =
  "\n; Safe division function\ndefine i32 @"
  ^ get_efkt_name AS.Div
  ^ "(i32 %a, i32 %b) {\n\
     entry:\n\
    \  ; Check if b is 0\n\
    \  %is_zero = icmp eq i32 %b, 0\n\n\
    \  ; Check if a is INT_MIN and b is -1\n\
    \  %is_int_min = icmp eq i32 %a, -2147483648\n\
    \  %is_minus_one = icmp eq i32 %b, -1\n\
    \  %is_int_min_div_minus_one = and i1 %is_int_min, %is_minus_one\n\n\
    \  ; Combine the two checks\n\
    \  %invalid_division = or i1 %is_zero, %is_int_min_div_minus_one\n\n\
    \  ; If either check is true, call raise(8)\n\
    \  br i1 %invalid_division, label %call_raise, label %continue\n\n\
     call_raise:\n\
    \  call void @raise(i32 8)\n\
    \  unreachable\n\n\
     continue:\n\
    \  %result = sdiv i32 %a, %b\n\
    \  ret i32 %result\n\
     }"
;;

let format_shl () =
  "define i32 @"
  ^ get_efkt_name AS.ShiftL
  ^ "(i32 %value, i32 %shift_amount) {\n\
    \  %is_negative = icmp slt i32 %shift_amount, 0\n\
    \  %is_large = icmp ugt i32 %shift_amount, 31\n\
    \  %invalid = or i1 %is_negative, %is_large\n\
    \  br i1 %invalid, label %error, label %valid\n\n\
     error:\n\
    \  ; Raise SIGFPE\n\
    \  call void @raise(i32 8)\n\
    \  unreachable\n\n\
     valid:\n\
    \  %result = shl i32 %value, %shift_amount\n\
    \  ret i32 %result\n\
     }"
;;

let format_shr () =
  "define i32 @"
  ^ get_efkt_name AS.ShiftR
  ^ "(i32 %value, i32 %shift_amount) {\n\
    \  %is_negative = icmp slt i32 %shift_amount, 0\n\
    \  %is_large = icmp ugt i32 %shift_amount, 31\n\
    \  %invalid = or i1 %is_negative, %is_large\n\
    \  br i1 %invalid, label %error, label %valid\n\n\
     error:\n\
    \  ; Raise SIGFPE\n\
    \  call void @raise(i32 8)\n\
    \  unreachable\n\n\
     valid:\n\
    \  %result = ashr i32 %value, %shift_amount\n\
    \  ret i32 %result\n\
     }"
;;

let format_check_null () =
  "define void @"
  ^ get_efkt_name_ops "check_null"
  ^ "(ptr %ptr) {\n\
     entry:\n\
    \ %is_null = icmp eq ptr %ptr, null\n\
    \ br i1 %is_null, label %raise_error, label %exit\n\n\
     raise_error:\n\
    \ call void @raise(i32 2)\n\
    \ unreachable\n\n\
     exit:\n\
    \ ret void\n\
     }\n"
;;

let format_check_array () =
  "\n\
  \   define void @" ^ get_efkt_name_ops "check_array" ^ "(i32 %index, i32 %length) {\n\
   entry:\n\
  \ %is_ge_zero = icmp sge i32 %index, 0\n\
  \ %is_lt_length = icmp slt i32 %index, %length\n\
  \ %is_valid = and i1 %is_ge_zero, %is_lt_length\n\
  \ br i1 %is_valid, label %exit, label %raise_error\n\n\
   raise_error:\n\
  \ call void @raise(i32 2)\n\
  \ unreachable\n\n\
   exit:\n\
  \ ret void\n\
   }\n"
;;

let format_alloc () =
  "; Function Attrs: nounwind ssp uwtable(sync)\ndefine noalias ptr @"
  ^ get_efkt_name_ops "alloc"
  ^ "(i32 noundef %cstm_0) local_unnamed_addr #0 {\n\
     %cstm_2 = icmp eq i32 %cstm_0, 0\n\
     %cstm_3 = select i1 %cstm_2, i32 1, i32 %cstm_0\n\
     %cstm_4 = sext i32 %cstm_3 to i64\n\
     %cstm_5 = tail call ptr @calloc(i64 noundef 1, i64 noundef %cstm_4) #4\n\
     %cstm_6 = icmp eq ptr %cstm_5, null\n\
     br i1 %cstm_6, label %c0_alloc7, label %c0_alloc9\n\n\
     c0_alloc7:                                                ; preds = %cstm_1\n\
     %cstm_8 = tail call i32 @raise(i32 noundef 12) #5\n\
     br label %c0_alloc9\n\n\
     c0_alloc9:                                                ; preds = %cstm_7, %cstm_1\n\
     ret ptr %cstm_5\n\
     } "
;;

let format_alloc_array () =
  "; Function Attrs: nounwind ssp uwtable(sync)\ndefine noalias nonnull ptr @"
  ^ get_efkt_name_ops "alloc_array"
  ^ "(i32 noundef %cstm_0, i32 noundef %cstm_1) local_unnamed_addr #0 {\n\
     %cstm_3 = icmp slt i32 %cstm_1, 0\n\
     br i1 %cstm_3, label %c0_alloc_array4, label %c0_alloc_array6\n\n\
     c0_alloc_array4:                                                ; preds = %cstm_2\n\
     %cstm_5 = tail call i32 @raise(i32 noundef 12) #5\n\
     br label %c0_alloc_array6\n\n\
     c0_alloc_array6:                                                ; preds = %cstm_4, \
     %cstm_2\n\
     %cstm_7 = icmp sgt i32 %cstm_0, 0\n\
     br i1 %cstm_7, label %c0_alloc_array8, label %c0_alloc_array13\n\n\
     c0_alloc_array8:                                                ; preds = %cstm_6\n\
     %cstm_9 = udiv i32 1073741816, %cstm_0\n\
     %cstm_10 = icmp slt i32 %cstm_9, %cstm_1\n\
     br i1 %cstm_10, label %c0_alloc_array11, label %c0_alloc_array13\n\n\
     c0_alloc_array11:                                               ; preds = %cstm_8\n\
     %cstm_12 = tail call i32 @raise(i32 noundef 12) #5\n\
     br label %c0_alloc_array13\n\n\
     c0_alloc_array13:                                               ; preds = %cstm_11, \
     %cstm_8, %cstm_6\n\
     %cstm_14 = mul nsw i32 %cstm_1, %cstm_0\n\
     %cstm_15 = sext i32 %cstm_14 to i64\n\
     %cstm_16 = add nsw i64 %cstm_15, 8\n\
     %cstm_17 = tail call ptr @calloc(i64 noundef 1, i64 noundef %cstm_16) #4\n\
     store i32 %cstm_1, ptr %cstm_17, align 4\n\
     %cstm_18 = getelementptr inbounds ptr, ptr %cstm_17, i64 1\n\
     ret ptr %cstm_18\n\
     }"
;;

let format_pre () =
  [ ""
  ; "declare dso_local void @raise(i32) #1"
  ; "declare dso_local noalias ptr @calloc(i64, i64) local_unnamed_addr #1"
  ; format_div ()
  ; format_mod ()
  ; format_shl ()
  ; format_shr ()
  ; format_check_null ()
  ; format_check_array ()
  ; format_alloc ()
  ; format_alloc_array ()
  ]
  |> String.concat ~sep:"\n"
;;

let get_pre (file : string) : string =
  if mac
  then
    sprintf
      "; ModuleID = '%s'\n\
       target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128\"\n\
       target triple = \"arm64-apple-macosx12.0.0\"\n\
       %s"
      file
      (format_pre ())
  else
    sprintf
      "; ModuleID = '%s'\n\
       source_filename = \"%s\"\n\
       target datalayout = \
       \"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128\"\n\
       target triple = \"x86_64-pc-linux-gnu\"\n\
      \ %s"
      file
      file
      (format_pre ())
;;

let get_post (_ : string) : string =
  if mac
  then
    "\n\
    \  attributes #1 = { nofree norecurse nosync nounwind readnone ssp uwtable \
     \"frame-pointer\"=\"non-leaf\" \"min-legal-vector-width\"=\"0\" \
     \"no-trapping-math\"=\"true\" \"probe-stack\"=\"__chkstk_darwin\" \
     \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"apple-m1\" \
     \"target-features\"=\"+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz\" \
     }"
    ^ "attributes #2 = { \"frame-pointer\"=\"non-leaf\" \"no-trapping-math\"=\"true\" \
       \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"apple-m1\" \
       \"target-features\"=\"+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz\" \
       }\n\
       attributes #3 = { mustprogress nofree norecurse nosync nounwind ssp willreturn \
       memory(none) uwtable(sync) \"frame-pointer\"=\"non-leaf\" \
       \"no-trapping-math\"=\"true\" \"stack-protector-buffer-size\"=\"8\" \
       \"target-cpu\"=\"apple-m1\" \
       \"target-features\"=\"+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz\" \
       }\n\
       attributes #4 = { allocsize(0,1) }\n\
       attributes #5 = { nounwind }\n\
      \ "
    ^ "\n\
      \     \n\n\
      \  !llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8}\n\
      \  !llvm.ident = !{!9}\n\
      \  \n\
      \  !0 = !{i32 2, !\"SDK Version\", [2 x i32] [i32 12, i32 3]}\n\
      \  !1 = !{i32 1, !\"wchar_size\", i32 4}\n\
      \  !2 = !{i32 1, !\"branch-target-enforcement\", i32 0}\n\
      \  !3 = !{i32 1, !\"sign-return-address\", i32 0}\n\
      \  !4 = !{i32 1, !\"sign-return-address-all\", i32 0}\n\
      \  !5 = !{i32 1, !\"sign-return-address-with-bkey\", i32 0}\n\
      \  !6 = !{i32 7, !\"PIC Level\", i32 2}\n\
      \  !7 = !{i32 7, !\"uwtable\", i32 1}\n\
      \  !8 = !{i32 7, !\"frame-pointer\", i32 1}\n\
      \  !9 = !{!\"Apple clang version 14.0.0 (clang-1400.0.29.102)\"}\n"
  else
    "attributes #0 = { norecurse nounwind readnone uwtable \
     \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \
     \"disable-tail-calls\"=\"false\" \"frame-pointer\"=\"none\" \
     \"less-precise-fpmad\"=\"false\" \"min-legal-vector-width\"=\"0\" \
     \"no-infs-fp-math\"=\"false\" \"no-jump-tables\"=\"false\" \
     \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \
     \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \
     \"target-cpu\"=\"x86-64\" \"target-features\"=\"+cx8,+fxsr,+mmx,+sse,+sse2,+x87\" \
     \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }\n\
     attributes #1 = { nounwind }\n\
    \ "
    ^ "attributes #2 = { nounwind \"frame-pointer\"=\"none\" \
       \"no-trapping-math\"=\"true\" \"stack-protector-buffer-size\"=\"8\" \
       \"target-cpu\"=\"x86-64\" \"target-features\"=\"+cx8,+fxsr,+mmx,+sse,+sse2,+x87\" \
       \"tune-cpu\"=\"generic\" }\n\n\
      \    attributes #3 = { mustprogress nofree norecurse nosync nounwind readnone \
       willreturn uwtable \"frame-pointer\"=\"none\" \"min-legal-vector-width\"=\"0\" \
       \"no-trapping-math\"=\"true\" \"stack-protector-buffer-size\"=\"8\" \
       \"target-cpu\"=\"x86-64\" \"target-features\"=\"+cx8,+fxsr,+mmx,+sse,+sse2,+x87\" \
       \"tune-cpu\"=\"generic\" }\n\n\
      \    attributes #4 = { nounwind allocsize(0,1) }\n\n\
      \    attributes #5 = { nounwind }\n\
      \       "
    ^ " \n\
      \         !llvm.module.flags = !{!0}\n\
       !llvm.ident = !{!1}\n\
       !0 = !{i32 1, !\"wchar_size\", i32 4}\n\
       !1 = !{!\"clang version 10.0.0-4ubuntu1 \"}\n\
      \       !5 = !{i32 1, !\"sign-return-address-with-bkey\", i32 0}\n"
;;
