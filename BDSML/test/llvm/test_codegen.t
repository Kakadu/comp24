  $ dune exec test_codegen
  { i64, i8, i1, [4 x i8], ptr } { i64 42, i8 99, i1 true, [4 x i8] c"BDSM", ptr @unit_t }{ i1, i64 } { i1 true, i64 1 }{ i64, i64 } { i64 1, i64 2 }; ModuleID = 'BDSML'
  source_filename = "BDSML"
  target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
  target triple = "x86_64-pc-linux-gnu"
  
  define void @unit_t() {
  entry:
  }
