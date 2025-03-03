
  $ ./llvm_demo.exe << EOF | lli-17
  > let rec sum a = if a = 0 then 0 else a + sum (a - 1)
  > let dva = 2
  > let bul = 1 = 1
  > let main = sum 3
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  define i64 @sum(i64 %a) {
  entry:
    %sub_result = sub i64 %a, 1
    %call_result = call i64 @sum(i64 %sub_result)
    %add_result = add i64 %a, %call_result
    ret i64 %add_result
  }
  
  define i64 @dva() {
  entry:
    ret i64 2
  }
  
  define i64 @bul() {
  entry:
    ret i64 1
  }
  
  define i64 @main() {
  entry:
    %call_result = call i64 @sum(i64 3)
    ret i64 %call_result
  }
  
