type reg =
  | Zero (* Вечный и неизменный ноль *)
  | Ra (* Адрес возврата *)
  | Sp (* Stack pointer, указатель стека *)
  | S of int (* Рабочие регистры 0-11 *)
  | A of int (* Аргументы функции 0-7 / Возвращаемое значение функции 0,1 *)
  | T of int (* Временные регистры 0-6 *)
[@@deriving show { with_path = false }]

type imm =
  | ImmInt of int
  | ConstAddr of string * string
[@@deriving show { with_path = false }]

type addr =
  | Id of string
  | Reg of reg
[@@deriving show { with_path = false }]

type cond =
  | Beq (* if(r1==r2)goto addr *)
  | Bge (* if(r1>=r2)goto addr *)
  | Bgt (* if(r1>r2)goto addr *)
  | Blt (* if(r1<r2)goto addr *)
  | Ble (* if(r1>=r2)goto addr *)
  | Bne (* if(r1!=r2)goto addr *)
[@@deriving show { with_path = false }]

type math_i =
  | Add (* + *)
  | And (* & *)
  | Sll (* << *)
  | Srl (* >> *)
  | Or (* | *)
  | Xor (* ^ *)
[@@deriving show { with_path = false }]

type math_op =
  | I of math_i
  | Mul (* * *)
  | Sub (* - *)
  | Div (* / *)
[@@deriving show { with_path = false }]

type instruction =
  | Attribute of string
  | Global of string
  | Tag of string
  | Math of math_op * reg * reg * reg (* rd = r1 (op) r2 *)
  | Mathi of math_i * reg * reg * imm (* rd = r1 (op) N *)
  | Beqz of reg * addr (* if(r1==0)goto addr *)
  | Bnez of reg * addr (* if(r1!=0)goto addr *)
  | Bnch of cond * reg * reg * addr (* if(r1 ? r2)goto addr *)
  | Call of addr (* вызов функции func *)
  | Jmp of addr (* goto addr *)
  | La of reg * addr (* rd = addr *)
  | Li of reg * imm (* rd = N *)
  | Mv of reg * reg (* rd = rs *)
  | Ret (* возврат из функции *)
  | Ld of reg * imm * reg (* считать 8 байта по адресу r1+N *)
  | Sd of reg * imm * reg (* записать 8 байта по адресу r1+N *)
  | Lui of reg * imm
  | Ecall
(* Unused
   | Lb of reg * int * reg               (* считать 1 байт по адресу r1+N *)
   | Lh of reg * int * reg               (* cчитать 2 байта по адресу r1+N *)
   | Lw of reg * int * reg               (* считать 4 байта по адресу r1+N *)
   | Sb of reg * int * reg               (* записать 1 байт по адресу r1+N *)
   | Sh of reg * int * reg               (* записать 2 байта по адресу r1+N *)
   | Sw of reg * int * reg               (* записать 4 байта по адресу r1+N *)
*)
[@@deriving show { with_path = false }]
