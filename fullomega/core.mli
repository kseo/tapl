(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val typeof : context -> term -> ty
type store
val emptystore : store
val shiftstore : int -> store -> store 
val eval : context -> store -> term -> term * store
val kindof : context -> ty -> kind
val tyeqv : context -> ty -> ty -> bool
val simplifyty : context -> ty -> ty
val evalbinding : context -> store -> binding -> binding * store
