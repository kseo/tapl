(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type ty =
    TyVar of int * int
  | TyId of string
  | TyArr of ty * ty
  | TyString
  | TyUnit
  | TyRecord of (string * ty) list
  | TyBool
  | TyFloat
  | TyNat
  | TySome of string * ty
  | TyAll of string * ty

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmLet of info * string * term * term
  | TmFix of info * term
  | TmString of info * string
  | TmUnit of info
  | TmAscribe of info * term * ty
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmInert of info * ty
  | TmPack of info * ty * term * ty
  | TmUnpack of info * string * string * term * term
  | TmTAbs of info * string * term
  | TmTApp of info * term * ty

type binding =
    NameBind 
  | TyVarBind
  | VarBind of ty
  | TyAbbBind of ty
  | TmAbbBind of term * (ty option)

type command =
  | Eval of info * term
  | Bind of info * string * binding
  | SomeBind of info * string * string * term

(* Contexts *)
type context
val emptycontext : context 
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val getTypeFromContext : info -> context -> int -> ty

(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> ty -> ty
val typeSubstTop: ty -> ty -> ty
val tytermSubstTop: ty -> term -> term

(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val printty : context -> ty -> unit
val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo: term -> info

