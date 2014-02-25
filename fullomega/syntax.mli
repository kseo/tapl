(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type kind = 
    KnStar
  | KnArr of kind * kind

type ty =
    TyVar of int * int
  | TyId of string
  | TyArr of ty * ty
  | TyRecord of (string * ty) list
  | TyRef of ty
  | TyString
  | TyUnit
  | TyBool
  | TyFloat
  | TyAll of string * kind * ty
  | TyNat
  | TySome of string * kind * ty
  | TyAbs of string * kind * ty
  | TyApp of ty * ty

type term =
    TmAscribe of info * term * ty
  | TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmString of info * string
  | TmUnit of info
  | TmLoc of info * int
  | TmRef of info * term
  | TmDeref of info * term 
  | TmAssign of info * term * term
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmLet of info * string * term * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmInert of info * ty
  | TmFix of info * term
  | TmTAbs of info * string * kind * term
  | TmTApp of info * term * ty
  | TmPack of info * ty * term * ty
  | TmUnpack of info * string * string * term * term

type binding =
    NameBind 
  | TyVarBind of kind
  | VarBind of ty
  | TyAbbBind of ty * (kind option)
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
val printkn : context -> kind -> unit
val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo: term -> info

