(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure TyIdDict = RedBlackDict (structure Key = TyId)
structure AbsIdDict = RedBlackDict (structure Key = AbsId)
structure ContIdDict = RedBlackDict (structure Key = ContId)
