structure Dec =
  struct
    datatype ('pat,'exp) valbind =
      ValPat of 'pat * 'exp
    | Rec of ('pat,'exp) valbind

    type 'ty datbind = (TyVar.t list * TyCon.t * (VId.t Op.t * 'ty Ty.t option) list) list
    type 'ty typbind = (TyVar.t list * TyCon.t * 'ty) list
    datatype 'ty exbind =
      ExnNew of 'ty Ty.t option
    | ExnCopy of VId.t Long.t Op.t

    datatype fixity =
      Left of int option
    | Right of int option
    | Non

    datatype ('ty,'atpat,'pat,'exp,'dec) dec =
      Val of TyVar.t list * ('pat,'exp) valbind list
    | Fun of TyVar.t list * (VId.t Op.t * 'atpat list * 'ty Ty.t * 'exp) list list
    | Type of 'ty typbind
    | Datatype of 'ty datbind * 'ty typbind
    | DataRepl of TyCon.t * TyCon.t Long.t
    | AbsType of 'ty datbind * 'ty typbind list * 'dec
    | Exception of (VId.t Op.t * 'ty exbind) list
    | Local of 'dec * 'dec
    | Open of StrId.t Long.t list
    | Sequence of 'dec * 'dec
    | Fixity of fixity * VId.t list
  end
