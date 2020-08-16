structure Pattern =
  struct
    datatype ('ty,'pat) patrow =
      Wildcard
    | Row of Lab.t * 'pat * ('ty,'pat) patrow option
    | LabelAsVar of VId.t * 'ty Ty.t option * 'pat option * ('ty,'pat) patrow option

    datatype ('ty,'atpat,'pat) atpat =
      Wild
    | SCon of SCon.t
    | Var of VId.t Long.t Op.t
    | Record of ('ty,'pat) patrow
    | Unit
    | Tuple of 'pat list
    | List of 'pat list
    | Paren of 'pat

    datatype ('ty,'atpat,'pat) pat =
      AtPat of 'atpat
    | Constructed of VId.t Long.t Op.t * 'atpat
    | InfixConstructed of 'pat * VId.t * 'pat
    | Typed of 'pat * 'ty Ty.t
    | Layered of VId.t Op.t * 'ty Ty.t option * 'pat
  end
