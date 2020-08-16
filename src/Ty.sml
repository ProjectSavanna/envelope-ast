structure Ty =
  struct
    datatype 'ty t =
      Var of string
    | Record of (Lab.t * 'ty) list
    | Construction of 'ty list * TyCon.t Long.t
    | Tuple of 'ty list
    | Function of 'ty * 'ty
    | Paren of 'ty
  end
