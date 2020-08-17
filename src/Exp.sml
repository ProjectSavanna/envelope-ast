structure Exp =
  struct
    datatype ('atexp,'exp,'dec) atexp =
      SCon of SCon.t
    | Var of VId.t Long.t Op.t
    | Record of (Lab.t * 'exp) list
    | Selector of Lab.t
    | Tuple of 'exp list
    | List of 'exp list
    | Sequence of 'exp list
    | Let of 'dec * 'exp list
    | Paren of 'exp

    datatype 'atexp app =
      PrefixApp of 'atexp * 'atexp list
    | InfixApp of 'atexp app * VId.t * 'atexp app

    datatype ('atexp,'exp,'ty,'pat,'dec) exp =
      Atom of 'atexp
    | App of 'atexp app
    | Typed of 'exp * 'ty Ty.t
    | AndAlso of 'exp * 'exp
    | OrElse of 'exp * 'exp
    | Handle of 'exp * ('pat,'exp) Match.t
    | Raise of 'exp
    | If of 'exp * 'exp * 'exp
    | While of 'exp * 'exp
    | Case of 'exp * ('pat,'exp) Match.t
    | Fn of ('pat,'exp) Match.t
  end
