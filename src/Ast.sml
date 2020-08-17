structure Ast =
  struct
    datatype 'a ty = Ty of 'a * 'a ty Ty.t

    datatype 'a atpat = AtPat of 'a * ('a ty,'a pat) Pattern.atpat
    and 'a pat = Pat of 'a * ('a ty,'a atpat,'a pat) Pattern.pat

    datatype 'a atexp = AtExp of 'a * ('a atexp,'a exp,'a dec) Exp.atexp
    and 'a exp = Exp of 'a * ('a atexp,'a exp,'a ty,'a pat,'a dec) Exp.exp
    and 'a dec = Dec of 'a * ('a ty,'a atpat,'a pat,'a exp,'a dec) Dec.dec
  end
