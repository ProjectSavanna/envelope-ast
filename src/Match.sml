structure Match =
  struct
    type ('pat,'exp) t = ('pat * 'exp) list  (* nonempty *)
  end
