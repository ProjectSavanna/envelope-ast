signature SYMBOL =
  sig
    include READ SHOW
  end

structure Symbols =
  struct
    val reserved = [
      "abstype",
      "and",
      "andalso",
      "as",
      "case",
      "datatype",
      "do",
      "else",
      "end",
      "exception",
      "fn",
      "fun",
      "handle",
      "if",
      "in",
      "infix",
      "infixr",
      "let",
      "local",
      "nonfix",
      "of",
      "op",
      "open",
      "orelse",
      "raise",
      "rec",
      "then",
      "type",
      "val",
      "with",
      "withtype",
      "while",
      "(",
      ")",
      "[",
      "]",
      "{",
      "}",
      ",",
      ":",
      ";",
      "...",
      "_",
      "|",
      "=",
      "=>",
      "->",
      "#"
    ]
    val isAlphaNumeric =
      fn #"'" => true
       | #"_" => true
       | c    => Char.isAlphaNum c

    val symbols = String.explode "!%&$#+-/:<=>?@\\~`^|*"
    val isSymbol = fn c => List.exists (Fn.curry (op =) c) symbols
  end

structure Identifier :> SYMBOL =
  struct
    type t = string

    local
      val isValid =
        fn "=" => true
         | s => (
            not (List.exists (Fn.curry (op =) s) Symbols.reserved) andalso (
              case String.explode s of
                nil     => false
              | c :: cs => (
                  if Char.isAlpha c orelse c = #"'"
                    then List.all Symbols.isAlphaNumeric cs
                    else List.all Symbols.isSymbol (c :: cs)
                )
            )
          )
    in
      val fromString = fn s => if isValid s then SOME s else NONE
    end

    val toString = Fn.id
  end

functor Exclude (val p : string -> bool) :> SYMBOL =
  struct
    type t = Identifier.t
    val fromString = fn s => if p s then Identifier.fromString s else NONE
    val toString = Identifier.toString
  end

structure VId = Exclude (val p = (fn nil => false | #"'" :: _ => false | _ => true) o String.explode)

structure TyVar :> SYMBOL =
  struct
    type t = Identifier.t * bool
    val fromString = fn s =>
      case String.explode s of
        nil              => NONE
      | #"'" :: nil      => NONE
      | #"'" :: c :: nil => Option.map (fn id => (id,false)) (Identifier.fromString s)
      | #"'" :: c :: _   => Option.map (fn id => (id,c = #"'")) (Identifier.fromString s)
      | _    :: _        => NONE
    val toString = fn (id,_) => Identifier.toString id
  end

structure TyCon =
  Exclude (
    val p = (
      fn nil         => false
       | #"*" :: nil => false
       | #"'" :: _   => false
       | _           => true
    ) o String.explode
  )

structure Lab = Exclude (val p = (fn nil => false | #"'" :: _ => false | _ => true) o String.explode)

structure StrId =
  Exclude (
    val p = (
      fn nil       => false
       | #"'" :: _ => false
       | cs        => List.all Symbols.isAlphaNumeric cs
    ) o String.explode
  )


structure Long =
  struct
    type 'a t = StrId.t list * 'a
    val toString = fn f => fn (strids,x) =>
      String.concat (List.map (fn strid => StrId.toString strid ^ ".") strids) ^ f x
  end

structure Op =
  struct
    type 'a t = 'a * bool
    val toString = fn f =>
      fn (x,false) => f x
       | (x,true ) => "op " ^ f x
  end


structure SCon =
  struct
    datatype t =
      Int of int
    | Real of real
    | Word of word
    | Char of char
    | String of string
  end
