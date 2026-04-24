signature STRING_MAP =
sig
  type 'a map
  val empty: 'a map
  val insert: 'a map * string * 'a -> 'a map
  val find: 'a map * string -> 'a option
  val exists: 'a map * string -> bool
end

structure StringMap: STRING_MAP =
struct
  type 'a map = (string * 'a) list

  val empty: 'a map = []

  fun insert (m, k, v) =
    (k, v) :: List.filter (fn (k', _) => k <> k') m

  fun find (m, k) =
    case List.find (fn (k', _) => k = k') m of
      NONE => NONE
    | SOME (_, v) => SOME v

  fun exists (m, k) =
    Option.isSome (find (m, k))

end
