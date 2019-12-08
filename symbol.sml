structure Symbol :
sig
  type symbol
  val createSymbol : string -> symbol
  val extractName : symbol -> string

  type 'a table
  val empty: 'a table
  val enter: 'a table * symbol * 'a -> 'a table
  val look : 'a table * symbol -> 'a option
end =

struct
  type symbol = string * int
  exception Symbol
  val nextSymbolIdx = ref 0
  structure H = HashTable

  val hashtable : (string, int) H.hash_table =
		H.mkTable(HashString.hashString, op = ) (128, Symbol)

  fun createSymbol name =
    case H.find hashtable name
       of SOME idx => (name, idx)
        | NONE =>
          let
            val idx = !nextSymbolIdx
          in
            nextSymbolIdx := idx + 1;
            H.insert hashtable (name, idx);
            (name, idx)
          end

  fun extractName(name, idx) = name

  structure Table = IntMapTable(
    type key = symbol
  	fun getInt(name, num) = num
    fun getKey num =
      case List.find (fn (_, num') => num = num') (H.listItemsi hashtable) of
        SOME symbol => symbol
      | NONE => raise Fail "not found")

  type 'a table = 'a Table.table
  val empty = Table.empty
  val enter = Table.enter
  val look = Table.look
end
