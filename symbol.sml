structure Symbol :
sig
  type symbol
  val createSymbol : string -> symbol
  val extractName : symbol -> string
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
end
