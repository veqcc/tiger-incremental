structure Symbol :
sig
  type symbol
  val symbol : string -> symbol
  val name : symbol -> string
end =

struct
  type symbol = string * int
  exception Symbol
  val nextsym = ref 0
  structure H = HashTable

  val hashtable : (string, int) H.hash_table =
		H.mkTable(HashString.hashString, op = ) (128, Symbol)

  fun symbol name =
    case H.find hashtable name
       of SOME i => (name, i)
        | NONE =>
          let
            val i = !nextsym
          in
            nextsym := i + 1;
            H.insert hashtable (name, i);
            (name, i)
          end

  fun name(s, n) = s
end
