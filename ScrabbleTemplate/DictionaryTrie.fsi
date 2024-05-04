module DictionaryTrie

  type CTrie

  val empty : unit -> Ctrie
  val char2num : Char -> Int
  val insert: word: string -> root: CTrie -> CTrie
  val step: char: char -> root: CTrie -> option<bool * CTrie>
  val lookup : string -> Dict -> bool