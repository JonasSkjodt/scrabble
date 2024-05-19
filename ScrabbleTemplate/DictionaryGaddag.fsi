module DictionaryGaddag
    type Dict = 
    | Node of Map<char, bool*Dict>

    val empty : unit -> Dict
    val insert : string -> Dict -> Dict
    val step : char -> Dict -> (bool*Dict) option
    val reverse : Dict -> (bool*Dict) option
    val lookup : string -> Dict -> bool
