﻿// Learn more about F# at http://fsharp.org

open DictionaryTrie

let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function 
        | 0 -> []
        | x -> (sprintf "%s%d" name x, dict, bot)::aux (x - 1)
   
    aux >> List.rev

[<EntryPoint>]
let main argv =
    
    ScrabbleUtil.DebugPrint.toggleDebugPrint false // Change to false to supress debug output

    //System.Console.BackgroundColor <- System.ConsoleColor.White
    //System.Console.ForegroundColor <- System.ConsoleColor.Black
    System.Console.Clear()

    let board        = ScrabbleUtil.StandardBoard.standardBoard ()
//    let board      = ScrabbleUtil.InfiniteBoard.infiniteBoard ()

//    let board      = ScrabbleUtil.RandomBoard.randomBoard ()
//    let board      = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)

//    let board      = ScrabbleUtil.HoleBoard.holeBoard ()
//    let board      = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    // let words     = readLines "../../../Dictionaries/English.txt"
    let words     = readLines "./Dictionaries/English.txt"


    let handSize   = 7u
    let timeout    = None
    let tiles      = ScrabbleUtil.English.tiles 1u
    let seed       = None
    let port       = 13001

    // let dictAPI =
    //     // Uncomment if you have implemented a dictionary. last element None if you have not implemented a GADDAG
    //     Some (DictionarySimple.empty, DictionarySimple.insert, DictionarySimple.lookup, DictionarySimple.step) //DictionarySimple.step(*, Some DictionarySimple.reverse*)) 
    //     None

    let playerDictAPI =
        // Uncomment if you have implemented a dictionary. last element None if you have not implemented a GADDAG
        //Some (DictionarySimple.empty, DictionarySimple.insert, DictionarySimple.lookup) //DictionarySimple.step, Some DictionarySimple.reverse) 
        Some (DictionaryTrie.empty, DictionaryTrie.insert, DictionaryTrie.step, None)
        
    
    let (playerDict, playerTime) = time (fun () -> ScrabbleUtil.Dictionary.mkDict words playerDictAPI)
    
    
    // ScrabbleUtil.DebugPrint.debugPrint ("Dictionary test sucessful\n")
    // let incorrectWords = ScrabbleUtil.Dictionary.test words 10 (playerDict false) // change the boolean to true if using a GADDAG
    // match incorrectWords with
    // | [] -> ScrabbleUtil.DebugPrint.debugPrint ("Dictionary test sucessful!\n")
    // | _ -> ScrabbleUtil.DebugPrint.debugPrint ("Dictionary test failed for at least the following words: \n")
    // List.iter (fun str -> ScrabbleUtil.DebugPrint.debugPrint (sprintf "%s\n" str)) incorrectWords


    // Uncomment this line to call your client
    let players    = [("BOT", playerDict, Oxyphenbutazone.Scrabble.startGame); ("Player 1", playerDict, ScrabbleClient.Scrabble.startGame)]  //ScrabbleClient is the name of the namespace in scrabble.fs and scrabble.fsi
    
    // let players    = [("Player 1", playerDict, ScrabbleClient.Scrabble.startGame); ("Player 2", playerDict, ScrabbleClient.Scrabble.startGame)]  //ScrabbleClient is the name of the namespace in scrabble.fs and scrabble.fsi

    // let (dictionary, time: System.TimeSpan) =
    //     time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI)
    

    // int at the end is amount of bots 
    //let players = spawnMultiples "OxyphenButazone" dictionary Oxyphenbutazone.Scrabble.startGame 2

    // 0 0 19S1 0 1 1A1 0 2 22V4 0 3 5E1    save

    // 0 0 19S1 <----- placening 'S' on the board
    // 0            0            19-----------S---------1
    // ^placementX  ^placementY  ^tileID      ^Char     ^point value
    
    // 0 0 1A1 0 1 1A1 0 2 8H4     a


    
    do ScrabbleServer.Comm.startGame 
          board playerDict handSize timeout tiles seed port players
    
    ScrabbleUtil.DebugPrint.forcePrint ("Server has terminated. Press Enter to exit program.\n")
    System.Console.ReadLine () |> ignore 
    
    0
    