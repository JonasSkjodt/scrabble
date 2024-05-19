// Learn more about F# at http://fsharp.org
// Multi-player: yes
// Dictionary(Trie): yes
// playing on all boards: no
// parallelism: no
// Respect the timeout flag: no



// open DictionaryTrie
open DictionaryGaddag

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

    System.Console.BackgroundColor <- System.ConsoleColor.White
    System.Console.ForegroundColor <- System.ConsoleColor.Black
    System.Console.Clear()

    //let board        = ScrabbleUtil.StandardBoard.standardBoard ()
    let board      = ScrabbleUtil.InfiniteBoard.infiniteBoard ()

//    let board      = ScrabbleUtil.RandomBoard.randomBoard ()
//    let board      = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)

//    let board      = ScrabbleUtil.HoleBoard.holeBoard ()
//    let board      = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    let words     = 
        try readLines "../../../Dictionaries/English.txt" 
        with _ -> readLines "./Dictionaries/English.txt"


    let handSize   = 7u
    let timeout    = None
    let tiles      = ScrabbleUtil.English.tiles 1u
    let seed       = None
    let port       = 13001

    let dictAPI =
        //Some (DictionaryTrie.empty, DictionaryTrie.insert, DictionaryTrie.step, None)
        Some (DictionaryGaddag.empty, DictionaryGaddag.insert, DictionaryGaddag.step, Some DictionaryGaddag.reverse)
        
    // this dicitonary is built on a Ternary Search Trie 
    let (dictionary, playerTime) = time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI)


    // Uncomment one of these lines for the number of players you want to play against

    // One player and one bot
    //let players    = [("BOT", dictionary, Oxyphenbutazone.Scrabble.startGame); ("Player 1", dictionary, ScrabbleClient.Scrabble.startGame)]  //ScrabbleClient is the name of the namespace in scrabble.fs and scrabble.fsi
    
    // Two players
    let players    = [("Player 1", dictionary, ScrabbleClient.Scrabble.startGame); ("Player 2", dictionary, ScrabbleClient.Scrabble.startGame)]  //ScrabbleClient is the name of the namespace in scrabble.fs and scrabble.fsi
    //let playersLocal    = [("Player 1", dictionary, LocalPlayer.Scrabble.startGame); ("Player 2", dictionary, LocalPlayer.Scrabble.startGame)]
    // Two bots
    //let players = spawnMultiples "OxyphenButazone" dictionary Oxyphenbutazone.Scrabble.startGame 2
    
    do ScrabbleServer.Comm.startGame 
          board dictionary handSize timeout tiles seed port players
    
    ScrabbleUtil.DebugPrint.forcePrint ("Server has terminated. Press Enter to exit program.\n")
    System.Console.ReadLine () |> ignore 
    
    0
    