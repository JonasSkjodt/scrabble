namespace ScrabbleClient

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO


open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.
//type tile = Set of char * int
module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.


    type state = {
        dict            : ScrabbleUtil.Dictionary.Dict
        playerNumber    : uint32
        hand            : MultiSet.MultiSet<uint32>   
        playerTurn      : uint32
        numberOfPlayers : uint32
        letterPlacement : Map<coord, uint32>
        board           : coord -> bool
    }

    let mkState d pn h pT nOP lP squareFun= {dict = d;  playerNumber = pn; hand = h; playerTurn = pT; numberOfPlayers = nOP; letterPlacement = lP; board = squareFun}

    let dict st             = st.dict
    let playerNumber st     = st.playerNumber
    let hand st             = st.hand
    let playerTurn st       = st.playerTurn
    let numberOfPlayers st  = st.numberOfPlayers
    let letterPlacement st  = st.letterPlacement    

    let rec removeTiles (move : list<coord * (uint32 * (char * int))>) hand =
                    match move with
                    | (_, (tileID, _)) :: tail when MultiSet.contains tileID hand ->
                        removeTiles tail (MultiSet.removeSingle tileID hand)
                    | [] -> hand
                    | _ -> failwith "Tile played could not be found in player hand"

    let rec addNewTiles (newPieces : (uint32 * uint32) list ) hand =
                    match newPieces with
                    | [] -> hand
                    | newTile :: tail -> addNewTiles tail (MultiSet.add (fst newTile) (snd newTile) hand)

    let rec updateBoard ms (board : Map<coord, uint32>) = 
        match ms with
        | [] -> board
        | x :: xs -> (updateBoard xs board).Add (fst x, x 
                                                        |> snd 
                                                        |> fst)  
    
    let changeTurn pId numP= 
        match pId with
        | p when p = numP -> 1u
        | _ -> pId + 1u

module MudBot =

    //checkword
    let checkWord pos l (rack: MultiSet.MultiSet<uint32>) dir (st: State.state) (pieces : Map<uint32, tile>) =
        //x, y are the directions we are going to check
        let rec checkHorAndVer pos x y word =
            match st.letterPlacement.ContainsKey pos with
            | true ->
                let newPos = (fst pos + x, snd pos + y)
                let tile = pieces.[st.letterPlacement.[pos]]
                let letter = tile 
                            |> Set.toList 
                            |> List.head 
                            |> fst 
                            |> string
                checkHorAndVer newPos x y (if x < 0 || y < 0 then letter + word 
                                          else word + letter)
            | false -> word

        // Check if the word is valid
        let validWord (pos: coord) (dir: string) (l: char) =
            let word =
                match dir with
                | "hor" ->
                    let leftWord = checkHorAndVer (fst pos - 1,
                                                  snd pos)
                                                  -1 0 ""
                    let rightWord = checkHorAndVer (fst pos + 1,
                                                   snd pos)
                                                   1 0 ""

                    leftWord + string l + rightWord
                    
                | "ver" ->
                    let upWord = checkHorAndVer (fst pos,
                                                 snd pos - 1) 
                                                 0 -1 ""            
                    let downWord = checkHorAndVer (fst pos,
                                                  snd pos + 1)
                                                  0 1 ""

                    upWord + string l + downWord

                | _ -> failwith "Invalid direction"

            // Check if the word is in the dictionary
            if String.length word > 1 then
                st.dict 
                |> Dictionary.lookup word
            else
                true

        validWord pos dir l
    
    // Move generation inspired from Gordon's Scrabble bot implementation
    // https://ericsink.com/downloads/faster-scrabble-gordon.pdf
    let rec gen pos word rack arc anchSqr dir (st : State.state) (pieces : Map<uint32, tile>) wordTiles =    
        let possibleWords = []
        let offsetCoords = // Add the position offset to the anchor square coordinates (depending on the direction we are planning to go)
            match dir with 
            | "hor"  -> coord((fst anchSqr) + pos, snd anchSqr)
            | "ver" -> coord(fst anchSqr, (snd anchSqr) + pos)
        // If a letter, x, is already on this square then goOn
        if not (st.board offsetCoords) then [] // 
        else
            //if letters remain on the rack
            if st.letterPlacement.ContainsKey offsetCoords then
                // Go in here if there is a letter on the offsetCoords square thus we do not place letters from the rack
                goOn 
                    pos (Map.find offsetCoords st.letterPlacement)
                    word
                    (MultiSet.toList rack)
                    (Dictionary.step
                    (Map.find
                    (Map.find offsetCoords st.letterPlacement) pieces
                    |> Set.toList
                    |> List.head
                    |> fst) arc) // Create newArc by stepping 
                    dir pieces wordTiles anchSqr st // The rest of the args
            else 
                // Go in here if there is no letter on the offsetCoords square thus we place letters from the rack
                MultiSet.fold (fun plays L _ ->
                    goOn
                        pos L
                        word
                        (MultiSet.toList (MultiSet.remove L 1u rack))
                        (Dictionary.step ((Map.find L pieces)
                        |> Set.toList
                        |> List.head
                        |> fst) arc)
                        dir pieces wordTiles anchSqr 
                        st @ plays
                        ) possibleWords rack
    
    and goOn pos L word rack newArc dir pieces wordTiles anchor (st: State.state) = 

        let possibleWords = []
        let letter = Map.find L pieces 
                    |> Set.toList 
                    |> List.head 
                    |> fst
        let offsetCoords = 
            match dir with
            | "hor"  -> coord((fst anchor) + pos, (snd anchor))
            | "ver" -> coord((fst anchor), (snd anchor) + pos)

        // Check if the move is valid
        if not (checkWord offsetCoords letter 
                         (MultiSet.ofList rack) 
                         (if dir = "hor" then "ver" else "hor") 
                         st pieces) then possibleWords
        else
            let (newWord, nextPosCoords) =
                match pos with
                | pos when pos <= 0 ->
                    (letter.ToString() + word, 
                    match dir with
                    | "hor" -> coord(
                                    (fst anchor) + pos - 1, 
                                     snd anchor)
                    | "ver" -> coord(
                                    fst anchor, 
                                     (snd anchor) + pos - 1)
                    )
                // When pos > 0
                | _ ->
                    (word + letter.ToString(),
                    match dir with
                    | "hor" -> coord(
                                    (fst anchor) + pos + 1,
                                     snd anchor)
                    | "ver" -> coord(
                                    fst anchor, 
                                     (snd anchor) + pos + 1)
                    )

            let wordTiles =
                if st.letterPlacement.ContainsKey offsetCoords then wordTiles
                else (offsetCoords, 
                     (L, 
                     (Map.find L pieces 
                     |> Set.toList 
                     |> List.head)
                     )) :: wordTiles

            // Check if the newWord is in the dictionary and next position is not occupied.
            let plays =
                if Dictionary.lookup newWord st.dict && not (st.letterPlacement.ContainsKey nextPosCoords) then
                    if List.length wordTiles > 0 then wordTiles :: possibleWords
                    else possibleWords
                else
                    possibleWords

            // Handle arcs
            match newArc with
            | Some arc ->
                let arcPlays =
                    // Update the position
                    let pos = if pos <= 0 then pos - 1 else pos + 1
                    gen pos newWord (MultiSet.ofList rack) (snd arc) anchor dir st pieces wordTiles
                let plays = arcPlays @ plays
                match Dictionary.step (char 0) (snd arc) with
                | Some newArc when not (st.letterPlacement.ContainsKey nextPosCoords) ->
                    let arcPlays =
                        let pos = if pos <= 0 then 1 else pos + 1
                        gen pos newWord (MultiSet.ofList rack) (snd newArc) anchor dir st pieces wordTiles
                    arcPlays @ plays
                | _ -> plays
            | None -> plays

    let startWordGeneration (st : State.state) pieces anchor = 
        let rack = st.hand
        let startingDict = st.dict
        let wordTiles = []
        let dir = "hor"
                
        let horWords = if (not (st.letterPlacement.ContainsKey (coord(fst anchor + 1, snd anchor)))) 
                        then gen 0 "" rack startingDict anchor dir st pieces wordTiles else []
        let verWords = if (not (st.letterPlacement.ContainsKey (coord(fst anchor, snd anchor + 1)))) 
                         then (gen 0 "" rack startingDict anchor ("ver") st pieces wordTiles) else []

        horWords @ verWords

    let move pieces (st: State.state) =
        let possibleWords =
            if st.letterPlacement.ContainsKey (coord(0, 0)) then
                // convert anchors to array
                let anchors = Map.toArray st.letterPlacement
                // Generate all possible moves
                let moves =
                    anchors
                    |> Array.map (fun (anchor, _) ->
                        startWordGeneration st pieces anchor)
                // take the generated moves array and make it into a single list
                Array.fold (fun acc move -> move @ acc) [] moves
            else
                // If the (0, 0) coordinate is not in the letterPlacement, start from there
                startWordGeneration st pieces (0, 0)
                
        // Find the longest move
        List.fold (fun (longestMove) move -> 
            if List.length move > List.length longestMove then move 
            else longestMove) [] possibleWords


module Scrabble =
    open System.Threading
    let playGame cstream (pieces : Map<uint32,tile>) (st : State.state) =

        // Pieces has got to be the single worst datatype i have ever seen in my whole life. Thank you to however made it. Especially love the PDF definition

        let rec aux (st : State.state) =
            let move = "" //insert bot moves
            
            if st.playerTurn = st.playerNumber then
                Thread.Sleep (1 * 500)
                Print.printHand pieces (State.hand st)
                let move = MudBot.move pieces st //start the mudbot move

                let serverMsg move = 
                    match move with
                    | [] -> SMPass
                    | _ -> SMPlay move

                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (serverMsg move)
                            
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            // pid = playerid
            // ms = letters on the board
            // points = points from a play
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                // The message CMPlaySuccess is sent to the player who made the move

                // place letters on the board
                let boardWithLetters = State.updateBoard ms st.letterPlacement

                // create a new hand with old tiles removed and new tiles added
                let newHand = (State.removeTiles ms st.hand)
                let newHand' = State.addNewTiles newPieces newHand

                // update st with the new playernumber via changeTurn
                let newTurn = State.changeTurn st.playerNumber st.numberOfPlayers
                let st' = { st with hand = newHand' ; playerTurn = newTurn ; letterPlacement = boardWithLetters}
                    
                aux st'

            // Successful play by you. Update your state (remove old tiles, add the new ones, etc.)
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                // place letters on the board
                let boardWithLetters = State.updateBoard ms st.letterPlacement

                // update st with new playernumber and new points
                let newTurn = State.changeTurn pid st.numberOfPlayers
                let st' = {st with playerTurn = newTurn ; letterPlacement = boardWithLetters}
                
                aux st'

            // Pass 
            | RCM (CMPassed (pid )) -> 
                (* Passed play. Update your state *)

                let newTurn = State.changeTurn pid st.numberOfPlayers
                let st' = { st with playerTurn = newTurn }
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *) // this is seen from the other player

                let newTurn = State.changeTurn pid st.numberOfPlayers
                let st' = { st with playerTurn = newTurn } // This state needs to be updated

                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        //let dict = dictf false // Uncomment if using a trie for your dictionary
        
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        let board = ScrabbleLib.simpleBoardLangParser.parseSimpleBoardProg boardP

        fun () -> playGame cstream tiles (State.mkState dict playerNumber handSet playerTurn numPlayers Map.empty board)
        
