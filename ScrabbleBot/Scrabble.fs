namespace ScrabbleClient

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO


open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

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
        board           : Parser.board
        dict            : ScrabbleUtil.Dictionary.Dict
        playerNumber    : uint32
        hand            : MultiSet.MultiSet<uint32>   
        playerTurn      : uint32
        numberOfPlayers : uint32
        letterPlacement : Map<coord, uint32 * (char * int)>
    }

    let mkState b d pn h pT nOP lP= {board = b; dict = d;  playerNumber = pn; hand = h; playerTurn = pT; numberOfPlayers = nOP; letterPlacement = lP}

    let board st            = st.board
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

    let rec addNewTiles newPieces hand =
                    match newPieces with
                    | newTile :: tail -> addNewTiles tail (MultiSet.addSingle (fst newTile) hand)
                    | [] -> hand

    let changeTurn pId numP= 
        match pId with
        | p when p = numP -> 1u
        | _ -> pId + 1u
        
    let private (|FoundValue|_|) key map = Map.tryFind key map

    let updateBoard tiles = 
        let updatePlacement tile coord st =
            match st.letterPlacement with
            | FoundValue coord _ ->  failwith "There is already a tile at the coord"  //map when Map.containsKey coord map -> failwith "There is already a tile at the coord"
            | _ -> {st with letterPlacement = Map.add coord tile st.letterPlacement} 

        Seq.foldBack (fun (coord, tile) acc -> updatePlacement tile coord acc) tiles 

    // let updateBoard tiles st = 
    //     let updatePlacement (coord, tile) map =
    //         match Map.tryFind coord map with
    //         | Some _ -> failwith "There is already a tile at the coord"
    //         | None -> Map.add coord tile map 

    //     tiles
    //     |> Seq.fold (fun acc (coord, tile) -> updatePlacement (coord, tile) acc) st.letterPlacement
    //     |> fun updatedMap ->  updatedMap

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            if State.playerNumber st = State.playerTurn st then
                Thread.Sleep (1 * 500)
                Print.printHand pieces (State.hand st)
            //printfn "Updated hand: %A" (st.hand)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // create empty move
            
            let move = ""

            if st.playerTurn = st.playerNumber then
                
                let input = System.Console.ReadLine()
                
                let bool inp  = 
                    match inp with
                    | "" -> false
                    | inp  -> true

                let boolReal = bool input

                let move = 
                    match boolReal with
                    | true -> RegEx.parseMove input
                    | false -> []
                    // let move = RegEx.parseMove input

                let play = ""

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
                
                let newHand = (State.removeTiles ms st.hand)
                let newHand' = State.addNewTiles newPieces newHand

                let newTurn = State.changeTurn st.playerNumber st.numberOfPlayers
                let st' = { st with hand = newHand' ; playerTurn = newTurn}
                    
                aux st'

            // Successful play by you. Update your state (remove old tiles, add the new ones, etc.)
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                // update st with new playernumber and new points
                let newTurn = State.changeTurn pid st.numberOfPlayers
                let st' = {st with playerTurn = newTurn}
                
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

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn numPlayers  Map.empty)
        
