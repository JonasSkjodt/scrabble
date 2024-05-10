//import stuff (already done)

//define types (already done)

//initilization game state (already done)

//server messages (already done)



// move (main bot logic)



// communicate to the server (already done)

//bot
//read the tiles
//make words from hand
//move
//play the hand

//do it again as another player

//check if bots turn
if State.playerNumber st = State.playerTurn st then
    //read the tiles
    let handTiles = st.hand |> MultiSet.toList |> List.map //something something 

    //make words from hand
    let validMoves = generateSomeValidMoves handTiles st.dict st.board

    //move
    let move = chooseTheBestMove validMoves

    //play hand
    let serverMsg move = 
        match move with
        | [] -> SMPass
        | _ -> SMPlay move

    send cstream (serverMsg move)

else
    //yadda