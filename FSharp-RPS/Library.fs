namespace RPS

open System

type GameId = Guid

type PlayerId = Guid

type Move =
    | Rock
    | Paper
    | Scissors

type PlayerMove =
    { PlayerId: PlayerId
      Move: Move }

type GameResult =
    | Won of PlayerId
    | Tied

type GameProgress =
    | Uninitialized
    | NotStarted
    | FirstMoveMade of PlayerMove
    | GameEnded of GameResult

type GameState =
    { GameId: GameId
      GameProgress: GameProgress }

// Commands
type Command =
    | CreateGame of GameId * PlayerId
    | Play of GameId * PlayerId * move: Move

// Events
type DomainEvent =
    abstract GameId: GameId

type GameCreated =
    { GameId: GameId
      CreatedBy: PlayerId }
    interface DomainEvent with
        member x.GameId = x.GameId

type MoveMade =
    { GameId: GameId
      PlayerId: PlayerId
      Move: Move }
    interface DomainEvent with
        member x.GameId = x.GameId

type GameWon =
    { GameId: GameId
      WinnerId: PlayerId }
    interface DomainEvent with
        member x.GameId = x.GameId

type GameTied =
    { GameId: GameId }
    interface DomainEvent with
        member x.GameId = x.GameId

// Derive current state from previous events
module RehydrateGameState =
    let private apply (state: GameState) (event: DomainEvent) =
        match event with
        | :? GameCreated as e ->
            { GameId = e.GameId
              GameProgress = NotStarted }
        | :? MoveMade as e ->
            if (state.GameProgress = NotStarted) then
                { state with GameProgress = FirstMoveMade { PlayerId = e.PlayerId; Move = e.Move } }
            else
                state
        | :? GameWon as e ->
            match state.GameProgress with
            | FirstMoveMade _ -> { state with GameProgress = GameEnded (Won e.WinnerId) }
            | _ -> state
        | :? GameTied ->
            match state.GameProgress with
            | FirstMoveMade _ -> { state with GameProgress = GameEnded Tied }
            | _ -> state
        | _ -> state

    let rehydrateGameStateFrom (events: DomainEvent list): GameState =
        let initialState =
            { GameId = GameId.NewGuid()
              GameProgress = Uninitialized }
        List.fold apply initialState events

// The actual game
module Game =
    let private createGame state gameId createdBy: DomainEvent list =
        match state.GameProgress with
        | Uninitialized ->
            let gameCreated =
                { GameId = gameId
                  CreatedBy = createdBy }
            [ gameCreated ]
        | _ -> []

    // Simulate infix function with extension function....
    type private Move with
        member m1.beats (m2: Move) =
            match (m1, m2) with
            | (Rock, Scissors) -> true
            | (Paper, Rock) -> true
            | (Scissors, Paper) -> true
            | _ -> false

    let private play state playerId move: DomainEvent list =
        match state.GameProgress with
        | NotStarted ->
            [ { MoveMade.GameId = state.GameId; PlayerId = playerId; Move = move } ]
        | FirstMoveMade {PlayerId=player1; Move=firstMove}  ->
            let moveMadeEvent = { MoveMade.GameId = state.GameId; PlayerId = playerId; Move = move }
            let gameResultEvent = if firstMove = move then
                                    { GameTied.GameId = state.GameId } :> DomainEvent
                                  elif firstMove .beats move then
                                    { GameWon.GameId = state.GameId; WinnerId = player1} :> DomainEvent
                                  else
                                    { GameWon.GameId = state.GameId; WinnerId = playerId} :> DomainEvent
            [moveMadeEvent; gameResultEvent]
        | _ -> []

    let handle (state: GameState) (cmd: Command): DomainEvent list =
        match cmd with
        | CreateGame(gameId, createdBy) -> createGame state gameId createdBy
        | Play(_, playerId, move) -> play state playerId move
