module GamePlayTest

open NFluent
open Xunit
open RPS
open RecreateGameState

[<Fact>]
let ``Returns game created event with supplied game id and creator when creating new game``() =
    // Given
    let state: GameState =
        { GameId = GameId.NewGuid()
          GameProgress = Uninitialized }
    let gameId = GameId.NewGuid()
    let playerId = PlayerId.NewGuid()
    let cmd : Command = CreateGame(gameId, playerId)
    
    // When
    let events : DomainEvent seq = Game.handle state cmd |> Seq.ofList
    
    // Then
    Check.That(events).ContainsExactly({GameId = gameId; CreatedBy = playerId })
    
[<Fact>]
let ``Returns move made event with supplied game id, player id and move player is making the first move of the game``() =
    // Given
    let gameId = GameId.NewGuid()
    let playerId = PlayerId.NewGuid()
    let previousEvents : DomainEvent list = [{GameCreated.GameId = gameId; CreatedBy = playerId }]
    
    let state = recreateGameStateFrom previousEvents 
    let cmd = Play(gameId, playerId, move = Rock)
    
    // When
    let events : DomainEvent seq = Game.handle state cmd |> Seq.ofList
    
    // Then
    Check.That(events).ContainsExactly({MoveMade.GameId = gameId; PlayerId = playerId; Move = Rock })
    
[<Fact>]
let ``Returns move made and game won events when second player beats first player``() =
    // Given
    let gameId = GameId.NewGuid()
    let firstPlayerId = PlayerId.NewGuid()
    let secondPlayerId = PlayerId.NewGuid()
    let previousEvents : DomainEvent list = [{GameCreated.GameId = gameId; CreatedBy = firstPlayerId }
                                             {MoveMade.GameId = gameId; PlayerId = firstPlayerId; Move = Rock }]
    
    let state = recreateGameStateFrom previousEvents 
    let cmd = Play(gameId, secondPlayerId, move = Paper)
    
    // When
    let events : DomainEvent seq = Game.handle state cmd |> Seq.ofList
    
    // Then
    Check.That(events).Contains({GameWon.GameId = gameId; WinnerId = secondPlayerId},
                                {MoveMade.GameId = gameId; PlayerId = secondPlayerId; Move = Paper })
    
[<Fact>]
let ``Returns move made and game tied events when second player makes same move as first player``() =
    // Given
    let gameId = GameId.NewGuid()
    let firstPlayerId = PlayerId.NewGuid()
    let secondPlayerId = PlayerId.NewGuid()
    let previousEvents : DomainEvent list = [{GameCreated.GameId = gameId; CreatedBy = firstPlayerId }
                                             {MoveMade.GameId = gameId; PlayerId = firstPlayerId; Move = Scissors }]
    
    let state = recreateGameStateFrom previousEvents 
    let cmd = Play(gameId, secondPlayerId, move = Scissors)
    
    // When
    let events : DomainEvent seq = Game.handle state cmd |> Seq.ofList
    
    // Then
    Check.That(events).Contains({GameTied.GameId = gameId},
                                {MoveMade.GameId = gameId; PlayerId = secondPlayerId; Move = Scissors })