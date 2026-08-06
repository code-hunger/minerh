A proof-of-concept that an interactive game can be written in Haskell.

I recreated part of my favorite mobile game, RoboMiner.

I used this project to learn how to use 
- monad transformers, to encode my program in a type-safe way that only later is evaluated in IO
- the ST (Threaded state) monad, to efficiently create a pure value of a large matrix whose construction requires mutable operations (without having to copy the entire matrix on each wanted mutation)
- existential types, to make a type-safe indexing operation into the grid (following the idea in the justified-containers package). This allows bug-free board indexing that is never out of bounds.

The game has a TTY frontend (simply `cabal run`) and an SDL2 frontend (`cabal run exes -- SDL`).
Controls are arrows/hjkl.

Here's a "screenshot" from the gameplay with the tty frontend:

```
```
```
Stats: moving parts (0), player is Standing (Coord {x = 159, y = 1983})
┌────────────────────────────────────────────────────────────┐
│        🪨    🪜                        🔥🪨              🔥│
│      🪨🪨    🪜              🔥🪨                🪨        │
│              🪜        🪨      🔥          🪨              │
│    🪨🪨      🪜                  🪨    🔥🔥    🪨  🪨🪨🪨🔥│
│        🔥🪨  🪜          🪨  🪨🪨        🪨          🔥    │
│🪨            🪜        🔥🔥          🪨🪨      🔥🪨      🪨│
│          🪨  🪜      🪨      🔥      🔥                🪨  │
│    🔥🔥      🪜                🪨          🪨        🪨  🪨│
│              🪜        🪨🔥      🪨  🪨🪨                  │
│      🪨      🪜              🪨  🪨🪨      🪨🪨  🪨      🪨│
│              🪜          🪨  🪨                            │
│  🔥    🪨    🪜    🪨    🪨            🪨🪨                │
│    🔥        🪜          🪨      🪨  🔥            🪨    🪨│
│🔥  🔥    🪨  🪜    🔥  🪨                          🪨    🔥│
│          🪨  🪜                  🪨  🪨        🪨      🪨🔥│
│              🪜              ◉◉                          🪨│
│              🪜              🪜    🪨          🪨🔥        │
│    🔥🔥      🪜    🪨        🪜      🔥      🪨  🪨  🪨    │
│    🔥🪨🪨    🪜              🪜        🔥              🪨  │
│    🪨        🪜              🪜    🔥  🔥🪨                │
│  🪨          🪜🪜        🔥  🪜        🔥    🪨  🪨🪨  🪨🪨│
│              🪜🪜        🪨  🪜  🪨        🪨            🔥│
│🪨🔥      🪨  🪜🪜    🔥🔥    🪜          🔥    🪨          │
│🪨🔥          🪜🪜            🪜        🪨          🪨      │
│  🪨    🪨    🪜🪜  🪨  🪨  🪜🪜                        🪨  │
│🔥          🪨🪜🪜        🪜🪜🪜          🪜      🔥        │
│🪨          🪨🪜🪜          🪜🪜          🪜    🔥🔥🪨    🪨│
│            🪨🪜🪜            🪜          🪜      🪨  🪨    │
│      🪜🪜🪜🪨🪜🪜            🪜🪜  🪜    🪜          🪨    │
│🪜🪜🪜🪜🪜🪜🪜🪜              🪜🪜🪜  🪨🪨🪜                │
└────────────────────────────────────────────────────────────┘
```
