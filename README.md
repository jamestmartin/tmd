# tmd
A Minecraft server implementation in Haskell.

"tmd" doesn't stand for anything in particular.
It's just an homage to the Minecraft server I used to run.

## Project Status
I'm not currently working on this project because I got bored of Minecraft (again).
However, I'm leaving it up because some of it could still be valuable if I try to make a Minecraft server again in the future.
I will be archiving the repository in the mean time.

Please do not judge me based on this code.
It was an experiment with dependently-typed Haskell; I would not write real-world code this way.
It's not even *good* dependently-typed Haskell, and I acknowledge that.

Also, the current master commit is broken. The most recent working commit was `b000b6c`.

## Current Features
* Supports protocol version 498 (Minecraft 1.14.4).
* Responds to status requests and pings by modern Minecraft clients.
* Accepts login handshakes, although the server immediately responds with a Disconnect packet and closes the connection.
