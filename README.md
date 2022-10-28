# Stream Transformation in Elm

This project tests out some ideas for stream transformation--my
ultimate goal is to contribute to kmonad. Elm makes testing ideas easy
because it comes with a builtin UI: you can initialize some state
(`init`) and define a function (`update`) that changes that state and
produces some output each time an event is received.

Note: Elm builds to JS and therefore is typically used for web
development. However, by passing events to and from a node.js repl,
you can easily use Elm for a simple, command-line type
applicatioin. For this project I built directly off of a template
project called Platform.worker written by @jxxcarlson to demonstrate
the propensity for Elm to be used in a command-line scenario.

## To run

```
$ sh make.sh
$ node src/repl.js
```

## What I'm Trying Out

The primary idea I wanted to try was modeling transformation as a
stack of user-defined layers. At first I imagined that each layer
could itself manipulate the stack, in addition to being able to emit
events to other layers for propogation up (down, through, etc) the
stack. At this point I have started with a more manageable model where
each layer manages its own piece of state and emits events.

If I continue with this idea, I have some ideas as to how I might
allow stack layers to manipulate the rest of the stack:

- Construct an algebra of state operations and allow the layer to emit
  a list of state operations

- Pass the entire layer stack to each layer and allow direct manipulation

An interesting loop arises if you want allow a layer to directly
"replace itself" like so:

`Event -> State -> (List Event, (Event -> State -> (List Event, ...))`

An infinite type!

## Sending event back through the stack

I don't have a rigorous definition for what requirements a "stream
transformer" API would have to meet in order to allow any arbitrary
transformation. That being said, I have a hunch that under my
layer model such an API would have to allow a layer to send events
back to itself. There are a few ways to do this "loopback" of events:

- using ports through the JS runtime: Elm sends an event through a
  special port, and JS initializes a dedicated listener that passes
  all events from this port back through the standard port to Elm

- using the Elm runtime: output a `Cmd Msg` in `update` that will be
  received by `update` shortly. see
  https://medium.com/elm-shorts/how-to-turn-a-msg-into-a-cmd-msg-in-elm-5dd095175d84
  for more

- direct recursion: while running `update`, invoke `update` again

## Thoughts about Elm

The way I'm using Elm, it's basically just worse Haskell. No pattern
matching on arguments, no typeclasses, no monad transformers...