// A repl for communicating with an Elm Platform.worker program
// See README for installaion directions
// Sample session:
//    $ node run/repl.js
//    > 33
//    100
//
// The worker applied a mystery function the input
// 33 to produce the output 100

// Main.elm has two ports:
//
//     port get : (InputType -> msg) -> Sub msg
//     port put : OutputType -> Cmd msg
//
// The first port (get) listens for data from
// the Javascript program (here!).  Then second port
// sends data from the Elm to the Javascript program.

const repl = require('repl');

// Link to Elm code
var Elm = require('./main').Elm;
var main = Elm.Main.init();

var prev;

//var prev2;

// Eval function for the repl
// args: command, context, filename, callback
function eval(input, _, __, callback) {
  if (prev) { main.ports.put.unsubscribe(prev) }
  prev = function putCallback (data) { callback(null, data) }
  main.ports.put.subscribe(prev)
  //if (prev2) { main.ports.wait.unsubscribe(prev2) }
  //prev2 = function putCallback2 (data) {
  //    setTimeout(function() {callback(null, data)},1000)
  //}
  //main.ports.wait.subscribe(prev2)
  main.ports.get.send(Number(input))
}

main.ports.loopback.subscribe(function(data) {
   main.ports.get.send(data)
});

repl.start({ prompt: '> ', eval: eval });


