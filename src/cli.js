
const repl = require('repl');

// Link to Elm code
var Elm = require('./main').Elm;
var main = Elm.Main.init();

// Eval function for the repl
function eval(cmd, _, _,  callback) {
  main.ports.put.subscribe(
    function putCallback (data) {
      main.ports.put.unsubscribe(putCallback)
      callback(null, data)
    }
  )
  main.ports.get.send(cmd)
}

function myWriter(output) {
  return output
}

console.log("\nType 'h' for help\n")

repl.start({ prompt: '> ', eval: eval, writer: myWriter});
