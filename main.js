import { Elm } from './src/Main.elm';

// Start Elm
var div = document.getElementById('main');
const app = Elm.Main.init({
  node: div,
  flags: {}
  }
);
