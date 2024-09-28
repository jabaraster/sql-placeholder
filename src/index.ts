import * as awsConfig from "./aws-exports";
import * as Elm from "./Main.elm";

Elm.Main.init({
  node: document.getElementById("main"),
  flags: {
    awsConfig,
  },
});
