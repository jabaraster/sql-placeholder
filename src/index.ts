import { Elm } from "./Main.elm";
import { format } from "sql-formatter";

const app = Elm.Main.init({
  node: document.getElementById("main"),
});

app.ports.formatSql.subscribe((src: string) => {
  try {
    const formatted = format(src);
    app.ports.receiveFormattedSql.send(formatted);
  } catch (e) {
    console.log(e);
  }
});
