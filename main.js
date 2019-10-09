(function() {
  "use strict";
  const programsDebug = {
    Clocker: {
      description:
        "A sample app for clocking times. It doesn't actually work: this is just a placeholder app for debugging.",
      version: 1,
      code: "Some code goes here but the language hasn't been invented yet."
    },
    Messenger: {
      description:
        "This app will be for sending text messages around, but remember that this is just a placeholder app.",
      version: 0,
      code: "A bit of code."
    },
    Truelang: {
      description:
        "Truelang is the built-in programming language of BigWebThing.",
      version: 0,
      code: "It doesn't exist yet."
    }
  };

  function makeProgramMenuDiv(programName, program) {
    const div = document.createElement("DIV");
    const name = document.createElement("H2");
    const nameText = document.createTextNode(programName);
    name.appendChild(nameText);
    div.appendChild(name);
    const version = document.createElement("P");
    const versionText = document.createTextNode("Version " + program.version);
    version.appendChild(versionText);
    div.appendChild(version);
    const description = document.createElement("P");
    const descriptionText = document.createTextNode(program.description);
    description.appendChild(descriptionText);
    div.appendChild(description);
    return div;
  }

  function parseString(code, elements, line, col, i) {
  }

  function parser(code) {
    // let err = ""
    // while true {
    //     if (!parseString(s)) {continue}
    //     
    //     ir (s.code[parserState.i] === '"') {
    //         s.col++
    //         s.i++
    //         const err = parseString(s)
    //         if (err) {
    //           return err
    //         }
    //     }
    // }
    for (const i = 0; i < codeLen; i++) {
      // Increment position.
      const c = code[i];
      if (c === "\n") {
        line += 1;
      } else {
        column += 1;
      }

      // String parser
      if (parsingString) {
          
      }
      if (stringEscape) {
        partialString += c;
        stringEscape = false;
        continue;
      }
      if (c === '"') {
        if (parsingString) {
          elements.push(function(s) {
            s.append(partialString);
          });
          parsingString = false;
          partialString = "";
          continue;
        }
        parsingString = true;
        continue;
      }
      if (parsingString) {
        if (c === "\\") {
          stringEscape = true;
          continue;
        }
        partialString += c;
        continue;
      }


    }
    return elements;
  }

  function compile(code) {
    const s = {
      code: code,
      elements: [],
      line: 0,
      col: 0,
      i: 0}
    const err = parser(s);
    if (err) {
      return prettyError(err, s.line, s.col)
    } 
    let stack = [];
    const numElements = parserState.elements.length;
    for (const i = 0; i < numStatements; i++) {
      parserState.elements[i](stack);
    }
    return stack[0];
    // for
    //   const div = document.createElement('div')
    //   const txt = document.createTextNode(code)
    //   div.appendChild(txt)
    //   return div
  }

  const divIdEnd = "ProgramDiv";

  function makeProgramMenuItem(name, program) {
    const div = document.createElement("DIV");
    const divId = document.createAttribute("id");
    const idName = name + divIdEnd;
    divId.value = idName;
    div.setAttributeNode(divId);
    const button = document.createElement("BUTTON");
    const programDiv = compile(program.code);
    const progDivClass = document.createAttribute("id");
    progDivClass.value = name + "progDiv";
    programDiv.setAttributeNode(progDivClass);
    button.onclick = function() {
      const ref = name + "progDiv";
      const toRemove = document.getElementById(ref);
      if (toRemove) {
        toRemove.remove();
      } else {
        document.getElementById(idName).appendChild(programDiv);
      }
    };
    button.appendChild(makeProgramMenuDiv(name, program));
    div.appendChild(button);
    return div;
  }

  function afterRetrievingPrograms(programs) {
    for (const [name, program] of Object.entries(programs)) {
      const progButton = makeProgramMenuItem(name, program);
      document.body.appendChild(progButton);
    }
  }

  function main() {
    localforage.getItem("programs").then(afterRetrievingPrograms);
  }

  localforage.setItem("programs", programsDebug).then(main);
})();
