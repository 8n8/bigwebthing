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

  function parser(code, elements) {
    let partialString = "";
    let stringEscape = false;
    let parsingString = false;
    const codeLen = code.length;
    for (let i = 0; i < codeLen; i++) {
      const c = code[i];

      // String parser
      if (parsingString) {
        if (stringEscape) {
          partialString += c;
          stringEscape = false;
          continue;
        }
        if (c === '"') {
          elements.push(function(s) { s.push(partialString) })
          parsingString = false
          partialString = ""
          continue
        }
        if (c === "\\") {
          stringEscape = true
          continue
        }
      }
      if (c === '"') {
        parsingString = true
        continue
      }

      // Function parser
      if (c === 'd') {
        if (code[i+1] === "e") {
          if (code[i+2] === "f") {
          }
        }
      }

    }
  }

  function compile(code) {
    const elements = [];
    const err = parser(code, elements);
    if (err) {
      return prettyError(err);
    }
    const stack = [];
    const numElements = elements.length;
    for (let i = 0; i < numElements; i++) {
      elements[i](stack);
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
