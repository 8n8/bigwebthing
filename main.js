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
    },
    FirstRealProgram: {
      description: "A little tiny real program.",
      version: 0,
      code: '"hello world" print'
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

  const okFirstNameChar = new Set([
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "_"
  ]);

  const okNotFirstNameChars = new Set([
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "_"
  ]);

  function parseName(code, i, defs) {
    if (!okFirstNameChar.has(code[i])) {
      const errMsg =
        "the first character of a name must be an English letter or an underscore";
      return [i, errMsg, ""];
    }
    let newName = code[i];
    i++;
    while (true) {
      const c = code[i];
      if (c === " ") {
        i++;
        break;
      }
      if (!okNotFirstNameChars.has(c)) {
        const errMsg =
          "characters after the first in a name must be an English letter, a number, or an underscore, and names must be followed by a space";
        return [i, errMsg, ""];
      }
      newName += c;
      i++;
    }

    if (reservedNames.has(newName)) {
      const errMsg = 'name "' + newName + '" is reserved';
      return [i, errMsg, ""];
    }
    return [i, "", newName];
  }

  function parseString(code, elements, p, defs) {
    if (code[p.i] !== '"') {
      p.done = false;
      return;
    }

    let stringEscape = false;
    let partialString = "";

    while (true) {
      const c = code[p.i];
      if (stringEscape) {
        partialString += c;
        stringEscape = false;
        p.i++;
        continue;
      }

      if (c === '"') {
        elements.push(function(s) {
          s.push(partialString);
        });
        p.i++;
        if (code[p.i] !== " " && code[p.i] !== "\n") {
          p.errMsg = "a string must be followed by a space or newline";
          p.done = false;
          return;
        }
      }

      if (c === "\\") {
        stringEscape = true;
        p.i++;
        continue;
      }

      partialString += c;
      p.i++;
    }
  }

  const reservedNames = new Set(["def"]);

  function parseDef(code, elements, p, defs) {
    if (code.slice(p.i, p.i + 4) !== "def ") {
      p.done = false;
      return;
    }
    p.i += 4;
    const [newI, newErrMsg, newName] = parseName(code, p.i, defs);
    p.i = newI;
    if (newErrMsg) {
      p.errMsg = newErrMsg;
      p.done = false;
      return;
    }
    if (newName in defs) {
      p.errMsg = 'name "' + newName + '" has already been used';
      p.done = false;
      return;
    }

    if (code[p.i] !== "{") {
      p.errMsg = 'expecting "{"';
      p.done = false;
      return;
    }
    p.i++;

    while (true) {
      p.i = parseZeroOrMoreSpaces(code, p.i);

      parseString(code, elements, p, defs);
      if (p.done) {
        continue;
      }
      if (p.errMsg) {
        return;
      }

      parseDef(code, elements, p, defs);
      if (p.done) {
        continue;
      }
      if (p.errMsg) {
        return;
      }

      parseRetrieve(code, elements, p, defs);
      if (p.done) {
        continue;
      }
      if (p.errMsg) {
        return;
      }

      if (code[p.i] === "}") {
        p.i++;
        return;
      }

      if (!p.done) {
        p.errMsg = "expecting blah, blah or blah";
        p.done = false;
        return;
      }
    }
  }

  function parseRetrieve(code, elements, p, defs) {
    const [newI, newErrMsg, name] = parseName(code, p.i, defs);
    p.i = newI;
    if (newErrMsg) {
      p.errMsg = newErrMsg;
      p.done = false;
      return;
    }
    const lookedUpCode = defs[name];
    if (!lookedUpCode) {
      p.errMsg = 'could not find name "' + name + '"';
      p.done = false;
      return;
    }
    elements.concat(lookedUpCode);
    if (code[p.i] !== " ") {
      p.errMsg = "expecting space";
      p.done = false;
    }
  }

  function parseZeroOrMoreSpaces(code, i) {
    while (true) {
      if (code[i] === " " || code[i] === "\n") {
        i++;
        continue;
      }
      return i;
    }
  }

  function parser(code, elements, p, defs) {
    const codeLen = code.length;
    while (p.i <= codeLen) {
      p.i = parseZeroOrMoreSpaces(code, p.i);

      parseString(code, elements, p, defs);
      if (p.done) {
        continue;
      }
      if (p.errMsg) {
        return;
      }

      parseDef(code, elements, p, defs);
      if (p.done) {
        continue;
      }
      if (p.errMsg) {
        return;
      }

      parseRetrieve(code, elements, p, defs);
      if (p.done) {
        continue;
      }
      if (p.errMsg) {
        return;
      }

      if (!p.done) {
        p.errMsg = "expecting blah, blah or blah";
        return;
      }
    }
  }

  const standardLibrary = {
    print: function(s) {
      console.log(s[-1]);
      s = s.slice(0, -1);
    }
  };

  function compile(code) {
    const elements = [];
    const [, err] = parser(code, elements, 0, standardLibrary);
    if (err) {
      console.log(err);
      return;
    }
    const stack = [];
    const numElements = elements.length;
    for (let i = 0; i < numElements; i++) {
      elements[i](stack);
    }
    return stack[0];
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
