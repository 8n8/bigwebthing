(function() {
  "use strict";

  const programsDebug = {
    FirstRealProgram: {
      description: "A little tiny real program.",
      version: 0,
      code: '{ "h" print } def hello hello',
      types: '() def hello'
    }
  };

  function makeProgramMenuDiv(programName, program) {
    const div = document.createElement("DIV");
    const name = document.createElement("H2");
    const nameText = document.createTextNode(programName);
    name.appendChild(nameText);
    div.appendChild(name);
    const version = document.createElement("P");
    const versionText = document.createTextNode(
        "Version " + program.version);
    version.appendChild(versionText);
    div.appendChild(version);
    const description = document.createElement("P");
    const descriptionText = document.createTextNode(
        program.description);
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

  function parseName(code, i) {
    if (!okFirstNameChar.has(code[i])) {
      const errMsg =
        "the first character of a name must be an English letter or an underscore";
      return [i, errMsg, ""];
    }
    let newName = code[i];
    i++;
    const codeLen = code.length;
    while (true) {
      const c = code[i];
      if (c === " ") {
        i++;
        break;
      }
      if (codeLen === i) {
        return [i, "", newName];
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

  function parseStringHelper(code, p) {
    p.done = true;
    if (code[p.i] !== '"') {
      p.done = false;
      return;
    }
    p.i++;

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
        p.i++;
        return partialString;
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

  function parseString(code, elfs, elts, p) {
    str = parseStringHelper(code, p);
    if (!p.done) {
      return;
    }
    elts.push(function(dets, created, typeStack) {
      typeStack.push(stringType); 
    });
    elfs.push(function(defs, progStack, progDivId) {
      progStack.push(str);
    });
  }

  const reservedNames = new Set(["def"]);

  function parseDef(code, ns, elfs, elts, p) {
    p.done = true;
    if (code.slice(p.i, p.i + 4) !== "def ") {
      p.done = false;
      return;
    }
    p.i += 4;
    const [newI, newErrMsg, newName] = parseName(code, p.i);
    p.i = newI;
    if (newErrMsg) {
      p.errMsg = newErrMsg;
      p.done = false;
      return;
    }
    const fullName = ns + "." + newName;
    elts.push(eltOpDef(ns, newName));
    elfs.push(function(defs, stack, progDivId) {
      defs[fullName] = stack.pop();
    });
  }

  function parseDet(code, ns, elts, p) {
    p.done = true;
    if (code.slice(p.i, p.i + 4) !== "det ") {
      p.done = false;
      return;
    }
    p.i += 4;
    const [newI, newErrMsg, newName] = parseName(code, p.i);
    p.i = newI;
    if (newErrMsg) {
      p.errMsg = newErrMsg;
      p.done = false;
      return;
    }
    const fullName = ns + "." + newName;
    elts.push(function(dets, created, typeStack) {
      if (typeStack.length === 0) {
          return 'you need to put a type block on the stack ' +
              'before a det'
      } 
      if (dets[fullName]) {
          return 'multiple definitions of name "' + newName + '"';
      }
      dets[fullName] = typeStack.pop();
    });
  }

  function eltOpDef(ns, newName) {
    return function(dets, created, typeStack) {
      const fullName = ns + "." + newName;
      const funType = dets[fullName];
      if (!funType) {
        return "there is no type defined for \"" + newName + "\"";
      }
      if (typeStack.length === 0) {
          return 'you need to put something on the stack before a ' +
              '"def"';
      }
      if (created.has(fullName)) {
          return 'multiple definitions of name "' + newName + '"';
      }
      const topType = typeStack.pop();
      if (funType !== topType) {
        return ("type mismatch: the type of the top item on the " +
          "stack is\n" + topType + "\n, but the type defined for \"" +
                newName + "\" is\n" + funType);
      }
      created.add(fullName);
      return "";
    };
  }

  function parseProgramBlock(code, ns, elfs, elts, p) {
    p.done = true;
    if (code[p.i] !== "{") {
      p.done = false;
      return;
    }
    p.i++;

    let blockElts = [];
    let blockElfs = [];

    while (true) {
      p.i = parseZeroOrMoreSpaces(code, p.i);
      if (code[p.i] === "}") {
        p.i++;
        p.done = true;

        elts.push(function(dets, created, typeStack) {
          typeStack.push(blockElts);
        });
        elfs.push(function(defs, progStack, progDivId) {
          progStack.push(blockElfs);
        });
        return;
      }

      parseProgramElement(code, ns, blockElfs, blockElts, p);
      if (p.done) {
        continue;
      }
      if (p.errMsg) {
        return;
      }

      if (!p.done) {
        p.errMsg = "error in block: expecting blah, blah or blah";
        p.done = false;
        return;
      }
    }
  }

  function parseProgramElement(code, ns, elfs, elts, p) {
    parseRetrieve(code, ns, elfs, elts, p);
    if (p.done || p.errMsg) { return; }

    parseString(code, elfs, elts, p)
    if (p.done || p.errMsg) { return; }

    parseDef(code, ns, elfs, elts, p)
    if (p.done || p.errMsg) { return; }

    parseDet(code, ns, elts, p)
    if (p.done || p.errMsg) { return; }

    parseProgramBlock(code, ns, elfs, elts, p)
    if (p.done || p.errMsg) { return; }

    if (!p.done) {
      p.errMsg = "parseProgramElement: expecting blah blah blah"
    }
  }

  function parseRetrieve(code, ns, elfs, elts, p) {
    const [newI, newErrMsg, name] = parseName(code, p.i);
    if (newErrMsg) {
      p.errMsg = newErrMsg;
      p.done = false;
      return;
    }
    p.done = true;
    p.i = newI;
    const fullName = ns + "." + name;
    elts.push(function(dets, created, typestack) {
      if (!created.has(fullName)) {
        return 'no definition "' + name + '"';
      }
      const lookedUpTypes = dets[fullName]
      if (!lookedUpTypes) {
        return 'could not find types for name "' + name + '"';
      }
      typestack.push(lookedUpTypes);
    })
    elfs.push(function(defs, progStack, progDivId) {
      progStack.push(defs[fullName]);
    })
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

  function parseTypes(code, elts) {
    const p = { done: true, i: 0, errMsg: "" };
    const codeLen = code.length;
    while (p.i < codeLen) {
      p.i = parseZeroOrMoreSpaces(code, p.i);
      parseTypeElement(code, "", elts, p)
      if (p.done) {
        continue;
      }
      return;
    }
  }

  function parser(code, types, elts, elfs) {
    const p = { done: true, i: 0, errMsg: "" };
    const codeLen = code.length;
    while (p.i < codeLen) {
      p.i = parseZeroOrMoreSpaces(code, p.i);
      parseProgramElement(code, "", elfs, elts, p)
      if (p.done) {
          continue;
      }
      return;
    }
  }

  function slPrint(progDivId) {
    return function(s) {
      const div = document.getElementById(progDivId);
      div.innerHTML = "";
      const txt = document.createTextNode(s.pop());
      div.appendChild(txt);
    };
  }

  function standardLibrary(progDivId) {
    return { print: [slPrint(progDivId)] };
  }

  const stringType = 0

  const standardTypes = { print: [[stringType], []] }

  function prettyErr(code, err, i, progDivId) {
    const div = document.getElementById(progDivId);
    div.innerHTML = "";

    const pcode = document.createElement("p");
    const txtcode = document.createTextNode(errMsg);
    pcode.appendChild(txtcode);
    div.appendChild(pcode);
  }

  function parseTypeString(code, elts, p) {
    str = parseStringHelper(code, p);
    if (!p.done) {
      return;
    }
    elts.push(function(dets, created, typestack) {
      typestack.push(str);
    });
  }

  function parseDet(code, ns, elts, p) {
    p.done = true;
    if (code.slice(p.i, p.i + 4) !== "def ") {
      p.done = false;
      return;
    }
    p.i += 4;
    const [newI, newErrMsg, newName] = parseName(code, p.i);
    p.i = newI;
    if (newErrMsg) {
      p.errMsg = newErrMsg;
      p.done = false;
      return;
    }
    const fullName = ns + "." + newName;
    elts.push(function(dets, created, typestack) {
      if (dets[fullName) {
        return 'multiple definitions of name "' + newName + '"';
      }
      if (typestack.length === 0) {
        return 'you need to put something on the stack before a ' +
            '"det"';
      }
    });
  }

  function parseTypeBlock(code, ns, elts, p) {
    p.done = true;
    if (code[p.i] !== "{") {
      p.done = false;
      return;
    }
    p.i++;

    let blockElts = [];
  
    while (true) {
      p.i = parseZeroOrMoreSpaces(code, p.i);]
      if (code[p.i] === "}") {
        p.i++;
        p.done = true;

        elts.push(function(dets, created, typestack) {
          typestack.push(blockElts);
        })
        return;
      }

      parseTypeElement(code, ns, elts, p);
      if (p.done) {
        continue;
      }
      if (p.errMsg) {
        return;
      }

      if (!p.done) {
        p.errMsg = "error in type block: expecting blah or blah";
        p.done = false;
        return;
      }
    }
  }

  function parseTypeElement(code, ns, elts, p) {
    parseTypeRetrieve(code, ns, elts, p)
    if (p.done || p.errMsg) {return;}

    parseTypeString(code, elts, p);
    if (p.done || p.errMsg) {return;}

    parseDet(code, ns, elts, p);
    if (p.done || p.errMsg) {return;}

    parseTypeBlock(code, ns, elts, p);
    if (p.done || p.errMsg) {return;}

    if (!p.done) {
      p.errMsg = "parseTypeElement: expecting blah blah blah"
    }
  }

  function parseTypeRetrieve(code, ns, elts, p) {
    const [newI, newErrMsg, name] = parseName(code, p.i);
    if (newErrMsg) {
      p.errMsg = newErrMsg;
      p.done = false;
      return;
    }
    p.done = true;
    p.i = newI;
    const fullName = ns + "." + name;
    elts.push(function(dets, created, typestack) {
      const lookedUpTypes = dets[fullName];
      if (!lookedUpTypes) {
        return 'could not find type definition "' + name + '"';
      }
      typestack.push(lookedUpTypes);
    })
  }

  function compile(code, types, progDivId) {
    let elts = []
    let elfs = []
    parseTypes(types, elts)
    parser(code, types, elts, elfs);
    
    errMsg = runTypeCheck(elts)
    if (errMsg !== "") {
      prettyErr(errMsg);
      return;
    }

    runProgram(elfs, progDivId);
  }
  
  function runProgram(elfs) {
    const defs = {};
    let progStack = [];
    for (let i = 0; i < elfs.length; i++) {
      elfs[i](defs, progStack, progDivId);
    }
  }

  function runTypeCheck(elts) {
    const created = new Set([]);
    const dets = {};
    let typeStack = [];
    const numElts = elts.length;
    for (let i = 0; i < numElts; i++) {
      let errMsg = elts[i](dets, created, typeStack);
      if (errMsg) {
        return errMsg;
      }
    }
  }

  const divIdEnd = "ProgramDiv";
  const subDivEnd = "SubDiv";

  function makeProgramMenuItem(name, program) {
    const div = document.createElement("DIV");
    const divId = document.createAttribute("id");
    const idName = name + divIdEnd;
    divId.value = idName;
    div.setAttributeNode(divId);
    const button = document.createElement("BUTTON");
    button.onclick = function() {
      const ref = name + subDivEnd;
      const toRemove = document.getElementById(ref);
      if (toRemove) {
        toRemove.remove();
      } else {
        const programDiv = document.createElement("div");
        const progDivId = document.createAttribute("id");
        const progIdName = name + subDivEnd;
        progDivId.value = progIdName;
        programDiv.setAttributeNode(progDivId);
        document.getElementById(idName).appendChild(programDiv);
        compile(program.code, program.types, progIdName);
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