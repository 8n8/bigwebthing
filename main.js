(function() {
  "use strict";

  const programsDebug = {
    FirstRealProgram: {
      description: "A little tiny real program.",
      version: 0,
      code: '{ "hello world!" Print ! } def Hello hello !',
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

  function parseRunBlock(code, ns, elfs, elts, p) {
    if (code[p.i] !== "!") {
      p.done = false;
      return;
    }
    p.done = true;
    p.i++;
    elts.push(function(dets, typestack) {
      if (typestack.length === 0) {
        return "there's nothing on the stack";
      }
      const blockCandidate = typestack.pop(); 
      if (!Array.isArray(blockCandidate)) {
        return "the top item on the stack is not an array";
      }
      const err = runTypeCheck(blockCandidate, dets, typestack)
      return err
    })
    elfs.push(function(defs, progStack) {
      const blockCandidate = progStack.pop();
      runProgram(blockCandidate, defs, progStack)
    })
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
        return [i, "", newName.toLowerCase()];
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
    return [i, "", newName.toLowerCase()];
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
    const str = parseStringHelper(code, p);
    if (!p.done) {
      return;
    }
    elts.push(function(dets, typeStack) {
      typeStack.push(stringTypeConst); 
    });
    elfs.push(function(defs, progStack) {
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
    const fullName = makeFullName(ns, newName);
    elts.push(eltOpDef(ns, newName));
    elfs.push(function(defs, stack) {
      defs[fullName] = stack.pop();
    });
  }

  function eltOpDef(ns, newname) {
    return function(dets, typestack) {
      const fullname = makeFullName(ns, newname);
      if (typestack.length === 0) {
          return 'you need to put something on the stack before a ' +
              '"def"';
      }
      if (dets[fullname]) {
          return 'multiple definitions of name "' + newname + '"';
      }
      dets[fullname] = typestack.pop()
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

        elts.push(function(dets, typestack) {
          typestack.push(blockElts);
        });
        elfs.push(function(defs, progStack) {
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
    parseRunBlock(code, ns, elfs, elts, p);
    if (p.done || p.errMsg) {return;}

    parseString(code, elfs, elts, p)
    if (p.done || p.errMsg) { return; }

    parseDef(code, ns, elfs, elts, p)
    if (p.done || p.errMsg) { return; }

    parseProgramBlock(code, ns, elfs, elts, p)
    if (p.done || p.errMsg) { return; }

    parseRetrieve(code, ns, elfs, elts, p);
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
    const fullName = makeFullName(ns, name);
    elts.push(function(dets, typestack) {
      if (!dets[fullName]) {
        return 'no definition "' + name + '"';
      }
      const lookedUpTypes = dets[fullName]
      if (!lookedUpTypes) {
        return 'could not find types for name "' + name + '"';
      }
      typestack.push(lookedUpTypes);
    })
    elfs.push(function(defs, progStack) {
      progStack.push(defs[fullName]);
    })
  }

  function makeFullName(ns, name) {
    if (!ns) {
      return name
    }
    return ns + "." + name;
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

  function parser(code, elts, elfs) {
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
    return function(dontCare1, progStack) {
      const div = document.getElementById(progDivId);
      div.innerHTML = "";
      const txt = document.createTextNode(progStack.pop());
      div.appendChild(txt);
    };
  }

  function standardLibrary(progDivId) {
    return { print: [slPrint(progDivId)] };
  }

  const stringTypeConst = 0

  function prettyErr(errMsg, progDivId) {
    const div = document.getElementById(progDivId);
    div.innerHTML = "";

    const pcode = document.createElement("p");
    const txtcode = document.createTextNode(errMsg);
    pcode.appendChild(txtcode);
    div.appendChild(pcode);
  }

  function compile(code, progDivId) {
    let elts = []
    let elfs = []
    parser(code, elts, elfs);
    
    const dets = standardTypes();
    let typeStack = [];
    const errMsg = runTypeCheck(elts, dets, typeStack)
    if (errMsg) {
      prettyErr(errMsg, progDivId);
      return;
    }
    if (typeStack.length !== 0) {
      prettyErr("the type stack should be empty at the end of the program, but it has this left in it:\n" + typeStack, progDivId)
    }

    const defs = standardLibrary(progDivId)
    let progStack = []
    runProgram(elfs, defs, progStack);
  }
  
  function runProgram(elfs, defs, progStack) {
    for (let i = 0; i < elfs.length; i++) {
      elfs[i](defs, progStack);
    }
  }

  function runTypeCheck(elts, dets, typeStack) {
    const numElts = elts.length;
    for (let i = 0; i < numElts; i++) {
      let errMsg = elts[i](dets, typeStack);
      if (errMsg) {
        return errMsg;
      }
    }
  }

  function standardTypes() {
    return {
      str: stringType,
      print: [printType],
      pop: [popType],
    }
  }

  function popType(dets, typestack) {
    if (typestack.length === 0) {
      return '"pop" failed because there was nothing on the stack'
    }
    typestack.pop()
  }

  function stringType(dets, typestack) {
    typestack.push(stringTypeConst)
  }

  function printType(dets, typestack) {
    if (typestack.length === 0) {
      return '"print" needs something on the stack to print'
    }
    const top = typestack.pop()
    if (top !== stringTypeConst) {
        return '"print" requires the top of the stack to be a string'
    }
  }

  const divIdEnd = "ProgramDiv";
  const subDivEnd = "SubDiv";

  // The 'gui' parameter is a description of the current view in
  // the GUI. It is an object, and always has a 'page' field, which
  // is 'choose' for the program chooser page or 'run' for the
  // program running page.
  function render(gui) {
    if (gui.page === "choose") {
      renderChoose(gui)
    }
    if (gui.page === "run") {
      renderRun(gui)
    }
  }

  function runtime() {
    while (true) {
    }
  }

  function makeProgramDiv(childId, parentId, program) {
    const programDiv = document.createElement("div");
    const progDivId = document.createAttribute("id");
    progDivId.value = childId;
    programDiv.setAttributeNode(progDivId);
    document.getElementById(parentId).appendChild(programDiv);
    compile(program.code, childId);
  }

  function makeProgramMenuItem(name, program) {
    const div = document.createElement("DIV");
    const divId = document.createAttribute("id");
    const parentId = name + divIdEnd;
    divId.value = parentId;
    div.setAttributeNode(divId);
    const button = document.createElement("BUTTON");
    button.onclick = function() {
      const programId = name + subDivEnd;
      const toRemove = document.getElementById(programId);
      if (toRemove) {
        toRemove.remove();
      } else {
        makeProgramDiv(programId, parentId, program)
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
