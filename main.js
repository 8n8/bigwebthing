(function() {
  "use strict";

  let domIdIota = 0
  let blobIdIotaGlobal = 0

  function makeProgramStartButtonContents(programName, program) {
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

  function typeArrayEq(a, b) {
    const lena = a.length;
    if (lena !== b.length) {
      return false;
    }
    for (let i = 0; i < lena; i++) {
       
      if (!typeEq(a[i], b[i])) {
        return false;
      }
    }
    return true;
  }

  function typeEq(a, b) {
    if (Array.isArray(a)) {
      return b === BLOCKTYPE;
    }
    if (Array.isArray(b)) {
      return a === BLOCKTYPE;
    }
    if (typeof a === 'object') {
      if (typeof b !== 'object') {
        return false;
      }
      if (a.container !== b.container) {
        return false;
      }
      if (!typeEq(a.inside, b.inside)) {
        return false;
      }
      return true
    }
    if (typeof a === 'function') {
      return b === BLOCKTYPE;
    }
    if (typeof b === 'function') {
      return a === BLOCKTYPE;
    }
    return a === b;
  }

  function parseTypeCheck(code, ns, elfs, elts, p) {
    p.done = true;
    if (code[p.i] !== '<') {
      p.done = false;
      return;
    }
    p.i++;
    let partial = false;
    if (code.slice(p.i, p.i+2) === '..') {
      partial = true;
      p.i = p.i + 2;
    }
    let types = [];
    while (true) {
      p.i = parseZeroOrMoreSpaces(code, p.i);
      if (code[p.i] === '>') {
        const i = p.i;
        elts.push(function(dets, typestack) {
          const [line, col] = lineCol(code, i);
          let toCompare = typestack;
          if (partial) {
            toCompare = typestack.slice(
              typestack.length - types.length,
              typestack.length);
          }
          if (!typeArrayEq(types, toCompare)) {
            return (
              prettyPos(i, code) + '\n' +
              'typestack does not match type declaration:\n' +
              'expecting ' + types + '\n' +
              'but got ' + typestack);
          }
          return '';
        })
        p.i++;
        return;
      }
      const type = parseType(code, p);
      if (!p.done || p.errMsg) { return; }

      types.push(type);

      p.i = parseZeroOrMoreSpaces(code, p.i);
      if (code[p.i] !== ',') {
        p.done = false;
        p.errMsg = 'All elements in type declaration must end ' +
          'with a comma.';
        return;
      }
      p.i++;
    }
  }

  function parseType(code, p) {
    p.done = true;
    if (code.slice(p.i, p.i + 3) === 'str') {
      p.i = p.i + 3;
      return STRTYPE;
    }

    if (code.slice(p.i, p.i + 5) === 'block') {
      p.i = p.i + 5;
      return BLOCKTYPE;
    }

    if (code.slice(p.i, p.i + 2) === '[]') {
      p.i = p.i + 2;
      const type = parseType(code, p);
      return {container: LISTTYPE, inside: type};
    }

    p.done = false;
    p.errMsg = 'no matching type';
  }
 
  function displayLeftText(name, docName, parentDiv, rightId) {
    localforage.getItem(docName).then(function (doc) {
      const id = domId();
      const textBox = textBoxHelp(id);
      parentDiv.appendChild(textBox);
      textBox.value = doc;
      parentDiv.appendChild(textBox);

      const saveButton = document.createElement('button'); 
      saveButton.innerHTML = 'Save';
      saveButton.className = 'saveButton';
      parentDiv.appendChild(saveButton);

      saveButton.onclick = function() {
        localforage.setItem(docName, textBox.value)
      }

      const runButton = document.createElement('button');
      runButton.onclick = function() {
        localforage.getItem('programs').then(function(programs) {
          const program = programs[name];
          updateRightDoc(program, rightId);
        })
      }
      runButton.innerHTML = 'Run';
      runButton.className = 'runButton';
      parentDiv.appendChild(runButton);
    })
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

  const okNonFirstNameChars = new Set([
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

  function parseRetrieveName(code, i) {
    return parseNameHelp(
      code, i, (new Set(okNonFirstNameChars)).add('.'));
  }

  function parseMakeName(code, i) {
    return parseNameHelp(code, i, okNonFirstNameChars);
  }

  function parseNameHelp(code, i, okNonFirstChars) {
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
      if (c === " " || c === "\n") {
        i++;
        break;
      }
      if (codeLen === i) {
        return [i, "", newName.toLowerCase()];
      }
      if (!okNonFirstChars.has(c)) {
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
      typeStack.push(STRTYPE); 
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
    const [newI, newErrMsg, newName] = parseMakeName(code, p.i);
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
      if (fullname in dets) {
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

  function parseLineComment(code, p) {
    if (code.slice(p.i, p.i+2) !== '//') {
      p.done = false;
      return;
    }
    p.i = p.i + 2;
    const codeLen = code.length;
    while (p.i < codeLen) {
      if (code[p.i] === '\n') {
        p.i++;
        p.done = true;
        return;
      }
      p.i++;
    }
  }

  function parseBlockComment(code, p) {
    if (code.slice(p.i, p.i+2) !== '/*') {
      p.done = false
      return;
    }
    p.i = p.i + 2;
    const codeLen = code.length;
    while (p.i < codeLen) {
      if (code.slice(p.i, p.i+2) === '*/') {
        p.i = p.i + 2;
        p.done = true;
        return;
      }
      p.i++;
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

    parseLineComment(code, p)
    if (p.done || p.errMsg) { return; }

    parseBlockComment(code, p)
    if (p.done || p.errMsg) { return; }

    parseTypeCheck(code, ns, elfs, elts, p)
    if (p.done || p.errMsg) { return; }

    parseRetrieve(code, ns, elfs, elts, p);
    if (p.done || p.errMsg) { return; }

    if (!p.done) {
      p.errMsg = "parseProgramElement: expecting blah blah blah"
    }
  }

  function parseRetrieve(code, ns, elfs, elts, p) {
    const [newI, newErrMsg, name] = parseRetrieveName(code, p.i);
    if (newErrMsg) {
      p.errMsg = newErrMsg;
      p.done = false;
      return;
    }
    p.done = true;
    p.i = newI;
    const fullName = makeFullName(ns, name);
    const i = p.i;
    elts.push(function(dets, typestack) {
      if (!(fullName in dets)) {
        return (
          prettyPos(i, code) + '\n' +
          'no definition "' + name + '"');
      }
      typestack.push(dets[fullName]);
    })
    elfs.push(function(defs, progStack) {
      progStack.push(defs[fullName]);
    })
  }

  function prettyPos(i, code) {
    const [line, col] = lineCol(code, i);
    return (
      'line ' + line + '\n' +
      'col  ' + col)
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

  function parser(code, elts, elfs, p) {
    const codeLen = code.length;
    while (p.i < codeLen) {
      p.i = parseZeroOrMoreSpaces(code, p.i);
      if (p.i == codeLen) {
        return
      }
      parseProgramElement(code, "", elfs, elts, p)
      if (p.done) {
          continue;
      }
      return;
    }
  }

  function slPrint(io) {
    return function(dontCare1, progStack) {
      io.rightDoc += progStack.pop();
    };
  }

  function slIoGetContacts(io) {
    return function(dontCare1, progStack) {
      if (io.contacts === null) {
        progStack.push([]);
        return;
      }
      progStack.push(io.contacts.keys());
    }
  }

  function slStrUnlines(io) {
    return function(dontCare1, progStack) {
      const lines = progStack.pop();
      let unlines = '';
      for (let i = 0; i < lines.length; i++) {
        unlines += '\n' + lines[i];
      }
      progStack.push(unlines);
    }
  }

  function slStrCat(io) {
    return function(dontCare1, progStack) {
      const str2 = progStack.pop();
      const str1 = progStack.pop();
      progStack.push(str1 + str2);
    }
  }

  function standardLibrary(io) {
    return {
      print: [slPrint(io)],
      'io.getcontacts': [slIoGetContacts(io)],
      'str.unlines': [slStrUnlines(io)],
      'str.cat': [slStrCat(io)],
    };
  }

  const STRTYPE = 0;
  const LISTTYPE = 1;
  const BLOCKTYPE = 2;

  function prettyErr(errMsg, progDivId) {
    const div = document.getElementById(progDivId);
    div.innerHTML = "";

    const pcode = document.createElement("p");
    const txtcode = document.createTextNode(errMsg);
    pcode.appendChild(txtcode);
    div.appendChild(pcode);
  }

  const internalErrPreamble = 'Internal type error:\n';

  const nonEmptyTypeStackErr = "the type stack should be empty at the end of the program, but it has this left in it:\n"

  function lineCol(code, i) {
    let line = 1;
    let col = 1;
    for (let j = 0; j <= i; j++) {
      let c = code[j];
      if (c === '\n') {
        line++;
        col = 0;
      } else {
        col++;
      }
    }
    return [line, col];
  }
    
  function compile(program, contacts) {
    let elts = [];
    let elfs = [];
    const p = { done: true, i: 0, errMsg: "" };
    parser(program.code, elts, elfs, p);
    
    if (p.errMsg) {
      const [line, col] = lineCol(program.code, p.i);
      return {rightDoc: internalErrPreamble + "line " + line + "\ncol  " + col + "\n" + p.errMsg, messages: []};
    }
    const dets = standardTypes();
    let typeStack = [];
    const errMsg = runTypeCheck(elts, dets, typeStack)
    if (errMsg) {
      return {rightDoc: internalErrPreamble + errMsg, messages: []};
    }
    if (typeStack.length !== 0) {
      const msg = internalErrPreamble + nonEmptyTypeStackErr +
        typeStack;
      return {messages: [], rightDoc: msg};
    }

    const io = {messages: [], rightDoc: '', 'contacts': contacts};
    const defs = standardLibrary(io)
    let progStack = [];
    runProgram(elfs, defs, progStack);
    return io;
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
      'io.getcontacts': [ioGetContactsType],
      'str.unlines': [strUnlinesType],
      'str.cat': [strCatType],
    }
  }

  function strCatType(dets, typestack) {
    const stackLen = typestack.length;
    if (stackLen < 2) {
      return '"cat" failed because there were only ' + stackLen +
        ' items on stack, but it needs 2';
    }
    const top = typestack.pop();
    if (top !== STRTYPE) {
      return '"cat" needs the top item on the stack to be a ' +
        'string, but it is a ' + top 
    }
    const next = typestack.pop();
    if (next !== STRTYPE) {
      return '"cat" needs the second item on the stack to be a ' +
        'string, but it is a ' + next;
    }
    typestack.push(STRTYPE);
  }
 
  function strUnlinesType(dets, typestack) {
    const expected = {container: LISTTYPE, inside: STRTYPE};
    if (typestack.length === 0) {
      return '"unlines" failed because there was nothing on the stack';
    } 
    if (!typeEq(typestack.pop(), expected)) {
      return '"unlines" needs a list of strings on the stack'
    }
    typestack.push(STRTYPE);
  }

  function ioGetContactsType(dets, typestack) {
    typestack.push({container: LISTTYPE, inside: STRTYPE});
  }

  function popType(dets, typestack) {
    if (typestack.length === 0) {
      return '"pop" failed because there was nothing on the stack'
    }
    typestack.pop()
  }

  function stringType(dets, typestack) {
    typestack.push(STRTYPE)
  }

  function printType(dets, typestack) {
    if (typestack.length === 0) {
      return '"print" needs something on the stack to print'
    }
    const top = typestack.pop()
    if (top !== STRTYPE) {
        return '"print" requires the top of the stack to be a string'
    }
  }

  const divIdEnd = "ProgramDiv";

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

  function blobIdIota() {
    const now = Date.now();
    const id = now * 1000 + blobIdIotaGlobal;
    if (blobIdIotaGlobal > 998) {
      blobIdIotaGlobal = 0;
    } else {
      blobIdIotaGlobal++;
    }
    return '' + id;
  }

  function makeShowProgramDiv(name, program, parent, programId) {
    const topDiv = document.createElement('div');
    topDiv.appendChild(makeEditProgDiv(name));
    topDiv.id = programId;
    parent.appendChild(topDiv);

    const leftRightDiv = document.createElement("div");
    topDiv.appendChild(leftRightDiv);


    leftRightDiv.className = "programDiv";

    const leftDiv = document.createElement('div');
    leftDiv.className = 'leftDiv';
    const rightId = domId();
    displayLeftText(name, program.homedoc, leftDiv, rightId);
    leftRightDiv.appendChild(leftDiv);

    leftRightDiv.appendChild(makeRightDoc(rightId));
    updateRightDoc(program, rightId);
  }

  function updateRightDoc(program, rightId) {
    localforage.getItem('contacts').then(function(contacts) {
      const right = document.getElementById(rightId);

      const io = compile(program, contacts);
      right.textContent = io.rightDoc;
      
      for (let i = 0; i < io.messages.length; i++) {
        sendMsg(io.messages[i]);
      }
    })
  }

  function domId() {
    const id = domIdIota;
    domIdIota++;
    return id.toString();
  }

  function displayLeftDoc(leftDocName) {
    const div = document.createElement('div');
    const divId = domId();
    div.id = divId;
    document.getElementById(parentId).appendChild(div);
    displayLeftText(leftDocName, divId);
  }

  function textBoxHelp(id) {
    const textBox = document.createElement('textarea');
    textBox.id = id;
    textBox.className = "leftTextBox";
    textBox.rows = "25";
    return textBox;
  }

  function downloadButtonP() {
    const p = document.createElement("p");
    const txt = document.createTextNode(tooBigButtonMsg);
    p.appendChild(txt);
  }

  function blobMaker(parentId, parentFolder, parentFolderName) {
    const div = document.createElement("div");
    const divId = parentId + "makeNewPart";
    div.id = divId;

    function set(newBlob) {
      const blobName = blobIdIota();
      parentFolder.push(blobName);
      localforage.setItem(parentFolderName, parentFolder);
      localforage.setItem(blobName, newBlob).then(function () {
        div.remove();
        displayLeftDoc(blobName, parentId);
      })
    }

    const textBoxId = domId();
    const textBox = textBoxHelp(textBoxId);
    div.appendChild(textBox);
    
    const saveButton = document.createElement('button');
    saveButton.innerHTML = 'Save';
    saveButton.onclick = function() {
      const newText = textBox.value;
      if (!newText) {
        return;
      }
      set(newText);
    }
    div.appendChild(saveButton);

    const makeFolderButton = document.createElement('button'); 
    makeFolderButton.innerHTML = 'New folder';
    makeFolderButton.onclick = function() {
      set([]);      
    }
    div.appendChild(makeFolderButton);

    document.getElementById(parentId).appendChild(div);
  }

  function makeEditProgDiv(programName) {
    const topDiv = document.createElement('div');
    
    const editorDivId = domId();

    const button = document.createElement('button');
    topDiv.appendChild(button);
    button.innerHTML = 'Edit program';
    button.onclick = function() {
      let editorDiv = document.getElementById(editorDivId);
      if (editorDiv) {
        editorDiv.remove();
        button.innerHTML = 'Edit program';
        return;
      }

      localforage.getItem('programs').then(function (programs) {
        let program = programs[programName];
        editorDiv = document.createElement('div');
        editorDiv.id = editorDivId;
        topDiv.appendChild(editorDiv);
        button.innerHTML = 'Close editor';

        const textBox = document.createElement('textarea');
        textBox.className = 'codeEditorBox';
        textBox.rows = '25';
        editorDiv.appendChild(textBox);
        textBox.value = program.code;
        
        const saveButton = document.createElement('button');
        editorDiv.appendChild(saveButton);
        saveButton.innerHTML = 'Save';
        saveButton.onclick = function () {
          program.code = textBox.value;
          programs[programName] = program;
          localforage.setItem('programs', programs);
        }
      })
    }
    
    const deleteButton = document.createElement('button');
    deleteButton.innerHTML = "Delete program";
    topDiv.appendChild(deleteButton);
    const confirmMsg = "Are you sure you want to delete " + programName + "?";
    deleteButton.onclick = function() {
      if (confirm(confirmMsg)) {
        localforage.getItem('programs').then(function(programs) {
          delete programs[programName];
          localforage.setItem('programs', programs)
            .then(afterRetrievingPrograms(programs));
        })
      }
    }
    return topDiv;
  }

  function makeProgramMenuItem(name, program) {
    const div = document.createElement("DIV");
    const parentId = name + divIdEnd;
    div.id = parentId;
    const button = document.createElement("BUTTON");
    const programId = domId();
    button.onclick = function() {
      const toRemove = document.getElementById(programId);
      if (toRemove) {
        toRemove.remove();
      } else {
        makeShowProgramDiv(name, program, div, programId);
      }
    };
    button.appendChild(makeProgramStartButtonContents(name, program));
    div.appendChild(button);
    return div;
  }

  function makeRightDoc(id) {
    const code = document.createElement('code');
    code.id = id;
    return code;
  }

  function noProgramsMsg() {
    const p = document.createElement('p');
    p.innerHTML = 'No programs found.'
    return p
  }

  function getById(id) {
    return document.getElementById(id);
  }

  const uploaderHtml = `
<p>Add a new program:<p>
<p>Name: <input type="text" id="nameUpload"></p>
<p>Description: <input type="text" id="descriptionUpload"></p>
<p>Version: <input type="number" id="versionUpload" min="0"></p>`

  function readCodeUpload(event) {
    localforage.getItem("programs").then(function(programs) {
      if (!programs) {
        programs = {};
      }
      const name = getById("nameUpload").value;
      const description = getById("descriptionUpload").value;
      const version = getById("versionUpload").value;

      const fileHandle = event.target.files[0];
      const reader = new FileReader();
      const homeBlobId = blobIdIota();
      reader.onload = function(event) {
        programs[name] = {
          "description": description,
          "version": version,
          "code": event.target.result,
          "inbox": [],
          "homedoc": homeBlobId};
        localforage.setItem('programs', programs)
          .then(localforage.setItem(homeBlobId, '')
          .then(afterRetrievingPrograms(programs)));
      }

      reader.onerror = error => reject(error);
      reader.readAsText(fileHandle);
    });
  }

  function programUploader() {
    const form = document.createElement('form');
    form.innerHTML = uploaderHtml;
    const uploadFile = document.createElement('input');
    uploadFile.type = 'file';
    uploadFile.onchange = readCodeUpload;
    form.appendChild(uploadFile);
    return form;
  }

  const programMenuId = "programMenu";

  function afterRetrievingPrograms(programs) {
    const oldProgramDiv = document.getElementById(programMenuId);
    if (oldProgramDiv) {
      oldProgramDiv.remove();
    }
    
    const div = document.createElement('div');
    div.id = programMenuId
    if (!programs) {
      div.appendChild(noProgramsMsg());
    } else {
      for (const [name, program] of Object.entries(programs)) {
        const progButton = makeProgramMenuItem(name, program);
        div.appendChild(progButton);
      }
    }
    document.body.appendChild(div);
  }

  function main() {
    document.body.appendChild(programUploader());
    localforage.getItem('programs').then(afterRetrievingPrograms);
  }

  main();
})();
