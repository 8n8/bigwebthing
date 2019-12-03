(function() {
  "use strict";

  let domIdIota = 0
  let blobIdIotaGlobal = 0

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
 
  function displayLeftText(docName, parentId) {
    const parent = document.getElementById(parentId);
 
    localforage.getItem(docName).then(function (doc) {
      const id = domId();
      const textBox = textBoxHelp(id);
      parent.appendChild(textBox);
      textBox.value = doc;

      const button = document.createElement('button'); 
      button.onclick = function() {
        localforage.setItem(docName, textBox.value)
      }
      button.innerHTML = 'Save';
      parent.appendChild(button);
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

  function makeProgramDiv(name, childId, parentId, program) {
    const programDiv = document.createElement("div");
    programDiv.class = "programDiv";
    programDiv.id = childId;
    document.getElementById(parentId).appendChild(programDiv);
    const leftDiv = document.createElement('div');
    displayLeftDoc(program.homedoc, childId);
    const rightDoc = compile(program);
    updateRightDoc(rightDoc, parentId);
  }

  const UTF16 = 0;
  const IMAGE = 1;
  const VIDEO = 2;

  function domId() {
    const id = domIdIota;
    domIdIota++;
    return id.toString();
  }

  function displayLeftDoc(leftDocName, parentId) {
    const div = document.createElement('div');
    const divId = domId();
    div.id = divId;
    document.getElementById(parentId).appendChild(div);
    displayLeftText(leftDocName, divId);
    // localforage.getItem(leftDocName).then(function(leftDoc) {
    //   if (Array.isArray(leftDoc)) {
    //     for (let i = 0; i < leftDoc.length; i++) {
    //         displayLeftDoc(leftDoc[i], divId);
    //     }
    //     blobMaker(divId, leftDoc, leftDocName);
    //     return;
    //   }
    //   displayLeftText(leftDocName, leftDoc, divId);
    // })
  }

  function textBoxHelp(id) {
    const textBox = document.createElement('textarea');
    textBox.id = id;
    textBox.rows = "10";
    textBox.cols = "50";
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

    // const uploadFile = document.createElement('input');
    // uploadFile.type = 'file';
    // uploadFile.onchange = function(event) {
    //   set(event.target.files[0]);
    // }
    // div.appendChild(uploadFile);

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

  function updateRightDoc(rightDoc, leftRightParentId) {

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
        makeProgramDiv(name, programId, parentId, program);
      }
    };
    button.appendChild(makeProgramMenuDiv(name, program));
    div.appendChild(button);
    return div;
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
      const homeBlobId = blobId();
      reader.onload = function(event) {
        programs[name] = {
          "description": description,
          "version": version,
          "code": event.target.result,
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
