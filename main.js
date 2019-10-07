(function () {
  'use strict'
  const programsDebug = {
    Clocker: {
      description: "A sample app for clocking times. It doesn't " +
      'actually work: this is just a placeholder app for ' +
      'debugging.',
      version: 1,
      code: "Some code goes here but the language hasn't been " +
      'invented yet.'
    },
    Messenger: {
      description: 'This app will be for sending text messages ' +
      'around, but remember that this is just a placeholder app.',
      version: 0,
      code: 'A bit of code.'
    },
    Truelang: {
      description: 'Truelang is the built-in programming language ' +
      'of BigWebThing.',
      version: 0,
      code: "It doesn't exist yet."
    }
  }

  function makeProgramMenuDiv (programName, program) {
    const div = document.createElement('DIV')
    const name = document.createElement('H2')
    const nameText = document.createTextNode(programName)
    name.appendChild(nameText)
    div.appendChild(name)
    const version = document.createElement('P')
    const versionText = document.createTextNode(
      'Version ' + program.version)
    version.appendChild(versionText)
    div.appendChild(version)
    const description = document.createElement('P')
    const descriptionText = document.createTextNode(
      program.description)
    description.appendChild(descriptionText)
    div.appendChild(description)
    return div
  }

  function compile (code) {
    return (
      "const div = document.createElement('div');\n" +
      "const txt = document.createTextNode(\"" + code + "\");\n" +
      "div.appendChild(txt);\n" +
      "return div;")
  }

  const divIdEnd = 'ProgramDiv'

  function makeProgramMenuItem (name, program) {
    const div = document.createElement('DIV')
    const divClass = document.createAttribute('id')
	const idName = name + divIdEnd
    divClass.value = idName
    div.setAttributeNode(divClass)
    const button = document.createElement('BUTTON')
    const compiled = compile(program.code)
    const programDiv = (new Function(compiled))()
    button.onclick = function () { document.getElementById(idName).appendChild(programDiv) }
    button.appendChild(makeProgramMenuDiv(name, program))
    div.appendChild(button)
    return div
  }

  function afterRetrievingPrograms (programs) {
    for (const [name, program] of Object.entries(programs)) {
      const progButton = makeProgramMenuItem(name, program)
      document.body.appendChild(progButton)
    }
  }

  function main () {
    localforage.getItem('programs').then(afterRetrievingPrograms)
  }

  localforage.setItem('programs', programsDebug).then(main)
})()
