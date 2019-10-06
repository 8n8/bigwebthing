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

  const afterProgDivId = 'ProgramDiv'

  function compile(name, code) {
    const div = document.createElement('div')
    const debugText = document.createTextNode(code)
    div.appendChild(debugText)
    return div
  }

  function runProgram (name, program) {
    const progDiv = compile(program.name, program.code)
    const parentDiv = document.getElementById(name + afterProgDivId)
    parentDiv.appendChild(progDiv)
  }

  function makeProgramMenuItem (name, program) {
    const button = document.createElement('BUTTON')
    button.onclick = function () { runProgram(name, program) }
    button.appendChild(makeProgramMenuDiv(name, program))
    const div = document.createElement('DIV')
    div.appendChild(button)
    const divclass = document.createAttribute('id')
    divclass.value = name + afterProgDivId
    div.setAttributeNode(divclass)
    return div
  }

  function makeProgramMenu (programs) {
    const div = document.createElement('div')
  }

  function afterRetrievingPrograms (programs) {
    document.body.innerHTML = (makeProgramMenu(programs))
  }

  function main () {
    localforage.getItem('programs').then(afterRetrievingPrograms)
  }

  localforage.setItem('programs', programsDebug).then(main)
})()
