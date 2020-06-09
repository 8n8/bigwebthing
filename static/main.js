(function () {
  ('use strict')

  function initOnClick (button) {
    return {
      io: addOnclick,
      value: {
        id: button + 'Button',
        onclick: () => tick(onTopButtonClick, button)
      }
    }
  }

  function encodeString (s) {
    const encoder = new TextEncoder()
    const encoded = encoder.encode(s)
    const len = encodeInt(encoded.length)
    return combine(len, encoded)
  }

  function combineMany (bs) {
    let totalLength = 0
    for (const b of bs) {
      totalLength += b.length
    }

    const buf = new ArrayBuffer(totalLength)
    const result = new Uint8Array(buf)

    let i = 0
    for (const b of bs) {
      for (const bel of b) {
        result[i] = bel
        i += 1
      }
    }

    return result
  }

  function encodeDraft (draft) {
    return combineMany(
      oneByte(0),
      encodeInt(draft.to),
      encodeString(draft.subject),
      encodeString(draft.userInput),
      encodeInt(draft.code.length),
      draft.code
    )
  }

  function replaceChildren (parentId, newChildren) {
    return {
      io: ioReplaceChildren,
      value: { parentId: parentId, children: newChildren }
    }
  }

  function initOnClicks () {
    const buttons = [
      'write',
      'contacts',
      'inbox',
      'outbox',
      'drafts',
      'pricing',
      'account',
      'help'
    ]
    const outputs = []
    for (const button of buttons) {
      outputs.push(initOnClick(button))
    }
    return outputs
  }

  function initOutputs () {
    return [
      { io: cacheQuery, value: 'iota' },
      { io: cacheQuery, value: 'page' },
      { io: cacheQuery, value: 'inboxIds' },
      { io: cacheQuery, value: 'draftIds' },
      { io: cacheQuery, value: 'outboxIds' },
      { io: cacheQuery, value: 'myName' },
      { io: cacheQuery, value: 'contacts' }
    ].concat(initOnClicks())
  }

  function makeSubjectDom (subject) {
    const p = document.createElement('p')
    if (subject !== undefined) {
      p.textContent = 'Subject: ' + subject
      return p
    }
    p.textContent = 'No subject'
    p.classList.add('noneMessage')
    return p
  }

  function makeToDom (to) {
    const p = document.createElement('p')
    p.textContent = 'To: ' + to
    return p
  }

  function makeFromDom (from) {
    const p = document.createElement('p')
    p.textContent = 'From: ' + from
    return p
  }

  function combine (a, b) {
    const lena = a.length
    const lenb = b.length
    const buf = new ArrayBuffer(lena + lenb)
    const combined = new Uint8Array(buf)
    for (let i = 0; i < lena; i++) {
      combined[i] = a[i]
    }
    for (let i = lena; i < lena + lenb; i++) {
      const bval = b[i - lena]
      combined[i] = bval
    }
    return combined
  }

  function encodeInt (n) {
    const buf = new ArrayBuffer(8)
    const result = new Uint8Array(buf)
    for (let i = 0; i < 8; i++) {
      result[i] = n >> (i * 8) && 0xff
    }
    return result
  }

  function decodeInt (eightBytes) {
    let result = 0
    for (let i = 0; i < 8; i++) {
      result += eightBytes[i] * Math.pow(256, i)
    }
    return result
  }

  function isDifficult (hash, difficulty) {
    for (let i = 0; i < 32; i++) {
      if (hash[i] < difficulty) {
        return false
      }
    }
    return true
  }

  function proofOfWork (powInfo) {
    const buffer = new ArrayBuffer(8)
    const bufferView = new Uint8Array(buffer)
    const counter = new Int32Array(buffer)
    while (true) {
      const combined = combine(powInfo.unique, bufferView)
      const hash = nacl.hash(combined).slice(0, 32)
      if (isDifficult(hash, powInfo.difficulty)) {
        return combined
      }
      counter[0] = counter[0] + 1
    }
  }

  function turnButtonOn (id) {
    return [
      {
        io: addCssClass,
        value: { id: id, cssClass: 'selectedButton' }
      },
      {
        io: removeCssClass,
        value: { id: id, cssClass: 'notSelectedButton' }
      }
    ]
  }

  function turnButtonOff (id) {
    return [
      {
        io: addCssClass,
        value: { id: id, cssClass: 'notSelectedButton' }
      },
      {
        io: removeCssClass,
        value: { id: id, cssClass: 'selectedButton' }
      }
    ]
  }

  function makeSubjectView (subject) {
    const p = document.createElement('p')
    if (subject === undefined) {
      p.textContent = 'No subject'
      p.classList.add('noneMessage')
      return p
    }
    p.textContent = subject
    return p
  }

  function makeFromView (from) {
    const p = document.createElement('p')
    p.textContent = from
    return p
  }

  function decodeSmallString (raw, i) {
    const rawLen = raw.length
    if (rawLen < 4 + i) {
      return [
        null,
        'smallstring at position ' +
                i +
                ' is only ' +
                (rawLen - i) +
                ' bytes long'
      ]
    }
    const stringLength = decodeInt(raw.slice(i, i + 4))
    i += 4
    const stringBytes = raw.slice(i, i + stringLength)
    const decoded = new TextDecoder().decode(stringBytes)
    return [{ key: 'smallString', value: decoded }, '']
  }

  function decodeOrdering (raw, i) {
    const rawLen = raw.length
    if (rawLen < 4 + i) {
      return [
        {},
        'ordering at position ' +
                i +
                ' is only ' +
                (rawLen - i) +
                ' bytes long'
      ]
    }
    const numElements = decodeInt(raw.slice(i, i + 4))
    i += 4
    const ordering = []
    for (let _ = 0; _ < numElements; _++) {
      let doc, err;
      [doc, err, i] = decodeDocumentHelp(raw, i)
      if (err !== '') {
        return [null, err, i]
      }
      ordering.push(doc)
    }
    return [{ key: 'ordering', value: ordering }, '', i]
  }

  function decodeDocument (raw) {
    const [doc, err, i] = decodeDocumentHelp(raw, 0)
    if (err !== '') {
      return [null, err + ': ' + i]
    }
    const rawLength = raw.length
    if (i < rawLength - 1) {
      return [
        null,
        'could not decode whole document: stopped at position ' + i
      ]
    }
    return [doc, '']
  }

  function decodeDocumentHelp (raw, i) {
    if (raw.length === 0) {
      return [{}, 'empty']
    }
    const indicator = raw[0]
    switch (indicator) {
      case 0:
        return decodeOrdering(raw, 1)
      case 1:
        return decodeSmallString(raw, 1)
    }
    return [null, 'bad indicator: ' + indicator, i]
  }

  function makeOutputErr (err) {
    const p = document.createElement('p')
    p.textContent = 'Internal error in message program: ' + err
    return p
  }

  function makeSmallStringView (smallString) {
    const pre = document.createElement('pre')
    pre.textContent = smallString
    return pre
  }

  function makeOrderingView (ordering) {
    const div = document.createElement('div')
    for (const o of ordering) {
      const el = makeOutputViewHelp(o)
      div.appendChild(el)
    }
    return div
  }

  function makeOutputViewHelp (doc) {
    switch (doc.key) {
      case 'smallString':
        return makeSmallStringView(doc.value)
      case 'ordering':
        return makeOrderingView(doc.value)
    }
  }

  function makeOutputView (output) {
    const [doc, decodeErr] = decodeDocument(output, 0)
    if (decodeErr !== '') {
      return makeOutputErr(decodeErr)
    }
    return makeOutputViewHelp(doc)
  }

  function makeUserInputView (userInput) {
    const pre = document.createElement('pre')
    pre.textContent = userInput
    return pre
  }

  function makeBlobView (blob) {
    const div = document.createElement('div')

    const name = document.createElement('p')
    name.textContent = blob.name

    const size = document.createElement('p')
    size.textContent = prettyBytes(blob.size)

    const mime = document.createElement('p')
    mime.textContent = 'File type: ' + blob.mime

    return div
  }

  function makeCodeView (code) {
    const div = document.createElement('div')

    const name = document.createElement('p')
    name.textContent = code.name

    const size = document.createElement('p')
    size.textContent = prettyBytes(code.size)

    return div
  }

  function makeInboxBlobsView (blobs) {
    const div = document.createElement('div')
    for (const blob of blobs) {
      div.appendChild(makeBlobView(blob))
    }
    return div
  }

  function drawBoxItemView (message) {
    const children = [
      makeSubjectView(message.subject),
      makeFromView(message.from),
      makeOutputView(message.output),
      makeUserInputView(message.userInput),
      makeCodeView(message.code)
    ]

    if (message.blobs !== undefined) {
      children.push(makeInboxBlobsView(message.blobs))
    }

    return [replaceChildren('page', children)]
  }

  function onOutboxMenuClick (messageId, state) {
    return [[kv(lookupOutboxMessage, messageId)], state]
  }

  function drawInboxMenuItem (message) {
    const button = document.createElement('button')
    button.type = 'button'
    button.classList.add('messageButton')
    button.appendChild(makeSubjectDom(message.subject))
    button.appendChild(makeFromDom(message.from))
    button.onclick = () => tick(onInboxMenuClick, message.id)
    return button
  }

  function drawInbox (state) {
    if (state.inboxSummary.length === 0) {
      return [replaceChildren('page', [noMessagesDom()])]
    }
    if (state.inboxItem !== undefined) {
      return drawBoxItemView(state.inboxItem)
    }
    const inbox = []
    for (const message of state.inboxSummary) {
      inbox.push(drawInboxMenuItem(message))
    }
    return [replaceChildren('page', inbox)]
  }

  function drawOutboxItem (message) {
    const button = document.createElement('button')
    button.type = 'button'
    button.classList.add('messageButton')
    button.appendChild(makeSubjectDom(message.subject))
    button.appendChild(makeToDom(message.to))
    button.onclick = () => tick(onOutboxMenuClick, message.id)
    return button
  }

  function drawOutbox (state) {
    if (state.outboxSummary.length === 0) {
      return [replaceChildren('page', [noMessagesDom()])]
    }
    if (state.outboxItem !== undefined) {
      return drawBoxItemView(state.outboxItem)
    }
    const outbox = []
    for (const message of state.outboxSummary) {
      outbox.push(drawOutboxItem(message))
    }
    return [replaceChildren('page', outbox)]
  }

  function makeDraftToDom (to) {
    const p = document.createElement('p')
    if (to !== undefined) {
      p.textContent = 'To: ' + to
      return p
    }
    p.textContent = 'No recipient'
    p.classList.add('noneMessage')
    return p
  }

  function drawDraftsItem (draft) {
    const button = document.createElement('button')
    button.type = 'button'
    button.classList.add('messageButton')
    button.appendChild(makeSubjectDom(draft.subject))
    button.appendChild(makeDraftToDom(draft.to))
    button.onclick = () => tick(onDraftsMenuClick, draft.id)
    return button
  }

  function drawDrafts (state, output) {
    if (state.draftsSummary.length === 0) {
      return [replaceChildren('page', [noMessagesDom()])]
    }
    if (state.openDraft !== undefined) {
      return drawWrite(state.openDraft)
    }
    const drafts = []
    for (const draftSummary of state.draftsSummary) {
      drafts.push(drawDraftsItem(draftSummary))
    }
    return [replaceChildren('page', drafts)]
  }

  function makeSubjectBox (subject) {
    const id = 'writerSubjectBox'
    const container = document.createElement('div')
    const label = document.createElement('label')
    label.setAttribute('for', id)
    label.innerHTML = 'Subject'
    container.appendChild(label)

    const box = document.createElement('input')
    box.type = 'text'
    box.value = subject
    box.oninput = (e) => tick(onUpdatedSubjectBox, e.target.value)
    box.id = id
    container.appendChild(box)
    return container
  }

  function makeToBox (to) {
    const id = 'writerToBox'
    const container = document.createElement('div')

    const label = document.createElement('label')
    label.setAttribute('for', id)
    label.innerHTML = 'To'
    container.appendChild(label)

    const box = document.createElement('input')
    box.type = 'text'
    box.value = to
    box.oninput = (e) => tick(onUpdatedToBox, e.target.value)
    box.id = id
    container.appendChild(box)
    return container
  }

  function addContactBox (boxContents) {
    const id = 'addContactBox'
    const container = document.createElement('div')

    const label = document.createElement('label')
    label.setAttribute('for', id)
    label.innerHTML = 'Add a new contact'
    container.appendChild(label)

    const box = document.createElement('input')
    box.type = 'text'
    box.value = boxContents
    box.oninput = (e) => tick(onUpdatedAddContactBox, e.target.value)
    box.id = id
    container.appendChild(box)

    const button = document.createElement('button')
    button.type = 'button'
    button.textContent = 'Add contact'
    button.onclick = () => tick(onAddContactButtonClick, '')
    container.appendChild(button)

    return container
  }

  function longestRow (rows) {
    let longest = 0
    for (const row of rows) {
      const length = row.length
      if (length > longest) {
        longest = length
      }
    }
    return longest
  }

  function makeUserInputBox (userInput) {
    const id = 'writerUserInputBox'
    const container = document.createElement('div')
    const label = document.createElement('label')
    label.setAttribute('for', id)
    label.innerHTML = 'Message'
    container.appendChild(label)

    const box = document.createElement('textarea')
    const rows = userInput.split('\n')
    box.cols = longestRow(rows)
    box.rows = rows.length
    box.oninput = (e) => tick(onUpdatedUserInput, e.target.value)
    box.id = id
    container.appendChild(box)
    return container
  }

  function codeUploaderHelp () {
    const id = 'writerCodeUploader'
    const container = document.createElement('div')
    const label = document.createElement('label')
    label.setAttribute('for', id)
    label.innerHTML = 'Upload code'
    container.appendChild(label)

    const browse = document.createElement('input')
    browse.type = 'file'
    browse.id = id
    browse.addEventListener(
      'change',
      () => tick(onCodeFilesUpload, this.files),
      false
    )
    container.appendChild(browse)
    return container
  }

  function prettyBytes (n) {
    if (n < 1000) {
      return n + 'B'
    }

    if (n < 1000000) {
      return Math.round(n / 1000) + 'KB'
    }

    if (n < 1000000000) {
      return Math.round(n / 1000000) + 'MB'
    }
  }

  function makeCodeUploader (code) {
    if (code === undefined) {
      return codeUploaderHelp()
    }

    const div = document.createElement('div')
    div.id = 'codeUploader'

    const title = document.createElement('h1')
    title.textContent('Message program')
    div.appendChild(title)

    const filename = document.createElement('span')
    filename.textContent = code.filename
    div.appendChild(filename)

    const size = document.createElement('span')
    size.textContent = 'Size: ' + prettyBytes(code.size)
    div.appendChild(size)

    const deleteButton = document.createElement('button')
    deleteButton.type = 'button'
    deleteButton.onclick = () => tick(onDeleteCode, code.draftId)
    deleteButton.textContent = 'Delete'
    div.appendChild(deleteButton)

    return div
  }

  function makeBlobViewer (blob) {
    const container = document.createElement('div')

    const filename = document.createElement('p')
    filename.textContent = blob.filename
    container.appendChild(filename)

    const size = document.createElement('p')
    size.textContent = 'Size: ' + prettyBytes(blob.size)
    container.appendChild(size)

    const deleteButton = document.createElement('button')
    deleteButton.type = 'button'
    deleteButton.onclick = () =>
      tick(onDeleteBlob, { draftId: blob.draftId, blobId: blob.id })
    deleteButton.textContent = 'Delete'
    container.appendChild(deleteButton)

    const downloadButton = document.createElement('button')
    downloadButton.type = 'button'
    downloadButton.onclick = () => tick(onDownloadBlob, blob)
    downloadButton.textContent = 'Download'
    container.appendChild(downloadButton)

    return container
  }

  function makeBlobsViewer (blobs) {
    const container = document.createElement('div')
    container.id = 'writerBlobsViewer'

    const title = document.createElement('h1')
    title.textContent = 'Attached files'
    container.appendChild(title)

    for (const blob of blobs) {
      container.appendChild(makeBlobViewer(blob))
    }

    return container
  }

  function makeBlobUploader () {
    const id = 'writerBlobUploader'
    const container = document.createElement('div')

    const label = document.createElement('label')
    label.setAttribute('for', id)
    label.innerHTML = 'Attach a file'
    container.appendChild(label)

    const browse = document.createElement('input')
    browse.type = 'file'
    browse.id = id
    browse.multiple = true
    browse.addEventListener(
      'change',
      () => tick(onBlobFilesUpload, this.files),
      false
    )
    container.appendChild(browse)

    return container
  }

  function onSendButtonClick (_, state) {
    if (!readyToSend(state.openDraft)) {
      return [[], state]
    }
    return [
      {
        io: sendDraft,
        value: {
          draft: state.openDraft,
          keys: state.cryptoKeys,
          toKeys: state.contacts[state.openDraft.to],
          myName: state.myName
        }
      },
      state
    ]
  }

  function makeSendButton () {
    const button = document.createElement('button')
    button.type = 'button'
    button.textContent = 'Send'
    button.onclick = () => tick(onSendButtonClick, null)
    return button
  }

  function readyToSend (draft) {
    return (
      draft !== undefined &&
        draft.to !== undefined &&
        draft.code !== undefined
    )
  }

  function drawWrite (state) {
    let draft = state.openDraft
    if (draft === undefined) {
      draft = {
        to: '',
        subject: '',
        userInput: '',
        blobIds: []
      }
    }

    const children = [
      makeSendButton(),
      makeSubjectBox(draft.subject),
      makeToBox(draft.to),
      makeUserInputBox(draft.userInput)
    ]

    if (state.draftOutput !== undefined) {
      children.push(makeOutputView(state.draftOutput))
    }

    children.push(makeBlobsViewer(draft.blobs))
    if (readyToSend(draft)) {
      children.push(makeBlobUploader())
    }
    children.push(makeCodeUploader(draft.code))

    const runner = kv(runWasm, {
      userInput: draft.userInput,
      runner: state.wasmRunner,
      code: draft.code
    })

    return [replaceChildren('page', children), runner]
  }

  function onNewOutput (output, state) {
    if (state.openDraft === undefined) return [[], state]
    state.draftOutput = output
    return [drawWrite(state), state]
  }

  function drawContact (contact) {
    const div = document.createElement('div')
    div.classList.add('contactView')

    const name = document.createElement('span')
    name.textContent = contact
    div.appendChild(name)

    const deleteButton = document.createElement('button')
    deleteButton.textContent = 'Delete'
    deleteButton.onclick = () => tick(onDeleteContact, contact)
    div.append(deleteButton)

    return div
  }

  function drawContacts (state) {
    const children = []

    const myName = state.myName === undefined ? 'Requesting...' : state.myName
    children.push(myNameDom(myName))

    children.push(addContactBox(state.addContactBox))

    const h1 = document.createElement('h1')
    h1.textContent = 'My contacts'
    children.append(h1)

    for (const contact in state.contacts) {
      children.push(drawContact(contact))
    }
    return [replaceChildren('page', children)]
  }

  function drawPricing (state) {
    const span = document.createElement('span')
    span.textContent = 'TODO'
    return [replaceChildren('page', [span])]
  }

  function drawAccount (state) {
    const span = document.createElement('span')
    span.textContent = 'TODO'
    return [replaceChildren('page', [span])]
  }

  function drawHelp (state) {
    const span = document.createElement('span')
    span.textContent = 'TODO'
    return [replaceChildren('page', [span])]
  }

  const drawFunc = {
    inbox: drawInbox,
    write: drawWrite,
    contacts: drawContacts,
    outbox: drawOutbox,
    drafts: drawDrafts,
    pricing: drawPricing,
    account: drawAccount,
    help: drawHelp
  }

  function drawPage (oldPage, state) {
    let buttonOn = []
    let buttonOff = []
    if (state.page !== oldPage) {
      buttonOff =
            oldPage === undefined ? [] : turnButtonOff(oldPage + 'Button')

      buttonOn = turnButtonOn(state.page + 'Button')
    }
    const drawJobs = drawFunc[state.page](state)
    return drawJobs.concat(buttonOn).concat(buttonOff)
  }

  function oneByte (route) {
    const buffer = new ArrayBuffer(1)
    const view = new Uint8Array(buffer)
    view[0] = route
    return view
  }

  function myNameDom (myName) {
    const p = document.createElement('p')
    p.textContent = 'My username is: ' + myName
    return p
  }

  function myNameFromCache (maybeMyName, state) {
    if (maybeMyName === null) {
      return [[{ io: requestMyName, value: state.cryptoKeys }], state]
    }
    state.myName = maybeMyName
    if (state.page === 'contacts') {
      const outputs = [replaceChildren('myName', [maybeMyName])]
      return [outputs, state]
    }
    return [[], state]
  }

  function inboxIdsFromCache (inboxIds, state) {
    if (state.page !== 'inbox') {
      return [[], state]
    }
    if (inboxIds === null) {
      state.inboxIds = []
    }
    state.inboxIds = inboxIds
    return [drawPage(null, state), state]
  }

  function draftIdsFromCache (draftIds, state) {
    if (state.page !== 'drafts') {
      return [[], state]
    }
    if (draftIds === null) {
      state.draftIds = []
    }
    state.draftIds = draftIds
    return [drawPage(null, state), state]
  }

  function outboxIdsFromCache (outboxIds, state) {
    if (state.page !== 'outbox') {
      return [[], state]
    }
    if (outboxIds === null) {
      state.outboxIds = []
    }
    state.outboxIds = outboxIds
    return [drawPage(null, state), state]
  }

  function pageFromCache (page, state) {
    state.page = page
    if (page === 'inbox' || page === null) {
      if (
        state.openInboxItem === undefined &&
            state.inboxSummary === undefined
      ) {
        return [[{ key: getInboxSummary, value: state.inboxIds }], state]
      }
    }
    if (
      page === 'drafts' &&
        state.openDraft === undefined &&
        state.draftsSummary === undefined
    ) {
      return [[{ key: getDraftsSummary, value: state.draftIds }], state]
    }
    if (
      page === 'outbox' &&
        state.openSent === undefined &&
        state.outboxSummary === undefined
    ) {
      return [[{ key: getOutboxSummary, value: state.outboxIds }], state]
    }
    const oldPage = state.page
    state.page = page
    return [drawPage(oldPage, state), state]
  }

  function iotaFromCache (iota, state) {
    state.iota = iota === null ? 0 : iota
    return [[], state]
  }

  function contactsFromCache (contacts, state) {
    if (contacts === null) {
      return [[], state]
    }
    state.contacts = contacts
    return [[], state]
  }

  const updateOnCacheResponseSwitch = {
    page: pageFromCache,
    myName: myNameFromCache,
    inboxIds: inboxIdsFromCache,
    draftIds: draftIdsFromCache,
    outboxIds: outboxIdsFromCache,
    iotaIds: iotaFromCache,
    contacts: contactsFromCache
  }

  function onError (error, state) {
    state.error = error
    return [[{ key: 'draw', value: state }], state]
  }

  function onNewName (newName, state) {
    state.myName = newName
    return [[{ key: 'draw', value: state }], state]
  }

  function setItem (key, value) {
    return {
      io: cacheValue,
      value: { key: key, value: value }
    }
  }

  function newSubject (draftId, draftsSummary, subject) {
    for (const draft of draftsSummary) {
      if (draft.id === draftId) {
        draft.subject = subject
        break
      }
    }
    return draftsSummary
  }

  function onUpdatedSubjectBox (subject, state) {
    if (state.openDraft === undefined) {
      return [[], state]
    }
    if (state.openDraft.id === undefined) {
      state.openDraft.id = state.iota.toString()
      state.iota += 1
    }
    state.openDraft.subject = subject
    state.draftsSummary = newSubject(
      state.openDraft.id,
      state.draftsSummary,
      subject
    )
    const ioJobs = [
      {
        key: updateTextBox,
        value: { id: 'writerSubjectBox', value: subject }
      },
      setItem('iota', state.iota),
      setItem(state.openDraft.id, state.openDraft)
    ]
    return [ioJobs, state]
  }

  function validRecipient (recipient) {
    if (recipient === '') {
      return false
    }
    if (recipient === '0') {
      return true
    }
    if (recipient[0] === '0') {
      return false
    }
    const digits = Set(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
    for (const c of recipient) {
      if (!digits.has(c)) {
        return false
      }
    }
    return true
  }

  function onUpdatedToBox (to, state) {
    if (state.openDraft === undefined) {
      return [[], state]
    }
    if (!validRecipient(to)) {
      return [
        [
          {
            key: updateTextBox,
            value: { id: 'writerToBox', value: '' }
          }
        ],
        state
      ]
    }
    if (state.openDraft.id === undefined) {
      state.openDraft.id = state.iota.toString()
      state.iota += 1
    }
    state.openDraft.to = to
    const ioJobs = [
      { key: updateTextBox, value: { id: 'writerToBox', value: to } },
      setItem('iota', state.iota),
      setItem(state.openDraft.id, state.openDraft)
    ]
    return [ioJobs, state]
  }

  function onDraftsSummary (draftsSummary, state) {
    if (state.page !== 'drafts' || state.openDraft !== undefined) {
      return [[], state]
    }
    state.draftsSummary = draftsSummary
    return [drawDrafts(state), state]
  }

  function onOutboxSummary (outboxSummary, state) {
    if (state.page !== 'outbox' || state.openSent !== undefined) {
      return [[], state]
    }
    state.outboxSummary = outboxSummary
    return [drawOutbox(state), state]
  }

  function onInboxSummary (inboxSummary, state) {
    if (state.page !== 'inbox' || state.openInboxItem !== undefined) {
      return [[], state]
    }
    state.inboxSummary = inboxSummary
    return [drawInbox(state), state]
  }

  function onCacheResponse (response, state) {
    return updateOnCacheResponseSwitch[response.key](response.value, state)
  }

  function onAddContactButtonClick (_, state) {
    const contact = state.addContactBox
    if (contact in state.contacts) {
      const err = contact + ' is already in your contacts'
      state.addContactError = err
      return [[{ io: onError, value: err }], state]
    }
    state.addContactBox = ''
    return [
      [
        {
          io: updateTextBox,
          value: { id: 'addContactBox', value: '' }
        },
        {
          io: downloadContactKeys,
          value: contact
        }
      ],
      state
    ]
  }

  function onUpdatedAddContactBox (contact, state) {
    if (!validRecipient(contact)) {
      return [
        [
          {
            key: updateTextBox,
            value: { id: 'addContactBox', value: '' }
          }
        ],
        state
      ]
    }
    state.addContactBox = contact
    return [
      [
        {
          key: updateTextBox,
          value: { id: 'addContactBox', value: contact }
        }
      ],
      state
    ]
  }

  function onUpdatedUserInput (userInput, state) {
    if (state.openDraft === undefined) {
      return [[], state]
    }
    if (state.openDraft.id === undefined) {
      state.openDraft.id = state.iota
      state.iota += 1
    }
    state.openDraft.userInput = userInput
    const ioJobs = [
      {
        key: 'updatedTextBox',
        value: { id: 'writerUserInputBox', value: userInput }
      },
      setItem('iota', state.iota),
      setItem(state.openDraft.id, state.openDraft),
      kv(runWasm, {
        userInput: userInput,
        runner: state.wasmRunner,
        code: state.openDraft.code
      })
    ]
    return [ioJobs, state]
  }

  function onCodeUpload (code, state) {
    if (state.openDraft === undefined) {
      state.openDraft = {}
    }
    if (state.openDraft.id === undefined) {
      state.openDraft.id = state.iota
      state.iota += 1
    }
    state.openDraft.code = code
    const ioJobs = [
      kv(replaceDomWith, {
        id: 'codeUploader',
        newDom: makeCodeUploader(code)
      }),
      setItem('iota', state.iota),
      setItem(state.openDraft.id, state.openDraft)
    ]
    return [ioJobs, state]
  }

  function onDeleteCode (draftId, state) {
    if (state.openDraft === undefined) {
      return [[], state]
    }
    const openDraft = state.openDraft
    delete openDraft.code
    const ioJobs = [
      {
        key: replaceDomWith,
        value: {
          id: 'codeUploader',
          newDom: makeCodeUploader(undefined)
        }
      },
      setItem(state.openDraft.id, state.openDraft)
    ]
    return [ioJobs, state]
  }

  function onDeleteContact (contact, state) {
    state.contacts.delete(contact)
    return [
      drawContacts(state).push(setItem('contacts', state.contacts)),
      state
    ]
  }

  function onTopButtonClick (button, state) {
    if (state.page === button) {
      return [[], state]
    }

    const oldPage = state.page
    state.page = button

    return [drawPage(oldPage, state).push(setItem('page', button)), state]
  }

  function onDraftsMenuClick (messageId, state) {
    return [[kv(lookupDraft, messageId)], state]
  }

  function onInboxMenuClick (messageId, state) {
    return [[kv(lookupInboxMessage, messageId)], state]
  }

  function onInit (_, state) {
    return [initOutputs(), state]
  }

  function onLookedUpDraft (message, state) {
    if (state.page !== 'drafts') {
      return [[], state]
    }
    state.openDraft = message
    return [drawDrafts(state), state]
  }

  function onLookedUpInboxMessage (message, state) {
    if (state.page !== 'inbox') {
      return [[], state]
    }
    state.openInboxItem = message
    return [drawInbox(state), state]
  }

  function onLookedUpOutboxMessage (message, state) {
    if (state.page !== 'outbox') {
      return [[], state]
    }
    state.openOutboxItem = message
    return [drawOutbox(state), state]
  }

  function onDeleteBlob (ids, state) {
    if (state.openDraft === undefined) {
      return [[], state]
    }

    const oldBlobs = state.openDraft.blobs

    const newBlobs = []
    for (const oldBlob of oldBlobs) {
      if (oldBlob.id === ids.blobId && oldBlob.draftId === ids.draftId) {
        continue
      }
      newBlobs.push(oldBlob)
    }
    state.openDraft.blobs = newBlobs

    return [
      [
        setItem(ids.draftId, state.openDraft),
        {
          key: replaceDomWith,
          value: {
            id: 'writerBlobsViewer',
            newDom: makeBlobsViewer(newBlobs)
          }
        }
      ],
      state
    ]
  }

  function findBlob (blobs, id) {
    for (const blob in blobs) {
      if (blob.id === id) return blob
    }
  }

  function kv (key, value) {
    return { key: key, value: value }
  }

  function onDownloadBlob (ids, state) {
    if (state.openDraft === undefined) {
      return [[], state]
    }
    const blob = findBlob(state.openDraft.blobs, ids.blobId)
    return [kv(downloadBlob, blob), state]
  }

  function onCodeFilesUpload (files, state) {
    return kv(codeFilesUpload, files)
  }

  function onBlobFilesUpload (files, state) {
    return kv(blobFilesUpload, { files: files, draftId: state.openDraft.id })
  }

  function onBlobUpload (blobUpload, state) {
    if (state.openDraft === undefined) {
      state.openDraft = {}
    }
    if (state.openDraft.id === undefined) {
      state.openDraft.id = state.iota
      state.iota += 1
    }
    if (state.openDraft.blobs === undefined) {
      state.openDraft.blobs = []
    }
    const blobId = base64.fromBytes(
      nacl.hash(blobUpload.contents).slice(0, 32)
    )
    state.iota += 1
    const blob = {
      name: blobUpload.name,
      size: blobUpload.size,
      mime: blobUpload.mime,
      id: blobId
    }
    state.iota += 1
    state.openDraft.blobs.push(blob)
    const ioJobs = [
      kv(replaceDomWith, {
        id: 'writerBlobsViewer',
        newDom: makeBlobsViewer(state.openDraft.blobs)
      }),
      setItem('iota', state.iota),
      setItem(state.openDraft.id, state.openDraft),
      setItem(blobId, blobUpload.contents)
    ]
    return [ioJobs, state]
  }

  function arrToNums (arr) {
    const numbers = []
    const lenArr = arr.length
    for (let i = 0; i < lenArr; i++) {
      numbers.push(arr[i])
    }
    return numbers
  }

  function formatHttpError (body, statusCode) {
    return (
      'bad response: ' +
        statusCode +
        ': ' +
        String.fromCharChode.apply(null, arrToNums(body))
    )
  }

  function noMessagesDom () {
    const p = document.createElement('p')
    p.textContent = 'You have no messages yet.'
    p.classList.add('noneMessage')
    return p
  }

  /*
A chunk is like this:
+ 4 bytes: 32-bit counter, starting at 0
+ 4 bytes: total number of chunks in message
+ 32 bytes: hash of complete message
+ <= 15kB: chunk */
  function chopMessageIntoChunks (message) {
    const chunkLength = 15000
    const hash = nacl.hash(message).slice(0, 32)
    const numChunks = Math.ceil(message.length / chunkLength)
    const numChunksBytes = encodeInt(numChunks)
    const chunks = []
    for (let i = 0; i < numChunks; i++) {
      const chunkNum = encodeInt(i)
      const chunkStart = i * chunkLength
      const chunkEnd = (i + 1) * chunkLength
      const chunkBase = message.slice(chunkStart, chunkEnd)
      const combined = combineMany(chunkNum, numChunksBytes, hash, chunkBase)
      chunks.push(combined)
    }
    return chunks
  }

  function makeIdToken (route, message, authCode, secretSign, myName) {
    const toSign = combineMany(oneByte(route), message, authCode)
    const hash = nacl.hash(toSign).slice(0, 32)
    const signature = nacl.sign(hash, secretSign)
    return combineMany(encodeInt(myName), authCode, signature)
  }

  function onNewContact (arg, state) {
    state.contacts[arg.id] = arg.keys
    const ioJobs = [setItem('contacts', state.contacts)]
    if (state.page === 'contacts') {
      ioJobs.push(drawContacts(state))
    }
    return [ioJobs, state]
  }

  function constructCtoCMessage (chunk, to, keys, authCode, myName) {
    const encodedTo = encodeInt(to)
    const idToken = makeIdToken(
      8,
      combine(encodedTo, chunk),
      authCode,
      keys.signing.secretKey,
      myName
    )
    return combineMany(oneByte(8), idToken, encodedTo, chunk)
  }

  function onReceivingContactKeys (keys, state) {
    state.contacts[keys.id] = {
      signing: keys.raw.slice(0, 32),
      encryption: keys.raw.slice(32, 64)
    }
    const whitelist = {
      io: sendWhitelistRequest,
      value: {
        id: keys.id,
        myKeys: state.myKeys,
        myName: state.myName,
        theirKeys: state.contacts[keys.id]
      }
    }
    return [[whitelist], state]
  }

  function removeDraft (id, draftsSummary) {
    const newSummary = []
    for (const draft of draftsSummary) {
      if (draft.id === id) {
        continue
      }
      newSummary.push(draft)
    }
    return newSummary
  }

  function onDraftSent (draft, state) {
    state.draftsSummary = removeDraft(draft.id, state.draftsSummary)
    state.outboxSummary.push(draft)
    return [
      [
        setItem('draftsSummary', state.draftsSummary),
        setItem('outboxSummary', state.outboxSummary)
      ],
      state
    ]
  }

  async function getKeys () {
    let keys = await localforage.getItem('cryptoKeys')
    if (keys === null) {
      keys = {
        signing: nacl.sign.keyPair(),
        box: nacl.box.keyPair()
      }
      await localforage.setItem('cryptoKeys', keys)
    }
    return keys
  }

  async function getPowInfo () {
    const [response, responseErr] = await apiRequest(oneByte(3))
    if (responseErr !== '') {
      return [{}, responseErr]
    }
    return [{ difficulty: response[0], unique: response.slice(1) }, '']
  }

  async function apiRequest (requestBody) {
    const response = await fetch('/api', {
      method: 'POST',
      headers: { 'Content-Type': 'application/octet-stream' },
      body: requestBody
    })

    const body = await response.arrayBuffer()
    const bodyArray = new Uint8Array(body)

    if (!response.ok) {
      return [{}, formatHttpError(bodyArray, response.status)]
    }

    return [bodyArray, '']
  }

  function addCssClass (toAdd) {
    const el = document.getElementById(toAdd.id)
    el.classList.add(toAdd.cssClass)
  }

  function removeCssClass (toRemove) {
    const el = document.getElementById(toRemove.id)
    el.classList.remove(toRemove.cssClass)
  }

  async function requestMyName (maybeKeys) {
    const keys = maybeKeys === undefined ? await getKeys() : maybeKeys

    const [powInfo, err] = await getPowInfo()
    if (err !== '') {
      tick(onError, err)
      return
    }
    const pow = proofOfWork(powInfo)

    const request = combineMany(
      oneByte(1),
      pow,
      keys.signing.publicKey,
      keys.encryption.publicKey
    )

    const [response, responseErr] = await apiRequest(request)
    if (responseErr !== '') {
      tick(onError, responseErr)
      return
    }
    tick(onNewName, decodeInt(response))
  }

  async function cacheQuery (key) {
    const value = await localforage.getItem(key)
    tick(onCacheResponse, { key: key, value: value })
  }

  function ioReplaceChildren (key) {
    const parentEl = document.getElementById(key.parentId)
    while (parentEl.firstChild) {
      parentEl.removeChild(parentEl.lastChild)
    }
    for (const child of key.children) {
      parentEl.appendChild(child)
    }
  }

  function addOnclick (key) {
    const el = document.getElementById(key.id)
    el.onclick = key.onclick
  }

  function cacheValue (toCache) {
    localforage.setItem(toCache.key, toCache.value)
  }

  function updateTextBox (toAdd, _) {
    const box = document.getElementById(toAdd.id)
    box.value = toAdd.value
  }

  async function sendWhitelistRequest (arg) {
    const [powInfo, powErr] = await getPowInfo()
    if (powErr !== '') {
      tick(onError, powErr)
      return
    }

    const pow = proofOfWork(powInfo)
    const [authCode, authErr] = await apiRequest(oneByte(1))
    if (authErr !== '') {
      tick(onError, authErr)
      return
    }

    const idToken = makeIdToken(
      10,
      combine(pow, encodeInt(arg.id)),
      authCode,
      arg.myKeys.signing.secretKey,
      arg.myName
    )

    const request = combineMany([oneByte(10), idToken, pow, encodeInt(arg.id)])
    const [_, responseErr] = await apiRequest(request)
    if (responseErr !== '') {
      tick(onError, responseErr)
      return
    }

    tick(onNewContact, { id: arg.id, keys: arg.keys })
  }

  async function downloadContactKeys (id) {
    const request = combine(oneByte(2), encodeInt(id))
    const [response, responseErr] = await apiRequest(request)
    if (responseErr !== '') {
      tick(onError, responseErr)
      return
    }
    tick(onReceivingContactKeys, { raw: response, id: id })
  }

  async function getInboxSummary (inboxIds) {
    const summaries = []
    for (const id of inboxIds) {
      const message = await localforage.getItem(id)
      const summary = {
        subject: message.subject,
        id: id,
        from: message.from,
        time: message.time
      }
      summaries.push(summary)
    }
    tick(onInboxSummary, summaries)
  }

  async function getDraftsSummary (draftIds) {
    const summaries = []
    for (const id of draftIds) {
      const draft = await localforage.getItem(id)
      const summary = {
        subject: draft.subject,
        id: id,
        to: draft.to
      }
      summaries.push(summary)
    }
    tick(onDraftsSummary, summaries)
  }

  async function getOutboxSummary (outboxIds) {
    const summaries = []
    for (const id of outboxIds) {
      const message = await localforage.getItem(id)
      const summary = {
        subject: message.subject,
        id: id,
        to: message.to,
        time: message.time
      }
      summaries.push(summary)
    }
    tick(onOutboxSummary, summaries)
  }

  async function codeFilesUpload (files) {
    const file = files[0]
    const contents = await file.arrayBuffer()
    tick(onCodeUpload, {
      contents: contents,
      name: file.name,
      size: file.size,
      mime: file.type
    })
  }

  function replaceDomWith (newDom) {
    const old = document.getElementById(newDom.id)
    old.replaceWith(newDom.newDom)
  }

  // For interacting with the WASM generated by Rust. It just wraps
  // up a slightly tidied-up version of the code generated by
  // wasm-pack.
  class Wasm {
    async init (codeBytes) {
      const module_ = await WebAssembly.compile(this.codeBytes)
      this.wasm = await WebAssembly.instantiate(module_, {})
      this.WASM_VECTOR_LEN = 0
      this.mem8 = new Uint8Array(this.wasm.exports.memory.buffer)
      this.mem32 = new Int32Array(this.wasm.exports.memory.buffer)
    }

    _getMem8 () {
      if (this.mem8 !== this.wasm.exports.memory.buffer) {
        this.mem8 = new Uint8Array(this.wasm.exports.memory.buffer)
      }
      return this.mem8
    }

    _passStringToWasm (arg, malloc, realloc) {
      const cachedTextEncoder = new TextEncoder()
      if (realloc === undefined) {
        const buf = cachedTextEncoder.encode(arg)
        const ptr = malloc(buf.length)
        this._getMem8()
          .subarray(ptr, ptr + buf.length)
          .set(buf)
        this.WASM_VECTOR_LEN = buf.length
        return ptr
      }

      let len = arg.length
      let ptr = malloc(len)

      const mem = this._getMem8()

      let offset = 0

      for (; offset < len; offset++) {
        const code = arg.charCodeAt(offset)
        if (code > 0x7f) break
        mem[ptr + offset] = code
      }

      if (offset !== len) {
        if (offset !== 0) {
          arg = arg.slice(offset)
        }
        ptr = realloc(ptr, len, (len = offset + arg.length * 3))

        const view = this._getMem8().subarray(ptr + offset, ptr + len)
        const ret = cachedTextEncoder.encodeInto(arg, view)
        offset += ret.written ? ret.written : 0
      }

      this.WASM_VECTOR_LEN = offset
      return ptr
    }

    _getMem32 () {
      if (this.mem32.buffer !== this.wasm.exports.memory.buffer) {
        this.mem32 = new Int32Array(this.wasm.exports.memory.buffer)
      }
      return this.mem32
    }

    _getArrayU8FromWasm0 (ptr, len) {
      return this._getMem8().subarray(ptr / 1, ptr / 1 + len)
    }

    _run (s) {
      const ptr0 = this._passStringToWasm(
        s,
        this.wasm.exports.__wbindgen_malloc,
        this.wasm.exports.__wbindgen_realloc
      )
      const len0 = this.WASM_VECTOR_LEN
      this.wasm.exports.big_web_thing(8, ptr0, len0)
      const r0 = this._getMem32()[8 / 4 + 0]
      const r1 = this._getMem32()[8 / 4 + 1]
      const v1 = this._getArrayU8FromWasm0(r0, r1).slice()
      this.wasm.exports.__wbindgen_free(r0, r1 * 1)
      return v1
    }

    bigWebThing (s) {
      try {
        return this._run(s)
      } catch (err) {
        tick(onError, err)
      }
    }
  }

  async function lookupDraft (id) {
    const message = await localforage.getItem(id)
    tick(onLookedUpDraft, message)
  }

  async function lookupInboxMessage (id) {
    const message = await localforage.getItem(id)
    const compiled = new Wasm()
    await compiled.init(message.code.contents)
    message.output = compiled.bigWebThing(message.userInput)
    tick(onLookedUpInboxMessage, message)
  }

  async function downloadBlob (blobInfo) {
    const loaded = await localforage.getItem(blobInfo.id)
    const blob = new Blob([loaded], { type: blobInfo.mime })

    const url = window.URL.createObjectURL(blob)

    const a = document.createElement('a')
    a.href = url
    a.download = blobInfo.filename
    document.body.appendChild(a)
    a.click()
    document.body.removeChild(a)

    window.URL.revokeObjectURL(url)
  }

  async function blobFilesUpload (info) {
    for (const file of info.files) {
      const contents = await file.arrayBuffer()
      tick(onBlobUpload, {
        contents: contents,
        name: file.name,
        size: file.size,
        mime: file.type
      })
    }
  }

  async function lookupOutboxMessage (id) {
    const message = await localforage.getItem(id)
    const compiled = new Wasm()
    await compiled.init(message.code.contents)
    message.output = compiled.bigWebThing(message.userInput)
    tick(onLookedUpOutboxMessage, message)
  }

  async function runWasm (o) {
    if (o.code === undefined) return
    let runner = o.runner
    if (runner === undefined) {
      runner = new Wasm()
      await runner.init(o.code)
    }
    const output = runner.bigWebThing(o.userInput)
    tick(onNewOutput, output)
  }

  async function makeNonce () {
    const maybeCounter = await localforage.getItem('nonceCounter')
    const counter = maybeCounter === null ? 0 : maybeCounter
    const buffer = new ArrayBuffer(24)
    const result = new Uint8Array(buffer)
    const asI32s = new Int32Array(buffer)
    asI32s[0] = counter
    await localforage.setItem('nonceCounter', counter + 1)
    return result
  }

  async function sendChunk (chunk, toKeys, myKeys, myName, to) {
    const [authCode, authErr] = await apiRequest(oneByte(1))
    if (authErr !== '') {
      return authErr
    }
    const nonce = await makeNonce()
    const encrypted = nacl.box(
      chunk,
      nonce,
      toKeys.encryption,
      myKeys.encryption.secretKey
    )
    const withNonce = combine(nonce, encrypted)
    const request = constructCtoCMessage(
      withNonce,
      to,
      myKeys,
      authCode,
      myName
    )
    const [_, responseErr] = await apiRequest(request)
    if (responseErr !== '') {
      return responseErr
    }
    return ''
  }

  async function sendBytes (bytes, myKeys, myName, to, toKeys) {
    const chunks = chopMessageIntoChunks(bytes)
    const chunksLength = chunks.length
    for (let i = 0; i < chunksLength; i++) {
      const err = await sendChunk(chunks[i], toKeys, myKeys, myName, to)
      if (err !== '') {
        tick(onError, err)
        return
      }
    }
  }

  async function encodeBlob (blob) {
    const contents = await localforage.getItem(blob.id)
    return combineMany(
      oneByte(1),
      encodeString(blob.filename),
      encodeString(blob.mime),
      contents
    )
  }

  async function sendDraft (arg) {
    const draft = arg.draft
    const myKeys = arg.keys
    const myName = arg.myName
    const to = draft.to
    const toKeys = arg.toKeys

    const encodedDraft = encodeDraft(draft)
    const draftErr = await sendBytes(encodedDraft, myKeys, myName, to, toKeys)
    if (draftErr !== '') {
      tick(onError, draftErr)
      return
    }

    for (const blob of draft.blobs) {
      const encodedBlob = await encodeBlob(blob)
      const blobErr = await sendBytes(
        encodedBlob,
        myKeys,
        myName,
        to,
        toKeys
      )
      if (blobErr !== '') {
        tick(onError, blobErr)
        return
      }
    }
    arg.draft.to = to
    tick(onDraftSent(arg.draft))
  }

  let tick
  {
    let state = {}

    tick = (update, inputValue) => {
      let outputs;
      [outputs, state] = update(inputValue, state)
      for (const output of outputs) {
        output.io(output.value)
      }
    }

    tick(onInit, '')
  }
})()
