(function () {
  ('use strict')

  function initOnClick (page) {
    return {
      io: addOnclick,
      value: {
        id: page + 'Button',
        onclick: () => tick(onTopButtonClick, page)
      }
    }
  }

  function getItem (key) {
    return { io: cacheQuery, value: key }
  }

  const initOutputs = [
    'write',
    'contacts',
    'inbox',
    'drafts',
    'sent',
    'sending',
    'pricing',
    'account',
    'help'
  ].map(initOnClick).concat([
    'inboxSummary',
    'draftsSummary',
    'sendingSummary',
    'sentSummary',
    'myName',
    'contacts',
    'downloads',
    'iota'
  ].map(getItem)).push(() => makeWebsocket())

  function combine (uint8arrays) {
    let totalLength = 0
    for (const uint8array of uint8arrays) {
      totalLength += uint8array.length
    }

    const combined = new Uint8Array(new ArrayBuffer(totalLength))

    let i = 0
    for (const uint8array of uint8arrays) {
      for (const uint8 of uint8array) {
        combined[i] = uint8
        i += 1
      }
    }

    return combined
  }

  function encodeBlob (blob) {
    return combine([
      encodeString(blob.filename),
      encodeString(blob.mime),
      encodeInt64(blob.contents.length),
      blob.contents]
    )
  }

  function encodeBlobs (blobs) {
    const parts = []
    parts.push(encodeInt32(blobs.length))
    for (const blob of blobs) {
      parts.push(encodeBlob(blob))
    }
    return combine(parts)
  }

  function encodeString (string) {
    const encoder = new TextEncoder()
    const encoded = encoder.encode(string)
    const len = encodeInt64(encoded.length)
    return combine([len, encoded])
  }

  function encodeBytes (bytes) {
    return combine([encodeInt64(bytes.length), bytes])
  }

  function encodeCode (code) {
    return combine([
      encodeString(code.filename),
      encodeBytes(code.contents)])
  }

  function encodeDraft (draft, fullBlobs) {
    return combine([
      oneByte(0),
      encodeInt64(draft.to),
      encodeString(draft.subject === undefined ? '' : draft.subject),
      encodeString(
        draft.userInput === undefined ? '' : draft.userInput),
      encodeCode(draft.code),
      encodeBlobs(fullBlobs)]
    )
  }

  function replaceChildren (parentId, newChildren) {
    return () => ioReplaceChildren(parentId, newChildren)
  }

  function makeSubjectDom (subject) {
    const p = document.createElement('p')
    p.textContent = 'Subject: ' + subject
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

  function encodeInt64 (n) {
    const encoded = new Uint8Array(new ArrayBuffer(8))
    for (let i = 0; i < 8; i++) {
      encoded[i] = n >> (i * 8) && 0xff
    }
    return encoded
  }

  function encodeInt32 (n) {
    const encoded = new Uint8Array(new ArrayBuffer(4))
    for (let i = 0; i < 4; i++) {
      encoded[i] = n >> (i * 8) && 0xff
    }
    return encoded
  }

  function decodeInt32 (fourBytes) {
    let result = 0
    for (let i = 0; i < 4; i++) {
      result += fourBytes[i] * Math.pow(256, i)
    }
    return result
  }

  function decodeInt64 (eightBytes) {
    let result = 0
    for (let i = 0; i < 8; i++) {
      result += eightBytes[i] * Math.pow(256, i)
    }
    return result
  }

  function isDifficult (hash, difficulty) {
    for (const h of hash) if (h < difficulty) return false
    return true
  }

  function proofOfWork (powInfo) {
    const buffer = new ArrayBuffer(8)
    const bufferView = new Uint8Array(buffer)
    const counter = new Int32Array(buffer)
    while (true) {
      const combined = combine([powInfo.unique, bufferView])
      const hash = nacl.hash(combined).slice(0, 32)
      if (isDifficult(hash, powInfo.difficulty)) return combined
      counter[0] += 1
    }
  }

  function turnButtonOn (id) {
    return [
      () => addCssClass(id, 'selectedButton'),
      () => removeCssClass(id, 'notSelectedButton')]
  }

  function turnButtonOff (id) {
    return [
      () => addCssClass(id, 'notSelectedButton'),
      () => removeCssClass(id, 'selectedButton')]
  }

  function makeSubjectView (subject) {
    const p = document.createElement('p')
    p.textContent = subject
    return p
  }

  function makeFromView (from) {
    const p = document.createElement('p')
    p.textContent = 'From: ' + from
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
    const stringLength = decodeInt64(raw.slice(i, i + 8))
    i += 8
    const stringBytes = raw.slice(i, i + stringLength)
    i += stringLength
    const decoded = new TextDecoder().decode(stringBytes)
    return [{ key: 'smallString', value: decoded }, '', i]
  }

  function decodeOrdering (raw, i) {
    const rawLen = raw.length
    if (rawLen < 4 + i) {
      return [
        null,
        'ordering at position ' +
                i +
                ' is only ' +
                (rawLen - i) +
                ' bytes long'
      ]
    }
    const numElements = decodeInt32(raw.slice(i, i + 4))
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
      return [null, 'empty', i]
    }
    const indicator = raw[0]
    switch (indicator) {
      case 0:
        return decodeOrdering(raw, i + 1)
      case 1:
        return decodeSmallString(raw, i + 1)
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
    const [doc, decodeErr] = decodeDocument(output)
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
    name.textContent = code.filename

    const size = document.createElement('p')
    const codeSize = code.contents.length
    size.textContent = prettyBytes(codeSize)

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
    const children = []
    if (message.subject !== undefined) {
      children.push(makeSubjectView(message.subject))
    }
    children.concat([
      makeFromView(message.from),
      makeOutputView(message.output),
      makeUserInputView(message.userInput),
      makeCodeView(message.code)
    ])

    if (message.blobs !== undefined) {
      children.push(makeInboxBlobsView(message.blobs))
    }

    return [replaceChildren('page', children)]
  }

  const days = {
    0: 'Sunday',
    1: 'Monday',
    2: 'Tuesday',
    3: 'Wednesday',
    4: 'Thursday',
    5: 'Friday',
    6: 'Saturday'
  }

  const months = {
    0: 'January',
    1: 'February',
    2: 'March',
    3: 'April',
    4: 'May',
    5: 'June',
    6: 'July',
    7: 'August',
    8: 'September',
    9: 'October',
    10: 'November',
    11: 'December'
  }

  function formatTime (intTime) {
    const d = new Date(intTime)
    const hours = d.getHours()
    return [
      hours > 12 ? hours - 12 : hours,
      ':',
      d.getMinutes(),
      hours > 12 ? 'p' : 'a',
      'm ',
      days[d.getDay()],
      ' ',
      d.getDate(),
      ' ',
      months[d.getMonth()],
      ' ',
      d.getFullYear()
    ].join('')
  }

  function makeTimeDom (time) {
    const p = document.getElementById('p')
    p.textContent = formatTime(time)
    return p
  }

  function drawInboxMenuItem (message) {
    const button = document.createElement('button')
    button.type = 'button'
    button.classList.add('messageButton')
    if (message.subject !== undefined) {
      button.appendChild(makeSubjectDom(message.subject))
    }
    button.appendChild(makeFromDom(message.from))
    button.appendChild(makeTimeDom(message.time))
    button.onclick = () => tick(onInboxMenuClick, message.id)
    return button
  }

  function drawInbox (state) {
    if (state.inboxSummary.length === 0) {
      return [replaceChildren('page', [noMessagesDom()])]
    }
    if (state.openInboxItem !== undefined) {
      return drawBoxItemView(state.openInboxItem)
    }
    const inbox = []
    for (const message of state.inboxSummary) {
      inbox.push(drawInboxMenuItem(message))
    }
    return [replaceChildren('page', inbox)]
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

  function drawDraftsMenuItem (draft) {
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
      drafts.push(drawDraftsMenuItem(draftSummary))
    }
    return [replaceChildren('page', drafts)]
  }

  function onNewCryptoKeys (keys, state) {
    state.myKeys = keys
    return [[], state]
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

  function addContactErrorDom (error) {
    const errorMessage = document.createElement('p')
    errorMessage.id = 'addContactErrorMessage'
    errorMessage.textContent = error
    return errorMessage
  }

  function addContactBox (boxContents, error) {
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

    if (error !== undefined) {
      container.appendChild(addContactErrorDom(error))
    }

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
    box.rows = rows.length + 1
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
    if (n < 1e6) {
      return Math.round(n / 1000) + 'KB'
    }
    if (n < 1e9) {
      return Math.round(n / 1e6) + 'MB'
    }
  }

  function makeCodeUploader (code) {
    if (code === undefined) {
      return codeUploaderHelp()
    }

    const div = document.createElement('div')
    div.id = 'codeUploader'

    const title = document.createElement('h1')
    title.textContent = 'Message program'
    div.appendChild(title)

    const filename = document.createElement('span')
    filename.textContent = code.filename
    div.appendChild(filename)

    const size = document.createElement('span')
    size.textContent = 'Size: ' + prettyBytes(code.contents.length)
    div.appendChild(size)

    const deleteButton = document.createElement('button')
    deleteButton.type = 'button'
    deleteButton.onclick = () => tick(onDeleteCode, '')
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
    downloadButton.onclick = () => tick(onDownloadBlob, blob.id)
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
    return [
      () => sendDraft(
        state.openDraft,
        state.myKeys,
        state.myName,
        state.contacts[state.openDraft.to]),
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
    return (draft !== undefined && draft.to !== undefined && draft.code !== undefined)
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
      makeSubjectBox(draft.subject),
      makeToBox(draft.to),
      makeUserInputBox(draft.userInput)
    ]

    if (readyToSend(draft)) children.push(makeSendButton())

    if (draft.output !== undefined) {
      children.push(makeOutputView(draft.output))
    }

    children.concat([
      makeBlobsViewer(draft.blobs),
      makeBlobUploader(),
      makeCodeUploader(draft.code)])

    const runner = () =>
      runWasm(draft.userInput, draft.wasmRunner, draft.code)

    return [replaceChildren('page', children), runner]
  }

  function onNewWebsocket (socket, state) {
    state.websocket = socket
    return [[], state]
  }

  function onBadWebsocket (_, state) {
    return [makeWebsocketAfterDelay, state]
  }

  function makeWebsocketAfterDelay () {
    window.setTimeout(makeWebsocket, 3e4)
  }

  function decodeIdToken (raw) {
    return {
      senderId: raw.slice(0, 8),
      authCode: raw.slice(8, 16),
      signature: raw.slice(16, 112)
    }
  }

  function equalBytes (a, b) {
    const lena = a.length
    const lenb = b.length
    if (lena !== lenb) return false
    for (let i = 0; i < lena; i++) {
      if (b[i] !== a[i]) return false
    }
    return true
  }

  function validIdToken (idToken, msgHash, theirSigningKey) {
    const signed = nacl.sign.open(idToken.signature, theirSigningKey)
    return (signed !== null) && equalBytes(signed, msgHash)
  }

  function decryptMessage (message, state) {
    if (message.length < 122) {
      return ['', 'message is less than 122 bytes long']
    }

    const idToken = decodeIdToken(message.slice(1, 113))

    const signHash = nacl.hash(combine([
      oneByte(8),
      message.slice(121),
      idToken.authCode])).slice(0, 32)

    const sender = decodeInt64(idToken.senderId)

    const theirKeys = state.contacts[sender]
    if (theirKeys === undefined) {
      return ['', 'sender is not in contacts']
    }

    if (!validIdToken(idToken, signHash, theirKeys.signing)) {
      return ['', 'bad ID token']
    }

    const decrypted = nacl.box.open(
      message.slice(153),
      message.slice(121, 153),
      theirKeys.encryption,
      state.myKeys.encryption.secretKey)

    if (decrypted === null) {
      return ['', 'could not decrypt chunk']
    }
    return [decrypted, '']
  }

  function decodeMessage (decrypted) {
    return {
      counter: decodeInt32(decrypted.slice(0, 4)),
      totalChunks: decodeInt32(decrypted.slice(4, 8)),
      totalHash: decrypted.slice(8, 40),
      chunk: decrypted.slice(40)
    }
  }

  function onNewMessage (message, state) {
    const [decrypted, decErr] = decryptMessage(message, state)
    if (decErr !== '') return [[], state]

    const hash = nacl.hash(decrypted).slice(0, 32)
    const name = base64js.fromBytes(hash)

    const decoded = decodeMessage(decrypted)

    state.downloads.push(name)

    const id = state.iota.toString()
    state.iota += 1

    const ioJobs = [
      () => stitchUpMessages({
        downloads: state.downloads,
        message: decoded,
        name: name,
        hash: nacl.hash(message).slice(0, 32),
        myKeys: state.myKeys,
        myName: state.myName,
        id: id
      }),
      setItem('iota', state.iota)
    ]
    return [ioJobs, state]
  }

  function onNewOutput (output, state) {
    if (state.openDraft === undefined) return [[], state]
    state.openDraft.output = output
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

    const myName =
      state.myName === undefined ? 'Requesting...' : state.myName
    children.push(myNameDom(myName))

    children.push(
      addContactBox(state.addContactBox, state.addContactError))

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

  function makeToView (to) {
    const p = document.element('p')
    p.textContent = 'To: ' + to
    return p
  }

  function drawSentingItemView (message) {
    const children = []
    if (message.subject !== undefined) {
      children.push(makeSubjectView(message.subject))
    }
    children.concat([
      makeToView(message.to),
      makeOutputView(message.output),
      makeUserInputView(message.userInput),
      makeCodeView(message.code)])
    if (message.blobs !== undefined) {
      children.push(makeInboxBlobsView(message.blobs))
    }
    return [replaceChildren('page', children)]
  }

  function drawSentingMenuItem (message, onClick) {
    const button = document.createElement('button')
    button.type = 'button'
    button.classList.add('messageButton')
    button.appendChild(makeSubjectDom(message.subject))
    button.appendChild(makeToDom(message.to))
    button.appendChild(makeTimeDom(message.time))
    button.onclick = () => tick(onClick, message.id)
    return button
  }

  function onSendingMenuClick (messageId, state) {
    return [
      [() => lookupMessage(messageId, onLookedUpSendingMessage)],
      state]
  }

  function onSentMenuClick (messageId, state) {
    return [
      [() => lookupMessage(messageId, onLookedUpSentMessage)],
      state]
  }

  function onLookedUpSendingMessage (message, state) {
    if (state.page !== 'sending') {
      return [[], state]
    }
    state.openSendingItem = message
    return [drawSending(state), state]
  }

  function onLookedUpSentMessage (message, state) {
    if (state.page !== 'sent') {
      return [[], state]
    }
    state.openSentItem = message
    return [drawSent(state), state]
  }

  function drawSending (state) {
    if (state.sendingSummary.length === 0) {
      return [replaceChildren('page', [noMessagesDom()])]
    }
    if (state.sendingItem !== undefined) {
      return drawSentingItemView(state.sendingItem)
    }
    const sending = []
    for (const message of Object.values(state.sendingSummary)) {
      sending.push(drawSentingMenuItem(message, onSendingMenuClick))
    }
    return [replaceChildren('page', sending)]
  }

  function drawSent (state) {
    if (state.sentSummary.length === 0) {
      return [replaceChildren('page', [noMessagesDom()])]
    }
    if (state.sentItem !== undefined) {
      return drawSentingItemView(state.sentItem)
    }
    const sent = []
    for (const message of Object.values(state.sentSummary)) {
      sent.push(drawSentingMenuItem(message, onSentMenuClick))
    }
    return [replaceChildren('page', sent)]
  }

  const drawFunc = {
    inbox: drawInbox,
    write: drawWrite,
    contacts: drawContacts,
    drafts: drawDrafts,
    pricing: drawPricing,
    account: drawAccount,
    help: drawHelp,
    sending: drawSending,
    sent: drawSent
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
    p.id = 'myNameDom'
    return p
  }

  function myNameFromCache (maybeMyName, state) {
    if (maybeMyName === null) {
      return [[{ io: requestMyName, value: state.myKeys }], state]
    }
    state.myName = maybeMyName
    if (state.page === 'contacts') {
      return [
        [() => replaceTextContent('myNameDom', maybeMyName)],
        state]
    }
    return [[], state]
  }

  function replaceTextContent (id, text) {
    const element = document.getElementById(id)
    element.textContent = text
  }

  function sentSummaryFromCache (summary, state) {
    if (state.page !== 'sent') return [[], state]
    state.sentSummary = summary === null ? [] : summary
    return [drawSent(state), state]
  }

  function sendingSummaryFromCache (summary, state) {
    if (state.page !== 'sending') return [[], state]
    state.sendingSummary = summary === null ? [] : summary
    return [drawSending(state), state]
  }

  function inboxSummaryFromCache (summary, state) {
    if (state.page !== 'inbox') return [[], state]
    state.inboxSummary = summary === null ? [] : summary
    return [drawInbox(state), state]
  }

  function draftsSummaryFromCache (summary, state) {
    if (state.page !== 'drafts') return [[], state]
    state.draftsSummary = summary === null ? [] : summary
    return [drawDrafts(state), state]
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

  function downloadsFromCache (downloads, state) {
    if (downloads === null) {
      return [[], state]
    }
    state.downloads = downloads
    return [[], state]
  }

  const updateOnCacheResponseSwitch = {
    myName: myNameFromCache,
    inboxSummary: inboxSummaryFromCache,
    draftsSummary: draftsSummaryFromCache,
    sentSummary: sentSummaryFromCache,
    sendingSummary: sendingSummaryFromCache,
    iota: iotaFromCache,
    contacts: contactsFromCache,
    downloads: downloadsFromCache
  }

  function onError (error, state) {
    if (state.errors === undefined) state.errors = []
    state.errors.push(error)
    return [[], state]
  }

  function onNewName (newName, state) {
    state.myName = newName
    if (state.page === 'contacts') return [drawContacts(state), state]
  }

  function setItem (key, value) {
    return () => localforage.setItem(key, value)
  }

  function newSubject (draftId, draftsSummary, subject) {
    for (const draft of draftsSummary) {
      if (draft.id === draftId) {
        draft.subject = subject
        return draftsSummary
      }
    }
    return draftsSummary.push({ subject: subject, id: draftId })
  }

  function newTo (draftId, draftsSummary, to) {
    for (const draft of draftsSummary) {
      if (draft.id === draftId) {
        draft.to = to
        return draftsSummary
      }
    }
    return draftsSummary.push({ to: to, id: draftId })
  }

  function onUpdatedSubjectBox (subject, state) {
    if (state.openDraft === undefined) {
      state.openDraft = {}
    }
    if (state.openDraft.id === undefined) {
      state.openDraft.id = state.iota.toString()
      state.iota += 1
    }
    state.openDraft.subject = subject
    state.draftsSummary = newSubject(
      state.openDraft.id,
      state.draftsSummary,
      subject)
    const ioJobs = [
      () => updateTextBox('writerSubjectBox', subject),
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
      state.openDraft = {}
    }
    if (!validRecipient(to)) {
      return [[() => updateTextBox('writerToBox', '')], state]
    }
    if (state.openDraft.id === undefined) {
      state.openDraft.id = state.iota.toString()
      state.iota += 1
    }
    state.openDraft.to = to
    state.draftsSummary = newTo(
      state.openDraft.id,
      state.draftsSummary,
      to)
    const ioJobs = [
      () => updateTextBox('writerToBox', to),
      setItem('iota', state.iota),
      setItem(state.openDraft.id, state.openDraft)
    ]
    return [ioJobs, state]
  }

  function onCacheResponse (response, state) {
    return updateOnCacheResponseSwitch[response.key](response.value, state)
  }

  function updateAddContactError (error) {
    const errDom = addContactErrorDom(error)
    return () =>
      replaceDomWith('addContactErrorMessage', errDom)
  }

  function onAddContactButtonClick (_, state) {
    const contact = state.addContactBox
    if (contact in state.contacts) {
      const error = contact + ' is already in your contacts'
      state.addContactError = error
      return [[updateAddContactError(error)], state]
    }
    state.addContactBox = ''
    return [
      [() => updateTextBox('addContactBox', ''),
        () => downloadContactKeys(contact)],
      state]
  }

  function onUpdatedAddContactBox (contact, state) {
    delete state.addContactError
    if (!validRecipient(contact)) {
      return [[() => updateTextBox('addContactBox', '')], state]
    }
    state.addContactBox = contact
    return [[() => updateTextBox('addContactBox', contact)], state]
  }

  function onUpdatedUserInput (userInput, state) {
    if (state.openDraft === undefined) {
      state.openDraft = {}
    }
    if (state.openDraft.id === undefined) {
      state.openDraft.id = state.iota
      state.iota += 1
    }
    state.openDraft.userInput = userInput
    const ioJobs = [
      () => updateTextBox('writerUserInputBox', userInput),
      setItem('iota', state.iota),
      setItem(state.openDraft.id, state.openDraft),
      () => runWasm(
        userInput,
        state.openDraft.wasmRunner,
        state.openDraft.code.contents)
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
      () => replaceDomWith('codeUploader', makeCodeUploader(code)),
      setItem('iota', state.iota),
      setItem(state.openDraft.id, state.openDraft),
      () => runWasm(
        state.openDraft.userInput,
        undefined,
        code.contents)
    ]
    return [ioJobs, state]
  }

  function onDeleteCode (_, state) {
    if (state.openDraft === undefined) {
      return [[], state]
    }
    const openDraft = state.openDraft
    delete openDraft.code
    const newUploader = makeCodeUploader(undefined)
    const ioJobs = [
      () => replaceDomWith('codeUploader', newUploader),
      setItem(state.openDraft.id, state.openDraft)
    ]
    return [ioJobs, state]
  }

  function onDeleteContact (contact, state) {
    delete state.contacts[contact]

    const ioJobs = [setItem('contacts', state.contacts)]

    if (state.page === 'contacts') ioJobs.push(drawContacts(state))
    return [ioJobs, state]
  }

  function clearMessageViews (state) {
    delete state.openInboxItem
    delete state.openDraft
    delete state.openSentItem
    delete state.openSendingItem
    return state
  }

  function onTopButtonClick (page, state) {
    if (state.page === page) return [[], state]

    state = clearMessageViews(state)

    const oldPage = state.page
    state.page = page

    const ioJobs = drawFunc[page](state)
    ioJobs.concat(turnButtonOn(page + 'Button'))

    if (oldPage !== undefined) {
      ioJobs.concat(turnButtonOff(oldPage + 'Button'))
    }

    return [ioJobs, state]
  }

  function onDraftsMenuClick (messageId, state) {
    return [
      [() => lookupMessage(messageId, onLookedUpDraft)],
      state]
  }

  function onInboxMenuClick (messageId, state) {
    return [
      [() => lookupMessage(messageId, onLookedUpInboxMessage)],
      state]
  }

  function onInit (_, state) {
    return [initOutputs, state]
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

  function onDeleteBlob (ids, state) {
    if (state.openDraft === undefined) {
      return [[], state]
    }

    const oldBlobs = state.openDraft.blobs

    const newBlobs = []
    for (const oldBlob of oldBlobs) {
      if (
        oldBlob.id === ids.blobId &&
        oldBlob.draftId === ids.draftId) {
        continue
      }
      newBlobs.push(oldBlob)
    }
    state.openDraft.blobs = newBlobs

    const newDom = makeBlobsViewer(newBlobs)

    return [
      [
        setItem(ids.draftId, state.openDraft),
        () => localforage.removeItem(ids.blobId),
        () => replaceDomWith('writerBlobsViewer', newDom)
      ],
      state
    ]
  }

  function findBlob (blobs, id) {
    for (const blob in blobs) {
      if (blob.id === id) return blob
    }
  }

  function onDownloadBlob (blobId, state) {
    if (state.openDraft === undefined) {
      return [[], state]
    }
    const blob = findBlob(state.openDraft.blobs, blobId)
    return [[() => downloadBlob(blob)], state]
  }

  function onCodeFilesUpload (files, state) {
    return [[() => codeFilesUpload(files)], state]
  }

  function onBlobFilesUpload (files, state) {
    return [[() => blobFilesUpload(files)], state]
  }

  function onBlobUpload (blobUpload, state) {
    if (state.openDraft === undefined) {
      state.openDraft = {}
    }
    if (state.openDraft.id === undefined) {
      state.openDraft.id = state.iota.toString()
      state.iota += 1
    }
    if (state.openDraft.blobs === undefined) {
      state.openDraft.blobs = []
    }
    const blobId = state.iota.toString()
    state.iota += 1

    const blob = {
      name: blobUpload.name,
      size: blobUpload.size,
      mime: blobUpload.mime,
      id: blobId
    }
    state.openDraft.blobs.push(blob)
    const newBlobsViewer = makeBlobsViewer(state.openDraft.blobs)
    const ioJobs = [
      () => replaceDomWith('writerBlobsViewer', newBlobsViewer),
      setItem('iota', state.iota),
      setItem(state.openDraft.id, state.openDraft),
      setItem(blobId, blobUpload.contents)
    ]
    return [ioJobs, state]
  }

  function formatHttpError (body, statusCode) {
    return (
      'bad response: ' +
        statusCode +
        ': ' +
        new TextDecoder().decode(body)
    )
  }

  function noMessagesDom () {
    const p = document.createElement('p')
    p.textContent = 'There are no messages in this folder.'
    p.classList.add('noneMessage')
    return p
  }

  // A chunk is like this:
  // + 4 bytes: counter, starting at 0
  // + 4 bytes: total number of chunks in message
  // + 32 bytes: hash of complete message
  // + <= 15kB: chunk
  function chopMessageIntoChunks (message) {
    const chunkLength = 15000
    const hash = nacl.hash(message).slice(0, 32)
    const numChunks = Math.ceil(message.length / chunkLength)
    const numChunksBytes = encodeInt32(numChunks)
    const chunks = []
    for (let i = 0; i < numChunks; i++) {
      const chunkNum = encodeInt32(i)
      const chunkStart = i * chunkLength
      const chunkEnd = (i + 1) * chunkLength
      const chunkBase = message.slice(chunkStart, chunkEnd)
      const combined = combine([chunkNum, numChunksBytes, hash, chunkBase])
      chunks.push(combined)
    }
    return chunks
  }

  function makeIdToken (route, message, authCode, secretSign, myName) {
    const toSign = combine([oneByte(route), message, authCode])
    const hash = nacl.hash(toSign).slice(0, 32)
    const signature = nacl.sign(hash, secretSign)
    return combine([encodeInt64(myName), authCode, signature])
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
    const encodedTo = encodeInt64(to)
    const idToken = makeIdToken(
      8,
      combine([encodedTo, chunk]),
      authCode,
      keys.signing.secretKey,
      myName
    )
    return combine([oneByte(8), idToken, encodedTo, chunk])
  }

  function onReceivingContactKeys (keys, state) {
    const theirKeys = {
      signing: keys.raw.slice(0, 32),
      encryption: keys.raw.slice(32, 64)
    }
    state.contacts[keys.id] = theirKeys
    const whitelist = {
      io: sendWhitelistRequest,
      value: {
        id: keys.id,
        myKeys: state.myKeys,
        myName: state.myName,
        theirKeys: theirKeys
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

  function onDraftSent (d, state) {
    state.draftsSummary = removeDraft(d.draft.id, state.draftsSummary)
    state.sendingSummary[d.sentHash] = {
      id: d.draft.id,
      to: d.draft.to,
      subject: d.draft.subject,
      time: d.sentTime
    }
    const ioJobs = [
      setItem('draftsSummary', state.draftsSummary),
      setItem('sendingSummary', state.sendingSummary)
    ]

    if (state.page === 'drafts') ioJobs.push(drawDrafts(state))
    if (state.page === 'sending') ioJobs.push(drawSending(state))

    return [ioJobs, state]
  }

  async function getKeys () {
    let keys = await localforage.getItem('myKeys')
    if (keys === null) {
      keys = {
        signing: nacl.sign.keyPair(),
        box: nacl.box.keyPair()
      }
      await localforage.setItem('myKeys', keys)
      tick(onNewCryptoKeys, keys)
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

  function addCssClass (id, cssClass) {
    const el = document.getElementById(id)
    el.classList.add(cssClass)
  }

  function removeCssClass (id, cssClass) {
    const el = document.getElementById(id)
    el.classList.remove(cssClass)
  }

  async function requestMyName (maybeKeys) {
    const keys = maybeKeys === undefined ? await getKeys() : maybeKeys

    const [powInfo, err] = await getPowInfo()
    if (err !== '') {
      tick(onError, err)
      return
    }
    const pow = proofOfWork(powInfo)

    const request = combine([
      oneByte(1),
      pow,
      keys.signing.publicKey,
      keys.encryption.publicKey]
    )

    const [response, responseErr] = await apiRequest(request)
    if (responseErr !== '') {
      tick(onError, responseErr)
      return
    }
    tick(onNewName, decodeInt64(response))
  }

  async function cacheQuery (key) {
    const value = await localforage.getItem(key)
    tick(onCacheResponse, { key: key, value: value })
  }

  function ioReplaceChildren (parentId, newChildren) {
    const parentElement = document.getElementById(parentId)
    while (true) {
      const oldChild = parentElement.lastChild
      if (oldChild === null) break
      parentElement.removeChild(oldChild)
    }
    for (const newChild of newChildren) {
      parentElement.appendChild(newChild)
    }
  }

  function addOnclick (key) {
    document.getElementById(key.id).onclick = key.onclick
  }

  function updateTextBox (id, value) {
    document.getElementById(id).value = value
  }

  async function sendWhitelistRequest (arg) {
    const [powInfo, powErr] = await getPowInfo()
    if (powErr !== '') {
      tick(onError, powErr)
      return
    }

    const pow = proofOfWork(powInfo)
    const [authCode, authErr] = await apiRequest(oneByte(7))
    if (authErr !== '') {
      tick(onError, authErr)
      return
    }

    const encodedTheirId = encodeInt64(arg.id)

    const idToken = makeIdToken(
      10,
      combine([pow, encodedTheirId]),
      authCode,
      arg.myKeys.signing.secretKey,
      arg.myName
    )

    const request = combine([oneByte(10), idToken, pow, encodedTheirId])
    const [_, responseErr] = await apiRequest(request)
    if (responseErr !== '') {
      tick(onError, responseErr)
      return
    }

    tick(onNewContact, { id: arg.id, keys: arg.theirKeys })
  }

  async function downloadContactKeys (id) {
    const request = combine([oneByte(2), encodeInt64(id)])
    const [response, responseErr] = await apiRequest(request)
    if (responseErr !== '') {
      tick(onError, responseErr)
      return
    }
    tick(onReceivingContactKeys, { raw: response, id: id })
  }

  async function codeFilesUpload (files) {
    const file = files[0]
    const contents = await file.arrayBuffer()
    tick(onCodeUpload, {
      contents: contents,
      name: file.name,
      mime: file.type
    })
  }

  function replaceDomWith (id, newDom) {
    document.getElementById(id).replaceWith(newDom)
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

  async function lookupMessage (id, onDone) {
    const message = await localforage.getItem(id)

    if (message.code.contents === undefined) {
      tick(onDone, message)
      return
    }

    const userInput =
      message.userInput === undefined ? '' : message.userInput

    const compiled = new Wasm()
    await compiled.init(message.code.contents)
    message.output = compiled.bigWebThing(userInput)
    tick(onDone, message)
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

  async function blobFilesUpload (files) {
    for (const file of files) {
      tick(onBlobUpload, {
        contents: await file.arrayBuffer(),
        name: file.name,
        size: file.size,
        mime: file.type
      })
    }
  }

  async function runWasm (maybeUserInput, oldRunner, code) {
    if (code === undefined) return
    const userInput =
      maybeUserInput === undefined ? '' : maybeUserInput
    let runner = oldRunner
    if (runner === undefined) {
      runner = new Wasm()
      await runner.init(code)
    }
    const output = runner.bigWebThing(userInput)
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
    const [authCode, authErr] = await apiRequest(oneByte(7))
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
    const withNonce = combine([nonce, encrypted])
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

  function onNewReceipt (receipt, state) {
    const idToken = decodeIdToken(receipt.idToken)
    const msg = combine([oneByte(8), receipt.hash, idToken.authCode])
    const msgHash = nacl.hash(msg).slice(0, 32)
    const theirKeys = state.contacts[decodeInt64(idToken.senderId)]
    if (theirKeys === undefined) {
      return [[], state]
    }
    if (!validIdToken(idToken, msgHash, theirKeys.signing)) {
      return [[], state]
    }

    const sendingSummary = state.sendingSummary

    const sending = sendingSummary[idToken.msgHash]
    if (sending === undefined) {
      return [[], state]
    }

    delete sendingSummary[msgHash]
    state.sendingSummary = sendingSummary
    sending.receipt = receipt
    state.sentSummary = sending

    const ioJobs = [
      setItem('sendingSummary', state.sendingSummary),
      setItem('sentSummary', state.sentSummary)]

    if (state.page === 'sending') {
      ioJobs.concat(drawSending(state.sendingSummary))
    }
    if (state.page === 'sent') {
      ioJobs.concat(drawSent(state.sentSummary))
    }

    return [ioJobs, state]
  }

  function removeUsed (downloads, usedUp) {
    const asSet = new Set(usedUp)
    const newDownloads = []
    for (const download of downloads) {
      if (asSet.has(download)) continue
      newDownloads.push(download)
    }
    return newDownloads
  }

  function onNewUnpacked (v, state) {
    const ids = []
    for (const _ of v.message.blobs) {
      ids.push(state.iota)
      state.iota += 1
    }
    state.downloads = removeUsed(state.downloads, v.usedUp)

    return [
      [() => writeToDisk(v.message, v.id, ids),
        setItem('downloads', state.downloads)
      ],
      state]
  }

  function onNewSummary (summary, hash, state) {
    state.inboxSummary.push(summary)
    const theirKeys = state.contacts[summary.from]
    if (theirKeys === undefined) {
      return [[], state]
    }
    return [[
      setItem('inboxSummary', state.inboxSummary),
      () => sendReceipt(
        theirKeys, hash, state.myKeys, state.myName, summary.from)
    ], state]
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

  async function loadBlobs (blobs) {
    const fullBlobs = []
    for (const blob of blobs) {
      fullBlobs.push({
        mime: blob.mime,
        filename: blob.filename,
        contents: await localforage.getItem(blob.id)
      })
    }
    return fullBlobs
  }

  async function sendDraft (draft, myKeys, myName, toKeys) {
    const fullBlobs =
      draft.blobs === undefined ? [] : await loadBlobs(draft.blobs)
    const encodedDraft = encodeDraft(draft, fullBlobs)
    const draftErr = await sendBytes(
      encodedDraft, myKeys, myName, draft.to, toKeys)
    if (draftErr !== '') {
      tick(onError, draftErr)
    }

    const sentHash = nacl.hash(encodedDraft).slice(0, 32)

    const sentTime = new Date().valueOf()

    tick(onDraftSent, { sentTime: sentTime, draft: draft, sentHash: sentHash })
  }

  function makeWebsocket () {
    const socket = new WebSocket('/downloadMessages')
    socket.onopen = (_) => tick(onNewWebsocket, socket)
    socket.onmessage = (e) => tick(onNewMessage, e.data)
    socket.onclose = (_) => tick(onBadWebsocket, '')
    socket.onerror = (_) => tick(onBadWebsocket, '')
  }

  async function getMatchingNames (totalHash, downloads) {
    const names = []
    for (const name of downloads) {
      const contents = await localforage.getItem(name)
      if (equalBytes(contents.totalHash, totalHash)) {
        names.push(name)
      }
    }
    return names
  }

  async function getBlobs (relevantNames) {
    const blobs = []
    for (const name of relevantNames) {
      blobs.push(await localforage.getItem(name))
    }
    return blobs
  }

  async function deleteRemote (hash, mySecretSign, myName) {
    const [authCode, authErr] = await apiRequest(oneByte(7))
    if (authErr !== '') return
    const idToken = makeIdToken(
      9, hash, authCode, mySecretSign, myName)

    const request = combine([oneByte(9), idToken, hash])
    await apiRequest(request)
  }

  function joinChunks (chunks) {
    const assembled = []
    for (const chunk of chunks) {
      assembled.push(chunk.chunk)
    }
    return combine(assembled)
  }

  function sortChunks (chunks) {
    chunks.sort((c1, c2) => c1.counter - c2.counter)
    return chunks
  }

  function chunksAllThere (sortedChunks) {
    const chunksLength = sortedChunks.length
    if (chunksLength === 0) return false

    if (chunksLength !== sortedChunks[0].totalChunks) return false

    for (let i = 0; i < chunksLength; i++) {
      if (sortedChunks[i].counter !== i) {
        return false
      }
    }
    return true
  }

  function unpackDownloads (chunks) {
    const sorted = sortChunks(chunks)
    if (!chunksAllThere(sorted)) return
    return joinChunks(sorted)
  }

  async function writeToDisk (decoded, id, ids) {
    const smallBlobs = []
    for (const blob of decoded.blobs) {
      const blobId = ids.pop()
      smallBlobs.push({
        mime: blob.mime,
        filename: blob.filename,
        size: blob.contents.length,
        id: blobId
      })
      await localforage.setItem(blobId, decoded.contents)
    }
    decoded.blobs = smallBlobs
    localforage.setItem(id, decoded)
  }

  function decodeBlob (raw, i) {
    const dec = new TextDecoder()
    const fileNameLength = decodeInt64(raw.slice(i, i + 8))
    i += 8
    const fileName = dec.decode(raw.slice(i, i + fileNameLength))
    i += fileNameLength
    const mimeLength = decodeInt64(raw.slice(i, i + 8))
    const mime = dec.decode(raw.slice(i, i + mimeLength))
    const blobLength = decodeInt64(raw.slice(i, i + 8))
    i += 8
    const contents = raw.slice(i, i + blobLength)
    i += blobLength
    return [{
      filename: fileName,
      mime: mime,
      contents: contents
    }, i]
  }

  function decodeBlobs (raw, i) {
    const numBlobs = decodeInt32(raw.slice(i, i + 4))
    i += 4
    const blobs = []
    for (let _ = 0; _ < numBlobs; _++) {
      let decoded
      [decoded, i] = decodeBlob(raw, i)
      blobs.push(decoded)
    }
    return [blobs, i]
  }

  function decodeCode (raw, i) {
    const filenameLength = decodeInt64(raw.slice(i, i + 8))
    i += 8
    const filename = new TextDecoder().decode(
      raw.slice(i, i + filenameLength))
    i += filenameLength

    const codeLength = decodeInt64(raw.slice(i, i + 8))
    i += 8
    const code = raw.slice(i, i + codeLength)
    i += codeLength
    return [{ contents: code, filename: filename }, i]
  }

  function decodeDraft (raw) {
    let i = 0
    const to = decodeInt64(raw.slice(i, i + 8))
    const dec = new TextDecoder()
    i += 8
    const subjectLength = decodeInt64(raw.slice(i, i + 8))
    i += 8
    const subject = dec.decode(raw.slice(i, i + subjectLength))
    i += subjectLength
    const userInputLength = decodeInt64(raw.slice(i, i + 8))
    i += 8
    const userInput = dec.decode(raw.slice(i, i + userInputLength))
    i += userInputLength

    let code
    [code, i] = decodeCode(raw, i)

    let blobs
    [blobs, i] = decodeBlobs(raw.slice(i))
    return {
      to: to,
      subject: subject,
      userInput: userInput,
      code: code,
      blobs: blobs
    }
  }

  function decodeUnpacked (raw) {
    const rawLength = raw.length
    if (rawLength === 0) return

    const indicator = raw[0]
    if (indicator === 0) {
      return decodeDraft(raw.slice(1))
    }
    if (indicator === 1) {
      return raw.slice(1)
    }
  }

  async function sendReceipt (theirKeys, hash, myKeys, myName, to) {
    await sendBytes(hash, myKeys, myName, to, theirKeys)
  }

  async function stitchUpMessages (v) {
    await localforage.setItem(v.name, v.message)
    await localforage.setItem('downloads', v.downloads)
    deleteRemote(v.hash, v.myKeys.signing.secretKey, v.myName)

    const relevantNames = await getMatchingNames(
      v.message.totalHash, v.downloads)

    const relevantBlobs = await getBlobs(relevantNames)

    const unpacked = unpackDownloads(relevantBlobs)
    if (unpacked === undefined) return

    for (const name of relevantNames) {
      localforage.removeItem(name)
    }

    const decoded = decodeUnpacked(unpacked)

    if (decoded.time > new Date().valueOf()) return

    if (decoded.subject === undefined) {
      tick(onNewReceipt, decoded)
      return
    }

    tick(onNewUnpacked, { message: decoded, id: v.id, usedUp: relevantNames })
    const summary = {
      subject: decoded.subject,
      from: decoded.from,
      id: v.id,
      time: decoded.time
    }
    const hash = nacl.hash(unpacked.slice(0, 32))
    tick((state) => onNewSummary(summary, hash, state))
  }

  let tick
  {
    let state = { page: 'inbox' }

    tick = (update, inputValue) => {
      let outputs;
      [outputs, state] = update(inputValue, state)
      for (const output of outputs) {
        output()
      }
    }

    tick(onInit, '')
  }
})()
