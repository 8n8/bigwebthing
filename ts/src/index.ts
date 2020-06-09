(function () {
    "use strict";
    const localforage = require("localforage");
    const base64js = require("base64-js");
    const nacl = require("tweetnacl");

    async function localGet(key: string): Promise<any> {
        const value = await localforage.getItem(key);
        return value;
    }

    async function localSet(key: string, value: any): Promise<void> {
        await localforage.setItem(key, value);
    }

    function fromBytes(bytes: Uint8Array): string {
        return base64js.fromByteArray(bytes);
    }

    function toBytes(b64: string): Uint8Array {
        return base64js.toByteArray(b64);
    }

    // @ts-ignore
    const app = Elm.Main.init({ node: document.getElementById("main") });

    async function rustBindGenWrapper(
        wasm: any,
        userInput: string
    ): Promise<Uint8Array> {
        let WASM_VECTOR_LEN = 0;

        let cachegetUint8Memory0: Uint8Array | null = null;
        function getUint8Memory0() {
            if (
                cachegetUint8Memory0 === null ||
                // @ts-ignore
                cachegetUint8Memory0.buffer !== wasm.exports.memory.buffer
            ) {
                // @ts-ignore
                cachegetUint8Memory0 = new Uint8Array(
                    wasm.exports.memory.buffer
                );
            }
            return cachegetUint8Memory0;
        }

        function passStringToWasm0(
            arg: string,
            malloc: Function,
            realloc: Function
        ) {
            const cachedTextEncoder = new TextEncoder();
            if (realloc === undefined) {
                const buf = cachedTextEncoder.encode(arg);
                const ptr = malloc(buf.length);
                getUint8Memory0()
                    .subarray(ptr, ptr + buf.length)
                    .set(buf);
                WASM_VECTOR_LEN = buf.length;
                return ptr;
            }

            let len = arg.length;
            let ptr = malloc(len);

            const mem = getUint8Memory0();

            let offset = 0;

            for (; offset < len; offset++) {
                const code = arg.charCodeAt(offset);
                if (code > 0x7f) break;
                mem[ptr + offset] = code;
            }

            if (offset !== len) {
                if (offset !== 0) {
                    arg = arg.slice(offset);
                }
                ptr = realloc(ptr, len, (len = offset + arg.length * 3));
                const view = getUint8Memory0().subarray(
                    ptr + offset,
                    ptr + len
                );
                const ret = cachedTextEncoder.encodeInto(arg, view);

                offset += ret.written ? ret.written : 0;
            }

            WASM_VECTOR_LEN = offset;
            return ptr;
        }

        let cachegetInt32Memory0: Int32Array | null = null;
        function getInt32Memory0() {
            if (
                cachegetInt32Memory0 === null ||
                // @ts-ignore
                cachegetInt32Memory0.buffer !== wasm.exports.memory.buffer
            ) {
                // @ts-ignore
                cachegetInt32Memory0 = new Int32Array(
                    wasm.exports.memory.buffer
                );
            }
            return cachegetInt32Memory0;
        }

        function getArrayU8FromWasm0(ptr: number, len: number) {
            return getUint8Memory0().subarray(ptr / 1, ptr / 1 + len);
        }

        function big_web_thing(s: string): Uint8Array {
            const ptr0 = passStringToWasm0(
                s,
                // @ts-ignore
                wasm.exports.__wbindgen_malloc,
                // @ts-ignore
                wasm.exports.__wbindgen_realloc
            );
            const len0 = WASM_VECTOR_LEN;
            // @ts-ignore
            wasm.exports.big_web_thing(8, ptr0, len0);
            const r0 = getInt32Memory0()[8 / 4 + 0];
            const r1 = getInt32Memory0()[8 / 4 + 1];
            const v1 = getArrayU8FromWasm0(r0, r1).slice();
            // @ts-ignore
            wasm.exports.__wbindgen_free(r0, r1 * 1);
            return v1;
        }

        return big_web_thing(userInput);
    }

    let wasmModule: any = null;

    async function runWasm(toRun: wasmToRun): Promise<wasmResult> {
        if (wasmModule === null || wasmModule.name !== toRun.name) {
            const rawWasm = await localforage.getItem(toRun.name);
            const compiled = await WebAssembly.compile(rawWasm);
            wasmModule = await WebAssembly.instantiate(compiled, {});
        }
        try {
            const result = await rustBindGenWrapper(
                wasmModule,
                toRun.userInput
            );
            return { output: fromBytes(result), err: "" };
        } catch (err) {
            return { output: "", err };
        }
    }

    interface FromElm {
        key: string;
        value:
            TupdatedUserInput |
            TupdatedRecipient |
            TupdatedSubject |
            TnewCode |
            TrequestBlob |
            TmakeNewDraft |
            TdeleteBlob |
            TaddNewContact |
            TrunDraftWasm |
            TrunMessageWasm |
            TsendDraft |
            TnewBlob;
    }

    interface TupdatedUserInput {
        id: string;
        userInput: string
    }

    interface TupdatedRecipient {
        id: string;
        recipient: number;
    }

    interface TupdatedSubject {
        id: string;
        subject: string;
    }

    interface TnewCode {
        code: string;
        filename: string;
    }

    interface TrequestBlob {
        id: string;
    }

    interface TmakeNewDraft {}

    interface TdeleteBlob {
        blobId: string;
        draftId: string;
    }

    interface TaddNewContact {
        contact: number;
    }

    interface TrunDraftWasm {
        draftId: string;
    }

    interface TrunMessageWasm {
        messageId: string;
    }

    interface TsendDraft {
        draftId: string;
    }

    interface TnewBlob {
        fileName: string;
        draftId: string;
        contents: string;
    }

    interface draftT {
        to?: number;
        time: number;
        subject: string;
        userInput: string;
        code?: codeT;
        blobs : {[id: string]: String};
    }

    interface codeT {
        fileName: string;
        blobId: string;
    }

    interface draftsT {
        [id: string]: draftT;
    }

    let DRAFTS: draftsT = {};
    async function getDraft(id: string): Promise<draftT | null> {
        if (DRAFTS[id] === null) {
            const draft = await localGet(id);
            if (fromDisk === null) {
                return null
            }
            DRAFTS[id] = draft;
        }
        return DRAFTS[id];
    }

    async function setDraft(id: string, draft: draftT) {
        DRAFTS[id] = draft;
        await localSet(id, draft);
    }

    async function updatedDraftUserInput(t: TupdatedUserInput) {
        let draft: draftT = await getDraft(t.id);
        if (draft === null) {
            return;
        }
        draft.userInput = t.userInput;
        setDraft(t.id, draft);
        app.ports.jsToElm.send(
            {key: "updatedDraft", value: {id: t.id, draft: draft}});
    }

    let IOTA = 0
    async function newIota(): string {
        IOTA = IOTA + 1;
        await localSet("iota", IOTA);
        return IOTA.toString();
    }

    async function newBlob(t: TnewBlob) {
        let draft: draftT = await getDraft(t.draftId);
        if (draft === null) {
            return;
        }
        const contents: Uint8Array = toBytes(t.contents);
        const blobId = newBlob();
        await localSet(blobId, contents);
        draft.code = {fileName: t.fileName, blobId: blobId};
        await setDraft(t.id, draft);
    }

    async function sendDraft(t: TsendDraft) {
        const draft = await getDraft(t.draftId);
        if (!("to" in draft)) {
            app.ports.jsToElm.send(
                {key: "sendError", value: "no recipient"});
        }
        if (!("code" in draft)) {
            app.ports.jsToElm.send(
                {key: "sendError", value: "no code"})
        }
        const encodedDraft: Uint8Array = encodeDraft(draft);
        const cryptoKeys
    }

    async function processFromElm(f: FromElm) {
        if (f.key === "updatedDraftUserInput") {
            updatedDraftUserInput(f.value);
            return;
        }
        if (f.key === "updatedRecipient") {
            updatedRecipient(f.value);
            return;
        }
        if (f.key === "updatedSubject") {
            updatedSubject(f.value);
            return;
        }
        if (f.key === "newCode") {
            newCode(f.value);
            return;
        }
        if (f.key === "requestBlob") {
            requestBlob(f.value);
            return;
        }
        if (f.key === "makeNewDraft") {
            makeNewDraft(f.value);
            return;
        }
        if (f.key === "deleteBlob") {
            deleteBlob(f.value);
            return;
        }
        if (f.key === "addNewContact") {
            addNewContact(f.value);
            return;
        }
        if (f.key === "runDraftWasm") {
            runDraftWasm(f.value);
            return;
        }
        if (f.key === "runMessageWasm") {
            runMessageWasm(f.value);
            return;
        }
        if (f.key === "sendDraft") {
            sendDraft(f.value);
            return;
        }
        if (f.key === "newBlob") {
            newBlob(f.value as TnewBlob);
            return;
        }
    }

    app.ports.elmToJs.subscribe(function (f: FromElm) {
        processFromElm(f);
    })
})();
