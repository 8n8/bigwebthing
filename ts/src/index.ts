(function () {
    "use strict";
    const localforage = require("localforage");
    const base64js = require("base64-js");
    const nacl = require("tweetnacl");

    function fromBytes(bytes: Uint8Array): string {
        return base64js.fromByteArray(bytes);
    }

    function toBytes(b64: string): Uint8Array {
        return base64js.toByteArray(b64);
    }

    // @ts-ignore
    const app = Elm.Main.init({ node: document.getElementById("main") });

    interface SignOpen {
        signedMessage: string;
        theirPublicKey: string;
    }

    app.ports.naclSignOpen.subscribe(function (p: SignOpen) {
        const result = nacl.sign.open(
            toBytes(p.signedMessage),
            toBytes(p.theirPublicKey)
        );
        if (result === null) {
            app.ports.naclSignOpenReturn({ message: "", err: true });
            return;
        }
        app.ports.naclSignOpenReturn.send({
            message: fromBytes(result),
            err: false,
        });
    });

    interface toSign {
        message: string;
        mySecretKey: string;
    }

    app.ports.naclSign.subscribe(function (p: toSign) {
        const result = nacl.sign(toBytes(p.message), toBytes(p.mySecretKey));
        app.ports.naclSignReturn.send(fromBytes(result));
    });

    app.ports.naclSignKeyPair.subscribe(function () {
        const result = nacl.sign.keyPair();
        app.ports.naclSignKeyPairReturn.send({
            publicKey: fromBytes(result.publicKey),
            secretKey: fromBytes(result.secretKey),
        });
    });

    app.ports.naclBoxKeyPair.subscribe(function () {
        const result = nacl.box.keyPair();
        app.ports.naclBoxKeyPairReturn.send({
            publicKey: fromBytes(result.publicKey),
            secretKey: fromBytes(result.secretKey),
        });
    });

    interface Box {
        message: string;
        nonce: string;
        theirPublicKey: string;
        mySecretKey: string;
    }

    app.ports.naclBox.subscribe(function (p: Box) {
        const result = nacl.box(
            toBytes(p.message),
            toBytes(p.nonce),
            toBytes(p.theirPublicKey),
            toBytes(p.mySecretKey)
        );
        app.ports.naclBoxReturn.send(fromBytes(result));
    });

    interface BoxOpen {
        box: string;
        nonce: string;
        theirPublicKey: string;
        mySecretKey: string;
    }

    app.ports.naclBoxOpen.subscribe(function (p: BoxOpen) {
        const result = nacl.box.open(
            toBytes(p.box),
            toBytes(p.nonce),
            toBytes(p.theirPublicKey),
            toBytes(p.mySecretKey)
        );
        if (result === null) {
            app.ports.naclBoxOpenReturn.send({ message: "", err: true });
            return;
        }
        app.ports.naclBoxOpenReturn.send({
            message: fromBytes(result),
            err: false,
        });
    });

    interface KeyVal {
        key: string;
        value: string;
    }

    app.ports.localSet.subscribe(function (p: KeyVal) {
        localforage.setItem(p.key, toBytes(p.value));
    });

    app.ports.localGet.subscribe(function (key: string) {
        localforage.getItem(key, function (err: string, value: Uint8Array) {
            app.ports.localGetReturn({
                err: err,
                value: fromBytes(value),
            });
        });
    });

    interface wasmToRun {
        name: string;
        userInput: string;
    }

    interface wasmResult {
        err: string;
        output: string;
    }

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

    app.ports.sendWasmToRun.subscribe(function (p: wasmToRun) {
        runWasm(p).then(function (result: wasmResult) {
            app.ports.wasmOutput.send(result);
        });
    });
})();
