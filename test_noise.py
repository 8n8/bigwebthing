import crypto

ENCODED_TEST_LENGTH = 296


def parse_test_data(raw):
    clientStatic = 0
    clientEphemeral = clientStatic + 32
    serverStatic = clientEphemeral + 32
    serverEphemeral = serverStatic + 32
    kk1 = serverEphemeral + 32
    kk2 = kk1 + 48
    plain1 = kk2 + 48
    cipher1 = plain1 + 10
    plain2 = cipher1 + 26
    cipher2 = plain2 + 10

    return {
        "clientStatic": raw[clientStatic:clientEphemeral],
        "clientEphemeral": raw[clientEphemeral:serverStatic],
        "serverStatic": raw[serverStatic:serverEphemeral],
        "serverEphemeral": raw[serverEphemeral:kk1],
        "kk1": raw[kk1:kk2],
        "kk2": raw[kk2:plain1],
        "plain1": raw[plain1:cipher1],
        "cipher1": raw[cipher1:plain2],
        "plain2": raw[plain2:cipher2],
        "cipher2": raw[cipher2:],
    }


def one_cacophony_test(t):
    kk1 = crypto.make_kk1(
        t["clientStatic"],
        t["clientEphemeral"],
        crypto.make_public(t["serverStatic"]),
    )
    assert kk1 == t["kk1"]
    kk2, sx = crypto.make_kk2(
        t["serverStatic"],
        t["serverEphemeral"],
        crypto.make_public(t["clientStatic"]),
        kk1,
    )
    assert kk2 == t["kk2"]
    cx = crypto.read_kk2(
        t["clientStatic"],
        t["clientEphemeral"],
        crypto.make_public(t["serverStatic"]),
        kk2,
    )
    cipher1 = crypto.encrypt_with_ad(cx['tx'], b'', t["plain1"])
    assert cipher1 == t["cipher1"]
    plain1 = crypto.decrypt_with_ad(sx['rx'], b'', cipher1)
    assert plain1 == t["plain1"]
    cipher2 = crypto.encrypt_with_ad(sx['tx'], b'', t["plain2"])
    assert cipher2 == t["cipher2"]
    plain2 = crypto.decrypt_with_ad(cx['rx'], b'', cipher2)
    assert plain2 == t["plain2"]


def test_cacophony():
    with open("noisetestgen/tests", "rb") as f:
        while True:
            raw = f.read(ENCODED_TEST_LENGTH)
            if len(raw) == 0:
                break
            one_cacophony_test(parse_test_data(raw))


test_cacophony()
