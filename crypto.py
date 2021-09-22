from cryptography.hazmat.primitives.ciphers.aead import ChaCha20Poly1305
from cryptography.hazmat.primitives.asymmetric import x25519
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.hmac import HMAC


def read_kk2(my_static, my_ephemeral, their_public, kk2):
    s = make_secret(my_static)
    e = make_secret(my_ephemeral)
    rs = x25519.X25519PublicKey.from_public_bytes(their_public)
    hs = _initialize(True, s, e, rs)
    kk1 = bytearray()

    e_es_ss_initiator(hs)

    _encrypt_and_hash(hs['symmetric_state'], b'')

    hs['re'] = from_public_bytes(bytes(kk2[:DHLEN]))
    e_ee_se_initiator(hs)

    _decrypt_and_hash(hs['symmetric_state'], bytes(kk2[DHLEN:]))

    tx, rx = _split(hs['symmetric_state'])
    return {'tx': tx, 'rx': rx}


def make_kk2(my_static, my_ephemeral, their_public, kk1):
    s = make_secret(my_static)
    e = make_secret(my_ephemeral)
    rs = x25519.X25519PublicKey.from_public_bytes(their_public)
    hs = _initialize(False, s, e, rs)

    hs['re'] = from_public_bytes(bytes(kk1[:DHLEN]))
    _mix_hash(hs['symmetric_state'], _get_public(hs['re']))

    _mix_key(hs['symmetric_state'], hs['s'].exchange(hs['re']))

    _mix_key(hs['symmetric_state'], hs['s'].exchange(hs['rs']))

    _decrypt_and_hash(hs['symmetric_state'], bytes(kk1[DHLEN:]))

    kk2 = bytearray()
    kk2 += _get_public(hs['e'])
    _mix_hash(hs['symmetric_state'], _get_public(hs['e']))

    _mix_key(hs['symmetric_state'], hs['e'].exchange(hs['re']))

    _mix_key(hs['symmetric_state'], hs['e'].exchange(hs['rs']))

    kk2 += _encrypt_and_hash(hs['symmetric_state'], b'')
    rx, tx = _split(hs['symmetric_state']) 
    return kk2, {'tx': tx, 'rx': rx}


def make_kk1(my_static, my_ephemeral, their_public):
    s = make_secret(my_static)
    e = make_secret(my_ephemeral)
    rs = x25519.X25519PublicKey.from_public_bytes(their_public)
    hs = _initialize(True, s, e, rs)
    kk1 = bytearray()

    kk1 += _get_public(hs['e'])

    e_es_ss_initiator(hs)

    kk1 += _encrypt_and_hash(hs['symmetric_state'], b'')

    return kk1


def e_ee_se_initiator(hs):
    _mix_hash(hs['symmetric_state'], _get_public(hs['re']))
    _mix_key(hs['symmetric_state'], hs['e'].exchange(hs['re']))
    _mix_key(hs['symmetric_state'], hs['s'].exchange(hs['re']))


def e_es_ss_initiator(hs):
    _mix_hash(hs['symmetric_state'], _get_public(hs['e']))
    _mix_key(hs['symmetric_state'], hs['e'].exchange(hs['rs']))
    _mix_key(hs['symmetric_state'], hs['s'].exchange(hs['rs']))


MAX_NONCE = 2 ** 64 - 1

# Cipher functions


def _ENCRYPT(k, n, ad, plaintext):
    return ChaCha20Poly1305(k).encrypt(_format_nonce(n), plaintext, ad)


def _DECRYPT(k, n, ad, encrypted):
    return ChaCha20Poly1305(k).decrypt(_format_nonce(n), encrypted, ad)


DHLEN = 32


# Hash functions

HASH_LEN = 32


def _HASH(x):
    digest = hashes.Hash(hashes.BLAKE2s(HASH_LEN))
    digest.update(x)
    return digest.finalize()


def _HMAC_HASH(key, data):
    hmac = HMAC(key, hashes.BLAKE2s(HASH_LEN))
    hmac.update(data)
    return hmac.finalize()


def _HKDF(chaining_key, input_key_material, num_outputs):
    # Sets temp_key = HMAC-HASH(chaining_key, input_key_material).
    temp_key = _HMAC_HASH(chaining_key, input_key_material)

    # Sets output1 = HMAC-HASH(temp_key, byte(0x01)).
    output1 = _HMAC_HASH(temp_key, b"\x01")

    # Sets output2 = HMAC-HASH(temp_key, output1 || byte(0x02)).
    output2 = _HMAC_HASH(temp_key, output1 + b"\x02")

    # If num_outputs == 2 then returns the pair (output1, output2).
    if num_outputs == 2:
        return output1, output2

    # Sets output3 = HMAC-HASH(temp_key, output2 || byte(0x03)).
    output3 = _HMAC_HASH(temp_key, output2 + b"\x03")

    # Returns the triple (output1, output2, output3).
    return output1, output2, output3


# CipherState functions

def _initialize_key(cs, key):
    cs['k'] = key
    cs['n'] = 0


def _has_key(cs):
    return cs['k'] is not None


def _set_nonce(cs, n):
    cs['n'] = n


def _format_nonce(n):
    return (
        b"\x00\x00\x00\x00" + n.to_bytes(length=8, byteorder="little"))


def encrypt_with_ad(cs, ad, plaintext):
    if cs['n'] == MAX_NONCE:
        raise RuntimeError("Nonce has reached maximum")

    if cs['k'] is None:
        return plaintext

    encrypted = _ENCRYPT(cs['k'], cs['n'], ad, plaintext)
    cs['n'] += 1
    return encrypted


def decrypt_with_ad(cs, ad, ciphertext):
    if cs['n'] == MAX_NONCE:
        return RuntimeError("Nonce has reached maximum")

    if cs['k'] is None:
        return ciphertext

    decrypted = _DECRYPT(cs['k'], cs['n'], ad, ciphertext)
    cs['n'] += 1
    return decrypted


# Symmetric state functions


def _initialize_symmetric():
    return {
        "cipher_state": {'k': None, 'n': 0},
        "h": _HASH(b"Noise_KK_25519_ChaChaPoly_BLAKE2s"),
        "ck": _HASH(b"Noise_KK_25519_ChaChaPoly_BLAKE2s"),
    }


def _mix_key(s, input_key_material: bytes):
    """
    :param input_key_material:
    :return:
    """
    # Sets ck, temp_k = HKDF(ck, input_key_material, 2).
    s["ck"], temp_k = _HKDF(s["ck"], input_key_material, 2)

    # Calls InitializeKey(temp_k).
    _initialize_key(s['cipher_state'], temp_k)


def _mix_hash(s, data: bytes):
    s["h"] = _HASH(s["h"] + data)


def _encrypt_and_hash(s, plaintext: bytes) -> bytes:
    ciphertext = encrypt_with_ad(s["cipher_state"], s['h'], plaintext)
    _mix_hash(s, ciphertext)
    return ciphertext


def _decrypt_and_hash(s, ciphertext: bytes) -> bytes:
    plaintext = decrypt_with_ad(
        s["cipher_state"],
        s["h"],
        ciphertext,
    )
    _mix_hash(s, ciphertext)
    return plaintext


def _split(s):
    temp_k1, temp_k2 = _HKDF(s['ck'], b'', 2)
    c1 = {"k": temp_k1, "n": 0}
    c2 = {"k": temp_k2, "n": 0}

    # Returns the pair (c1, c2).
    return c1, c2


def make_public(key):
    return _get_public(x25519.X25519PrivateKey.from_private_bytes(key))


def make_secret(key):
    return x25519.X25519PrivateKey.from_private_bytes(key)


def _get_public(key):
    try:
        key = key.public_key()
    except AttributeError:
        pass

    return key.public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw
    )


# HandshakeState functions

def _initialize(initiator, s, e, rs):
    hs = {
        'symmetric_state': _initialize_symmetric(),
        'cipher_state': {'k': None, 'n': 0},
        's': s,
        'e': e,
        'rs': rs,
        're': None,
        'initiator': initiator,
        'message_patterns': [['e', 'es', 'ss'], ['e', 'ee', 'se']]
    }

    _mix_hash(hs['symmetric_state'], b'')

    _mix_hash(
        hs['symmetric_state'],
        _get_public(s if initiator else rs))
    _mix_hash(
        hs['symmetric_state'],
        _get_public(rs if initiator else s))

    return hs


def _write_message(hs, payload, message_buffer):
    message_pattern = hs['message_patterns'].pop(0)
    for token in message_pattern:
        if token == 'e':
            message_buffer += _get_public(hs['e'])
            _mix_hash(hs['symmetric_state'], _get_public(hs['e']))

        elif token == 'ee':
            _mix_key(hs['symmetric_state'], hs['e'].exchange(hs['re']))

        elif token == 'es':
            if hs['initiator']:
                _mix_key(
                    hs['symmetric_state'],
                    hs['e'].exchange(hs['rs']))

            else:
                _mix_key(
                    hs['symmetric_state'],
                    hs['s'].exchange(hs['re']))

        elif token == 'se':
            if hs['initiator']:
                _mix_key(
                    hs['symmetric_state'],
                    hs['s'].exchange(hs['re']))

            else:
                _mix_key(
                    hs['symmetric_state'],
                    hs['e'].exchange(hs['rs']))

        elif token == 'ss':
            _mix_key(hs['symmetric_state'], hs['s'].exchange(hs['rs']))

        else:
            raise NotImplementedError(f'Pattern token: {token}')

    message_buffer += _encrypt_and_hash(hs['symmetric_state'], payload)

    if len(hs['message_patterns']) == 0:
        return _split(hs['symmetric_state'])


def from_public_bytes(public_bytes):
    if len(public_bytes) != 32:
        raise ValueError("Invalid length of public_bytes! Should be 32")
    return x25519.X25519PublicKey.from_public_bytes(public_bytes)


def _read_message(hs, message, payload_buffer):
    message_pattern = hs['message_patterns'].pop(0)
    for token in message_pattern:
        if token == 'e':
            hs['re'] = from_public_bytes(bytes(message[:DHLEN]))
            message = message[DHLEN:]
            _mix_hash(hs['symmetric_state'], _get_public(hs['re']))

        elif token == 'ee':
            _mix_key(hs['symmetric_state'], hs['e'].exchange(hs['re']))

        elif token == 'es':
            if hs['initiator']:
                _mix_key(
                    hs['symmetric_state'],
                    hs['e'].exchange(hs['rs']))
            else:
                _mix_key(
                    hs['symmetric_state'],
                    hs['s'].exchange(hs['re']))

        elif token == 'se':
            if hs['initiator']:
                _mix_key(
                    hs['symmetric_state'],
                    hs['s'].exchange(hs['re']))
            else:
                _mix_key(
                    hs['symmetric_state'],
                    hs['e'].exchange(hs['rs']))

        elif token == 'ss':
            _mix_key(hs['symmetric_state'], hs['s'].exchange(hs['rs']))

        else:
            raise NotImplementedError(f"Pattern token: {token}")

    payload_buffer += _decrypt_and_hash(
        hs['symmetric_state'],
        bytes(message))

    if len(hs['message_patterns']) == 0:
        return _split(hs['symmetric_state'])
