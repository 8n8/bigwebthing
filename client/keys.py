from noise.connection import NoiseConnection
from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey

proto = NoiseConnection.from_name(b'Noise_NN_25519_ChaChaPoly_SHA256')

e = X25519PrivateKey.generate()
s = X25519PrivateKey.generate()

proto.noise_protocol.keypairs = {'e': e, 's': s}
proto.set_as_initiator()

proto.start_handshake()
