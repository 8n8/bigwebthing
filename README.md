# BigWebThing

BigWebThing is for creating and publishing documents. Everything on the server is public, but users can restrict access to the content of their documents using cryptography.

## Server cache

+ An SQLITE database, which contains these tables:

    kk1s
        sessionid: first 24 bytes of KK1
        remainder: the other 24 bytes of the KK1
        sender: 32-byte public static Noise key
        recipient: 32-byte public static Noise key
    kk2s
        sessionid:
        kk2: 48-byte KK2
        sender:
        recipient:
    transports
        sessionid:
        transport: 92-byte transport
        sender:
        recipient:
    blobs
        id: 24-byte blob ID
        uploader: 32-byte public static Noise key
    payments
        payer: 32-byte public static Noise key
        amount: in pence
        date:
    
+ A directory full of encrypted blobs, each with a 24-byte name, and no more than 16KB long.

+ A file containing the server static keys. 

+ A file containing the public key of the user authorised to upload payments.

## Client cache

SQLITE database of KX states
    kxstates
        sessionid: 24-byte session ID
        kxstate: 80 bytes

## Hydro KX state encoding

80 bytes
    32 bytes: ephemeral public key
    32 bytes: ephemeral private key
    12 bytes: hash state -> state
    1 byte: hash state -> buf off
    3 bytes: hash state -> align

## KK transport encoding

92 bytes
    36 bytes: crypto header
    24 bytes: encrypted blob ID
    32 bytes: encrypted key to decrypt blob with

## Blob encoding

<= 15937 bytes
    2 bytes: size
    36 bytes: crypto header
    <= 15899: encrypted
        one of
            15899 bytes: not the last chunk
                1 byte: 0
                24 bytes: ID of next blob
                15874 bytes: the chunk
            <= 15899 bytes: the last chunk
                1 byte: 1
                32 bytes: hash of whole file
                <= 15866 bytes: the chunk

## Server API

### To server

48 bytes: XX1

64 bytes: XX3

<= 16KB
    2 bytes: size
    <= 15998 bytes: encrypted
        36 bytes: crypto header
        one of
            81 bytes: KK1        
                1 byte: 0
                32 bytes: recipient
                48 bytes: KK1
            105 bytes: KK2
                1 byte: 1
                24 bytes: session ID
                48 bytes: KK2
                32 bytes: recipient
            149 bytes: transport
                1 byte: 2
                24 bytes: session ID
                92 bytes: transport
                32 bytes: recipient
            <= 15962 bytes: blob
                1 byte: 3
                24 bytes: blob ID
                <= 15937 bytes: blob
            37 bytes: payment
                1 byte: 4
                32 bytes: payer ID
                4 bytes: amount in pence
            25 bytes: get blob
                1 byte: 5
                24 bytes: blob ID

### From server

96 bytes: XX2

<= 15998 bytes
    2 bytes: size
    <= 15996 bytes: encrypted
        36 bytes: crypto header
        one of
            81 bytes: KK1
                1 byte: 0
                32 bytes: sender ID
                48 bytes: KK1
            105 bytes: KK2
                1 byte: 1
                32 bytes: sender ID
                24 bytes: session ID
                48 bytes: KK2
            149 bytes: transport
                1 byte: 2
                32 bytes: sender ID
                24 bytes: session ID
                92 bytes: transport
            <= 15960 bytes: blob
                1 byte: 3
                24 bytes: blob ID
                <= 15935 bytes: blob
