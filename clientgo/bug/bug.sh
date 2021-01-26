#!/bin/sh

# 1
mkdir A
mkdir B
cd A
AID=`~/go/bin/client myid`
cd ../B
BID=`~/go/bin/client myid`

# 2
~/go/bin/client addcontact "$AID"
cd ../A
~/go/bin/client addcontact "$BID"

# 3
~/go/bin/client
cp public ../B

# public contains KK1 from A = 49: OK

# 4
cd ../B
~/go/bin/client
cp public ../A

# public contains KK1 from A and KK2 and KK1 from B = 147: OK

# 5
cd ../A
~/go/bin/client write "$BID" "hi B"
# public contains: KK1 from A, and KK2 and KK1 from B, and transport from A = 188
~/go/bin/client
# public has: KK1 from A, KK2 and *KK1 from B, transport from A, and KK1 and *KK2 from A
# So B should use the second KK1, and the second KK2
cp public ../B

# 6
cd ../B
~/go/bin/client write "$AID" "hi A"
# public now has: KK1 from A, KK2 and *KK1 from B, transport from A, KK1 from A, and *KK2 from A, and *transport from B = 327
# So it's the second KK1 (from B), second KK2 (from A), and second transport
cp public ../A

# 7
cd ../A
~/go/bin/client read

# cleanup
cd ..
rm -rf A
rm -rf B
