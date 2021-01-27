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

exit 0

# 4
cd ../B
~/go/bin/client
cp public ../A

# 5
cd ../A
~/go/bin/client write "$BID" "hi B"
~/go/bin/client
cp public ../B

# 6
cd ../B
~/go/bin/client write "$AID" "hi A"
cp public ../A

# 7
cd ../A
~/go/bin/client read

# cleanup
cd ..
rm -rf A
rm -rf B
