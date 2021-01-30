#!/bin/bash

# Set up two users, A and B, and add each as a contact to the other.
mkdir A
mkdir B
cd A
AID=`~/go/bin/client myid`
cd ../B
BID=`~/go/bin/client myid`
~/go/bin/client addcontact "$AID"
cd ../A
~/go/bin/client addcontact "$BID"

# Update A's crypto
~/go/bin/client
cp public ../B

# Update B's crypto
cd ../B
~/go/bin/client
cp public ../A

# A write a message to B
cd ../A
~/go/bin/client write "$BID" "hi B"
~/go/bin/client
cp public ../B

# B write a message to A
cd ../B
~/go/bin/client write "$AID" "success"
cp public ../A

# A read B's message
cd ../A
~/go/bin/client read

# Cleanup
cd ..
rm -rf A
rm -rf B
