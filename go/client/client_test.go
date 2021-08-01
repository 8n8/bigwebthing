
package main

import (
        "testing"
)


func TestCsprng(t *testing.T) {
        var secret [64]byte
        csprng, err := makeNewCsprng(secret[:])
        if err != nil {
                t.Errorf("couldn't make new CSPRNG: %s", err)
        }

        var r1 [10]byte
        n, err := csprng.Read(r1[:])
        if err != nil || n != 10 {
                t.Errorf("couldn't read from CSPRING")
        }

        var r2 [10]byte
        n, err = cspring.Read(r2[:])
        if err != nil || n != 10 {
                t.Errorf("couldn't read from CSPRING (2)")
        }

        if r1 == r2 {
                t.Errorf("CSPRNG generating identical values")
        }
}

