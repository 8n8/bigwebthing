package main

import "testing"

const jobApplication = `
-- Type your name in double quotes, like this: "Jane Jones"
jobApplication name =
    to "true" (+ "Candidate name is: " name)

to "friend" jobApplication
`

func TestJobApplicationSender(t *testing.T) {
	f, err := truelang(jobApplication)
	if err != nil {
		t.Errorf("truelang failed on correct program:\nPROGRAM:\n%s\nERROR:\n%s", jobApplication, err)
	}

	got := f(&Start)
	if len(got.send) != 1 {
		t.Errorf("expecting one send, but got %d", len(got.send))
	}
	if got.send[0].to != "friend" {
		t.Errorf("expecting a send, but got %v", got)
	}
}
