package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"time"
	"errors"
)

func main() {
	fmt.Printf("Starting build at %s\n", time.Now().Format("Mon Jan 2 15:04:05"))
	err := build()
	if err != nil {
		fmt.Println(err.Error())
		return
	}
}

func build() error {
	ok, err := run("elm", "elm-test")
	fmt.Println(string(ok))
	if err != nil {
		return err
	}

	_, err = run("elm", "elm-format", "--yes", "src", "tests")
	if err != nil {
		panic(err)
	}

	return elmMake()
}

const goPreamble = `
package main

var Elm = %#v
`

func elmMake() error {
	ok, err := run("elm", "elm", "make", "src/Main.elm", "--optimize", "--output=tmp.js")
	fmt.Println(string(ok))
	if err != nil {
		return err
	}

	elm, err := os.ReadFile("elm/tmp.js")
	if err != nil {
		panic(err)
	}

	err = os.Remove("elm/tmp.js")
	if err != nil {
		panic(err)
	}

	err = os.WriteFile("go/elm.go", []byte(fmt.Sprintf(goPreamble, elm)), 0644)
	if err != nil {
		panic(err)
	}

	return nil
}

func run(dir string, name string, args ...string) ([]byte, error) {
	cmd := exec.Command(name, args...)

	var stderr bytes.Buffer
	var stdout bytes.Buffer

	cmd.Stderr = &stderr
	cmd.Stdout = &stdout
    cmd.Dir = dir

	errRun := cmd.Run()

	ok, errOut := ioutil.ReadAll(&stdout)
	if errOut != nil {
		panic(errOut)
	}

	if errRun != nil {
		errBytes, err := ioutil.ReadAll(&stderr)
		if err != nil {
			panic(err)
		}

		return ok, errors.New(string(errBytes))
	}

	return ok, nil
}
