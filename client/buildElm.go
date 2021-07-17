package main

import (
	"bytes"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"time"
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
	ok, err := run("elm-test")
	fmt.Println(string(ok))
	if err != nil {
		return err
	}

	_, err = run("elm-format", "--yes", "src", "tests")
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
	ok, err := run("elm", "make", "src/Main.elm", "--optimize", "--output=tmp.js")
	fmt.Println(string(ok))
	if err != nil {
		return err
	}

	uglify()

	return nil
}

func panicErr(err error) {
	if err != nil {
		panic(err)
	}
}

func uglify() {
	uglify1Cmd := exec.Command(
		"uglifyjs",
		"tmp.js",
		"--compress",
		"pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe")
	uglify1Cmd.Dir = "elm"
	uglify1Out, err := uglify1Cmd.StdoutPipe()
	panicErr(err)

	uglify2Cmd := exec.Command("uglifyjs", "--mangle")

	var uglify2Out bytes.Buffer
	uglify2Cmd.Stdin = uglify1Out
	uglify2Cmd.Stdout = &uglify2Out
	uglify2Cmd.Dir = "elm"

	panicErr(uglify1Cmd.Start())
	panicErr(uglify2Cmd.Start())

	panicErr(uglify1Cmd.Wait())
	panicErr(uglify2Cmd.Wait())

	ugly, err := ioutil.ReadAll(&uglify2Out)
	panicErr(err)
	panicErr(os.WriteFile("go/elm.go", []byte(fmt.Sprintf(goPreamble, ugly)), 0644))
	panicErr(os.Remove("elm/tmp.js"))
}

func run(name string, args ...string) ([]byte, error) {
	cmd := exec.Command(name, args...)

	var stderr bytes.Buffer
	var stdout bytes.Buffer

	cmd.Stderr = &stderr
	cmd.Stdout = &stdout
	cmd.Dir = "elm"

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
