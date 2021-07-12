package main

import (
	"fmt"
	"os"
	"io"
)

func initProgram() Program {
	return Program{
		Bind: make([]Bind, 0),
		Lookup: make([]Lookup, 0),
		FunctionCall: make([]FunctionCall, 0),
		Module: make([]Module, 0),
		Import: make([]Import, 0),
		Export: make([]Export, 0),
		Scope: make([]Scope, 0),
		Branch: make([]Branch, 0),
		Function: make([]Function, 0),
		PublicKey: make([]PublicKey, 0),
		FriendlyName: make([]FriendlyName, 0),
		Path: make([]Path, 0),
		String: make([]String, 0),
		Int: make([]Int, 0),
		Custom: make([]CustomValue, 0),
	}
}

func main() {
	err := mainErr()
	if err != nil {
		fmt.Printf("%s", err)
	}
}

type Parser struct {
	pos int
	id int
	indent int
}

func parseMain(program *Program, raw []byte) error {
	parser := Parser{pos: 0, id: 0, indent: 0}
	return parseLetIn(program, raw, &parser)
}

func parseLetIn(program *Program, raw []byte, parser *Parser) error {
	for
		ok, tmp := true, parser.pos;
		ok;
		ok, tmp = (parser.pos > tmp), parser.pos {

		err := parseBind(program, raw, parser)
		if err != nil {
			return err
		}
	}

	return parseValue(program, raw, parser)
}

func parseBind(program *Program, raw []byte, parser *Parser) error {
	start := parser.pos

	name, err := parseName(raw, parser)
	if err != nil {
		return BindParseError{err}
	}

	err = parseToken(raw, parser, " = ")
	if err != nil {
		return BindParseError{err}
	}

	value := parser.id
	err = parseValue(program, raw, parser)
	if err != nil {
		return BindParseError{err}
	}

	end := parser.pos

	bind := Bind{Id: parser.id, Name: name, Value: value}
	parser.id++

	location := Location{Id: bind.Id, Start: start, End: end}

	program.Bind = append(program.Bind, bind)
	program.Location = append(program.Location, location)

	return nil
}

func parseToken(raw []byte, parser *Parser, token string) error {
	
}

func parseName(raw []byte, parser *Parser) ([]byte, error) {
	name := make([]byte, 0)

	if !isFirstNameChar(raw[parser.pos]) {
		return name, BadFirstNameChar{
			parser.pos,
			raw[parser.pos],
		}
	}

	for ; isNameChar(raw[parser.pos]); parser.pos++ {
		name = append(name, raw[parser.pos])
	}
	return name, nil
}

func isFirstNameChar(char byte) bool {
	for _, c := range firstNameChars {
		if c == char {
			return true
		}
	}
	return false
}

var firstNameChars = []byte{
	'_', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
	'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
	'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
	'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U',
	'V', 'W', 'X', 'Y', 'Z',
}

func isNameChar(char byte) bool {
	for _, c := range nameChars {
		if c == char {
			return true
		}
	}
	return false
}

var nameChars = []byte{
	'1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '_', 'a',
	'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
	'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y',
	'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
	'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
	'X', 'Y', 'Z',
}

type BadFirstNameChar struct {
	position int
	badChar byte
}

func (b BadFirstNameChar) Error() string {
	return fmt.Sprintf(
		"bad first name character at %d: %c",
		b.position,
		b.badChar)
}

type BindParseError struct {
	err error
}

func (b BindParseError) Error() string {
	return "bad bind parse: " + b.Error()
}

func mainErr() error {
	program := initProgram()
	raw, err := io.ReadAll(os.Stdin)
	if err != nil {
		return err
	}

	err = parseMain(&program, raw)
	if err != nil {
		return err
	}

	return nil
}

type Bind struct {
	Id int
	Name []byte
	Value int
}

type Function struct {
	Value int
	Parameter string
	Return int
	Scope int
	Sign bool
}

type Scope struct {
	Id int
	Bind int
}

type Module struct {
	Value int
	Content int
	Sign bool
}

type Import struct {
	Module int
	Name string
}

type Export struct {
	Module int
	Name string
}

type PublicKey struct {
	Value int
	PublicKey [32]byte
	Sign bool
}

type FriendlyName struct {
	Value int
	FriendlyName string
	Sign bool
}

type Path struct {
	Value int
	Path string
	Sign bool
}

type FunctionCall struct {
	Value int
	Function int
	Argument int
	Sign bool
}

type Lookup struct {
	Value int
	Name string
	Sign bool
}

type Branch struct {
	Value int
	On int
	Result int
}

type String struct {
	Value int
	String string
	Sign bool
}

type Int struct {
	Value int
	Int int
	Sign bool
}

type CustomValue struct {
	Value int
	Custom Custom
	Sign bool
}

type Custom int

const (
	All Custom = iota
	AllModule
	AllPublicKey
	AllFriendlyName
	AllPath
	AllInt
	AllFloat3
)

type Location struct {
	Id int
	Start int
	End int
}

type Program struct {
	Bind []Bind
	Lookup []Lookup
	FunctionCall []FunctionCall
	Module []Module
	Import []Import
	Export []Export
	Scope []Scope
	Branch []Branch
	Function []Function
	PublicKey []PublicKey
	FriendlyName []FriendlyName
	Path []Path
	String []String
	Int []Int
	Custom []CustomValue
	Location []Location
}
