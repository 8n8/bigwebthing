package main

import (
	"testing"
)

func TestMakeDiffEmpty(t *testing.T) {
	old := ""
	new_ := ""
	expected := StringDiff{
		insert: "",
		start: 0,
		end: 0,
	}
	got := makeDiff(old, new_)
	if got != expected {
		t.Errorf(
			"makeDiff(%s, %s) = %v; want %v",
			old,
			new_,
			got,
			expected)
	}
}

func TestMakeDiffIdentical(t *testing.T) {
	old := "a"
	new_ := "a"
	expected := StringDiff{
		insert: "",
		start: 1,
		end: 0,
	}
	got := makeDiff(old, new_)
	if got != expected {
		t.Errorf(
			"makeDiff(%s, %s) = %v; want %v",
			old,
			new_,
			got,
			expected)
	}
}


func TestMakeDiffSimpleDelete(t *testing.T) {
	old := "aba"
	new_ := "aa"
	expected := StringDiff{
		insert: "",
		start: 1,
		end: 2,
	}
	got := makeDiff(old, new_)
	if got != expected {
		t.Errorf(
			"makeDiff(%s, %s) = %v; want %v",
			old,
			new_,
			got,
			expected)
	}
}


func TestMakeDiffSimpleInsert(t *testing.T) {
	old := ""
	new_ := "a"
	expected := StringDiff{
		insert: "a",
		start: 0,
		end: 0,
	}
	got := makeDiff(old, new_)
	if got != expected {
		t.Errorf(
			"makeDiff(%s, %s) = %v; want %v",
			old,
			new_,
			got,
			expected)
	}
}


func TestMakeDiffSimpleDelete(t *testing.T) {
	old := "a"
	new_ := ""
	expected := StringDiff{
		insert: "",
		start: 0,
		end: 1,
	}
	got := makeDiff(old, new_)
	if got != expected {
		t.Errorf(
			"makeDiff(%s, %s) = %v; want %v",
			old,
			new_,
			got,
			expected)
	}
}


func TestMakeDiffLargerInsert(t *testing.T) {
	old := "ac"
	new_ := "abc"
	expected := StringDiff{
		insert: "",
		start: 1,
		end: 1,
	}
	got := makeDiff(old, new_)
	if got != expected {
		t.Errorf(
			"makeDiff(%s, %s) = %v; want %v",
			old,
			new_,
			got,
			expected)
	}
}
