package main

import (
	"database/sql"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
)

func main() {
	_ = setupDb()
}

func setupDb() *sql.DB {
	db, err := sql.Open("sqlite3", "bigwebthing.sqlite")
	if err != nil {
		panic("couldn't open DB: " + err.Error())
	}

	x := func(sql_ string) {
		_, err := db.Exec(sql_)
		if err != nil {
			panic(fmt.Sprintf("couldn't run SQL: %s: %s", sql_, err))
		}
	}

	x(`
CREATE TABLE IF NOT EXISTS session (
	sessionid BLOB NOT NULL PRIMARY KEY,
	secret BLOB NOT NULL);`)
	x(`
CREATE TABLE IF NOT EXISTS kk1 (
	sessionid BLOB NOT NULL PRIMARY KEY,
	kk1 BLOB NOT NULL);`)
	x(`
CREATE TABLE IF NOT EXISTS kk2 (
	sessionid BLOB NOT NULL PRIMARY KEY,
	kk2 BLOB NOT NULL);`)
	x(`
CREATE TABLE IF NOT EXISTS contact (
	publickey BLOB NOT NULL PRIMARY KEY,
	friendly TEXT NOT NULL UNIQUE);`)
	x(`
CREATE TABLE IF NOT EXISTS file (
	hash BLOB NOT NULL PRIMARY KEY,
	description TEXT NOT NULL,
	isappbool INTEGER NOT NULL,
	timestamp INTEGER NOT NULL);`)
	x(`
CREATE TABLE IF NOT EXISTS appaccess (
	filehash BLOB NOT NULL,
	apphash BLOB NOT NULL,
	PRIMARY KEY(filehash, apphash));`)
	x(`
CREATE TABLE IF NOT EXISTS sendfile (
	id INTEGER NOT NULL PRIMARY KEY,
	filehash BLOB NOT NULL,
	timestamp INTEGER NOT NULL);`)
	x(`
CREATE TABLE IF NOT EXISTS sendfileerror (
	id INTEGER NOT NULL PRIMARY KEY,
	error TEXT NOT NULL);`)

	return db
}
