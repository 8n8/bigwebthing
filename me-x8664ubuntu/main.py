import sqlite3


def main():
    database = sqlite3.connect("bigwebthing.sqlite")
    cursor = database.cursor()
    makeTables(cursor)
    database.commit()
    database.close()


def makeTables(cursor):
    cursor.execute(
        """
CREATE TABLE IF NOT EXISTS senderror (
    sendid INTEGER NOT NULL PRIMARY KEY,
    message TEXT NOT NULL);
"""
    )
    cursor.execute(
        """
CREATE TABLE IF NOT EXISTS send (
    sendid INTEGER NOT NULL PRIMARY KEY,
    filehash BLOB NOT NULL,
    timestamp INTEGER NOT NULL);
"""
    )
    cursor.execute(
        """
CREATE TABLE IF NOT EXISTS contact (
    userid INTEGER NOT NULL PRIMARY KEY,
    noisekey BLOB NOT NULL UNIQUE,
    friendly TEXT NOT NULL UNIQUE);
"""
    )
    cursor.execute(
        """
CREATE TABLE IF NOT EXISTS useraccess (
    filehash BLOB NOT NULL,
    userkey BLOB NOT NULL,
    PRIMARY KEY (filehash, userkey));
"""
    )
    cursor.execute(
        """
CREATE TABLE IF NOT EXISTS appaccess (
    filehash BLOB NOT NULL,
    apphash BLOB NOT NULL);
"""
    )
    cursor.execute(
        """
CREATE TABLE IF NOT EXISTS app (
    filehash BLOB NOT NULL PRIMARY KEY);
"""
    )


main()
