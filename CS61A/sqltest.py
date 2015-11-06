import sqlite3

conn = sqlite3.connect('test.db')
print "Opened database successfully";

conn.execute('''create table company
    (ID INT PRIMARY KEY    Not NULL,
    NAME            TEXT   NOT NULL,
    AGE             INT    NOT NULL,
    ADDRESS         CHAR(50),
    SALARY          REAL);''')
print 'Table created successfully';
conn.close()