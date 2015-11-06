import sqlite3
conn = sqlite3.connect('example.db')
c = conn.cursor()
c.execute('''CREATE TABLE stocks
            (date text, tans text, symbol text, qty real, price real)''')
            
c.execute("INSERT INTO stocks VALUES('2015-10-24','BUY', 'RHAT', 100, 35.14)")

conn.commit()

conn.close()
