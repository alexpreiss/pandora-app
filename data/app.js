const express = require('express')
const db = require('sqlite')

const app = express()
const port = 8000

app.use(express.json())

app.listen(port, () => console.log(`Server listening on port ${port}!`))

db.open('./database.sqlite')
  .then(() => db.run(`CREATE TABLE IF NOT EXISTS Chats (id integer primary key, email text, username, text content text)`))

app.post('/sendchat', (req, res) => {
  db.run(`INSERT INTO chats (email, username, content) VALUES (?, ?, ?)`, [req.body.email, req.body.username, req.body.content])
  .then(() => res.status(201).send())
  .catch((err) => {
    console.error(err)
    res.status(500).send()
  })
})

app.get('/getchats', (req, res) => {
  db.all(`SELECT * FROM chats ORDER BY id DESC`)
    .then((chats) => res.send(chats) )
})
