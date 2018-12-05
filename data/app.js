const express = require('express')
const app = express()
var http = require('http').Server(app);
var io = require('socket.io')(http);
const db = require('sqlite')

const port = 8000

app.use(express.json())

db.open('./database.sqlite')
  .then(() => db.run(`CREATE TABLE IF NOT EXISTS Chats (id integer primary key, email text not null, username text, content text not null)`))

app.post('/sendchat', (req, res) => {
  db.run(`INSERT INTO Chats (email, username, content) VALUES (?, ?, ?)`, [req.body.email, req.body.username, req.body.content])
    .then(() => { res.status(201).send()
      io.emit('chat message', {email : req.body.email, username : req.body.username, content : req.body.content })
    })
    .catch((err) => {
      console.error(err)
      res.status(500).send()
    })
})

app.get('/getchats', (req, res) => {
  db.all(`SELECT * FROM chats ORDER BY id DESC`)
    .then((chats) => res.send(chats) )
})



http.listen(port, () => console.log(`Server listening on port ${port}!`))
