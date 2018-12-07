const express = require('express')
const app = express()
var http = require('http').Server(app);
var io = require('socket.io')(http);

console.log(process.env.DATABASE_URL)

var knex = require('knex')({
  client: 'pg',
  connection: process.env.DATABASE_URL,
  debug: true
})

const port = 8000

app.use(express.json())

// knex.schema.createTable('chats', function(table) {
//   table.increments().primary()
//   table.string('email').notNullable()
//   table.string('username').notNullable()
//   table.string('content').notNullable()
//   table.timestamps()
// })
// .then(() => console.log("this resolved"))

app.post('/sendchat', (req, res) => {
  knex('chats').insert({email: req.body.email, username: req.body.username, content: req.body.content})
    .then(() => {res.status(201).send()
      io.emit('chat message', {email : req.body.email, username : req.body.username, content : req.body.content })
    })
    .catch((err) => {
      console.error(err)
      res.status(500).send()
    })
})


app.get('/getchats', (req, res) => {
  knex.select().from('chats').orderBy('id', 'desc')
    .then((chats) => res.send(chats) )
})

http.listen(port, () => console.log(`Server listening on port ${port}!`))
