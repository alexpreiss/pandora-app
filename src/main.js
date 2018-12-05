import './css/globalStyle.css'
import io from 'socket.io-client'

import Elm from './Main.elm'

const socket = io('http://localhost:8000')

  const app = Elm.Main.embed(
    document.querySelector('div'),
    { password : (localStorage.password || null)
    , username : (localStorage.username || null )
    , audioLevel : (localStorage.audioLevel ? +localStorage.audioLevel : null )
    , email : (localStorage.email || null)
    , newUser : (localStorage.newUser || null )
  }
  )

function togglePause() {
  const song = document.getElementById('songAudio')
  if (song.paused) {
    song.play()
  }
  else {

    song.pause()
  }
}
function replaySong() {
  const song = document.getElementById('songAudio')
  song.currentTime = 0
}

function seekTrack(newTime) {
  const song = document.getElementById('songAudio')
  song.currentTime = newTime
}

function setAudio(level) {
  const song = document.getElementById('songAudio')
  song.volume = level
  localStorage.setItem('audioLevel', level)
}

function rememberPassword(password) {
  localStorage.setItem('password', password)}

function rememberEmail(email) {
  localStorage.setItem('email', email)
}

function rememeberNewUser() {
  localStorage.setItem('newUser', false)
}

function rememberUsername(username) {
  localStorage.setItem('username', username)
}

function forgetMe(token) {
  localStorage.removeItem('email')
  localStorage.removeItem('password')
  localStorage.removeItem('username')
  localStorage.removeItem('newUser')
 }

 function progressBarWidth() {
  const progressBar = document.getElementById('progressBar')
  app.ports.sendProgressBarWidth.send((progressBar.getBoundingClientRect()).width)
}



app.ports.getProgressBarWidth.subscribe ( progressBarWidth )
app.ports.togglePause.subscribe( togglePause )
app.ports.replaySong.subscribe( replaySong )
app.ports.audioLevel.subscribe( setAudio )
app.ports.rememberPassword.subscribe( rememberPassword )
app.ports.rememberEmail.subscribe( rememberEmail)
app.ports.sendNewTime.subscribe( seekTrack )
app.ports.logOutLocalStorage.subscribe( forgetMe )
app.ports.newUser.subscribe( rememeberNewUser )
app.ports.rememberUsername.subscribe( rememberUsername )
socket.on('chat message', function(msg){
  app.ports.chatSocket.send(msg)
})
