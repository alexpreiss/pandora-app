import './css/globalStyle.css'

import Elm from './Main.elm'

  const app = Elm.Main.embed(
    document.querySelector('div'),
    localStorage.authToken 
  )


// I dont think the elements have loaded at this point, but I still need to have
// this function insdie this file, because I need to have the app constant

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

function setAudio(level) {
  const song = document.getElementById('songAudio')
  song.volume = level
}

function rememberMe(token) {
   localStorage.setItem('authToken', token)}

app.ports.togglePause.subscribe( togglePause )
app.ports.replaySong.subscribe( replaySong )
app.ports.audioLevel.subscribe( setAudio )
app.ports.rememberMe.subscribe( rememberMe )
