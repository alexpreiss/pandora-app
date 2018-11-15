import './css/globalStyle.css'

import Elm from './Main.elm'

  const app = Elm.Main.embed(
    document.querySelector('div'),
    { authToken : (localStorage.authToken || null)
    , audioLevel : (localStorage.audioLevel ? +localStorage.audioLevel : null ) }
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

function rememberMe(token) {
   localStorage.setItem('authToken', token)}

 function progressBarWidth() {
  const progressBar = document.getElementById('progressBar')
  app.ports.sendProgressBarWidth.send((progressBar.getBoundingClientRect()).width)
}



app.ports.getProgressBarWidth.subscribe ( progressBarWidth )
app.ports.togglePause.subscribe( togglePause )
app.ports.replaySong.subscribe( replaySong )
app.ports.audioLevel.subscribe( setAudio )
app.ports.rememberMe.subscribe( rememberMe )
app.ports.sendNewTime.subscribe( seekTrack )
