'use strict'
const electron = require('electron')

const app = electron.app // this is our app
const globalShortcut = electron.globalShortcut
const BrowserWindow = electron.BrowserWindow // This is a Module that creates windows


let mainWindow // saves a global reference to mainWindow so it doesn't get garbage collected

app.on('ready', createWindow) // called when electron has initialized

// This will create our app window, no surprise there
function createWindow () {
  mainWindow = new BrowserWindow({
    width: 650,
    height: 572,
    minWidth : 650,
    minHeight: 572,
    title: 'Pandora',
    icon: `file://${ __dirname }/icon.icns`
  })

  const cookie = {url: 'https://www.pandora.com', name: 'csrftoken', value: 'coolestToken'}
  electron.session.defaultSession.cookies.set(cookie, (error) => {
    if (error) console.error(error)
  })

  // display the index.html file
  mainWindow.loadURL(`file://${ __dirname }/index.html`)

  // const reload = globalShortcut.register('CommandOrControl+R', function() {
  // 		console.log('CommandOrControl+R is pressed')
  // 		mainWindow.reload()
  // 	})
  // if (!reload) {
  //   console.log('reload registration failed')
  // }
  // else {
  //   console.log('reload registration bound!')
  // }

  var registered = globalShortcut.register('medianexttrack', function () {
    console.log('medianexttrack pressed');
  });
  if (!registered) {
    console.log('medianexttrack registration failed');
  } else {
    console.log('medianexttrack registration bound!');
  }

  mainWindow.on('closed', function () {
    mainWindow = null
  })

  require('./mainmenu.js')
}

/* Mac Specific things */

// when you close all the windows on a non-mac OS it quits the app
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') { app.quit() }
})

// if there is no mainWindow it creates one (like when you click the dock icon)
app.on('activate', () => {
  if (mainWindow === null) { createWindow() }
})
