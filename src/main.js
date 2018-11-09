'use strict'
const electron = require('electron')

const app = electron.app // this is our app
const BrowserWindow = electron.BrowserWindow // This is a Module that creates windows

let mainWindow // saves a global reference to mainWindow so it doesn't get garbage collected

app.on('ready', createWindow) // called when electron has initialized



// This will create our app window, no surprise there
function createWindow () {
  mainWindow = new BrowserWindow({
    width: 650,
    height: 550,
    minWidth : 650,
    minHeight: 550,
  })

  const cookie = {url: 'https://www.pandora.com', name: 'csrftoken', value: 'coolestToken'}
  electron.session.defaultSession.cookies.set(cookie, (error) => {
    if (error) console.error(error)
  })

  // display the index.html file
  mainWindow.loadURL(`file://${ __dirname }/index.html`)


  mainWindow.on('closed', function () {
    mainWindow = null
  })
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
