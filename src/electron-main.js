/* 
 *  Open the Electron GUI process and load the URL specified as
 *  argument.
 *
 *  Usage: node_modules/.bin/electron electron-main.js <ARGS>
 *
 *  E.g., http://localhost:8000/ciao/bndls/
 *        http://localhost:8001/
 */

// TODO: Allow customization of Windows from the Ciao side

const electron = require('electron')
const app = electron.app
const BrowserWindow = electron.BrowserWindow

// Root reference to the main window (avoid GC and automatic close)
let mainWindow

function cleanup() {
  // TODO: send a quit message to the main URL
}

process.on('exit', cleanup);
process.on('SIGINT', cleanup);
process.on('uncaughtException', cleanup);

function createWindow() {
  // Create the browser window.
  mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      nodeIntegration: false // Needed for jquery
    } 
  })

  // mainWindow.loadURL("http://localhost:8000/ciao/bndls/")
  mainWindow.loadURL(process.argv[2])

  mainWindow.on('closed', function() {
    // Dereference the window object
    mainWindow = null
  })
}

// Ready to create windows
app.on('ready', createWindow)

// Customize expected behavior in Mac OS
app.on('window-all-closed', function() {
  if (process.platform !== 'darwin') {
    app.quit()
  }
})
app.on('activate', function() { 
  if (mainWindow === null) {
    createWindow()
  }
})
