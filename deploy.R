# deploy app
tmp.enc <- options()$encoding
options(encoding = "UTF-8")
rsconnect::deployApp(appFileManifest = "data/fileManifest.txt", appName = "ukony")
options(encoding = tmp.enc)
