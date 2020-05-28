@echo off
del *.lrs
lazres.exe GLSceneObjects.lrs @GLSceneObjects.rc
lazres.exe GLScene.lrs @GLScene.rc
lazres.exe nonGLScene.lrs @nonGLScene.rc
lazres.exe GLSceneRunTime.lrs @GLSceneRunTime.rc
pause