@echo off
genMain.exe demos\%1.hs demos\%1.tests.hs > demos\%1.genMain.hs
ghc demos\%1.genMain.hs -ilib -main-is _mainSiette -fno-warn-tabs
demos\%1.genMain.exe
