ADDRBOOK
========================================
It is a simple application Hql based that handle an address book
focusing on Hql objects usage.
This app show how to use message, caption, etc.
and text translations. Three languages are available:
   - English (default)
   - Italian
   - Spanish

How to build
========================================
Obviously, before you need QtContribs and Hql libraries built!
So,
   cd addressbok/src
   hbmk2 addrbook.hbp

Build translations
========================================
If you don't want use translations, skip this chapter.
Before start application you need to create .qm files and
copy them along executable
To create .qm files you must use Qt lrelease like:
   lrelease -compress -removeidentical addrbook_es.ts -qm addrbook_es.qm
   lrelease -compress -removeidentical addrbook_it.ts -qm addrbook_it.qm

In the same directory where you will copy the binary executable,
make a directory with name "data" and a subdir with name "translations";
than copy *.qm files.

How to run
========================================
Basically, Qt shared library used. You can take example from bin directory.
You need to change path as you need and do
runner.bat

This is a simple structure
...bin/addrbook.exe
...bin/qt.conf
...bin/runner.bat
...bin/data
...bin/data/translations/addrbook_es.qm
...bin/data/translations/addrbook_it.qm
