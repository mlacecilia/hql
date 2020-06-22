/****************************************************************************
** $HQL_BEGIN_LICENSE$
**
** Copyright (C) 2020 by Luigi Ferraris
**
** This file is part of HQL project
**
** HQL is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** HQL is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with HQL.  If not, see <http://www.gnu.org/licenses/>.
**
** $HQL_END_LICENSE$
****************************************************************************/
#include "hbqtgui.ch"
#include "hbqtcore.ch"

#include "hbclass.ch"
#include "hbtrace.ch"

#include "dbinfo.ch"
#include "dbstruct.ch"
/*!

 \brief

*/
EXIT PROCEDURE __thisExit()
   IF ! ( __hbqt_itemsInGlobalList() == 0 )
      __hbqt_dump_itemsInGlobalList()
   ENDIF
RETURN

/*

   standard main function

   http://www.thedazzlersinc.com/source/2012/06/04/qt-qtableview-example-short-and-quick/
   https://stackoverflow.com/questions/8946127/refreshing-column-header-names-in-qt-model
   http://doc.qt.io/qt-5/sql-presenting.html
   http://doc.qt.io/qt-4.8/modelview.html#2-4-setting-up-headers-for-columns-and-rows

   http://doc.qt.io/qt-5/qstandarditem.html


   itemdelegate sembra che HbQt definisca virtual i metodi
      http://www.bogotobogo.com/Qt/Qt5_QTableView_QItemDelegate_ModelView_MVC.php

*/
PROCEDURE Main( ... )

   hbqt_errorsys()

   ShowWindow()

RETURN

PROCEDURE ShowWindow()

   LOCAL oWnd
   LOCAL nWarea

   WITH OBJECT oWnd := hqlMainWindow()
      :setWindowTitle("HQL test resources")
      :resize( 800, 600 )
      :setCentralWidget( hqlWidget() )

      WITH OBJECT :centralWidget()

         WITH OBJECT hqlPushButton()
            :setGeometry( 10, 10, 150, 30 )
            :setText( "createDb" )
            :hqlOnClicked( { || udfCreaDb() } )
         END WITH

         WITH OBJECT hqlPushButton()
            :setGeometry( 170, 10, 150, 30 )
            :setText( "connectDb" )
            :hqlOnClicked( { || nWarea := udfConnectDb( oWnd:tview() ) } )
         END WITH

         WITH OBJECT hqlPushButton()
            :setGeometry( 330, 10, 150, 30 )
            :setText( "disconnectDb" )
            :hqlOnClicked( { || nWarea := udfDisconnectDb( oWnd:tview(), nWarea ) } )
         END WITH

         WITH OBJECT myBrowse()
            :setGeometry( 10, 90, 700, 400 )
            :setObjectName( "TVIEW" )
         END WITH

      END WITH

   END WITH

   oWnd:hqlActivate()

RETURN

STATIC FUNCTION udfConnectDb( oTview )

   LOCAL nWarea

   nWarea := udfOpenDb()

   oTview:model:setWarea( nWarea )

RETURN nWarea

STATIC FUNCTION udfDisconnectDb( oTview, nWarea )

   (nWarea)->(DBCLOSEAREA())
   nWarea := 0

   oTview:model:setWarea( nWarea )

RETURN nWarea

STATIC PROCEDURE udfCreaDb()

   LOCAL aDbstru
   LOCAL cPath
   LOCAL cFile
   LOCAL cExt
   LOCAL lDone
   LOCAL nWorkArea
   LOCAL nCounter

   hb_FnameSplit( hb_ProgName(), @cPath, @cFile, @cExt )

   aDbStru := {}
   AADD( aDbStru, { "FDSTRING", "C", 030, 0 } )
   AADD( aDbStru, { "FDDATE",   "D", 008, 0 } )
   AADD( aDbStru, { "FDNUMBER", "N", 011, 3 } )
   AADD( aDbStru, { "FDBOOL",   "L", 001, 0 } )

   NETERR( .F. )
   BEGIN SEQUENCE WITH { | oErr | BREAK( oErr ) }

      lDone := DBCREATE( hb_FNameMerge( cPath, "dbftest", RDDINFO( RDDI_TABLEEXT ) ), aDbstru )
      lDone := IIF( NETERR(), .F., lDone )

   RECOVER
      lDone := .F.
   END SEQUENCE

   IF lDone
      IF ( nWorkArea := udfOpenDb() ) > 0
//         FOR nCounter := 1 TO 100
         FOR nCounter := 1 TO 250
            (nWorkArea)->(DBAPPEND())
            IF NETERR()
               EXIT
            ELSE
               (nWorkArea)->( FIELDPUT( FIELDPOS( "FDSTRING" ), "name_" + hb_NtoS( nCounter) ) )
               (nWorkArea)->( FIELDPUT( FIELDPOS( "FDDATE" ),   hb_Date( 1964, 1, 31 ) + nCounter - 1 ) )
               (nWorkArea)->( FIELDPUT( FIELDPOS( "FDNUMBER" ), 12345.678 + nCounter - 1 ) )
               (nWorkArea)->( FIELDPUT( FIELDPOS( "FDBOOL" ), .T. ) )
               (nWorkArea)->(DBCOMMIT())
            ENDIF
         NEXT nCounter
         (nWorkArea)->( DBCLOSEAREA() )
      ENDIF
   ENDIF

RETURN

STATIC FUNCTION udfOpenDb()

   LOCAL cPath
   LOCAL cFile
   LOCAL cExt
   LOCAL nWorkArea := 0
   LOCAL lDone

   hb_FnameSplit( hb_ProgName(), @cPath, @cFile, @cExt )

   IF hb_FileExists( hb_FNameMerge( cPath, "dbftest", RDDINFO( RDDI_TABLEEXT ) ) )

      NETERR( .F. )
      BEGIN SEQUENCE WITH { | oErr | BREAK( oErr ) }
         DBUSEAREA( .T., ;
                    NIL, ;
                    hb_FNameMerge( cPath, "dbftest", RDDINFO( RDDI_TABLEEXT ) ), ;
                    "dbftest" )
         lDone := .T.
      RECOVER
         lDone := .F.
      END SEQUENCE

      IF lDone
         nWorkArea := SELECT()
      ENDIF

   ENDIF

RETURN nWorkArea
