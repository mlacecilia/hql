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
#include "hqlinclude.ch"
#include "dbinfo.ch"
#include "dbstruct.ch"

/*!

 \brief starting procedure

*/
INIT PROCEDURE ThisInit()

   hb_CdpSelect( hb_CdpOS() )    // to align HVM to os codePage
   hb_SetTermCP( hb_CdpTerm() )  //where <cTermCP> is OS encoding and <cHostCP> is HVM encoding. When <cHostCP> is not given then _SET_CODEPAGE is used
   SET( _SET_OSCODEPAGE, hb_CdpOS() )
   SET( _SET_DBCODEPAGE, "ITWIN" )        // I choose Italian

   SET( _SET_EPOCH, 2000 )
   SET CENTURY ON
   SET( _SET_EXCLUSIVE, .F. )
   SET( _SET_DELETED, .F. )

RETURN

/*!

 \brief ending procedure

*/
EXIT PROCEDURE ThisExit()

   DBCOMMITALL()
   DBCLOSEALL()

RETURN

/*

   standard main procedure

*/
PROCEDURE Main()
   LOCAL oFont

   hqlErrorSys()  /*hbqt_errorsys()*/

   hqlAutoNameEnabled( .T. )

   hqlSetStyle( "Fusion" )

   hqlOnAboutToQuit( { || UDFOnAboutToQuit() } )

   hqlStart()

   oFont := hqlQapplication:font()
   oFont:setPointSize( 14 )
   hqlQapplication:setFont( oFont )

   UDFshowMainWindow()

RETURN

STATIC PROCEDURE UDFOnAboutToQuit()
   hql_Trace( PADR("Quitting QApplication", 25) + hb_TtoS(hb_DateTime()) )
RETURN

/*!

 \brief show mainwindow

*/
STATIC PROCEDURE UDFshowMainWindow()
   LOCAL oWnd, this, oSize
   LOCAL oMlayout
   LOCAL nWarea := 0

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      this := :hqlThis()
      :setWindowTitle( "HQLATMODEL, HQLTABLEVIEW tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/, this) )
      :centralWidget():setLayout( hqlVBoxLayout() )

      WITH OBJECT hqlMenuBar(/*name*/)
         WITH OBJECT :hqlAddMenu(/*name*/)
            :hqlCaption( "&File" ) //==>:setTitle( "&File" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Quit" )  //==>:setText( "&Quit" )
               :setIcon( QIcon( ":/hqlres/quit" ) )
               :setShortcut( QKeySequence( "Alt+Q" ) )
               :hqlOnTriggered( { || oWnd:hqlRelease() } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Close all windows" )
               :hqlOnTriggered( { || hqlQapplication:closeAllWindows() } )
               :setIcon( QIcon( ":/hqlres/exit" ) )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu(/*name*/)
            :setTitle( "&Tools" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&1 create Db" )
               :hqlOnTriggered( { || UDFcreateDb() } )
            END WITH
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&2 connect Db" )
               :hqlOnTriggered( { || nWarea := UDFconnectDb( oWnd:tview() ) } )
            END WITH
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&3 disconnect Db" )
               :hqlOnTriggered( { || nWarea := UDFdisconnectDb( oWnd:tview(), nWarea ) } )
            END WITH
         END WITH

      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()

      WITH OBJECT myBrowse("tview")
         :hqlAddMeToLayout( oMlayout )
      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   hql_exploreChildren( oWnd, .T. )

   oWnd:hqlActivate()

RETURN

STATIC FUNCTION UDFconnectDb( oTview )

   LOCAL nWarea

   nWarea := UDFopenDb()

   oTview:model:setWarea( nWarea )

RETURN nWarea

STATIC FUNCTION UDFdisconnectDb( oTview, nWarea )

   (nWarea)->(DBCLOSEAREA())
   nWarea := 0

   oTview:model:setWarea( nWarea )

RETURN nWarea

STATIC PROCEDURE UDFcreateDb()

   LOCAL aDbstru
   LOCAL cPath
   LOCAL cFile
   LOCAL cExt
   LOCAL lDone
   LOCAL nWorkArea
   LOCAL nCounter
   LOCAL numTasks := 250
   LOCAL oWnd

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

      // better to give a parent
      WITH OBJECT oWnd := hqlProgressDialog( /*name*/, HqlActiveWindow )
         :setWindowModality( Qt_WindowModal )
         :setLabelText( "Task in progress..." )
         :setMinimum( 0 )
         :setMaximum( numTasks )
      END WITH

      hqlQApplication:setOverrideCursor( QCursor( Qt_WaitCursor ) )

      oWnd:hqlActivate()

      IF ( nWorkArea := UDFopenDb() ) > 0
         FOR nCounter := 1 TO numTasks

            oWnd:setValue(nCounter)
            hqlProcessEvents() //   to keep responsive
            IF oWnd:wasCanceled()
               EXIT
            ENDIF

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

         oWnd:setValue(numTasks) // to close/end/destroy see maximum
         (nWorkArea)->(DBCLOSEAREA())

      ENDIF
   ENDIF

   hqlQApplication:restoreOverrideCursor()

   oWnd:hqlRelease() // required

RETURN

STATIC FUNCTION UDFopenDb()

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
