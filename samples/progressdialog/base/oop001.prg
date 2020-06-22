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
#include "fileio.ch"

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

   hqlErrorSys()  /*hbqt_errorsys()*/

   hqlSetStyle( "Fusion" )

   hqlOnAboutToQuit( { || UDFOnAboutToQuit() } )

   hqlStart()

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
   LOCAL oVlayout
   LOCAL oFont

   oFont := hqlQapplication:font()
   oFont:setPointSize( 14 )

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      this := :hqlThis()
      :setWindowTitle( "HQLPROGRESSDIALOG tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/, this) )
      :centralWidget():setLayout( QVBoxLayout() )
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oVlayout := :layout()

      WITH OBJECT hqlPushButton(/*name*/)
         :hqlAddMeToLayout( oVlayout )
         :hqlAlignMeToLayout( oVlayout, Qt_AlignCenter )
         :hqlCaption( "close &AllWindows" )
         :hqlOnClicked( { || hqlQApplication:closeAllWindows() } )
      END WITH

      WITH OBJECT hqlPushButton()
         :hqlAddMeToLayout( oVlayout )
         :hqlAlignMeToLayout( oVlayout, Qt_AlignCenter )
         :hqlCaption( "defined interval" )
         :hqlOnClicked( { || UDFdefined(oWnd) } )
      END WITH

      WITH OBJECT hqlPushButton()
         :hqlAddMeToLayout( oVlayout )
         :hqlAlignMeToLayout( oVlayout, Qt_AlignCenter )
         :hqlCaption( "long process" )
         :hqlOnClicked( { || UDFlong(oWnd) } )
      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFdefined(oParent)
   LOCAL oForm
   LOCAL numTasks := 100000
   LOCAL nAt
   LOCAL lCancelled := .F.

   WITH OBJECT oForm := hqlProgressDialog( /*name*/, oParent )
      :setLabelText( "Task in progress..." )
      //:setCancelButtonText( "Cancel" )
      :setMinimum( 0 )
      :setMaximum( numTasks )
      :hqlOnCanceled( { || hql_MsgStop( "user canceled" ) } )
      :setWindowModality( Qt_WindowModal )
   END WITH

   hqlQApplication:setOverrideCursor( QCursor( Qt_WaitCursor ) )

   oForm:setValue( 0 )
   oForm:hqlActivate()  //oForm:show()

   FOR nAt := 1 TO numTasks

      oForm:setValue( nAt )

      HqlProcessEvents()   // to keep responsive
      // HqlProcessEvents( QEventLoop_ExcludeUserInputEvents )   // to keep responsive but if you don't want to allow user input to interrupt your operation

      IF oForm:wasCanceled()
         lCancelled := .T.
         EXIT
      ENDIF

   NEXT nAt

   // from Qt docs
   // Progress starts at the value set by setMinimum(), and the progress dialog shows that the operation has finished
   //    when you call setValue() with the value set by setMaximum() as its argument.

   //   WARNING: oForm:value() always returns -1
   IF ( lCancelled )
      hql_Trace( "cancelled at value: " + hb_NtoS(nAt) )
   ELSE
      oForm:setValue( numTasks )
      hql_Trace( "end at: " + hb_NtoS(numTasks) )
   ENDIF
//   oForm:hqlRelease() // required

   hqlQApplication:restoreOverrideCursor()

RETURN

STATIC PROCEDURE UDFlong(oParent)
   LOCAL oForm
   LOCAL numTasks := 1000000
   LOCAL nAt
   LOCAL lCancelled := .F.
   LOCAL nHfile

   WITH OBJECT oForm := hqlProgressDialog( /*name*/, oParent )
      :setLabelText( "Task in progress..." )
      //:setCancelButtonText( "Cancel" )
      :setMinimumDuration( 0 )
      :setRange( 0, 0 )
      :setValue( 0 )
      :hqlOnCanceled( { || hql_MsgStop( "user canceled" ) } )
      :setWindowModality( Qt_WindowModal )
   END WITH

   nHfile := hb_Fcreate( hb_FnameMerge( hb_DirBase(), "oop001", "txt" ), FC_NORMAL, FO_READWRITE )
   IF ( nHfile > 0 )

      hqlQApplication:setOverrideCursor( QCursor( Qt_WaitCursor ) )

      oForm:setValue( 0 )
      oForm:hqlActivate()  //oForm:show()

      FSEEK( nHfile, 0 ,FS_END )

      FOR nAt := 1 TO numTasks

         HqlProcessEvents()   // to keep responsive
         // HqlProcessEvents( QEventLoop_ExcludeUserInputEvents )   // to keep responsive but if you don't want to allow user input to interrupt your operation

         IF oForm:wasCanceled()
            lCancelled := .T.
            EXIT
         ELSE
            FWRITE( nHfile, "line #" + STRZERO( nAt, 8 ) + " this is a simple text" + hb_Eol() )
            oForm:setValue( nAt )
         ENDIF

      NEXT nAt

      hqlQApplication:restoreOverrideCursor()

      FCLOSE( nHfile )

      // from Qt docs
      // Progress starts at the value set by setMinimum(), and the progress dialog shows that the operation has finished
      //    when you call setValue() with the value set by setMaximum() as its argument.

      //   WARNING: oForm:value() always returns -1
      IF ( lCancelled )
         hql_Trace( "cancelled at value: " + hb_NtoS(nAt) )
      ELSE
         oForm:setValue( numTasks )
         hql_Trace( "end at: " + hb_NtoS(numTasks) )
      ENDIF

   ELSE

      hql_Trace( "unable to open/create file" )

   ENDIF


RETURN
