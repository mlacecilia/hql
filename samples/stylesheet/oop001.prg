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

   /* already provided by hql --> hqlFw:registerResData( hbqtres_oop001() ) */

   hqlSetStyle( "Fusion" )

   /*
      WARNING, WARNING at this moment (Qt 5.7) it seems settings a stylesheet break style. IOW, You can use
      - setstyle and customize each widget every time (e.g. widget:setStyleSheet
      OR
      - load a .qss with any defined values, because QApplication:setStyleSheet break the style (e.g. font inheritance)
   */

   hqlLoadQss( ":/qss/mystyle.qss" ) // embedded stylesheet file
   /* hqlLoadQss( "<osPath>mystyle.qss" ) OS local stylesheet file */

   /* this works fine   HqlQApplication:setStyleSheet( "QMainWindow { background-image: url(:pgmhome); background-repeat:no-repeat; background-position:center; }" ) */

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
   LOCAL oWnd, oSize
   LOCAL oMlayout

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      :hqlCaption( "HQL STYLE SHEET tester" ) // ==>:setWindowTitle( "tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*name*/ ) )
      :centralWidget():setLayout( hqlVBoxLayout() )
      /* :setStyleSheet( "QMainWindow { background-image: url(:pgmhome); background-repeat:no-repeat; background-position:center;}" ) */
      :setStyleSheet( "QMainWindow { background-image: url(home.jpg); background-repeat:no-repeat; background-position:center;}" )

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

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :hqlCaption( "&Tools" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Enable/disable" )
               :hqlOnTriggered( { || UDFenableDisable( oWnd ) } )
            END WITH
         END WITH
      END WITH

      WITH OBJECT :centralWidget()
         oMlayout := :layout()

         WITH OBJECT hqlLineEdit( "lineEdit")
            :hqlAddMeToLayout( oMlayout )
            :hqlAlignMeToLayout( oMlayout, Qt_AlignHCenter )
            :setText( "this is a text" )
         END WITH

         WITH OBJECT hqlComboBox( "comboBox" )
            :hqlAddMeToLayout( oMlayout )
            :hqlAlignMeToLayout( oMlayout, Qt_AlignHCenter )
            :hqlAddRow( "row#1" )
            :hqlAddRow( "row#2" )
            :hqlAddRow( "row#3" )
            :hqlAddRow( "row#4" )
         END WITH

         WITH OBJECT hqlDateEdit( "dateEdit" )
            :hqlAddMeToLayout( oMlayout )
            :hqlAlignMeToLayout( oMlayout, Qt_AlignHCenter )
            :hqlSetCurrent()
         END WITH

         WITH OBJECT hqlLineEdit( "paswd")
            :hqlAddMeToLayout( oMlayout )
            :hqlAlignMeToLayout( oMlayout, Qt_AlignHCenter )
            :setEchoMode( QLineEdit_Password )
         END WITH

      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFenableDisable( oWnd )

   IF oWnd:lineEdit:isEnabled()

      oWnd:lineEdit:setEnabled( .F. )
      oWnd:comboBox:setEnabled( .F. )
      oWnd:dateEdit:setEnabled( .F. )
      oWnd:paswd:setEnabled( .F. )

   ELSE

      oWnd:lineEdit:setEnabled( .T. )
      oWnd:comboBox:setEnabled( .T. )
      oWnd:dateEdit:setEnabled( .T. )
      oWnd:paswd:setEnabled( .T. )

   ENDIF

RETURN
