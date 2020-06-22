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

   hqlAutoNameEnabled( .T. )

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

   WITH OBJECT oWnd := hqlMainWindow( /*name*/ )
      :hqlCaption( "HQL functions key tester" ) // ==>:setWindowTitle( "tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*name*/ ) )

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
            :setTitle( "&Tool" )

            WITH OBJECT :hqlAddMenu( /*name*/ )
               :setTitle( "&Set" )
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&1 F1" )
                  :hqlOnTriggered( { || udfSet(1) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&2 ESC" )
                  :hqlOnTriggered( { || udfSet(2) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&3 Alt+A" )
                  :hqlOnTriggered( { || udfSet(3) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&4 Alt+B" )
                  :hqlOnTriggered( { || udfSet(4) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&5 Shift+F1" )
                  :hqlOnTriggered( { || udfSet(5) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&6 Shift+F2" )
                  :hqlOnTriggered( { || udfSet(6) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&7 Control+F7" )
                  :hqlOnTriggered( { || udfSet(7) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&8 Control+F8" )
                  :hqlOnTriggered( { || udfSet(8) } )
               END WITH

               :addSeparator()

               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&All" )
                  :hqlOnTriggered( { || udfSet(0) } )
               END WITH
            END WITH

            WITH OBJECT :hqlAddMenu( /*name*/ )
               :setTitle( "&Clear" )
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&1 F1" )
                  :hqlOnTriggered( { || udfClear(1) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&2 ESC" )
                  :hqlOnTriggered( { || udfClear(2) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&3 Alt+A" )
                  :hqlOnTriggered( { || udfClear(3) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&4 Alt+B" )
                  :hqlOnTriggered( { || udfClear(4) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&5 Shift+F1" )
                  :hqlOnTriggered( { || udfClear(5) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&6 Shift+F2" )
                  :hqlOnTriggered( { || udfClear(6) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&7 Control+F7" )
                  :hqlOnTriggered( { || udfClear(7) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&8 Control+F8" )
                  :hqlOnTriggered( { || udfClear(8) } )
               END WITH

               :addSeparator()

               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&All" )
                  :hqlOnTriggered( { || udfClear(0) } )
               END WITH
            END WITH

         END WITH

      END WITH

      WITH OBJECT :centralWidget()

         WITH OBJECT hqlLabel( /*name*/ )
            :setGeometry( 10, 10, 400, 120 )
            :setText( "IF set these are functions key to test on window:" + hb_Eol() + ;
                      "<F1> <ESC> <Alt+A> <Alt+B> <Shift+F1> <Shift+F2>" + hb_Eol() + ;
                      "<Ctrl+F7> <Ctrl+F8>" + hb_Eol() + ;
                      "On lineEdit press <Ctrl+F9>" )
         END WITH

         WITH OBJECT hqlLineEdit( /*name*/ )
            :setGeometry( 10, 140, 200, 30 )
            :hqlSetFkey( Qt_Key_F9, Qt_ControlModifier, {|| hql_MsgStop( "CONTROL + F9 pressed" ) } )
         END WITH

         WITH OBJECT hqlPushButton( /*name*/ )
            :setGeometry( 10, 180, 200, 30 )
            :setText( "on child window" )
            :hqlOnClicked( { || __oop002( oWnd ) } )
         END WITH

         WITH OBJECT hqlPushButton( /*name*/ )
            :setGeometry( 10, 220, 200, 30 )
            :setText( "on modal dialog" )
            :hqlOnClicked( { || __oop003( oWnd ) } )
         END WITH

      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE __oop002( oWnd )

   LOCAL oProgram

   oProgram := oop002():new( oWnd )

   oProgram:activate()

RETURN

STATIC PROCEDURE __oop003( oWnd )

   LOCAL oProgram

   oProgram := oop003():new( oWnd )

   oProgram:activate()

RETURN

/*!

   a procedure

*/
STATIC PROCEDURE udfSet( nOpz )

   hb_Default( @nOpz, 0 )

   IF ( nOpz == 0 .OR. nOpz == 1 )
      HqlActiveWindow:hqlSetFkey( Qt_Key_F1,     Qt_NoModifier,      {|| hql_MsgStop( "F1 pressed" ) } )
   ENDIF
   IF ( nOpz == 0 .OR. nOpz == 2 )
      HqlActiveWindow:hqlSetFkey( Qt_Key_Escape, Qt_NoModifier,      {|| hql_MsgStop( "ESC pressed" ) } )
   ENDIF
   IF ( nOpz == 0 .OR. nOpz == 3 )
      HqlActiveWindow:hqlSetFkey( Qt_Key_A,      Qt_AltModifier,     {|| hql_MsgStop( "ALT + A pressed" ) } )
   ENDIF
   IF ( nOpz == 0 .OR. nOpz == 4 )
      HqlActiveWindow:hqlSetFkey( Qt_Key_B,      Qt_AltModifier,     {|| hql_MsgStop( "ALT + B pressed" ) } )
   ENDIF
   IF ( nOpz == 0 .OR. nOpz == 5 )
      HqlActiveWindow:hqlSetFkey( Qt_Key_F1,     Qt_ShiftModifier,   {|| hql_MsgStop( "SHIFT + F1 pressed" ) } )
   ENDIF
   IF ( nOpz == 0 .OR. nOpz == 6 )
      HqlActiveWindow:hqlSetFkey( Qt_Key_F2,     Qt_ShiftModifier,   {|| hql_MsgStop( "SHIFT + F2 pressed" ) } )
   ENDIF
   IF ( nOpz == 0 .OR. nOpz == 7 )
      HqlActiveWindow:hqlSetFkey( Qt_Key_F7,     Qt_ControlModifier, {|| hql_MsgStop( "CONTROL + F7 pressed" ) } )
   ENDIF
   IF ( nOpz == 0 .OR. nOpz == 8 )
      HqlActiveWindow:hqlSetFkey( Qt_Key_F8,     Qt_ControlModifier, {|| hql_MsgStop( "CONTROL + F8 pressed" ) } )
   ENDIF

RETURN

/*!

   a procedure

*/
STATIC PROCEDURE udfClear( nOpz )

   hb_Default( @nOpz, 0 )

   SWITCH nOpz
   CASE 0
      HqlActiveWindow:hqlClearFkey()
      EXIT
   CASE 1
      HqlActiveWindow:hqlClearFkey( Qt_Key_F1,     Qt_NoModifier )
      EXIT
   CASE 2
      HqlActiveWindow:hqlClearFkey( Qt_Key_Escape, Qt_NoModifier )
      EXIT
   CASE 3
      HqlActiveWindow:hqlClearFkey( Qt_Key_A,      Qt_AltModifier )
      EXIT
   CASE 4
      HqlActiveWindow:hqlClearFkey( Qt_Key_B,      Qt_AltModifier )
      EXIT
   CASE 5
      HqlActiveWindow:hqlClearFkey( Qt_Key_F1,     Qt_ShiftModifier )
      EXIT
   CASE 6
      HqlActiveWindow:hqlClearFkey( Qt_Key_F2,     Qt_ShiftModifier )
      EXIT
   CASE 7
      HqlActiveWindow:hqlClearFkey( Qt_Key_F7,     Qt_ControlModifier )
      EXIT
   CASE 8
      HqlActiveWindow:hqlClearFkey( Qt_Key_F8,     Qt_ControlModifier )
      EXIT
   ENDSWITCH

RETURN
