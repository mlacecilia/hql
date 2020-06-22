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
#include "hbclass.ch"
#include "hqlinclude.ch"

/*
*/
CREATE CLASS oop002

   EXPORTED:
   METHOD init
   METHOD activate

   PROTECTED:
   DATA oDialog                           INIT NIL

   HIDDEN:
   METHOD __createForm
   METHOD __udfSet
   METHOD __udfClear

END CLASS

/*!

 \brief initialize object instance
 \param[in] widget parent
 \return SELF

*/
METHOD init( oParent ) CLASS oop002

   ::__createForm( oParent )

RETURN Self

/*!

 \brief activate this dialog
 \param[in] none
 \return SELF

*/
METHOD activate() CLASS oop002

   ::oDialog:hqlActivate()

RETURN Self

/*!

 \brief create form dialog
 \param[in] widget parent
 \return NIL

*/
METHOD __createForm( oParent ) CLASS oop002

   LOCAL oValyout

   WITH OBJECT ::oDialog := hqlChildWindow( /*name*/, oParent )
      :setWindowTitle("HQL functions key tester")
      :resize( 600, 400 )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*name*/ ) )

      WITH OBJECT hqlMenuBar()

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :setTitle( "&File" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Quit" )
               :setIcon( QIcon( ":/hqlres/quit" ) )
               :setShortcut( QKeySequence( "Alt+Q" ) )
               :hqlOnTriggered( { || ::oDialog:close() } )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :setTitle( "&Tool" )

            WITH OBJECT :hqlAddMenu( /*name*/ )
               :setTitle( "&Set" )
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&1 F1" )
                  :hqlOnTriggered( { || ::__udfSet(1) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&2 ESC" )
                  :hqlOnTriggered( { || ::__udfSet(2) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&3 Alt+A" )
                  :hqlOnTriggered( { || ::__udfSet(3) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&4 Alt+B" )
                  :hqlOnTriggered( { || ::__udfSet(4) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&5 Shift+F1" )
                  :hqlOnTriggered( { || ::__udfSet(5) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&6 Shift+F2" )
                  :hqlOnTriggered( { || ::__udfSet(6) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&7 Control+F7" )
                  :hqlOnTriggered( { || ::__udfSet(7) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&8 Control+F8" )
                  :hqlOnTriggered( { || ::__udfSet(8) } )
               END WITH

               :addSeparator()

               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&All" )
                  :hqlOnTriggered( { || ::__udfSet(0) } )
               END WITH
            END WITH

            WITH OBJECT :hqlAddMenu( /*name*/ )
               :setTitle( "&Clear" )
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&1 F1" )
                  :hqlOnTriggered( { || ::__udfClear(1) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&2 ESC" )
                  :hqlOnTriggered( { || ::__udfClear(2) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&3 Alt+A" )
                  :hqlOnTriggered( { || ::__udfClear(3) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&4 Alt+B" )
                  :hqlOnTriggered( { || ::__udfClear(4) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&5 Shift+F1" )
                  :hqlOnTriggered( { || ::__udfClear(5) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&6 Shift+F2" )
                  :hqlOnTriggered( { || ::__udfClear(6) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&7 Control+F7" )
                  :hqlOnTriggered( { || ::__udfClear(7) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&8 Control+F8" )
                  :hqlOnTriggered( { || ::__udfClear(8) } )
               END WITH

               :addSeparator()

               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&All" )
                  :hqlOnTriggered( { || ::__udfClear(0) } )
               END WITH
            END WITH
         END WITH

      END WITH

      WITH OBJECT :centralWidget()

         WITH OBJECT oValyout := hqlVboxLayout( /*name*/ )
            :hqlSetLayoutOf( ::oDialog:centralWidget() )

            // row 0 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setSizePolicy( QSizePolicy( QSizePolicy_MinimumExpanding, QSizePolicy_MinimumExpanding ) )
               :setText( "IF set these are functions key to test on window:" + hb_Eol() + ;
                         "<F1> <ESC> <Alt+A> <Alt+B> <Shift+F1> <Shift+F2>" + hb_Eol() + ;
                         "<Ctrl+F7> <Ctrl+F8>" + hb_Eol() + ;
                         "On lineEdit press <Ctrl+F9>" )
               :hqlAddMeToLayout( oValyout )
            END WITH

            WITH OBJECT hqlLineEdit( /*name*/ )
               :hqlAddMeToLayout( oValyout )
               :hqlSetFkey( Qt_Key_F9, Qt_ControlModifier, {|| hql_MsgStop( "CONTROL + F9 pressed" ) } )
            END WITH

         END WITH

      END WITH

   END WITH

RETURN NIL

/*!

   a procedure

*/
METHOD __udfSet( nOpz ) CLASS oop002

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

RETURN NIL

/*!

   a procedure

*/
METHOD __udfClear( nOpz ) CLASS oop002

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

RETURN NIL
