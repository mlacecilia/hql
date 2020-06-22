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

/*!

 \brief

*/
INIT PROCEDURE ThisInit()

   hb_setTermCP( hb_cdpTerm() )           //where <cTermCP> is OS encoding and <cHostCP> is HVM encoding. When <cHostCP> is not given then _SET_CODEPAGE is used
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

   hqlErrorsys()

   hqlCheckObjectsAlive( .T. )

   hqlFw:automatedName( .T. )

   hqlStart()

   showMainForm()

hb_trace( HB_TR_ALWAYS, REPLICATE("=",30)+" END_MAIN_PROCEDURE "+REPLICATE("=",30) )

RETURN

/*!

 \brief show main form

*/
STATIC PROCEDURE showMainForm()

   LOCAL oWnd

   WITH OBJECT oWnd := hqlMainWindow( /*name*/ )
      :hqlCaption( "HQLINEDIT tester" ) // ==>:setWindowTitle( "tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*name*/ ) )
      :resize( 800, 600 )

      WITH OBJECT hqlMenuBar( /*name*/ )
         WITH OBJECT :hqlAddMenu( /*name*/ )
            :setTitle( "&File" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Quit" )
               :setIcon( QIcon( ":/hqlres/quit" ) )
               :setShortcut( QKeySequence( "Alt+Q" ) )
               :hqlOnTriggered( { || oWnd:close() } )
            END WITH
         END WITH
      END WITH

      WITH OBJECT :centralWidget()

         WITH OBJECT hqlPushButton( /*name*/ )
            :setGeometry( 10, 10, 200, 30 )
            :setText( "basic usage" )
            :hqlOnClicked( { || __oop002( oWnd ) } )
         END WITH

         WITH OBJECT hqlPushButton( /*name*/ )
            :setGeometry( 10, 50, 200, 30 )
            :setText( "hqlValid and hqlWhen tester" )
            :hqlOnClicked( { || __oop003( oWnd ) } )
         END WITH

         WITH OBJECT hqlPushButton( /*name*/ )
            :setGeometry( 10, 90, 200, 30 )
            :setText( "focusxxx tester" )
            :hqlOnClicked( { || __oop004( oWnd ) } )
         END WITH

      END WITH

   END WITH

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

STATIC PROCEDURE __oop004( oWnd )

   LOCAL oProgram

   oProgram := oop004():new( oWnd )

   oProgram:activate()

RETURN
