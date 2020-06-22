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

 \brief starting procedure

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

 \brief starting procedure

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

   // registers current program resources
   //hqlRegisterResData( hbqtres_defres() )

   hqlStart()

   showMainForm()

RETURN

/*!

 \brief show main form

*/
STATIC PROCEDURE showMainForm()
   LOCAL oWnd
   LOCAL oVlayout, oHlayout

   WITH OBJECT oWnd := hqlMainWindow( /*name*/ )   // hql objects: after the name follow the QT arguments
      :setWindowTitle("tester")
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*name*/, oWnd ) )
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

   END WITH

   oVlayout := QVBoxLayout( oWnd:centralWidget() )
   oWnd:centralWidget():setLayout( oVlayout )

   WITH OBJECT oWnd:centralWidget()

      oHlayout := QHBoxLayout( oWnd:centralWidget() )
      oHlayout:setContentsMargins( 0, 0, 0, 0 )
      WITH OBJECT hqlPushButton()
         :setText( "only validate" )
         :hqlOnClicked( { || op002( NIL, oWnd ):hqlActivate() } )
         :hqlAddMeToLayout( oHlayout )
      END WITH
      oHlayout:addStretch()
      oVlayout:addLayout( oHlayout, hb_BitOr(Qt_AlignVCenter,Qt_AlignLeft) )

      oHlayout := QHBoxLayout( oWnd:centralWidget() )
      oHlayout:setContentsMargins( 0, 0, 0, 0 )
      WITH OBJECT hqlPushButton()
         :setText( "with fixup" )
         :hqlAddMeToLayout( oHlayout )
         :hqlOnClicked( { || op003( NIL, oWnd ):hqlActivate() } )
      END WITH
      oHlayout:addStretch()
      oVlayout:addLayout( oHlayout, hb_BitOr(Qt_AlignVCenter,Qt_AlignLeft) )

      oHlayout := QHBoxLayout( oWnd:centralWidget() )
      oHlayout:setContentsMargins( 0, 0, 0, 0 )
      WITH OBJECT hqlPushButton()
         :setText( "p004" )
         :hqlAddMeToLayout( oHlayout )
         :hqlOnClicked( { || op004( NIL, oWnd ):hqlActivate() } )
      END WITH
      oHlayout:addStretch()
      oVlayout:addLayout( oHlayout, hb_BitOr(Qt_AlignVCenter,Qt_AlignLeft) )

   END WITH

   oVlayout:addStretch()   // to push up all
   oWnd:hqlActivate()

RETURN
