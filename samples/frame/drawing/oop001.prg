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
   LOCAL oMlayout, oHlayout

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      this := :hqlThis()
      :setWindowTitle( "HQLFRAME tester" )
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
      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()

      oHlayout := hqlHBoxLayout()
      oHlayout:addStretch()
      WITH OBJECT hqlSpinBox("myspin")
         :hqlAddMeToLayout( oHlayout )
         :setRange( 0, 200 )
      END WITH
      WITH OBJECT hqlPushButton( /*name*/ )
         :hqlAddMeToLayout( oHlayout )
         :hqlCaption( "press to draw" )
         :hqlOnClicked( { || UDFclicked( oWnd ) } )
      END WITH
      oHlayout:addStretch()
      oMlayout:addLayout( oHlayout )

      WITH OBJECT my_frame():new( "myframe" )
         :hqlAddMeToLayout( oMlayout )
      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFclicked( oWnd )
   LOCAL nRadius := oWnd:myspin:value() // r == 0 clear :-)

   oWnd:myframe:setCircleRadius( nRadius )
   oWnd:myframe:update()

RETURN

// =================================================================

#include "hbclass.ch"
#include "hqlhbqt.ch"

/*!

 \brief my_frame class definition

*/
CREATE CLASS my_frame INHERIT hql_frame

   EXPORTED:
   METHOD init
   METHOD setCircleRadius

   PROTECTED:
   DATA nRadius                           INIT 0
   METHOD __hqlConnect                    // override
   SIGNAL __hql_QPaint

END CLASS

/*!

 \brief initialize object instance
 \param[in] string name OR NIL (MANDATORY)
 \param[in] other Qt arguments
 \return SELF

*/
METHOD init( ... ) CLASS my_frame

   ::hql_frame:init( ... )

   /*
   ::setFrameStyle( hb_BitOr( QFrame_Box, QFrame_Plain ) )
   ::setLineWidth( 0 )
   ::setMidLineWidth( 3 )
   */

   ::setFrameStyle( hb_BitOr( QFrame_Panel, QFrame_Raised ) )
   ::setLineWidth( 2 )
   ::setMidLineWidth( 3 )

RETURN Self

METHOD setCircleRadius( ... ) CLASS my_frame

   IF hb_IsNumeric( hb_Pvalue(1) )
      ::nRadius := hb_Pvalue(1)
   ENDIF

RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] connect signal/event. Returns false if fails else true
 \param[in] none
 \return boolean

*/
METHOD __hqlConnect() CLASS my_frame

   IF ::hql_frame:__hqlConnect() .AND. ;
      ::connect( QEvent_Paint, { | oEvent, oPainter | ::__hql_QPaint( oEvent, oPainter ) } )
      RETURN .T.
   ENDIF

RETURN .F.

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
 \param[in] ... based on event/signal
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QPaint( oEvent, oPainter ) CLASS my_frame

   IF ::nRadius > 0
      oPainter:setBrush( QBrush( QColor( Qt_darkBlue ) ) )
      oPainter:drawEllipse( oEvent:rect:center(), ::nRadius, ::nRadius )
   ENDIF

RETURN .T.  //  I M P O R T A N T ----- REQUIRED
