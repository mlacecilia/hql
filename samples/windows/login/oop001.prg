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

   hqlQApplication:setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
   hqlQApplication:setOrganizationName( "HQL" )
   hqlQApplication:setOrganizationDomain( "hql.it" )
   hqlQApplication:setApplicationName( "hqltest" )

   hqlTranslations:addItem( "qt_", /*cpath*/ )  // when path empty, QLibraryInfo_TranslationsPath used but it works only if qt.conf exists

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

   WITH OBJECT oWnd := hqlMainWindow()
      :hqlCaption( "Login tester" )
      :setCentralWidget( hqlWidget( /*name*/ ) )
      :hqlOnActivate( { |oself| UDFOnActivate(oself) } ) // first/one time
   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   UDFsetBackground( oWnd )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFsetBackground( oWnd )
   LOCAL oRect
   LOCAL oGradient
   LOCAL oBrush
   LOCAL oPalette

   oRect := oWnd:geometry()

//   oGradient := QLinearGradient( oRect:width(), oRect:height(), oRect:width(), 0 )
   oGradient := QLinearGradient( oRect:width(), 0, oRect:width(), oRect:height() )
   oGradient:setColorAt( 0, QColor( 24, 49, 220 ) )
   oGradient:setColorAt( 1, QColor( 160, 244, 248 ) )

   oBrush := QBrush( oGradient  )

   oPalette := oWnd:palette()
   oPalette:setBrush( QPalette_Background, oBrush )

   oWnd:setPalette( oPalette )

RETURN

STATIC PROCEDURE UDFOnActivate( oWnd )
   LOCAL lGrant := myLogin()
   hql_Trace( "login granted: " + hb_ValToExp(lGrant) )
   IF ( !lGrant )
      oWnd:hqlRelease()
   ENDIF
RETURN
