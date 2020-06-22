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
   LOCAL oFont

   hqlErrorSys()  /*hbqt_errorsys()*/

   hqlSetStyle( "Fusion" )

   hqlQApplication:setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
   hqlQApplication:setOrganizationName( "HQL" )
   hqlQApplication:setOrganizationDomain( "hql.it" )
   hqlQApplication:setApplicationName( "hqltest" )

   oFont := hqlQApplication:font()
   oFont:setPointSize(12)
   hqlQApplication:setFont(oFont)

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
      :hqlCaption( "HQLMAINWINDOW tester" )
      :setCentralWidget( hqlWidget( /*name*/ ) )
      :hqlSetFKey( Qt_Key_F9, Qt_AltModifier, { |oself| UDFaltF9Press(oself) } )
      :hqlOnActivate( { |oself| UDFOnActivate(oself) } ) // first/one time
      :hqlOnEachActivation( { |oself| UDFOnEachActivation(oself) } )
      :hqlOnClose( { |oself| UDFOnClose(oself) } )
      :hqlOnDeactivate( { |oself| UDFOnDeactivate(oself) } )
      :hqlOnLanguageChange( { |oself| UDFOnLanguageChange(oself) } )
      :hqlOnMaximize( { |oself| UDFOnMaximize(oself) } )
      :hqlOnMinimize( { |oself| UDFOnMinimize(oself) } )
      :hqlOnResize( { |newSize,oldSize,oself| UDFOnResize(newSize,oldSize,oself) } )
      :hqlOnShow( { |oself| UDFOnShow(oself) } )
   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFaltF9Press(oself)
   hql_Trace( PADR("Fkey pressed: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFOnEachActivation(oself)
   hql_Trace( PADR("OnEachActivation: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFOnActivate(oself)
   hql_Trace( PADR("OnActivate: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFOnClose(oself)
   hql_Trace( PADR("OnClose: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFOnDeactivate(oself)
   hql_Trace( PADR("OnDeactivate: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFOnLanguageChange(oself)
   hql_Trace( PADR("OnLanguageChange: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFOnMaximize(oself)
   hql_Trace( PADR("OnMaximize: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFOnMinimize(oself)
   hql_Trace( PADR("OnMinimize: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFOnResize(newSize,oldSize,oself)
   hql_Trace( PADR("OnResize: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) + ;
                      " newSize WxH: " + hb_NtoS(newSize:width()) + " x " + hb_NtoS(newSize:height()) + ;
                      " oldSize WxH: " + hb_NtoS(oldSize:width()) + " x " + hb_NtoS(oldSize:height()) )
RETURN

STATIC PROCEDURE UDFOnShow(oself)
   hql_Trace( PADR("OnShow: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN
