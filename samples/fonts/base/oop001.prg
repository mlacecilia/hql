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
   LOCAL lRegistered, nFontId, oFont

   hqlErrorSys()  /*hbqt_errorsys()*/

   hqlSetStyle( "Fusion" )

   // BEFORE register resources data for embedded fonts; hbqtres_embres() used because qrc file has different name from program name.
   lRegistered := hqlRegisterResData( hbqtres_embres() )
   // add font to application
   nFontId := HqlAddApplicationFont( ":/fontembd/resources/Vera.ttf" )  /* the real file system path and prefix must be used */
hql_Trace( "Vera.ttf registered: " + hb_ValToExp(lRegistered) + " fontId: " + hb_NtoS(nFontId) )
   nFontId := HqlAddApplicationFont( ":/fontembd/resources/VeraBd.ttf" )  /* the real file system path and prefix must be used */
hql_Trace( "VeraBd.ttf registered: " + hb_ValToExp(lRegistered) + " fontId: " + hb_NtoS(nFontId) )
   nFontId := HqlAddApplicationFont( ":/fontembd/resources/VeraBI.ttf" )  /* the real file system path and prefix must be used */
hql_Trace( "VeraBI.ttf registered: " + hb_ValToExp(lRegistered) + " fontId: " + hb_NtoS(nFontId) )
   nFontId := HqlAddApplicationFont( ":/fontembd/resources/VeraIt.ttf" )  /* the real file system path and prefix must be used */
hql_Trace( "VeraIt.ttf registered: " + hb_ValToExp(lRegistered) + " fontId: " + hb_NtoS(nFontId) )

   // change application font
   oFont := QFont( "Bitstream Vera Sans", -1, QFont_Normal )
   oFont:setPointSize( 12 )
   hqlQApplication:setFont(oFont)

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
   LOCAL oFont, oLayout

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      this := :hqlThis()
      :setWindowTitle( "EMBEDDED FONT tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/, this) )
      :centralWidget():setLayout( QVBoxLayout() )
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oVlayout := :layout()

      WITH OBJECT hqlPushButton(/*name*/)
         :hqlAddMeToLayout( oVlayout )
         :hqlCaption( "close &AllWindows" )
         :hqlOnClicked( { || hqlQApplication:closeAllWindows() } )
      END WITH

      oFont := QFont( "Bitstream Vera Sans", -1, QFont_Bold )
      oFont:setPointSize( 18 )
      WITH OBJECT hqlGroupBox(/*name*/)
         :hqlAddMeToLayout( oVlayout )
         :setFont( oFont )
         :hqlCaption( oFont:key() )
         :setLayout( hqlVboxLayout(/*name*/) )
         oLayout := :hqlThis:layout()

         oFont := QFont( "Bitstream Vera Sans", -1, QFont_Bold, .T. )
         oFont:setPointSize( 14 )
         WITH OBJECT hqlPushButton(/*name*/)
            :hqlAddMeToLayout( oLayout )
            :hqlCaption( "&Exit" )
            :hqlOnClicked( { || oWnd:hqlRelease() } )
            :setFont( oFont )
            :setToolTip( oFont:key() )
         END WITH

         oFont := QFont( "Bitstream Vera Sans", -1, QFont_Normal, .T. )
         oFont:setPointSize( 14 )
         WITH OBJECT hqlLabel(/*name*/)
            :hqlAddMeToLayout( oLayout )
            :hqlCaption( oFont:key() )
         END WITH
      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN
