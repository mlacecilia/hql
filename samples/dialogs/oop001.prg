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

   hqlTranslations:addItem( "qt_", /*cpath*/ )  // when path empty, QLibraryInfo_TranslationsPath used but it works only if qt.conf exists
   //hqlTranslations:addItem( "qt_", hb_DirBase() + "translations" )  // given path for external file
   //hqlTranslations:addItem( "qt_", ":/hqlres/translations" )  // given embedded path

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
   LOCAL oMlayout, oHlayout

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      :setWindowTitle( "HQLCOLORDIALOG, HQLFONTDIALOG, HQLFILEDIALOG tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/) )
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
      oMlayout:addLayout( oHlayout )
      WITH OBJECT hqlPushButton(/*name*/)
         :hqlAddMeToLayout( oHlayout )
         :hqlCaption( "HQLCOLORDIALOG" )
         :hqlOnClicked( { || UDFcolorDialog( oWnd ) } )
      END WITH
      WITH OBJECT hqlPushButton(/*name*/)
         :hqlAddMeToLayout( oHlayout )
         :hqlCaption( "COLOR function" )
         :hqlOnClicked( { || UDFcolorFunction( oWnd ) } )
      END WITH

      oHlayout := hqlHBoxLayout()
      oMlayout:addLayout( oHlayout )
      WITH OBJECT hqlPushButton(/*name*/)
         :hqlAddMeToLayout( oHlayout )
         :hqlCaption( "HQLFONTDIALOG" )
         :hqlOnClicked( { || UDFfontDialog( oWnd ) } )
      END WITH
      WITH OBJECT hqlPushButton(/*name*/)
         :hqlAddMeToLayout( oHlayout )
         :hqlCaption( "FONT function" )
         :hqlOnClicked( { || UDFfontFunction( oWnd ) } )
      END WITH

      oHlayout := hqlHBoxLayout()
      oMlayout:addLayout( oHlayout )
      WITH OBJECT hqlPushButton(/*name*/)
         :hqlAddMeToLayout( oHlayout )
         :hqlCaption( "HQLFILEDIALOG" )
         :hqlOnClicked( { || UDFfileDialog( oWnd ) } )
      END WITH

   END WITH

   oMlayout:addStretch()   //pushUp

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFcolorDialog( oParent )
   LOCAL oWnd, nExit, oColor

   oWnd := hqlColorDialog( /*name*/, oParent )
   oWnd:hqlOnColorSelected( { |ocolor| hql_Trace( "from signal :" + ocolor:name() ) } )

   nExit := oWnd:hqlActivate()
   IF ( nExit == QDialog_Accepted )
      oColor := oWnd:selectedColor()
      hql_Trace( oColor:name() )
   ELSE
      hql_Trace( "not selected" )
   ENDIF

RETURN

STATIC PROCEDURE UDFcolorFunction( oParent )
   LOCAL nOptions := hb_BitOr( QColorDialog_DontUseNativeDialog, QColorDialog_ShowAlphaChannel )
   LOCAL oColor

   oColor := QColorDialog():getColor( QColor(Qt_green), oParent, "Select Color", nOptions )

   IF ( oColor:isValid() )
      hql_Trace( oColor:name() )
   ELSE
      hql_Trace( "not valid" )
   ENDIF

RETURN

STATIC PROCEDURE UDFfontDialog( oParent )
   LOCAL oWnd, nExit, oFont

   oWnd := hqlFontDialog( /*name*/, oParent )
   oWnd:hqlOnFontSelected( { |ofont| hql_Trace( "from signal :" + oFont:key() ) } )

   nExit := oWnd:hqlActivate()
   IF ( nExit == QDialog_Accepted )
      oFont := oWnd:selectedFont()
      hql_Trace( oFont:key() )
   ELSE
      hql_Trace( "not selected" )
   ENDIF

RETURN

STATIC PROCEDURE UDFfontFunction( oParent )
   LOCAL nOptions := hb_BitOr( QFontDialog_DontUseNativeDialog, QFontDialog_ScalableFonts, QFontDialog_NonScalableFonts, QFontDialog_MonospacedFonts, QFontDialog_ProportionalFonts )
   LOCAL oFont, xBool := 0

   oFont := QFontDialog():getFont( @xBool, hqlQApplication:font(), oParent, "Select Font", nOptions )

   IF ( xBool == 1 )
      hql_Trace( oFont:key() )
   ELSE
      hql_Trace( "not valid" )
   ENDIF

RETURN
