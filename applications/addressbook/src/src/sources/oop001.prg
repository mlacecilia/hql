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
#include "addrbook.ch"

/*!

 \brief main form

*/
CREATE CLASS oop001 INHERIT basic_form

   EXPORTED:
   METHOD init

   PROTECTED:
   METHOD __createWin                     // override
   METHOD __makeDbf
   METHOD __setBackground
   METHOD __switchLanguage
   METHOD __translateThis

   METHOD __oop003
   METHOD __oop005

END CLASS

/*!

 \brief initialize

*/
METHOD init( ... ) CLASS oop001

   ::basic_form:setParent( ... )

RETURN Self

/*!

 \brief create window

*/
METHOD __createWin() CLASS oop001

   WITH OBJECT ::oWindow := hqlMainWindow( /*name*/ )   // hql objects: after the name follow the QT arguments
      :hqlCaption("hql Address Book")
      :resize( 800, 600 )
      :setWindowIcon( QIcon( ":/pgmico" ) )
      :setCentralWidget( hqlWidget( /*name*/ ) )
      :hqlOnActivate( { || IIF( ! ::__makeDbf(), ::oWindow:hqlRelease(), NIL ) } )
      :hqlOnLanguageChange( { || ::__translateThis() } )

      WITH OBJECT hqlMenuBar( "mainmnu" )

         WITH OBJECT :hqlAddMenu( "filemnu" )
            :hqlCaption( hqlTran( "oop001", "&File" ) )
            WITH OBJECT :hqlAddAction( "quitopt" )
               :hqlCaption( hqlTran( "oop001", "&Quit" ) )
               :setIcon( QIcon( ":/hqlres/quit" ) )
               :setShortcut( QKeySequence( "Alt+Q" ) )
               :hqlOnTriggered( { || HqlQApplication:closeAllWindows() } )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu( "tabmnu" )
            :hqlCaption( hqlTran( "oop001", "&Tables" ) )
            WITH OBJECT :hqlAddAction( "peopleopt" )
               :hqlCaption( hqlTran( "oop001", "&Contacts" ) )
               :hqlOnTriggered( { || ::__oop005() } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction( "countopt" )
               :hqlCaption( hqlTran( "oop001", "c&Ounters" ) )
               :hqlOnTriggered( { || ::__oop003() } )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu( "optmnu" )
            :setTitle( hqlTran( "oop001", "&Options" ) )

            WITH OBJECT :hqlAddMenu( "langmnu" )
               :hqlCaption( hqlTran( "oop001", "&Language" ) )
               WITH OBJECT :hqlAddAction( "itaopt" )
                  :hqlCaption( hqlTran( "oop001", "&Italian" ) )
                  :setIcon( QIcon( ":/hqlres/cf_it" ) )
                  :hqlOnTriggered( { || ::__switchLanguage( "it" ) } )
               END WITH
               WITH OBJECT :hqlAddAction( "espopt" )
                  :hqlCaption( hqlTran( "oop001", "&Spanish" ) )
                  :setIcon( QIcon( ":/hqlres/cf_es" ) )
                  :hqlOnTriggered( { || ::__switchLanguage( "es" ) } )
               END WITH
               WITH OBJECT :hqlAddAction( "engopt" )
                  :hqlCaption( hqlTran( "oop001", "&English" ) )
                  :setIcon( QIcon( ":/hqlres/cf_gb" ) )
                  :hqlOnTriggered( { || ::__switchLanguage( "en" ) } )
               END WITH
            END WITH
         END WITH

      END WITH

   END WITH

   ::__setBackground()

RETURN NIL

/*!

 \brief set background

*/
METHOD __setBackground() CLASS oop001

   LOCAL oPalette

   oPalette := ::oWindow:palette()
   oPalette:setBrush( QPalette_Background, QBrush( QColor( 193, 218, 112, 255 ) ) )
   ::oWindow:setPalette( oPalette )

RETURN NIL

/*!

 \brief counters selection

*/
METHOD __oop003() CLASS oop001

   LOCAL oProgram

   oProgram := oop003():new( Self )

   oProgram:prepareUi()

   oProgram:activate()

RETURN NIL

/*!

 \brief counters selection

*/
METHOD __oop005() CLASS oop001

   LOCAL oProgram

   oProgram := oop005():new( Self )

   oProgram:prepareUi()

   oProgram:activate()

RETURN NIL

/*!

 \brief switch language

*/
METHOD __switchLanguage( cLang ) CLASS oop001

   SWITCH cLang
   CASE "it"
      HqlSwitchLang( QLocale_Italian, QLocale_Italy )
      EXIT
   CASE "es"
      HqlSwitchLang( QLocale_Spanish, QLocale_Spain )
      EXIT
   OTHERWISE
      HqlSwitchLang( QLocale_C, QLocale_UnitedStates )
   ENDSWITCH

RETURN NIL

/*!

 \brief translate this form

*/
METHOD __translateThis() CLASS oop001

   ::oWindow:mainmnu:filemnu:hqlCaption( hqlTran( "oop001", "&File" ) )
   ::oWindow:mainmnu:filemnu:quitopt:hqlCaption( hqlTran( "oop001", "&Quit" ) )

   ::oWindow:mainmnu:tabmnu:hqlCaption( hqlTran( "oop001", "&Tables" ) )
   ::oWindow:mainmnu:tabmnu:peopleopt:hqlCaption( hqlTran( "oop001", "&Contacts" ) )
   ::oWindow:mainmnu:tabmnu:countopt:hqlCaption( hqlTran( "oop001", "c&Ounters" ) )

   ::oWindow:mainmnu:optmnu:hqlCaption( hqlTran( "oop001", "&Options" ) )
   ::oWindow:mainmnu:optmnu:itaopt:hqlCaption( hqlTran( "oop001", "&Italian" ) )
   ::oWindow:mainmnu:optmnu:espopt:hqlCaption( hqlTran( "oop001", "&Spanish" ) )
   ::oWindow:mainmnu:optmnu:engopt:hqlCaption( hqlTran( "oop001", "&English" ) )

RETURN NIL

/*!

 \brief check/make file

*/
METHOD __makeDbf() CLASS oop001

   LOCAL oDataModel

   IF ! hb_vfDirExists( appcnf:get( "dbfdir" ) )
      hql_MsgStop( hqlTran( "apperror", "Directories structure doesn't exists" ), "Error", /*cDetailText*/, NIL, QIcon( ":/pgmico" ) )
      RETURN .F.
   ENDIF

   // counters
   oDataModel := countersDm( appcnf:get( "dbfdir" ) )
   IF ! oDataModel:create( /*lForced*/ )
      hql_MsgStop( hqlTran( "apperror", "Can't create file" ), oDataModel:tabName(), NIL, QIcon( ":/pgmico" ) )
      RETURN .F.
   ENDIF

   // addrbook
   oDataModel := addrbookDm( appcnf:get( "dbfdir" ) )
   IF ! oDataModel:create( /*lForced*/ )
      hql_MsgStop( hqlTran( "apperror", "Can't create file" ), oDataModel:tabName(), NIL, QIcon( ":/pgmico" ) )
      RETURN .F.
   ENDIF

RETURN .T.  // for the moment
