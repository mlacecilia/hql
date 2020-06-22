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

 \brief Returns a new my_login object instance

*/
FUNCTION myLogin()
   LOCAL oLogin := my_login():new()
   LOCAL lGrant := oLogin:activate()
RETURN lGrant

/*!

 \brief define my_login class

*/
CLASS my_login STATIC

   EXPORTED:
   METHOD init
   METHOD activate

   PROTECTED:
   VAR nAttempts                          INIT 3
   VAR aDblist                            INIT {}
   VAR lGrant                             INIT .F.
   VAR oWnd                               INIT NIL
   METHOD __createDblist
   METHOD __createUI
   METHOD __makeBoxUser
   METHOD __makeBoxButton
   METHOD __makeBoxDb
   METHOD __onClose
   METHOD __updatePath
   METHOD __validateUser

   HIDDEN:

ENDCLASS

METHOD init() CLASS my_login
   ::__createDblist()
   ::__createUI()
RETURN self

METHOD activate() CLASS my_login
   ::oWnd:hqlActivate()
RETURN ::lGrant

METHOD __createDblist() CLASS my_login
   AADD( ::aDblist, { "example01", hb_DirSepAdd(hb_Cwd()) + "example01", NIL } )
   AADD( ::aDblist, { "example02", hb_DirSepAdd(hb_Cwd()) + "example02", NIL } )
   AADD( ::aDblist, { "example03", hb_DirSepAdd(hb_Cwd()) + "example03", NIL } )
   AADD( ::aDblist, { "example04", hb_DirSepAdd(hb_Cwd()) + "example04", NIL } )
RETURN NIL

METHOD __createUI() CLASS my_login
   LOCAL oFont

   oFont := hqlQapplication:font()
   oFont:setPointSize( 14 )

   WITH OBJECT ::oWnd := hqlModalDialog()
      :setFont( oFont )
      :hqlCaption( "login system" )
      :hqlOnClose( {|| ::__onClose() } )
   END WITH

   ::oWnd:setLayout( hqlVBoxLayout() )

   ::__makeBoxUser()
   ::oWnd:layout():addItem( QSpacerItem( 40, 20, QSizePolicy_Expanding, QSizePolicy_Minimum ) )

   ::__makeBoxDb()

   ::oWnd:layout():addStretch()

   ::__makeBoxButton()
   ::oWnd:layout():addItem( QSpacerItem( 40, 20, QSizePolicy_Expanding, QSizePolicy_Minimum ) )

   ::oWnd:resize( QSize( 600, 600 ) )

RETURN NIL

METHOD __makeBoxUser() CLASS my_login
   LOCAL oGroup
   LOCAL oMlayout, oHlay


   WITH OBJECT oGroup := hqlGroupBox(/*name*/, ::oWnd )
      :setLayout( hqlVBoxLayout() )
      :hqlCaption( "User" )
      oMlayout := :layout()

      oHlay := hqlHBoxLayout()
      oHlay:setContentsMargins( 0, 0, 0, 0 )
      WITH OBJECT hqlLabel()
         :hqlAddMeToLayout( oHlay )
         :hqlCaption( "User login" )
         :setMinimumWidth( 110 )
      END WITH
      WITH OBJECT hqlLineEdit( "userlogin" )
         :hqlAddMeToLayout( oHlay )
      END WITH
      oHlay:addStretch()
      oMlayout:addLayout( oHlay )

      oHlay := hqlHBoxLayout()
      oHlay:setContentsMargins( 0, 0, 0, 0 )
      WITH OBJECT hqlLabel()
         :hqlAddMeToLayout( oHlay )
         :hqlCaption( "Password" )
         :setMinimumWidth( 110 )
      END WITH
      WITH OBJECT hqlPassword( "userpassw" )
         :hqlAddMeToLayout( oHlay )
      END WITH
      oHlay:addStretch()
      oMlayout:addLayout( oHlay )

   END WITH

   ::oWnd:layout():addWidget( oGroup )
RETURN NIL

METHOD __makeBoxButton() CLASS my_login
   LOCAL oMlayout

   oMlayout := hqlHBoxLayout()
   oMlayout:setContentsMargins( 0, 0, 0, 0 )
   WITH OBJECT hqlPushButton(/*name*/, ::oWnd)
      :hqlAddMeToLayout( oMlayout )
      :hqlCaption( "Login" )
      :setMinimumHeight( 50 )
      :hqlOnClicked( { || ::__validateUser() } )
      :setStyleSheet( 'background-color: #1450B8; color: #FFFFFF;' )
   END WITH

   ::oWnd:layout():addLayout( oMlayout )
RETURN NIL

METHOD __makeBoxDb() CLASS my_login
   LOCAL oGroup
   LOCAL oMlayout, oLayout
   LOCAL oCombo

   oMlayout := hqlVBoxLayout()

   WITH OBJECT oGroup := hqlGroupBox(/*name*/, ::oWnd )
      :setLayout( oMlayout )
      :hqlCaption( "Database" )

      oLayout := hqlHBoxLayout()
      oLayout:setContentsMargins( 0, 0, 0, 0 )
      WITH OBJECT hqlLabel()
         :hqlAddMeToLayout( oLayout )
         :hqlCaption( "Name" )
         :setMinimumWidth( 110 )
      END WITH
      WITH OBJECT oCombo := hqlComboBox( "dbname" )
         :hqlAddMeToLayout( oLayout )
         :hqlAddRows( ::aDblist )
         :hqlOnCurrentIndexChanged( { |nint,oself| ::__updatePath(nint,oself) } )
      END WITH
      oLayout:addStretch()
      oMlayout:addLayout( oLayout )

      oLayout := hqlHBoxLayout()
      oLayout:setContentsMargins( 0, 0, 0, 0 )
      WITH OBJECT hqlLabel()
         :hqlAddMeToLayout( oLayout )
         :hqlCaption( "Path" )
         :setMinimumWidth( 110 )
      END WITH
      WITH OBJECT hqlLineEdit( "dbpath" )
         :hqlAddMeToLayout( oLayout )
         :setReadOnly( .T. )
         :setStyleSheet( '[readOnly="true"] {background-color: #4486FA; color: #000000}' )
      END WITH
      oMlayout:addLayout( oLayout )

   END WITH

   ::oWnd:layout():addWidget( oGroup )

   ::__updatePath( 0, ocombo)

RETURN NIL

METHOD __onClose() CLASS my_login
   ::oWnd := NIL
RETURN NIL

METHOD __updatePath( nint, oself ) CLASS my_login
   LOCAL oPath := ::oWnd:findChild( "DBPATH", Qt_FindDirectChildrenOnly )   //Qt::FindChildrenRecursively
   LOCAL oVariant
   oVariant := oself:itemData( nint )
   oPath:setText( oVariant:toString() )
RETURN NIL

METHOD __validateUser() CLASS my_login
   LOCAL oUserLogin := ::oWnd:findChild( "USERLOGIN", Qt_FindDirectChildrenOnly )   //Qt::FindChildrenRecursively
   LOCAL oUserPassw := ::oWnd:findChild( "USERPASSW", Qt_FindDirectChildrenOnly )   //Qt::FindChildrenRecursively
   LOCAL cLogin := ALLTRIM( oUserLogin:text() )
   LOCAL cPassw := ALLTRIM( oUserPassw:text() )

   IF ( cLogin == "admin" .AND. cPassw == "12345" )
      ::lGrant := .T.
   ENDIF

   IF ( ::lGrant )
      ::oWnd:hqlRelease()
      RETURN NIL
   ENDIF

   ::nAttempts -= 1

   IF ( ::nAttempts < 1 )
      hql_MsgStop( "Login failed: program end" )
      ::oWnd:hqlRelease()
      RETURN NIL
   ENDIF

   oUserLogin:setFocus()

   hql_MsgWarn( "Invalid login; You have only" + hb_NtoS(::nAttempts) + " attempts" )

RETURN NIL
