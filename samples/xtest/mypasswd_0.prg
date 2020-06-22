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
#include "hqlhbqt.ch"

/*!

 \brief Returns a new my_password object instance
 \param(IN) ...
 \return object

*/
FUNCTION myPassword( ... )
RETURN my_password():new( ... )

/*!

 \brief define my_password class

*/
CLASS my_password INHERIT hql_lineEdit

   EXPORTED:
   METHOD init

   PROTECTED:
   VAR pHqlButton                         INIT NIL
   VAR nHqlEchoMode                       INIT QLineEdit_Password
   METHOD __hqlAdjust
   METHOD __hqlConnect
   METHOD __hqlCustomize
   SIGNAL __my_QClicked
   SIGNAL __my_QResize

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS my_password

   ::hql_lineEdit:init( cName, ... )

   ::__hqlCustomize()

   ::adjustSize() // to force resize so __hqlAdjust executed

RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] adjust widget
 \param[in] ...
 \return NIL

*/
METHOD __hqlAdjust() CLASS my_password
   LOCAL nFrameWidth
   LOCAL nDim
   LOCAL nRpadding
   LOCAL nX
   LOCAL nY
   LOCAL oButton := hql_ObjectFromId(::pHqlButton)

   nFrameWidth := ::style():pixelMetric( QStyle_PM_DefaultFrameWidth )

   // find min value to keep icon as a square
   nDim := MIN( ::size:height()-(nFrameWidth*2), ::size:width()-(nFrameWidth*2) )

   oButton:resize( QSize( nDim, nDim ) )
   oButton:setIconSize( QSize( nDim, nDim ) )

   // set right padding else text goes under icon
   nRpadding := oButton:size:height() + nFrameWidth
   ::setStyleSheet( "padding-right: " + hb_NtoC(nRpadding) + "px;" )

   // move button to the most right position. It uses ::rect because (I think) Qt re-map button position to its parent
   nX := ::rect:right() - oButton:size:height()
   nY := ::rect:bottom() - oButton:size:height()
   oButton:move( nX, nY )

RETURN NIL

/*!

 \brief [PROTECTED] connect signal to keep alive Harbour object. Without this next code like hqlPushButton( ... ) can't be used
 \param(IN)
 \return bool

*/
METHOD __hqlConnect() CLASS my_password
   ::hql_lineEdit:__hqlConnect()
   ::connect( QEvent_Resize , {|| ::__my_QResize() } )  // required to adjust button position
RETURN .T.

/*!

 \brief [PROTECTED]
 \param(IN)
 \return NIL

*/
METHOD __hqlCustomize() CLASS my_password
   LOCAL oButton

   ::setEchoMode( ::nHqlEchoMode )

   WITH OBJECT oButton := hqlToolButton(/*name*/, self )
      :setStyleSheet( "border: none; padding: 1px; border-left: 1px;" )
      :setCursor( QCursor( Qt_ArrowCursor ) )
      :setIcon( QIcon( ":/hqlres/eyeclose" ) )
      :setFocusPolicy( Qt_NoFocus )
      :hqlOnClicked( { || ::__my_QClicked() } )
   END WITH
   oButton:setFocusProxy( self )
   ::pHqlButton := oButton:hqlObjectId()

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __my_QResize() CLASS my_password
   ::__hqlAdjust()
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __my_QClicked() CLASS my_password
   LOCAL oButton := hql_ObjectFromId(::pHqlButton)

   IF ( ::nHqlEchoMode == QLineEdit_Password )
      ::nHqlEchoMode := QLineEdit_PasswordEchoOnEdit
      ::setEchoMode( ::nHqlEchoMode )
      oButton:setIcon( QIcon( ":/hqlres/eyeopen" ) )
   ELSE
      ::nHqlEchoMode := QLineEdit_Password
      ::setEchoMode( ::nHqlEchoMode )
      oButton:setIcon( QIcon( ":/hqlres/eyeclose" ) )
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
