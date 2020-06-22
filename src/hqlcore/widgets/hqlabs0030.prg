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

 \brief define hql_abs0030 class
   inherited by:
      hql_colorDialog,
      hql_fileDialog,
      hql_fontDialog,
      hql_messageBox,
      hql_progressDialog

*/
CLASS hql_abs0030 INHERIT hql_abs0001

   EXPORTED:
   METHOD hqlActivate
   METHOD hqlActivated                    INLINE ::lHqlActivated
   METHOD hqlCaption                      SETGET
   METHOD hqlOnClose
   METHOD hqlOnFinished
   METHOD hqlRelease

   PROTECTED:
   VAR lHqlActivated                      INIT .F.
   VAR bHqlClose                          INIT NIL
   VAR bHqlFinished                       INIT NIL
   VAR nHqlReleaseCode                    INIT 0
   METHOD __hqlConnect
   SIGNAL __hql_QClose                    // connected by default
   SIGNAL __hql_QFinished                 // connected by default

   HIDDEN:

ENDCLASS

/*!

 \brief show / raise / activateWindow
 \param(IN)
 \return Self

*/
METHOD hqlActivate() CLASS hql_abs0030

   ::show()

   ::raise()

   ::activateWindow()

   IF ( ::lHqlActivated )
      HqlFw:processEvents( /*nFlags*/, /*nMsecs*/ )
   ELSE
      ::lHqlActivated := .T.
      ::nHqlReleaseCode := ::exec()
   ENDIF

RETURN ::nHqlReleaseCode

/*!

 \brief set/get generic text
 \param(IN) [string]
 \return string

*/
METHOD hqlCaption( arg1 ) CLASS hql_abs0030
   IF ( hb_IsString(arg1) )
      ::setWindowTitle( arg1 )
   ENDIF
RETURN ::windowTitle()

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnClose( arg1 ) CLASS hql_abs0030
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlClose := arg1
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnFinished( arg1 ) CLASS hql_abs0030
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlFinished := arg1
   ENDIF
RETURN self

/*!

 \brief Release form
 \param(IN) numeric
 \return self

*/
METHOD hqlRelease( nExit ) CLASS hql_abs0030
   ::nHqlReleaseCode := hb_DefaultValue(nExit, 0)
   ::done( ::nHqlReleaseCode )
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] connect signal/event
 \param(IN) none
 \return bool

*/
METHOD __hqlConnect() CLASS hql_abs0030
   ::hql_abs0001:__hqlConnect()
   ::connect( QEvent_Close, { |oEvent| ::__hql_QClose(oEvent) } )
   ::connect( "finished(int)", { |nInt| ::__hql_QFinished(nInt) } )
RETURN .T.

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
METHOD __hql_QClose( oEvent ) CLASS hql_abs0030
   oEvent:accept()
   IF ( hb_IsEvalItem(::bHqlClose) )
      EVAL( ::bHqlClose, Self )  // Self always as last
   ENDIF
   ::__hqlSignalEverybody( self )
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
METHOD __hql_QFinished(nInt) CLASS hql_abs0030
   ::nHqlReleaseCode := nInt
   IF ( hb_IsEvalItem(::bHqlFinished) )
      EVAL( ::bHqlFinished, nInt, Self )  // Self always as last
   ENDIF
//   ::__hqlSignalEverybody( self )
//   OR
   ::close()
RETURN .F.

// ==================== HIDDEN section ====================
