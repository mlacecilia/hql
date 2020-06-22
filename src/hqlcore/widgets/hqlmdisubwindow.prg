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

 \brief Returns a new hql_mdiSubWindow object instance

*/
FUNCTION hqlMdiSubWindow( ... )
RETURN hql_mdiSubWindow():new( ... )

/*!

 \brief define hql_mdiSubWindow class

*/
CLASS hql_mdiSubWindow INHERIT hb_QMdiSubWindow, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlCaption                      SETGET
   METHOD hqlOnAboutToActivate
   METHOD hqlOnWindowStateChanged

   PROTECTED:
   VAR bHqlAboutToActivate                INIT NIL
   VAR bHqlWindowStateChanged             INIT NIL
   METHOD __hqlConnect
   SIGNAL __hql_QAboutToActivate
   SIGNAL __hql_QClose                                      // connected by default
   SIGNAL __hql_QWindowStateChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_mdiSubWindow

   ::QMdiSubWindow:init( ... )
   ::setAttribute( Qt_WA_DeleteOnClose )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief set/get generic text
 \param(IN) [string]
 \return string

*/
METHOD hqlCaption( arg1 ) CLASS hql_mdiSubWindow
   IF ( hb_IsString(arg1) )
      ::setWindowTitle( arg1 )
   ENDIF
RETURN ::windowTitle()

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnAboutToActivate( arg1 ) CLASS hql_mdiSubWindow
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlAboutToActivate := arg1
      IF ( hb_IsEvalItem(::bHqlAboutToActivate) )
         ::connect( "aboutToActivate()", { || ::__hql_QAboutToActivate() } )
      ELSE
         ::disconnect( "aboutToActivate()" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnWindowStateChanged( arg1 ) CLASS hql_mdiSubWindow
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlWindowStateChanged := arg1
      IF ( hb_IsEvalItem(::bHqlWindowStateChanged) )
         ::connect( "windowStateChanged(Qt::WindowStates,Qt::WindowStates)", { |nOldState,nNewState| ::__hql_QWindowStateChanged(nOldState,nNewState) } )
      ELSE
         ::disconnect( "windowStateChanged(Qt::WindowStates,Qt::WindowStates)" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] connect signal/event. Returns false if fails else true
 \param[in] none
 \return boolean

*/
METHOD __hqlConnect() CLASS hql_mdiSubWindow
   ::hql_abs0001:__hqlConnect()
   ::connect( QEvent_Close, { |oEvent| ::__hql_QClose(oEvent) } )
RETURN .T.

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ...
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QAboutToActivate() CLASS hql_mdiSubWindow
   IF ( hb_IsEvalItem( ::bHqlAboutToActivate ) )
      EVAL( ::bHqlAboutToActivate, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ...
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QClose( oEvent ) CLASS hql_mdiSubWindow
   oEvent:accept()
   ::__hqlSignalEverybody( self )
RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
 \param[in] ... based on event/signal
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QWindowStateChanged( nOldState, nNewState ) CLASS hql_mdiSubWindow
   IF ( hb_IsEvalItem( ::bHqlWindowStateChanged ) )
      EVAL( ::bHqlWindowStateChanged, nOldState, nNewState, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
