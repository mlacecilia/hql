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

 \brief Returns a new hql_systemTrayIcon object instance

*/
FUNCTION hqlSystemTrayIcon( ... )
RETURN hql_systemTrayIcon():new( ... )

/*!

 \brief define hql_systemTrayIcon class

*/
CLASS hql_systemTrayIcon INHERIT hb_QSystemTrayIcon, hql_abs0000

   EXPORTED:
   METHOD init
   ACCESS hqlActivatedReason              INLINE ::nActivatedReason
   METHOD hqlAddMenu
   METHOD hqlOnActivated
   METHOD hqlOnMessageClicked

   PROTECTED:
   VAR nActivatedReason                   INIT QSystemTrayIcon_Unknown
   VAR bHqlActivated                      INIT NIL
   VAR bHqlMessageClicked                 INIT NIL
   SIGNAL __hql_QActivated
   SIGNAL __hql_QMessageClicked

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

   MOST IMPORTANT
   on *nix system an error is shown (console) more or less like icon not set. On Windows not
   anyway, the best practice is to creates systemTrayIcon as usual in Hql and show; e.g.
   WITH OBJECT oForm := mainWindow
       WITH OBJECT systemTray( ... )
          settings
          :show()
          :showMessage( ... )
       END WITH
    END WITH

    oForm:hqlActivate()

*/
METHOD init( cName, ... ) CLASS hql_systemTrayIcon

   ::QSystemTrayIcon:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF
   IF ( !::__hqlHasParent() )
      hqlThrow( hqlErrorNew( 7007, PROCNAME() ) )
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

   // ::show() READ note

RETURN self

/*!

 \brief add a context menu

*/
METHOD hqlAddMenu( cName ) CLASS hql_systemTrayIcon

   LOCAL oObject

   // from Qt docs
   // NOTE: THE SYSTEM TRAY ICON DOES NOT TAKE OWNERSHIP OF THE MENU.
   // You must ensure that it is deleted at the appropriate time by, for example, creating the menu with a suitable parent object.

   // this is a simple menu   oObject := HqlMenu( cName, Self:parent:window() )
   oObject := hqlContextMenu( cName, ::parent:window() )
   // in any scenario, must be executed
   ::setContextMenu( oObject )

RETURN oObject

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnActivated( arg1 ) CLASS hql_systemTrayIcon
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlActivated := arg1
      IF ( hb_IsEvalItem(::bHqlActivated) )
         ::connect( "activated(QSystemTrayIcon::ActivationReason)" , { |nInt| ::__hql_QActivated(nInt) } )
      ELSE
         ::disconnect( "activated(QSystemTrayIcon::ActivationReason)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnMessageClicked( arg1 ) CLASS hql_systemTrayIcon
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlMessageClicked := arg1
      IF ( hb_IsEvalItem(::bHqlMessageClicked) )
         ::connect( "messageClicked()" , { || ::__hql_QMessageClicked() } )
      ELSE
         ::disconnect( "messageClicked()" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QActivated( nReason ) CLASS hql_systemTrayIcon
   ::nActivatedReason := nReason
   IF ( hb_IsEvalItem( ::bHqlActivated ) )
      EVAL( ::bHqlActivated, nReason, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QMessageClicked() CLASS hql_systemTrayIcon
   IF ( hb_IsEvalItem( ::bHqlMessageClicked ) )
      EVAL( ::bHqlMessageClicked, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
