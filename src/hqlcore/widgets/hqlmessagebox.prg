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

 \brief Returns a new hql_messageBox object instance

*/
FUNCTION hqlMessageBox( ... )
RETURN hql_messageBox():new( ... )

/*!

 \brief define hql_messageBox class

*/
CLASS hql_messageBox INHERIT hb_QMessageBox, hql_abs0030

   EXPORTED:
   METHOD init
   METHOD hqlActivate
   METHOD hqlSetTimed

   PROTECTED:
   VAR nHqlMsec                           INIT 0
   VAR lHqlTimerEnabled                   INIT .F.
   VAR nHqlTimerExitCode                  INIT 0
   SIGNAL __timeoutSlot

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_messageBox

   ::QMessageBox:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief Activate (show/rise) form
 \param(IN)
 \return numeric

*/
METHOD hqlActivate() CLASS hql_messageBox

   ::show()

   ::raise()

   ::activateWindow()

   IF ( ::lHqlActivated )
      HqlFw:processEvents( /*nFlags*/, /*nMsecs*/ )
   ELSE
      ::lHqlActivated := .T.
      IF ( ::lHqlTimerEnabled )
         WITH OBJECT hqlTimer(/*name*/, self )
            :setSingleShot( .T. )
            :setInterval( ::nHqlMsec )
            :hqlOnTimeout( { || ::__timeoutSlot() } )
            :start()
         END WITH
      ENDIF
      ::nHqlReleaseCode := ::exec()
   ENDIF

RETURN ::nHqlReleaseCode

/*!

 \brief Set timer for given interval; interval must be numeric >= 0
 \param(IN) numeric
 \return self

*/
METHOD hqlSetTimed( nMsec, nExitCode ) CLASS hql_messageBox
   IF ( hb_IsNumeric(nMsec) .AND. nMsec >= 0 )
      ::lHqlTimerEnabled := .T.
      ::nHqlMsec := nMsec
      IF ( hb_IsNumeric(nExitCode) .AND. nExitCode >= 0 )
         ::nHqlTimerExitCode := nExitCode
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
SIGNAL __timeoutSlot() CLASS hql_messageBox
   ::hqlRelease( ::nHqlTimerExitCode )
RETURN .F.

// ==================== HIDDEN section ====================
