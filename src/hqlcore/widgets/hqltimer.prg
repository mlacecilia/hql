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

 \brief Returns a new hql_timer object instance

*/
FUNCTION hqlTimer( ... )
RETURN hql_timer():new( ... )

/*!

 \brief define hql_timer class

*/
CLASS hql_timer INHERIT hb_QTimer, hql_abs0000

   EXPORTED:
   METHOD init
   METHOD hqlKill
   METHOD hqlOnTimeout

   PROTECTED:
   VAR bHqlTimeOut                        INIT NIL
   SIGNAL __hql_QTimeout

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_timer

   ::QTimer:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief kill timer
 \param[in] none
 \return self

*/
METHOD hqlKill() CLASS hql_timer
   LOCAL nTimerId

   nTimerId := ::timerId()

   IF ( ::isActive() )
      ::stop()
   ENDIF

   hql_SetNoParent( Self )

   IF ( nTimerId != -1 ) // works fine on linux and windows Qt 5.2.0
      ::__hqlDisconnect()
      HqlFw:QtApplication:killTimer( nTimerId )
   ENDIF

RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnTimeout( arg1 ) CLASS hql_timer
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlTimeOut := arg1
      IF ( hb_IsEvalItem(::bHqlTimeOut) )
         ::connect( "timeout()" , {|| ::__hql_QTimeout() } )
      ELSE
         ::disconnect( "timeout()" )
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
SIGNAL __hql_QTimeout() CLASS hql_timer
   IF ( hb_IsEvalItem( ::bHqlTimeOut ) )
      EVAL( ::bHqlTimeOut, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
