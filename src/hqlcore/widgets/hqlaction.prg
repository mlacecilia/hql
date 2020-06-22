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

 \brief Returns a new hql_action object instance

*/
FUNCTION hqlAction( ... )
RETURN hql_action():new( ... )

/*!

 \brief define hql_action class

*/
CLASS hql_action INHERIT hb_QAction, hql_abs0000

   EXPORTED:
   METHOD init
   METHOD hqlCaption                      SETGET
   METHOD hqlOnToggled
   METHOD hqlOnTriggered

   PROTECTED:
   VAR bHqlToggled                        INIT NIL
   VAR bHqlTriggered                      INIT NIL
   SIGNAL __hql_QToggled
   SIGNAL __hql_QTriggered

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_action

   ::QAction:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief set/get text
 \param(IN) [string]
 \return string

*/
METHOD hqlCaption( arg1 ) CLASS hql_action
   IF ( hb_IsString(arg1) )
      ::setText( arg1 )
   ENDIF
RETURN ::text()

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnToggled( arg1 ) CLASS hql_action
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlToggled := arg1
      IF ( hb_IsEvalItem(::bHqlToggled) )
         ::connect( "toggled(bool)" , { |lBool| ::__hql_QToggled(lBool) } )
      ELSE
         ::disconnect( "toggled(bool)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnTriggered( arg1 ) CLASS hql_action
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlTriggered := arg1
      IF ( hb_IsEvalItem(::bHqlTriggered) )
         ::connect( "triggered(bool)" , { |lBool| ::__hql_QTriggered(lBool) } )
      ELSE
         ::disconnect( "triggered(bool)" )
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
SIGNAL __hql_QToggled(lBool) CLASS hql_action
   IF ( hb_IsEvalItem( ::bHqlToggled ) )
      EVAL( ::bHqlToggled, lBool, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QTriggered(lBool) CLASS hql_action
   IF ( hb_IsEvalItem( ::bHqlTriggered ) )
      EVAL( ::bHqlTriggered, lBool, Self )   // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
