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

 \brief Returns a new hql_label object instance

*/
FUNCTION hqlLabel( ... )
RETURN hql_label():new( ... )

/*!

 \brief define hql_label class

*/
CLASS hql_label INHERIT hb_QLabel, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlOnHoverEnter
   METHOD hqlOnHoverLeave

   PROTECTED:
   VAR bHqlHoverEnter                     INIT NIL
   VAR bHqlHoverLeave                     INIT NIL
   SIGNAL __hql_QHoverEnter
   SIGNAL __hql_QHoverLeave

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_label

   ::QLabel:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnHoverEnter( arg1 ) CLASS hql_label
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlHoverEnter := arg1
      IF ( hb_IsEvalItem(::bHqlHoverEnter) )
         ::setAttribute( Qt_WA_Hover, .T. )  // required to enable HoveEnter and HoverLeave effects
         ::connect( QEvent_HoverEnter, { || ::__hql_QHoverEnter() } )
      ELSE
         ::disconnect( QEvent_HoverEnter )
         IF ( !::isConnected( QEvent_HoverLeave ) )
            ::setAttribute( Qt_WA_Hover, .F. )
         ENDIF
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnHoverLeave( arg1 ) CLASS hql_label
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlHoverLeave := arg1
      IF ( hb_IsEvalItem(::bHqlHoverLeave) )
         ::setAttribute( Qt_WA_Hover, .T. )  // required to enable HoveEnter and HoverLeave effects
         ::connect( QEvent_HoverLeave, { || ::__hql_QHoverLeave() } )
      ELSE
         ::disconnect( QEvent_HoverLeave )
         IF ( !::isConnected( QEvent_HoverEnter ) )
            ::setAttribute( Qt_WA_Hover, .F. )
         ENDIF
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
SIGNAL __hql_QHoverEnter() CLASS hql_label
   IF ( hb_IsEvalItem( ::bHqlHoverEnter ) )
      EVAL( ::bHqlHoverEnter, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QHoverLeave() CLASS hql_label
   IF ( hb_IsEvalItem( ::bHqlHoverLeave ) )
      EVAL( ::bHqlHoverLeave, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
