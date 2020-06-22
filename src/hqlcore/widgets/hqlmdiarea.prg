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

 \brief Returns a new hql_mdiArea object instance

*/
FUNCTION hqlMdiArea( ... )
RETURN hql_mdiArea():new( ... )

/*!

 \brief define hql_mdiArea class

*/
CLASS hql_mdiArea INHERIT hb_QMdiArea, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlOnSubWindowActivated

   PROTECTED:
   VAR bHqlSubWindowActivated             INIT NIL
   SIGNAL __hql_QSubWindowActivated

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_mdiArea

   ::QMdiArea:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnSubWindowActivated( arg1 ) CLASS hql_mdiArea
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlSubWindowActivated := arg1
      IF ( hb_IsEvalItem(::bHqlSubWindowActivated) )
         ::connect( "subWindowActivated(QMdiSubWindow*)", { |oMdiSubWin| ::__hql_QSubWindowActivated(oMdiSubWin) } )
      ELSE
         ::disconnect( "subWindowActivated(QMdiSubWindow*)" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ...
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QSubWindowActivated( oSubWin ) CLASS hql_mdiArea
   IF ( hb_IsEvalItem( ::bHqlSubWindowActivated ) )
      EVAL( ::bHqlSubWindowActivated, oSubWin, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
