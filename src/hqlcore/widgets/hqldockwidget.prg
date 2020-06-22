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

 \brief Returns a new hql_dockWidget object instance

*/
FUNCTION hqlDockWidget( ... )
RETURN hql_dockWidget():new( ... )

/*!

 \brief define hql_dockWidget class

*/
CLASS hql_dockWidget INHERIT hb_QDockWidget, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlOnDockLocationChanged

   PROTECTED:
   VAR bHqlDockLocationChanged            INIT NIL
   SIGNAL __hql_QDockLocationChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_dockWidget

   ::QDockWidget:init( ... )

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
METHOD hqlOnDockLocationChanged( arg1 ) CLASS hql_dockWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1) .OR. arg1 == NIL ) )
      ::bHqlDockLocationChanged := arg1
      IF ( hb_IsEvalItem(::bHqlDockLocationChanged) )
         ::connect( "dockLocationChanged( Qt::DockWidgetArea )" , { |nInt| ::__hql_QDockLocationChanged(nInt) } )
      ELSE
         ::disconnect( "dockLocationChanged( Qt::DockWidgetArea )" )
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
SIGNAL __hql_QDockLocationChanged( nInt ) CLASS hql_dockWidget
   IF ( hb_IsEvalItem( ::bHqlDockLocationChanged ) )
      EVAL( ::bHqlDockLocationChanged, nInt, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
