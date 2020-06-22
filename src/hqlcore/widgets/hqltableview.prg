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

 \brief Returns a new hql_tableView object instance

*/
FUNCTION hqlTableView( ... )
RETURN hql_tableView():new( ... )

/*!

 \brief define hql_tableView class

*/
CLASS hql_tableView INHERIT hb_QTableView, hql_abs0001

   EXPORTED:
   METHOD init

   PROTECTED:
   VAR oHqlAbstractItemModel              INIT NIL
   VAR oHqlSelectionModel                 INIT NIL
   VAR oHqlSortProxyModel                 INIT NIL
   METHOD __hqlCleaner

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

   // nb MORE IMPORTANT
   // it's required to have a Harbour class var where memorize the model object, for these reasons:
   // 1) HbQt doesn't manage a parent for QAbstractItemModel; if given, a crash will occured when program end is reached
   //   ::oHqlAbstractItemModel := the_model  ie dbf_model():new( Self )
   // 2) QAbstractItemModel object seems do not have signals to be connected; so, from HbQt point of view this object will die

*/
METHOD init( cName, ... ) CLASS hql_tableView

   ::QTableView:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF
   //  build HqlHeaderView this is a trick to keep alive object avoiding undesctructed object at end program
   ::setHorizontalHeader( hqlHeaderView(/*name*/, Qt_Horizontal, Self ) )
   ::setVerticalHeader( hqlHeaderView(/*name*/, Qt_Vertical, Self ) )

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] Object cleaner
 \param(IN)
 \return NIL

*/
METHOD __hqlCleaner() CLASS hql_tableView
   ::oHqlAbstractItemModel := NIL
   ::oHqlSelectionModel    := NIL
   ::oHqlSortProxyModel    := NIL
   ::hql_abs0001:__hqlCleaner()
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
