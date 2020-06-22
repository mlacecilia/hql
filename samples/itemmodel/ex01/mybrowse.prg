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

FUNCTION myBrowse( ... )
RETURN my_browse():new( ... )

/*!

 \brief my_browse class definition

*/
CREATE CLASS my_browse INHERIT hql_tableview

   EXPORTED:
   METHOD init

   PROTECTED:
   METHOD __hqlCleaner

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance

*/
METHOD init( ... ) CLASS my_browse

   ::hql_tableview:init( ... )

   // ::setModel( my_model():new( Self ) ) HbQt is not able to connect something to models; so we need a var to keep alive Hb object
   ::oHqlAbstractItemModel := my_model():new( Self )
   ::setModel( ::oHqlAbstractItemModel )


RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] Object cleaner
 \param(IN)
 \return NIL

*/
METHOD __hqlCleaner() CLASS my_browse

   ::oHqlAbstractItemModel:hqlCleaner()
   ::hql_tableview:__hqlCleaner()

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================

// ==================== HIDDEN section ====================
