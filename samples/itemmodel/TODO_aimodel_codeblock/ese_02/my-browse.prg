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
#include "hbtrace.ch"

#include "hbqtgui.ch"
#include "hbqtcore.ch"

FUNCTION myBrowse( ... )
RETURN my_browse():new( ... )

/*!

 \brief my_browse class definition

*/
CREATE CLASS my_browse INHERIT hql_tableview

   FRIEND CLASS my_browse

   EXPORTED:
   METHOD init
   METHOD hqlCleaner                      // override

   PROTECTED:

   HIDDEN:
   METHOD __h_hqlCleaner

ENDCLASS

/*!

 \brief initialize object instance

*/
METHOD init( ... ) CLASS my_browse

   ::hql_tableview:init( ... )

   ::setModel( my_model():new( Self ) )

RETURN Self

/*!

 \brief callable Hql cleaner
 \param[in] none
 \return NIL

*/
METHOD hqlCleaner() CLASS my_browse
   ::__h_hqlCleaner()
RETURN NIL

// ==================== PROTECTED section ====================

// ==================== HIDDEN section ====================

/*!

 \brief this class cleaner
 \param[in] none
 \return NIL

*/
METHOD __h_hqlCleaner() CLASS my_browse

   IF hb_IsObject( ::model() )
      ::model:hqlCleaner()
   ENDIF

   ::hql_tableview:hqlCleaner()

RETURN NIL
