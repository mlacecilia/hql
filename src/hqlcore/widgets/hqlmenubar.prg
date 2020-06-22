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

 \brief Returns a new hql_menuBar object instance

*/
FUNCTION hqlMenuBar( ... )
RETURN hql_menuBar():new( ... )

/*!

 \brief define hql_menuBar class
   WARNING to remember: from Qt doc "Although a popup menu is always a top-level widget"

*/
CLASS hql_menuBar INHERIT hb_QMenuBar, hql_abs0050

   EXPORTED:
   METHOD init
   METHOD hqlAddMeToLayout

   PROTECTED:

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_menuBar

   ::QMenuBar:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   IF ( ::__hqlHasParent() .AND. ::parent():isDerivedFrom("QMainWindow") )
      ::parent:setMenuBar( Self )
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief  add itself to a layout
 \param(IN) object
 \return Self

*/
METHOD hqlAddMeToLayout( oObject ) CLASS hql_menuBar
   IF ( oObject:isDerivedFrom("QLayout") )
      oObject:setMenuBar( Self )
   ENDIF
RETURN Self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
