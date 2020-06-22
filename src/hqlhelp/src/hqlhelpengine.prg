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

/*!

 \brief SINGLETON function returns a new hql_helpEngine object instance
 \param(IN)
 \return hql_frameWork

*/
FUNCTION __hql_help_engine()
   STATIC s_Object
   STATIC s_Once
   hb_ThreadOnce( @s_Once, {|| s_Object := hql_helpEngine():new() } )
RETURN s_Object

/*!

 \brief define hql_helpEngine class

*/
CLASS hql_helpEngine STATIC

   EXPORTED:
   METHOD init
   METHOD quit
   METHOD searchPaths                     INLINE ::oPathList
   METHOD showHelp

   PROTECTED:
   VAR oPathList                          INIT NIL
   METHOD __hqlCleaner

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance
 \param(IN) ...
 \return self

*/
METHOD init() CLASS hql_helpEngine
   ::oPathList := QStringList()
RETURN self

/*!

 \brief quit
 \param(IN)
 \return self

*/
METHOD quit() CLASS hql_helpEngine
   ::__hqlCleaner()
RETURN self

/*!

 \brief show help href, parent
 \param[in] string, object
 \return self

*/
METHOD showHelp( xHref, oParent ) CLASS hql_helpEngine
   LOCAL oWnd := hqlHelpWindow(oParent)

   oWnd:hqlSetSearcPaths( QStringList( ::oPathList ) )
   oWnd:hqlSetUrl( xHref )
   oWnd:hqlActivate()

RETURN Self

// ==================== PROTECTED section ====================

/*

 \brief [PROTECTED] Object cleaner
 \param(IN)
 \return NIL

*/
METHOD __hqlCleaner() CLASS hql_helpEngine
   ::oPathList := NIL
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
