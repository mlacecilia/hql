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

 \brief Returns a new hql_headerView object instance

*/
FUNCTION hqlHeaderView( ... )
RETURN hql_headerView():new( ... )

/*!

 \brief define hql_headerView class

*/
CLASS hql_headerView INHERIT hb_QHeaderView, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlOnSectionClicked
   METHOD hqlOnSectionCountChanged

   PROTECTED:
   VAR bHqlSectionClicked                 INIT NIL
   VAR bHqlSectionCountChanged            INIT NIL
   SIGNAL __hql_QSectionClicked
   SIGNAL __hql_QSectionCountChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_headerView

   ::QHeaderView:init( ... )

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
METHOD hqlOnSectionClicked( arg1 ) CLASS hql_headerView
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlSectionClicked := arg1
      IF ( hb_IsEvalItem(::bHqlSectionClicked) )
         ::connect( "sectionClicked(int)" , { |nInt| ::__hql_QSectionClicked(nInt) } )
      ELSE
         ::disconnect( "sectionClicked(int)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnSectionCountChanged( arg1 ) CLASS hql_headerView
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlSectionCountChanged := arg1
      // to be sortable, QTableWidget need header sections clickable --> ::setSectionsClickable( .T. )
      IF ( hb_IsEvalItem(::bHqlSectionCountChanged) )
         ::connect( "sectionCountChanged(int,int)" , { |nPrev,nNew| ::__hql_QSectionCountChanged(nPrev,nNew) } )
      ELSE
         ::disconnect( "sectionCountChanged(int,int)" )
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
SIGNAL __hql_QSectionClicked(nInt) CLASS hql_headerView
   IF ( hb_IsEvalItem( ::bHqlSectionClicked ) )
      EVAL( ::bHqlSectionClicked, nInt, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QSectionCountChanged(nPrev,nNew) CLASS hql_headerView
   IF ( hb_IsEvalItem( ::bHqlSectionCountChanged ) )
      EVAL( ::bHqlSectionCountChanged, nPrev, nNew, Self )   // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
