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

 \brief Returns a new hql_filterBox object instance
 \param(IN) ...
 \return object

*/
FUNCTION hqlFilterBox( ... )
RETURN hql_filterBox():new( ... )

/*!

 \brief define hql_filterBox class

*/
CLASS hql_filterBox INHERIT hql_lineEdit

   EXPORTED:
   METHOD init
   METHOD hqlSetAction
   METHOD hqlSetIcon

   PROTECTED:
   VAR bHqlAction                         INIT NIL
   VAR pHqlAction                         INIT NIL
   METHOD __hqlCustomize
   METHOD __hqlDoAction

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS hql_filterBox

   ::hql_lineEdit:init( ... )

   ::__hqlCustomize()

RETURN self

/*!

 \brief set action to be executed
 \param[in] block | NIL
 \return Self

*/
METHOD hqlSetAction( arg1 ) CLASS hql_filterBox
   IF ( hb_IsEvalItem(arg1) .OR. arg1 == NIL )
      ::bHqlAction := arg1
   ENDIF
RETURN self

/*!

 \brief set button icon
 \param[in] QIcon | string
 \return Self

*/
METHOD hqlSetIcon( arg1 ) CLASS hql_filterBox
   LOCAL oAction
   IF ( hb_IsString(arg1) .OR. hql_IsDerived(arg1, "QIcon") )
      oAction := hql_ObjectFromId(::pHqlAction)
      oAction:setIcon( QIcon(arg1) )
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED]
 \param(IN)
 \return NIL

*/
METHOD __hqlCustomize() CLASS hql_filterBox
   LOCAL oAction

   ::setPlaceholderText( "Filter..." )
   ::setToolTip( "Enter filter text and click icon or press enter." )

   oAction := hqlAction(/*name*/, self )
   oAction:setIcon( QIcon( ":/hqlres/filter" ) )
   oAction:hqlOnTriggered( { || ::__hqlDoAction() } )
   ::addAction( oAction, QLineEdit_TrailingPosition )

   ::pHqlAction := oAction:hqlObjectId()

RETURN NIL

/*!

 \brief [PROTECTED] do action related with current status
 \param(IN)
 \return NIL

*/
METHOD __hqlDoaction() CLASS hql_filterBox
   IF (hb_IsEvalItem(::bHqlAction) )
      EVAL( ::bHqlAction, ::text(), Self )  // Self always as last
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
