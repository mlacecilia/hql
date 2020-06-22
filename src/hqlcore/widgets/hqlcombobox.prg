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

 \brief Returns a new hql_comboBox object instance

*/
FUNCTION hqlComboBox( ... )
RETURN hql_comboBox():new( ... )

/*!

 \brief define hql_comboBox class

*/
CLASS hql_comboBox INHERIT hb_QComboBox, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlAddRow
   METHOD hqlAddRows
   METHOD hqlOnActivated                                    // on Windows works when you press enter on highlighted item
   METHOD hqlOnCurrentIndexChanged

   PROTECTED:
   VAR bHqlActivated                      INIT NIL
   VAR bHqlCurrentIndexChanged            INIT NIL
   METHOD __hqlAddItem
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QActivated
   SIGNAL __hql_QCurrentIndexChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_comboBox

   ::QComboBox:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief add a new row (item). nb it use Qt_UserRole (see Qt default) to store user data (QVariant)
 \param(IN) string [, data [, icon]]
 \return Self

*/
METHOD hqlAddRow( arg1, arg2, arg3 ) CLASS hql_comboBox
   IF ( hb_IsArray(arg1) )
      ::__hqlAddItem( hb_ArrayToParams(arg1) )
   ELSE
      ::__hqlAddItem( arg1, arg2, arg3 )
   ENDIF
RETURN Self

/*!

 \brief add many row (item). nb it use Qt_UserRole (see Qt default) to store user data (QVariant)
 \param(IN) array of { cText [, data [, icon]] }
 \return Self

*/
METHOD hqlAddRows( arg1 ) CLASS hql_comboBox
   LOCAL aItem

   IF ( hb_IsArray(arg1) )
      FOR EACH aItem IN arg1
         IF ( hb_IsArray(aItem) )
            ::__hqlAddItem( hb_ArrayToParams(aItem) )
         ENDIF
      NEXT
   ENDIF

RETURN Self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnActivated( arg1 ) CLASS hql_comboBox
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlActivated := arg1
      IF ( hb_IsEvalItem(::bHqlActivated) )
         ::connect( "activated(int)", { |nInt| ::__hql_QActivated(nInt) } )
      ELSE
         ::disconnect( "activated(int)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnCurrentIndexChanged( arg1 ) CLASS hql_comboBox
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlCurrentIndexChanged := arg1
      IF ( hb_IsEvalItem(::bHqlCurrentIndexChanged) )
         ::connect( "currentIndexChanged(int)" , {|nInt| ::__hql_QCurrentIndexChanged(nInt) } )
      ELSE
         ::disconnect( "currentIndexChanged(int)" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief helper to add item. nb it use Qt_UserRole (see Qt default) to store user data (QVariant)
 \param(IN) cText [, data [, icon]]
 \return Self

*/
METHOD __hqlAddItem( cText, xVariant, xIcon ) CLASS hql_comboBox
   LOCAL oVariant
   LOCAL oIcon

   cText := hb_DefaultValue( cText, "" )

   oVariant := IIF( xVariant == NIL, QVariant(), QVariant( xVariant ) )

   oIcon := IIF( xIcon == NIL, QIcon(), QIcon( xIcon ) )

   ::addItem( oIcon, cText, oVariant )

RETURN NIL

/*!

 \brief [PROTECTED] get current index. WARNING using C style (0 based)
 \param(IN) none
 \return numeric

*/
METHOD __hqlValueGet() CLASS hql_comboBox
RETURN (::currentIndex())

/*!

 \brief [PROTECTED] set current index. WARNING using C style (0 based)
 \param(IN) numeric
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_comboBox
   IF ( hb_IsNumeric(arg1) )
      ::setCurrentIndex(arg1)
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ...
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QActivated(nInt) CLASS hql_comboBox
   IF ( hb_IsEvalItem( ::bHqlActivated ) )
      EVAL( ::bHqlActivated, nInt, Self )   // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ...
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QCurrentIndexChanged(nInt) CLASS hql_comboBox
   IF ( hb_IsEvalItem( ::bHqlCurrentIndexChanged ) )
      EVAL( ::bHqlCurrentIndexChanged, nInt, Self )   // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
