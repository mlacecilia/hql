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

 \brief Returns a new hql_listWidget object instance

*/
FUNCTION hqlListWidget( ... )
RETURN hql_listWidget():new( ... )

/*!

 \brief define hql_listWidget class

*/
CLASS hql_listWidget INHERIT hb_QListWidget, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlAddRow
   METHOD hqlAddRows
   METHOD hqlCurrentData                                    // QListWidget doesn't has currentData method so this is a hql emulation
   METHOD hqlItemData                                       // QListWidget doesn't has itemData method so this is a hql emulation
   METHOD hqlOnCurrentRowChanged
   METHOD hqlOnItemActivated                               // on Windows works when you press enter on highlighted item
   METHOD hqlOnItemClicked

   PROTECTED:
   VAR bHqlCurrentRowChanged              INIT NIL
   VAR bHqlItemActivated                  INIT NIL
   VAR bHqlItemClicked                    INIT NIL
   METHOD __hqlAddItem
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QCurrentRowChanged
   SIGNAL __hql_QItemActivated
   SIGNAL __hql_QItemClicked

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_listWidget

   ::QListWidget:init( ... )

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
METHOD hqlAddRow( arg1, arg2, arg3 ) CLASS hql_listWidget
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
METHOD hqlAddRows( arg1 ) CLASS hql_listWidget
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

 \brief retrieve data for current item
   note 1 QListWidget doesn't has currentData method so this is a hql emulation
   note 2 following Qt behaviour, numeric arguments follow C style; eg. 0, 1, 2, ... instead of CLIPPER 1, 2, 3, ...
 \param(IN) numeric index
 \return Self

*/
METHOD hqlCurrentData( nRole ) CLASS hql_listWidget
   LOCAL oItem := ::currentItem()
   hb_Default( @nRole, Qt_UserRole )

   IF ( hb_IsObject(oItem) )
      RETURN oItem:data(nRole)
   ENDIF
RETURN QVariant()

/*!

 \brief retrieve data for given index.. WARNING using C style (0 based)
   QListWidget doesn't has itemData method so this is a hql extension
 \param(IN) numeric index [, numeric role]
 \return varaint

*/
METHOD hqlItemData( nIndex, nRole ) CLASS hql_listWidget
   nIndex := hb_DefaultValue(nIndex, -1)
   nRole := hb_DefaultValue(nRole, Qt_UserRole)
   IF ( nIndex < 0 .OR. nIndex > (::count()-1) )
      RETURN QVariant()
   ENDIF
RETURN ::item(nIndex):data(nRole)

/*!

 \brief set/get block.
 \param(IN) [OPTIONAL] block | NIL
 \return  block | NIL

*/
METHOD hqlOnCurrentRowChanged( arg1 ) CLASS hql_listWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlCurrentRowChanged := arg1
      IF ( hb_IsEvalItem(::bHqlCurrentRowChanged) )
         ::connect( "currentRowChanged(int)" , {|nInt| ::__hql_QCurrentRowChanged(nInt) } )
      ELSE
         ::disconnect( "currentRowChanged(int)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set/get block.
 \param(IN) [OPTIONAL] block | NIL
 \return  block | NIL

*/
METHOD hqlOnItemActivated( arg1 ) CLASS hql_listWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlItemActivated := arg1
      IF ( hb_IsEvalItem(::bHqlItemActivated) )
         ::connect( "itemActivated(QListWidgetItem*)", { |oItem| ::__hql_QItemActivated(oItem) } )
      ELSE
         ::disconnect( "itemActivated(QListWidgetItem*)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set/get block.
 \param(IN) [OPTIONAL] block | NIL
 \return  block | NIL

*/
METHOD hqlOnItemClicked( arg1 ) CLASS hql_listWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlItemClicked := arg1
      IF ( hb_IsEvalItem(::bHqlItemClicked) )
         ::connect( "itemClicked(QListWidgetItem*)", { |oItem| ::__hql_QItemClicked(oItem) } )
      ELSE
         ::disconnect( "itemClicked(QListWidgetItem*)" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief helper to add item. nb it use Qt_UserRole (see QComboBox default) to store user data (QVariant)
 \param(IN) cText [, data [, icon]]
 \return Self

*/
METHOD __hqlAddItem( cText, xVariant, xIcon ) CLASS hql_listWidget
   LOCAL oVariant
   LOCAL oIcon
   LOCAL oItem

   cText := hb_DefaultValue( cText, "" )

   oVariant := IIF( xVariant == NIL, QVariant(), QVariant( xVariant ) )

   oIcon := IIF( xIcon == NIL, QIcon(), QIcon( xIcon ) )

   oItem := QListWidgetItem()
   oItem:setText( cText )
   oItem:setIcon( oIcon )
   oItem:setData( Qt_UserRole, oVariant )

   ::addItem( oItem )

RETURN NIL

/*!

 \brief [PROTECTED] get. WARNING using C style (0 based)
 \param(IN) none
 \return numeric

*/
METHOD __hqlValueGet() CLASS hql_listWidget
RETURN (::currentRow())

/*!

 \brief [PROTECTED] set. WARNING using C style (0 based)
 \param(IN) numeric
 \return NIL

*/
METHOD __hqlValueSet(arg1) CLASS hql_listWidget
   IF ( hb_IsNumeric(arg1) )
      ::setCurrentRow(arg1)
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ...
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QCurrentRowChanged(nInt) CLASS hql_listWidget
   IF ( hb_IsEvalItem(::bHqlCurrentRowChanged) )
      EVAL( ::bHqlCurrentRowChanged, nInt, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ...
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QItemActivated(oItem) CLASS hql_listWidget
   IF hb_IsEvalItem( ::bHqlItemActivated )
      EVAL( ::bHqlItemActivated, oItem, Self )   // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ...
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QItemClicked(oItem) CLASS hql_listWidget
   IF hb_IsEvalItem( ::bHqlItemClicked )
      EVAL( ::bHqlItemClicked, oItem, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
