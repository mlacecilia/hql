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

 \brief Returns a new hql_treeWidget object instance

*/
FUNCTION hqlTreeWidget( ... )
RETURN hql_treeWidget():new( ... )

/*!

 \brief define hql_treeWidget class

*/
CLASS hql_treeWidget INHERIT hb_QTreeWidget, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlNewChild
   METHOD hqlOnCurrentItemChanged
   METHOD hqlOnItemActivated
   METHOD hqlOnItemClicked
   METHOD hqlOnSectionClicked

   PROTECTED:
   VAR bHqlCurrentItemChanged             INIT NIL
   VAR bHqlItemActivated                  INIT NIL
   VAR bHqlItemClicked                    INIT NIL
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QCurrentItemChanged
   SIGNAL __hql_QItemActivated
   SIGNAL __hql_QItemClicked

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_treeWidget

   ::QTreeWidget:init( ... )
   //  build HqlHeaderView this is a trick to keep alive object avoiding undesctructed object at end program
   ::setHeader( hqlHeaderView( /*name*/, Qt_Horizontal, Self ) )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief returns a QTreeWidgetItem empty; if parent given it will be used else current rootitem used
 \param[in] ...
 \return QTreeWidgetItem

*/
METHOD hqlNewChild( arg1 ) CLASS hql_treeWidget
   LOCAL oItem

   IF ( hql_IsDerived(arg1, "QTreeWidgetItem") )
      oItem := QTreeWidgetItem( arg1 )
      arg1:addChild( oItem )
   ELSE
      oItem := QTreeWidgetItem( ::invisibleRootItem() )
      ::invisibleRootItem():addChild( oItem )
   ENDIF

RETURN oItem

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnCurrentItemChanged( arg1 ) CLASS hql_treeWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlCurrentItemChanged := arg1
      IF ( hb_IsEvalItem(::bHqlCurrentItemChanged) )
         ::connect( "currentItemChanged(QTreeWidgetItem*,QTreeWidgetItem*)" , {|oCurrItem,oPrevItem| ::__hql_QCurrentItemChanged(oCurrItem,oPrevItem) } )
      ELSE
         ::disconnect( "currentItemChanged(QTreeWidgetItem*,QTreeWidgetItem*)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnItemActivated( arg1 ) CLASS hql_treeWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlItemActivated := arg1
      IF ( hb_IsEvalItem(::bHqlItemActivated) )
         ::connect( "itemActivated(QTreeWidgetItem*,int)" , {|oItem,nInt| ::__hql_QItemActivated(oItem,nInt ) } )
      ELSE
         ::disconnect( "itemActivated(QTreeWidgetItem*,int)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnItemClicked( arg1 ) CLASS hql_treeWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlItemClicked := arg1
      IF ( hb_IsEvalItem(::bHqlItemClicked) )
         ::connect( "itemClicked(QTreeWidgetItem*,int)" , {|oItem,nInt| ::__hql_QItemClicked(oItem,nInt ) } )
      ELSE
         ::disconnect( "itemClicked(QTreeWidgetItem*,int)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnSectionClicked( ... ) CLASS hql_treeWidget
RETURN ::header:hqlOnSectionClicked( ... )

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] default getting value
 \param[in] none
 \return numeric

*/
METHOD __hqlValueGet() CLASS hql_treeWidget
   LOCAL oModelIndex
   oModelIndex := ::currentIndex()
//   IF oModelIndex:isValid()
//      RETURN oModelIndex:row()
//   ENDIF
RETURN oModelIndex:row()

/*!

 \brief [PROTECTED] default setting value
 \param[in] ...
 \return NIL

*/
METHOD __hqlValueSet( nRow, nCol ) CLASS hql_treeWidget

   nRow := hb_DefaultValue(nRow, 0)

   // suggestion taken from http://doc.qt.io/qt-5/qabstractitemview.html#setCurrentIndex
   //::selectionModel():setCurrentIndex( ::model():index( nRow-1, 0, QModelIndex() ), QItemSelectionModel_NoUpdate )

   IF ( hb_IsNumeric(nCol) .AND. nCol >= 0 .AND. nCol <= (::columnCount()-1) )
      ::selectionModel():setCurrentIndex( ::model():index( nRow, nCol, QModelIndex() ), QItemSelectionModel_SelectCurrent )
   ELSE
      ::selectionModel():setCurrentIndex( ::model():index( nRow, 0, QModelIndex() ), hb_BitOr( QItemSelectionModel_SelectCurrent, QItemSelectionModel_Rows ) )
   ENDIF

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QCurrentItemChanged( oItemCurrent, oItemPrevious ) CLASS hql_treeWidget
   IF hb_IsEvalItem( ::bHqlCurrentItemChanged )
      EVAL( ::bHqlCurrentItemChanged, oItemCurrent, oItemPrevious, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QItemActivated( oItem, nColumn ) CLASS hql_treeWidget
   IF hb_IsEvalItem( ::bHqlItemActivated )
      EVAL( ::bHqlItemActivated, oItem, nColumn, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QItemClicked( oItem, nColumn ) CLASS hql_treeWidget
   IF hb_IsEvalItem( ::bHqlItemClicked )
      EVAL( ::bHqlItemClicked, oItem, nColumn, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
