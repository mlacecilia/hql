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

 \brief Returns a new hql_tableWidget object instance

*/
FUNCTION hqlTableWidget( ... )
RETURN hql_tableWidget():new( ... )

/*!

 \brief define hql_tableWidget class

*/
CLASS hql_tableWidget INHERIT hb_QTableWidget, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlAddColumn
   METHOD hqlAddRow
   METHOD hqlClear
   METHOD hqlCurrentColumn
   METHOD hqlCurrentRow
   METHOD hqlEmptyRow
   METHOD hqlGetRow
   METHOD hqlInsertRow
   METHOD hqlOnCellActivated
   METHOD hqlOnCellClicked
   METHOD hqlOnCellDoubleClicked
   METHOD hqlOnSectionClicked( ... )      INLINE (::hqlOnHsectionClicked( ... ) )
   METHOD hqlOnHsectionClicked
   METHOD hqlOnVsectionClicked
   METHOD hqlSelectCell
   METHOD hqlSelectedRows
   METHOD hqlSetRow
   METHOD hqlSort
   METHOD hqlSortable

   PROTECTED:
   VAR bHqlCellActivated                  INIT NIL
   VAR bHqlCellClicked                    INIT NIL
   VAR bHqlCellDoubleClicked              INIT NIL
   VAR aHqlHbColumns                      INIT {}
   METHOD __hqlCheckIsValidRow
   METHOD __hqlCleaner
   METHOD __hqlCustomizeItem
   SIGNAL __hql_QCellActivated
   SIGNAL __hql_QCellClicked
   SIGNAL __hql_QCellDoubleClicked

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_tableWidget

   ::QTableWidget:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF
   //  build HqlHeaderView this is a trick to keep alive object avoiding undesctructed object at end program
   ::setHorizontalHeader( hqlHeaderView(/*name*/, Qt_Horizontal, Self ) )
   ::setVerticalHeader( hqlHeaderView(/*name*/, Qt_Vertical, Self ) )

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief add a new column
 \param(IN)
 \return NIL

*/
METHOD hqlAddColumn( arg1 ) CLASS hql_tableWidget
   LOCAL oHbColumn
   LOCAL nC

   oHbColumn := hql_twCol():new( Self, ::horizontalHeader():defaultSectionSize() )

   ::setColumnCount( ::columnCount()+1 )  // Qt add column
   nC := ::columnCount()-1 // header index reference
   oHbColumn:setQtIndex( nC ) // memorize Qt header index reference
   AADD( ::aHqlHbColumns, oHbColumn )
   ::setHorizontalHeaderItem( nC, oHbColumn:header() ) // set real Qt headerItem kept from Hb object
   // create link between Qt horizontalHeader item and aHqlHbColumns item. NEVER, NEVER directly swap/delete item(s) on this list
   ::horizontalHeaderItem(nC):setData( Qt_UserRole, QVariant( LEN(::aHqlHbColumns) ) )

   IF ( hb_IsNumeric(arg1) .AND. arg1 >= 0 )
      oHbColumn:setWidth(arg1)
   ENDIF

RETURN oHbColumn

/*!

 \brief add row to table widget
 \param(IN) hql_tableWidgetRow object
 \return SELF

*/
METHOD hqlAddRow( oRow ) CLASS hql_tableWidget

   IF ( !::__hqlCheckIsValidRow( oRow ) )
      oRow := ::hqlEmptyRow()
   ENDIF

   ::hqlInsertRow( ::rowCount(), oRow) // row will be appended

RETURN oRow

/*!

 \brief by default only removes row[s]
 \param(IN)
 \return self

*/
METHOD hqlClear() CLASS hql_tableWidget
   ::setRowCount(0)   // it seems signal/event non emitted
RETURN Self

/*!

 \brief set/get current column. WARNING using C style (0 based)
 \param(IN) [numeric]
 \return numeric

*/
METHOD hqlCurrentColumn( nAt ) CLASS hql_tableWidget
   LOCAL oIndex

   IF ( hb_IsNumeric(nAt) )
      ::selectColumn(nAt)
      ::horizontalScrollBar:setSliderPosition(nAt)
   ENDIF

   // check if there is something selected and if the current index is selected; only isSelected doesn't works fine (it seems always true)
   oIndex := ::currentIndex()
   IF ( ::selectionModel:hasSelection() .AND. ::selectionModel:isSelected(oIndex) )
      nAt := oIndex:column()
   ELSE
      nAt := -1
   ENDIF
RETURN nAt

/*!

 \brief set/get current row. WARNING using C style (0 based)
 \param(IN) [numeric]
 \return numeric

*/
METHOD hqlCurrentRow( nAt ) CLASS hql_tableWidget
   LOCAL oIndex

   IF ( hb_IsNumeric(nAt) )
      ::selectRow(nAt)
      ::verticalScrollBar:setSliderPosition(nAt)
   ENDIF

   // check if there is something selected and if the current index is selected; only isSelected doesn't works fine (it seems always true)
   oIndex := ::currentIndex()
   IF ( ::selectionModel:hasSelection() .AND. ::selectionModel:isSelected(oIndex) )
      nAt := oIndex:row()
   ELSE
      nAt := -1
   ENDIF
RETURN nAt

/*!

 \brief returns an empty row where each position is defined as column identifier
 \param(IN)
 \return hql_twRow

*/
METHOD hqlEmptyRow() CLASS hql_tableWidget
   LOCAL oRow
   LOCAL nC
   LOCAL nAt
   LOCAL oItemNew

   oRow := {}

   FOR nC := 0 TO (::columnCount()-1)
      // headerItem has a reference to aHqlHbColumns list?
      IF ( ::horizontalHeaderItem(nC):data(Qt_UserRole):canConvert(QVariant_Int) )
         nAt := ::horizontalHeaderItem(nC):data( Qt_UserRole ):toint()
      ELSE  // no
         nAt := -1
      ENDIF
      oItemNew := QTableWidgetItem()
      // if headerItem has a refence create QTableWidgetItem customized
      IF ( nAt > 0 .AND. nAt <= LEN(::aHqlHbColumns) )
         ::__hqlCustomizeItem( oItemNew, ::aHqlHbColumns[nAt]:cell() )
      ENDIF

      AADD( oRow, oItemNew )
   NEXT

RETURN oRow

/*!

 \brief get a row. WARNING using C style (0 based)
 \param(IN) nRow
 \return hql_twRow

*/
METHOD hqlGetRow( nR ) CLASS hql_tableWidget
   LOCAL nC
   LOCAL oRow
   IF ( hb_IsNumeric(nR) .AND. nR >= 0 .AND. nR < ::rowCount() )
      oRow := {}
      FOR nC := 0 TO (::columnCount()-1)
         AADD( oRow, ::item(nR,nC) )
      NEXT
   ELSE
      oRow := ::hqlEmptyRow()
   ENDIF
RETURN oRow

/*!

 \brief insert row at given index; when omitted or out of range will be appendend. WARNING using C style (0 based)
 \param(IN)
 \return hql_twRow

*/
METHOD hqlInsertRow( nR, oRow ) CLASS hql_tableWidget
   LOCAL nC, nAt

   IF ( !::__hqlCheckIsValidRow( oRow ) )
      oRow := ::hqlEmptyRow()
   ENDIF

   IF ( hb_IsNumeric(nR) .AND. nR >= 0 .AND. nR < ::rowCount() )
      ::insertRow( nR )
   ELSE
      ::insertRow( ::rowCount() )
      nR := ::rowCount()-1
   ENDIF

   FOR nC := 0 TO (::columnCount()-1)
      // headerItem has a reference to aHqlHbColumns list?
      IF ( ::horizontalHeaderItem(nC):data(Qt_UserRole):canConvert(QVariant_Int) )
         nAt := ::horizontalHeaderItem(nC):data( Qt_UserRole ):toint()
      ELSE  // no
         nAt := -1
      ENDIF
      ::setItem( nR, nC, oRow[nC+1] )
      // if headerItem has a refence apply customization
      IF ( nAt > 0 .AND. nAt <= LEN(::aHqlHbColumns) )
         ::__hqlCustomizeItem( ::item(nR, nC), ::aHqlHbColumns[nAt]:cell() )
      ENDIF
   NEXT

RETURN oRow

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnCellActivated( arg1 ) CLASS hql_tableWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlCellActivated := arg1
      IF ( hb_IsEvalItem(::bHqlCellActivated) )
         ::connect( "cellActivated(int,int)" , { |nR,nC| ::__hql_QCellActivated(nR,nC) } )
      ELSE
         ::disconnect( "cellActivated(int,int)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnCellClicked( arg1 ) CLASS hql_tableWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlCellClicked := arg1
      IF ( hb_IsEvalItem(::bHqlCellClicked) )
         ::connect( "cellClicked(int,int)" , { |nR,nC| ::__hql_QCellClicked(nR,nC) } )
      ELSE
         ::disconnect( "cellClicked(int,int)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnCellDoubleClicked( arg1 ) CLASS hql_tableWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlCellDoubleClicked := arg1
      IF ( hb_IsEvalItem(::bHqlCellDoubleClicked) )
         ::connect( "cellDoubleClicked(int,int)" , { |nR,nC| ::__hql_QCellDoubleClicked(nR,nC) } )
      ELSE
         ::disconnect( "cellDoubleClicked(int,int)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnHsectionClicked( ... ) CLASS hql_tableWidget
   ::horizontalHeader:hqlOnSectionClicked( ... )
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnVsectionClicked( ... ) CLASS hql_tableWidget
   ::verticalHeader:hqlOnSectionClicked( ... )
RETURN self

/*!

 \brief select a cell WARNING it seems not working
 \param(IN) numeric, numeric
 \return self

*/
METHOD hqlSelectCell( nR, nC ) CLASS hql_tableWidget
   LOCAL oModelIndex
   IF ( hb_IsNumeric(nR) .AND. hb_IsNumeric(nC) )
      oModelIndex := ::model():index(nR, nC) // if modelIndex is not :valid() nothing selected
      IF ( oModelIndex:isValid() )
         ::selectionModel:select( oModelIndex, QItemSelectionModel_SelectCurrent )
         ::setCurrentIndex( oModelIndex ) // only select() seems not working to know hqlCurrentColumn or hqlCurrentRow
      ENDIF
   ENDIF
RETURN self

/*!

 \brief returns an array of rows number selected
 \param(IN) block | NIL
 \return Harbour_ARRAY

*/
METHOD hqlSelectedRows() CLASS hql_tableWidget
   LOCAL aRows := {}
   LOCAL oHqlSelectionModel := ::selectionModel()
   LOCAL nR
   FOR nR := 0 TO (::rowCount()-1)
      IF ( oHqlSelectionModel:isRowSelected(nR, QModelIndex()) )
         AADD( aRows, nR )
      ENDIF
   NEXT nR
RETURN aRows

/*!

 \brief set row at given index. WARNING using C style (0 based)
 \param(IN) numeric, hql_twRow
 \return Self

*/
METHOD hqlSetRow( nR, oRow ) CLASS hql_tableWidget
   LOCAL nC
   LOCAL nAt

   IF ( hb_IsNumeric(nR) .AND. nR >= 0 .AND. nR < ::rowCount() .AND. ::__hqlCheckIsValidRow( oRow ) )
      FOR nC := 0 TO (::columnCount()-1)
         // headerItem has a reference to aHqlHbColumns list?
         IF ( ::horizontalHeaderItem(nC):data(Qt_UserRole):canConvert(QVariant_Int) )
            nAt := ::horizontalHeaderItem(nC):data( Qt_UserRole ):toint()
         ELSE  // no
            nAt := -1
         ENDIF
         ::setItem( nR, nC, oRow[nC+1] )
         // if headerItem has a refence apply customization
         IF ( nAt > 0 .AND. nAt <= LEN(::aHqlHbColumns) )
            ::__hqlCustomizeItem( ::item(nR, nC), ::aHqlHbColumns[nAt]:cell() )
         ENDIF
      NEXT
   ENDIF
RETURN self

/*!

 \brief sort items
 \param(IN) column [, order type]
 \return Self

*/
METHOD hqlSort( nColumn, nOrder ) CLASS hql_tableWidget
   nColumn := hb_DefaultValue(nColumn, 0)
   nOrder := hb_DefaultValue(nOrder, Qt_AscendingOrder )
   nOrder := IIF( nOrder == Qt_AscendingOrder .OR. nOrder == Qt_DescendingOrder, nOrder, Qt_AscendingOrder )
   ::sortItems( nColumn, nOrder )
RETURN Self

/*!

 \brief enable/disable sort ability
 \param(IN) bool
 \return Self

*/
METHOD hqlSortable( arg1 ) CLASS hql_tableWidget
   arg1 := hb_DefaultValue(arg1, .F.)
   IF ( arg1 )
      ::horizontalHeader:setSectionsClickable(arg1)
   ELSE
      IF ( ::horizontalHeader:hqlOnSectionClicked() == NIL )
         ::horizontalHeader:setSectionsClickable(arg1)
      ENDIF
   ENDIF

   // sort indicator enabled / disabled in according
   ::horizontalHeader:setSortIndicatorShown(arg1)
   ::setSortingEnabled(arg1)
RETURN Self

// ==================== PROTECTED section ====================

/*
 \brief [PROTECTED] returns true if given row is valid
 \param(IN) oRow
 \return bool

*/
METHOD __hqlCheckIsValidRow( oRow ) CLASS hql_tableWidget
   LOCAL lValid := .F.
   LOCAL oItem
   IF ( hb_IsArray(oRow) .AND. LEN(oRow) > 0 .AND. LEN(oRow) >= ::columnCount() )
      lValid := .T.
      FOR EACH oItem IN oRow
         IF ( !hql_IsDerived(oItem, "QTableWidgetItem") )
            lValid := .F.
            EXIT
         ENDIF
      NEXT
   ENDIF
RETURN lValid

/*
 \brief [PROTECTED] Object cleaner
 \param(IN)
 \return NIL

*/
METHOD __hqlCleaner() CLASS hql_tableWidget
   ::aHqlHbColumns := {}
   ::hql_abs0001:__hqlCleaner()
RETURN NIL

/*!

 \brief [PROTECTED] customize item
 \param(IN) QTableWidgetItem, hql_twCol:cell()
 \return NIL

*/
METHOD __hqlCustomizeItem( oItem, oHbCell ) CLASS hql_tableWidget

   WITH OBJECT oItem
      :setBackground( oHbCell:background() )
      :setForeground( oHbCell:foreground() )
      :setFont( oHbCell:font() )
      :setFlags( oHbCell:flags() )
      :setTextAlignment( oHbCell:textAlignment() )
      :setToolTip( oHbCell:toolTip() )
      :setWhatsThis( oHbCell:whatsThis() )
   END WITH

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QCellActivated( nRow, nCol ) CLASS hql_tableWidget
   IF ( hb_IsEvalItem( ::bHqlCellActivated ) )
      EVAL( ::bHqlCellActivated, nRow, nCol, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QCellClicked( nRow, nCol ) CLASS hql_tableWidget
   IF ( hb_IsEvalItem( ::bHqlCellClicked ) )
      EVAL( ::bHqlCellClicked, nRow, nCol, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QCellDoubleClicked( nRow, nCol ) CLASS hql_tableWidget
   IF ( hb_IsEvalItem( ::bHqlCellDoubleClicked ) )
      EVAL( ::bHqlCellDoubleClicked, nRow, nCol, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================

//////////////////////////////////////// \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

/*!

 \brief hql_twCol class definition

*/
CLASS hql_twCol INHERIT hql_hqlobj STATIC

   EXPORTED:
   METHOD init
   METHOD cell                            INLINE ::oCell
   METHOD header                          INLINE ::oHeader
   METHOD hqlCleaner
   METHOD hqlHasParent
   METHOD setQtIndex
   METHOD setWidth
   METHOD width

   PROTECTED:
   VAR oCell                              INIT NIL
   VAR oHeader                            INIT NIL
   VAR pParent                            INIT NIL    // I need a prent (QTbaleWidget) to set/get column width
   VAR nQtIndex                           INIT -1
   VAR nWidth                             INIT 0

   HIDDEN:

ENDCLASS

/*!

 \brief

*/
METHOD init( oParent, nDefaultWidth ) CLASS hql_twCol

   IF ( hql_IsDerived(oParent, "hql_tableWidget") )
      ::pParent := oParent:hqlObjectId()
   ENDIF

   ::oCell := QTableWidgetItem()
   ::oHeader := QTableWidgetItem()

   IF ( hb_IsNumeric(nDefaultWidth) )
      ::nWidth := nDefaultWidth
   ENDIF

RETURN Self

/*!

 \brief cleaner
 \param(IN)
 \return self

*/
METHOD hqlCleaner() CLASS hql_twCol
   ::oCell := NIL
   ::oHeader := NIL
RETURN NIL

/*!

 \brief returns true if it has a parent
 \param(IN)
 \return self

*/
METHOD hqlHasParent() CLASS hql_twCol
RETURN ( hb_IsPointer(::pParent) .AND. hb_IsObject(hql_ObjectFromId(::pParent)) )

/*!

 \brief set header column reference
 \param(IN) numeric
 \return self

*/
METHOD setQtIndex( arg1 ) CLASS hql_twCol
   IF ( hb_IsNumeric(arg1) .AND. arg1 >= 0 )
      ::nQtIndex := INT(arg1)
   ENDIF
RETURN self

/*!

 \brief set column width
 \param(IN) numeric
 \return self

*/
METHOD setWidth( arg1 ) CLASS hql_twCol
   LOCAL oParent
   IF ( hb_IsNumeric(arg1) .AND. arg1 >= 0 )
      ::nWidth := INT(arg1)
      IF ( ::hqlHasParent() )
         oParent := hql_ObjectFromId(::pParent)
         oParent:setColumnWidth( ::nQtIndex, ::nWidth )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief returns column width
 \param(IN)
 \return numeric

*/
METHOD width() CLASS hql_twCol
   LOCAL oParent
   IF ( ::hqlHasParent() )
      oParent := hql_ObjectFromId(::pParent)
      RETURN oParent:columnWidth(::nQtIndex)
   ENDIF
RETURN ::nWidth
