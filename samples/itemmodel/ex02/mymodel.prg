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

 \brief my_model class definition

*/
CREATE CLASS my_model INHERIT hql_atmodel

   EXPORTED:
   METHOD init
   METHOD addColumn
   METHOD addRow
   METHOD clear
   METHOD refresh

   METHOD columnCount                     // override
   METHOD data                            // override
   METHOD headerData                      // override
   METHOD rowCount                        // override
   METHOD setData                         // override
   METHOD setHeaderData                   // override

   PROTECTED:
   VAR aColumnHeaders                     INIT {}
   VAR aDatas                             INIT {}
   METHOD __enlargeInternalDataStru

ENDCLASS

/*!

 \brief initialize object instance

*/
METHOD init( ... ) CLASS my_model

   ::hql_atModel:init( ... )

RETURN Self

/*!

 \brief add new column
 \param[in] none
 \return NIL

*/
METHOD addColumn() CLASS my_model

   ::emitLayoutAboutToBeChanged()
   ::beginInsertColumns( QModelIndex(), LEN( ::aColumnHeaders ), LEN( ::aColumnHeaders ) )   // to append 1 column
   AADD( ::aColumnHeaders, QStandardItem() )
   // we need to enlarge internal data structure
   ::__enlargeInternalDataStru()
   ::endInsertColumns()
   ::emitLayoutChanged()

RETURN Self

/*!

 \brief add new row
 \param[in] none
 \return NIL

*/
METHOD addRow() CLASS my_model

   LOCAL aColumns
   LOCAL nC

   IF LEN( ::aColumnHeaders ) > 0

      ::emitLayoutAboutToBeChanged()
      ::beginInsertRows( QModelIndex(), LEN( ::aDatas ), LEN( ::aDatas ) )
      aColumns := {}
      ASIZE( aColumns, LEN( ::aColumnHeaders ) )
      // AFILL can't be used else the same object is replicated for every column
      FOR nC := 1 TO LEN( ::aColumnHeaders )
         aColumns[ nC ] := QStandardItem()
      NEXT nC
      AADD( ::aDatas, aColumns )
      ::endInsertRows()
      ::emitLayoutChanged()

   ENDIF

RETURN Self

/*!

 \brief clear
 \param[in] none
 \return NIL

*/
METHOD clear() CLASS my_model

   ::beginResetModel()

   ::aColumnHeaders := {}
   ::aDatas := {}

   ::endResetModel()

RETURN Self

/*!

 \brief

*/
METHOD refresh() CLASS my_model

   ::emitLayoutAboutToBeChanged()
   ::emitLayoutChanged()

RETURN Self

/*!

 \brief override hql_atmodel

*/
METHOD columnCount() CLASS my_model
RETURN LEN( ::aColumnHeaders )

/*!

 \brief

*/
METHOD data( oModelIndex, nRole ) CLASS my_model

   IF ( !oModelIndex:isValid() )
      RETURN ::hql_atModel:data( oModelIndex, nRole )
   ENDIF

   IF oModelIndex:row()+1 > LEN( ::aDatas )
      RETURN ::hql_atModel:data( oModelIndex, nRole )
   ENDIF

   IF oModelIndex:column()+1 > LEN( ::aColumnHeaders )
      RETURN ::hql_atModel:data( oModelIndex, nRole )
   ENDIF

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      RETURN ::aDatas[ oModelIndex:row()+1, oModelIndex:column()+1 ]:text()

   CASE Qt_DecorationRole              //1.  The data to be rendered as a decoration in the form of an icon. (QColor, QIcon or QPixmap)
      RETURN ::aDatas[ oModelIndex:row()+1, oModelIndex:column()+1 ]:icon()

   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
      RETURN ::aDatas[ oModelIndex:row()+1, oModelIndex:column()+1 ]:text()

   CASE Qt_ToolTipRole                 //3.  The data displayed in the item's tooltip. (QString)
      RETURN ::aDatas[ oModelIndex:row()+1, oModelIndex:column()+1 ]:tooltip()

   CASE Qt_StatusTipRole               //4.  The data displayed in the status bar. (QString)
      EXIT

   CASE Qt_WhatsThisRole               //5.  The data displayed for the item in "What's This?" mode. (QString)
      RETURN ::aDatas[ oModelIndex:row()+1, oModelIndex:column()+1 ]:whatsThis()

   CASE Qt_FontRole                    //6.  The font used for items rendered with the default delegate. (QFont)
      RETURN ::aDatas[ oModelIndex:row()+1, oModelIndex:column()+1 ]:font()

   CASE Qt_TextAlignmentRole           //7.  The alignment of the text for items rendered with the default delegate. (Qt::AlignmentFlag)
      RETURN ::aDatas[ oModelIndex:row()+1, oModelIndex:column()+1 ]:textAlignment()

   CASE Qt_BackgroundRole              //8.  The background brush used for items rendered with the default delegate. (QBrush)
      EXIT

   CASE Qt_ForegroundRole              //9. The foreground brush (text color, typically) used for items rendered with the default delegate. (QBrush)
      EXIT

   CASE Qt_CheckStateRole              //10. This role is used to obtain the checked state of an item. (Qt::CheckState)
      EXIT

   CASE Qt_AccessibleTextRole          //11. The text to be used by accessibility extensions and plugins, such as screen readers. (QString)
      EXIT

   CASE Qt_AccessibleDescriptionRole   //12. A description of the item for accessibility purposes. (QString)
      EXIT

   CASE Qt_SizeHintRole                //13. The size hint for the item that will be supplied to views. (QSize)
      EXIT

   CASE Qt_UserRole                    //32. The first role that can be used for application-specific purposes.
      EXIT

   ENDSWITCH

RETURN ::hql_atModel:data( oModelIndex, nRole )

/*!

 \brief override hql_atmodel

*/
METHOD headerData( nSection, nOrientation, nRole ) CLASS my_model

   IF nOrientation == Qt_Horizontal .AND. nSection+1 > LEN( ::aColumnHeaders )
      RETURN ::hql_atModel:headerData( nSection, nOrientation, nRole )
   ENDIF

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN ::aColumnHeaders[ nSection+1 ]:text()
      ELSEIF nOrientation == Qt_Vertical
         RETURN hb_NtoS( nSection+1 )
      ENDIF
      EXIT

   CASE Qt_DecorationRole              //1.  The data to be rendered as a decoration in the form of an icon. (QColor, QIcon or QPixmap)
      IF nOrientation == Qt_Horizontal
         RETURN ::aColumnHeaders[ nSection+1 ]:icon()
      ENDIF
      EXIT

   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN ::aColumnHeaders[ nSection+1 ]:text()
      ELSEIF nOrientation == Qt_Vertical
         RETURN hb_NtoS( nSection+1 )
      ENDIF
      EXIT

   CASE Qt_ToolTipRole                 //3.  The data displayed in the item's tooltip. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN ::aColumnHeaders[ nSection+1 ]:tooltip()
      ENDIF
      EXIT

   CASE Qt_StatusTipRole               //4.  The data displayed in the status bar. (QString)
      EXIT

   CASE Qt_WhatsThisRole               //5.  The data displayed for the item in "What's This?" mode. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN ::aColumnHeaders[ nSection+1 ]:whatsThis()
      ENDIF
      EXIT

   CASE Qt_FontRole                    //6.  The font used for items rendered with the default delegate. (QFont)
      IF nOrientation == Qt_Horizontal
         RETURN ::aColumnHeaders[ nSection+1 ]:font()
      ENDIF
      EXIT

   CASE Qt_TextAlignmentRole           //7.  The alignment of the text for items rendered with the default delegate. (Qt::AlignmentFlag)
      IF nOrientation == Qt_Horizontal
         RETURN ::aColumnHeaders[ nSection+1 ]:textAlignment()
      ENDIF
      EXIT

   CASE Qt_BackgroundRole              //8.  The background brush used for items rendered with the default delegate. (QBrush)
      IF nOrientation == Qt_Horizontal
         IF ::aColumnHeaders[ nSection+1 ]:background:isOpaque()
            RETURN ::aColumnHeaders[ nSection+1 ]:background()
         ENDIF
      ENDIF
      EXIT

   CASE Qt_ForegroundRole              //9. The foreground brush (text color, typically) used for items rendered with the default delegate. (QBrush)
      IF nOrientation == Qt_Horizontal
         IF ::aColumnHeaders[ nSection+1 ]:foreground:isOpaque()
            RETURN ::aColumnHeaders[ nSection+1 ]:foreground()
         ENDIF
      ENDIF
      EXIT

   CASE Qt_CheckStateRole              //10. This role is used to obtain the checked state of an item. (Qt::CheckState)
      IF nOrientation == Qt_Horizontal
         RETURN ::aColumnHeaders[ nSection+1 ]:checkState()
      ENDIF
      EXIT

   CASE Qt_AccessibleTextRole          //11. The text to be used by accessibility extensions and plugins, such as screen readers. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN ::aColumnHeaders[ nSection+1 ]:accessibleText()
      ENDIF
      EXIT

   CASE Qt_AccessibleDescriptionRole   //12. A description of the item for accessibility purposes. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN ::aColumnHeaders[ nSection+1 ]:accessibleDescription()
      ENDIF
      EXIT

   CASE Qt_SizeHintRole                //13. The size hint for the item that will be supplied to views. (QSize)
      IF nOrientation == Qt_Horizontal
         IF ! ::aColumnHeaders[ nSection+1 ]:sizeHint:isEmpty()
            RETURN ::aColumnHeaders[ nSection+1 ]:sizeHint()
         ENDIF
      ENDIF
      EXIT

   CASE Qt_UserRole                    //32. The first role that can be used for application-specific purposes.
      EXIT

   ENDSWITCH

RETURN ::hql_atModel:headerData( nSection, nOrientation, nRole )

/*!

 \brief

*/
METHOD rowCount() CLASS my_model
RETURN LEN( ::aDatas )

/*!

 \brief

*/
METHOD setData( oIndex, oVariant, nRole ) CLASS my_model  //   value is Harbour type

   LOCAL lReturn := .F.

   IF ( !oIndex:isValid() )
      RETURN .F.
   ENDIF

   IF oIndex:row()+1 > LEN( ::aDatas )
      RETURN ::hql_atModel:setData( oIndex, oVariant, nRole )
   ENDIF

   IF oIndex:column()+1 > LEN( ::aColumnHeaders )
      RETURN ::hql_atModel:setData( oIndex, oVariant, nRole )
   ENDIF

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      ::aDatas[ oIndex:row()+1, oIndex:column()+1 ]:setText( oVariant:toString() )
      lReturn := .T.  // mandatory
      EXIT

   CASE Qt_DecorationRole              //1.  The data to be rendered as a decoration in the form of an icon. (QColor, QIcon or QPixmap)
      ::aDatas[ oIndex:row()+1, oIndex:column()+1 ]:setIcon( QIcon( oVariant ) )
      lReturn := .T.  // mandatory
      EXIT

   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
      ::aDatas[ oIndex:row()+1, oIndex:column()+1 ]:setText( oVariant:toString() )
      lReturn := .T.  // mandatory
      EXIT

   CASE Qt_ToolTipRole                 //3.  The data displayed in the item's tooltip. (QString)
      ::aDatas[ oIndex:row()+1, oIndex:column()+1 ]:setToolTip( oVariant:toString() )
      lReturn := .T.  // mandatory
      EXIT

   CASE Qt_StatusTipRole               //4.  The data displayed in the status bar. (QString)
      EXIT

   CASE Qt_WhatsThisRole               //5.  The data displayed for the item in "What's This?" mode. (QString)
      ::aDatas[ oIndex:row()+1, oIndex:column()+1 ]:setWhatsThis( oVariant:toString() )
      lReturn := .T.  // mandatory
      EXIT

   CASE Qt_FontRole                    //6.  The font used for items rendered with the default delegate. (QFont)
      ::aDatas[ oIndex:row()+1, oIndex:column()+1 ]:setFont( QFont( oVariant ) )
      lReturn := .T.  // mandatory
      EXIT

   CASE Qt_TextAlignmentRole           //7.  The alignment of the text for items rendered with the default delegate. (Qt::AlignmentFlag)
      ::aDatas[ oIndex:row()+1, oIndex:column()+1 ]:setTextAlignment( oVariant:toInt() )
      EXIT

   CASE Qt_BackgroundRole              //8.  The background brush used for items rendered with the default delegate. (QBrush)
      EXIT

   CASE Qt_ForegroundRole              //9. The foreground brush (text color, typically) used for items rendered with the default delegate. (QBrush)
      EXIT

   CASE Qt_CheckStateRole              //10. This role is used to obtain the checked state of an item. (Qt::CheckState)
      EXIT

   CASE Qt_AccessibleTextRole          //11. The text to be used by accessibility extensions and plugins, such as screen readers. (QString)
      EXIT

   CASE Qt_AccessibleDescriptionRole   //12. A description of the item for accessibility purposes. (QString)
      EXIT

   CASE Qt_SizeHintRole                //13. The size hint for the item that will be supplied to views. (QSize)
      EXIT

   CASE Qt_UserRole                    //32. The first role that can be used for application-specific purposes.
      EXIT

   ENDSWITCH

   IF lReturn
      ::EmitDataChanged( oIndex, oIndex )
   ENDIF

RETURN lReturn

/*!

 \brief

*/
METHOD setHeaderData( nSection, nOrientation, oVariant, nRole ) CLASS my_model  //   value is Harbour type

   LOCAL lReturn := .F.

   IF nOrientation == Qt_Horizontal .AND. nSection+1 > LEN( ::aColumnHeaders )
      RETURN ::hql_atModel:setHeaderData( nSection, nOrientation, oVariant, nRole )
   ENDIF

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      IF nOrientation == Qt_Horizontal
         ::aColumnHeaders[ nSection+1 ]:setText( oVariant:toString() )
         lReturn := .T.  // mandatory
      ENDIF
      EXIT

   CASE Qt_DecorationRole              //1.  The data to be rendered as a decoration in the form of an icon. (QColor, QIcon or QPixmap)
      EXIT

   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
      IF nOrientation == Qt_Horizontal
         ::aColumnHeaders[ nSection+1 ]:setText( oVariant:toString() )
         lReturn := .T.  // mandatory
      ENDIF
      EXIT

   CASE Qt_ToolTipRole                 //3.  The data displayed in the item's tooltip. (QString)
      EXIT

   CASE Qt_StatusTipRole               //4.  The data displayed in the status bar. (QString)
      EXIT

   CASE Qt_WhatsThisRole               //5.  The data displayed for the item in "What's This?" mode. (QString)
      EXIT

   CASE Qt_FontRole                    //6.  The font used for items rendered with the default delegate. (QFont)
      EXIT

   CASE Qt_TextAlignmentRole           //7.  The alignment of the text for items rendered with the default delegate. (Qt::AlignmentFlag)
      IF nOrientation == Qt_Horizontal
         ::aColumnHeaders[ nSection+1 ]:setTextAlignment( oVariant:toInt() )
         lReturn := .T.  // mandatory
      ENDIF
      EXIT

   CASE Qt_BackgroundRole              //8.  The background brush used for items rendered with the default delegate. (QBrush)
      IF nOrientation == Qt_Horizontal
         ::aColumnHeaders[ nSection+1 ]:setBackground( oVariant )
         lReturn := .T.  // mandatory
      ENDIF
      EXIT

   CASE Qt_ForegroundRole              //9. The foreground brush (text color, typically) used for items rendered with the default delegate. (QBrush)
      EXIT

   CASE Qt_CheckStateRole              //10. This role is used to obtain the checked state of an item. (Qt::CheckState)
      EXIT

   CASE Qt_AccessibleTextRole          //11. The text to be used by accessibility extensions and plugins, such as screen readers. (QString)
      EXIT

   CASE Qt_AccessibleDescriptionRole   //12. A description of the item for accessibility purposes. (QString)
      EXIT

   CASE Qt_SizeHintRole                //13. The size hint for the item that will be supplied to views. (QSize)
      EXIT

   CASE Qt_UserRole                    //32. The first role that can be used for application-specific purposes.
      EXIT

   ENDSWITCH

   IF lReturn
      ::EmitHeaderDataChanged( nOrientation, nSection, nSection )
   ENDIF

RETURN lReturn

// ==================== PROTECTED section ====================

METHOD __enlargeInternalDataStru() CLASS my_model

   LOCAL nRow
   LOCAL aColumns
   LOCAL nCol

   FOR nRow := 1 TO LEN( ::aDatas )
      aColumns := ::aDatas[ nRow ]
      aColumns := ASIZE( aColumns, LEN( ::aColumnHeaders ) )
      FOR nCol := 1 TO LEN( ::aColumnHeaders )
         IF EMPTY( aColumns[ nCol ] )
            aColumns[ nCol ] := QStandardItem()
         ENDIF
      NEXT nCol

      ::aDatas[ nRow ] := aColumns

   NEXT nRow

RETURN NIL

// ==================== HIDDEN section ====================
