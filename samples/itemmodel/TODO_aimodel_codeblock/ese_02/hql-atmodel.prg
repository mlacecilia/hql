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

#include "hbqtqaim.ch"

/*!

 \brief hql_atModel class definition

*/
CREATE CLASS hql_atModel INHERIT hb_hqlAbstractTableModel, hql_qobj

   FRIEND CLASS hql_atModel

   EXPORTED:
   METHOD init
   METHOD hqlCleaner                      // override

   PROTECTED:
   METHOD __columnCount
   METHOD __data
   METHOD __flags
   METHOD __headerData
   METHOD __rowCount
   METHOD __setData
   METHOD __setHeaderData
   METHOD __canFetchMore
   METHOD __fetchMore
   METHOD __queryData

   HIDDEN:
   METHOD __h_hqlCleaner

ENDCLASS

/*!

 \brief initialize object instance

*/
METHOD init( ... ) CLASS hql_atModel

   ::hqlAbstractTableModel:init( ... )

   ::hbSetBlock( { | t, r, x, y, value | ::__queryData( t, r, x, y, value ) } )

RETURN Self

/*!

 \brief callable Hql cleaner
 \param[in] none
 \return NIL

*/
METHOD hqlCleaner() CLASS hql_atModel
   ::__h_hqlCleaner()
RETURN NIL

// ==================== PROTECTED section ====================

/*!

 \brief

*/
METHOD __columnCount() CLASS hql_atModel
RETURN 0 // default to be returned

/*!

 \brief

*/
METHOD __data( nRole, nColumn, nRow ) CLASS hql_atModel

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      RETURN QVariant() //""   // "cell " + hb_NtoS( nColumn+1 ) + "," + hb_NtoS( nRow+1 )

   CASE Qt_DecorationRole              //1.  The data to be rendered as a decoration in the form of an icon. (QColor, QIcon or QPixmap)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
      RETURN QVariant() //""   // "edit cell " + hb_NtoS( nColumn+1 ) + "," + hb_NtoS( nRow+1 )

   CASE Qt_ToolTipRole                 //3.  The data displayed in the item's tooltip. (QString)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_StatusTipRole               //4.  The data displayed in the status bar. (QString)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_WhatsThisRole               //5.  The data displayed for the item in "What's This?" mode. (QString)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_FontRole                    //6.  The font used for items rendered with the default delegate. (QFont)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_TextAlignmentRole           //7.  The alignment of the text for items rendered with the default delegate. (Qt::AlignmentFlag)
      RETURN QVariant() //Qt_AlignHCenter + Qt_AlignVCenter

   CASE Qt_BackgroundRole              //8.  The background brush used for items rendered with the default delegate. (QBrush)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_ForegroundRole              //9. The foreground brush (text color, typically) used for items rendered with the default delegate. (QBrush)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_CheckStateRole              //10. This role is used to obtain the checked state of an item. (Qt::CheckState)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_AccessibleTextRole          //11. The text to be used by accessibility extensions and plugins, such as screen readers. (QString)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_AccessibleDescriptionRole   //12. A description of the item for accessibility purposes. (QString)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_SizeHintRole                //13. The size hint for the item that will be supplied to views. (QSize)
      RETURN QVariant() //NIL //RETURN QVariant()

   CASE Qt_UserRole                    //32. The first role that can be used for application-specific purposes.
      RETURN QVariant() //NIL //RETURN QVariant()

   ENDSWITCH

   HB_SYMBOL_UNUSED( nColumn )
   HB_SYMBOL_UNUSED( nRow )

RETURN QVariant() // default to be returned

/*!

 \brief

*/
METHOD __flags( nColumn, nRow ) CLASS hql_atModel

   HB_SYMBOL_UNUSED( nColumn )
   HB_SYMBOL_UNUSED( nRow )

RETURN hb_BitOr( Qt_ItemIsSelectable, Qt_ItemIsEnabled, Qt_ItemNeverHasChildren ) // see QAbstractTableModel::flags

/*!

 \brief

*/
METHOD __headerData( nRole, nOrientation, nSection ) CLASS hql_atModel

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN QVariant() //""
      ENDIF
      EXIT

   CASE Qt_DecorationRole              //1.  The data to be rendered as a decoration in the form of an icon. (QColor, QIcon or QPixmap)
      EXIT

   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN QVariant() //hb_NtoS( nSection+1 )
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

   HB_SYMBOL_UNUSED( nSection )

RETURN QVariant() // default to be returned

/*!

 \brief

*/
METHOD __rowCount() CLASS hql_atModel
RETURN 0  // default to be returned

/*!

 \brief

*/
METHOD __setData( nRole, nColumn, nRow, value ) CLASS hql_atModel  //   value is Harbour type

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      EXIT

   CASE Qt_DecorationRole              //1.  The data to be rendered as a decoration in the form of an icon. (QColor, QIcon or QPixmap)
      EXIT

   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
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

   HB_SYMBOL_UNUSED( nColumn )
   HB_SYMBOL_UNUSED( nRow )
   HB_SYMBOL_UNUSED( value )

RETURN QVariant( .F. )  // default to be returned

/*!

 \brief

*/
METHOD __setHeaderData( nRole, nSection, nOrientation, value ) CLASS hql_atModel  //   value is Harbour type

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      EXIT

   CASE Qt_DecorationRole              //1.  The data to be rendered as a decoration in the form of an icon. (QColor, QIcon or QPixmap)
      EXIT

   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
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

   HB_SYMBOL_UNUSED( nSection )
   HB_SYMBOL_UNUSED( nOrientation )
   HB_SYMBOL_UNUSED( value )

RETURN QVariant( .F. )  // default to be returned

/*!

 \brief

*/
METHOD __canFetchMore() CLASS hql_atModel
hb_trace( HB_TR_ALWAYS, "ritorna falso" )
RETURN QVariant( .F. )  // default to be returned

/*!

 \brief

*/
METHOD __fetchMore() CLASS hql_atModel
RETURN QVariant()

/*!

 \brief

*/
METHOD __queryData( nInfoType, nRole, nA, nB, value ) CLASS hql_atModel

   // nInfoType is HbQt defined value(s)

   SWITCH nInfoType

   CASE HBQT_QAIM_columnCount
      RETURN ::__columnCount()

   CASE HBQT_QAIM_data
      RETURN ::__data( nRole, nA, nB )

   CASE HBQT_QAIM_flags
      RETURN ::__flags( nA, nB )

   CASE HBQT_QAIM_headerData
      RETURN ::__headerData( nRole, nA, nB )

   CASE HBQT_QAIM_rowCount
      RETURN ::__rowCount()

   CASE HBQT_QAIM_setData
      RETURN ::__setData( nRole, nA, nB, value ) //   value is Harbour type

   CASE HBQT_QAIM_setHeaderData
      RETURN ::__setHeaderData( nRole, nA, nB, value ) //   value is Harbour type

   CASE HBQT_QAIM_canFetchMore
      RETURN ::__canFetchMore()

   CASE HBQT_QAIM_fetchMore
      RETURN ::__fetchMore()

   ENDSWITCH

RETURN QVariant() // always NIL if not managed, in this way C++ returns QVariant()

// ==================== HIDDEN section ====================

/*!

 \brief this class cleaner
 \param[in] none
 \return NIL

*/
METHOD __h_hqlCleaner() CLASS hql_atModel

   ::hbClearBlock()  // mandatory to avoid freezed block at Harbour level

   ::hql_qobj:hqlCleaner()

RETURN NIL
