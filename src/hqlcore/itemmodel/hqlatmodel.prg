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

 \brief hql_atmodel class definition
   a=abstract, t=table

*/
CLASS hql_atmodel INHERIT hb_hqlAbstractTableModel, hql_abs0000

   EXPORTED:
   METHOD init
   METHOD canFetchMore                                      // overload hqlAbstractTableModel
   METHOD columnCount                                       // overload
   METHOD data                                              // overload
   METHOD fetchMore                                         // overload
   METHOD flags                                             // overload
   METHOD headerData                                        // overload
   METHOD rowCount                                          // overload
   METHOD setData                                           // this doesn't exists into hqlAbstractTableModel
   METHOD setHeaderData                                     // this doesn't exists into hqlAbstractTableModel

   PROTECTED:

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance

*/
METHOD init( ... ) CLASS hql_atmodel

   ::hqlAbstractTableModel:init( ... )

   //HBQT_CONNECT returns Error Base/4005: Bound error
   //   ::connect( "headerDataChanged(Qt::Orientation, int, int)", { |orientation,first,last| ::__hql_QHeaderDataChanged( orientation, first, last ) } )

   //HBQT_CONNECT returns Error Base/9999: Argument error __HBQT_ERROR
   //::connect( "headerDataChanged(int, int, int)", { |orientation,first,last| ::__hql_QHeaderDataChanged( orientation, first, last ) } )

RETURN Self

/*!

 \brief returns true or false if there are more data
 \param[in] [OPTIONAL] modelindex parent: with table not used
 \return boolean

*/
METHOD canFetchMore( /*oModelIndex*/ ) CLASS hql_atmodel
RETURN .F.  // default to be returned

/*!

 \brief returns number of columns
 \param[in] [OPTIONAL] modelindex parent: with table not used
 \return numeric

*/
METHOD columnCount( /*oModelIndex*/ ) CLASS hql_atmodel
RETURN 0  // default to be returned

/*!

 \brief returns info related to modelIndex (row / column) and role
 \param[in] valid modelIndex
 \param[in] numeric role
 \return variant IOW different value type

*/
METHOD data( oModelIndex, nRole ) CLASS hql_atmodel

   IF ( !oModelIndex:isValid() )
      RETURN NIL
   ENDIF

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

RETURN NIL

/*!

 \brief do something if canFetchMore returns true
 \param[in] none
 \return none

*/
METHOD fetchMore( /*oModelIndex*/ ) CLASS hql_atmodel
RETURN NIL  // default to be returned

/*!

 \brief returns flags related to modelIndex (row / column)
 \param[in] valid modelIndex
 \return numeric

*/
METHOD flags( /*oModelIndex*/ ) CLASS hql_atmodel
RETURN hb_BitOr( Qt_ItemIsSelectable, Qt_ItemIsEnabled, Qt_ItemNeverHasChildren )  // default to be returned

/*!

 \brief returns info related to section, orientation and role
 \param[in] numeric section; IOW column or row based on orientation
 \param[in] numeric orientation; IOW horizontal or vertical
 \param[in] numeric role
 \return variant IOW different value type

*/
METHOD headerData( nSection, nOrientation, nRole ) CLASS hql_atmodel

   IF ( nSection < 0 )
      RETURN NIL
   ENDIF

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN NIL
      ENDIF
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

RETURN NIL

/*!

 \brief returns number of rows
 \param[in] [OPTIONAL] modelindex parent: with table not used
 \return numeric

*/
METHOD rowCount( /*oModelIndex*/ ) CLASS hql_atmodel
RETURN 0  // default to be returned

/*!

 \brief set data IOW set cell info
 \param[in] modelIndex
 \param[in] variant value IOW string, numeric, boolean and object (QBrush, QIcon, QColor, QPixmap, QFont )
 \param[in] numeric role
 \return boolean

*/
METHOD setData( /*oIndex, oVariant, nRole*/ ) CLASS hql_atmodel
RETURN .F.  // default to be returned

/*!

 \brief set header data info
 \param[in] numeric section
 \param[in] numeric orientation
 \param[in] variant value IOW string, numeric, boolean and object (QBrush, QIcon, QColor, QPixmap, QFont )
 \param[in] numeric role
 \return boolean

*/
METHOD setHeaderData( /*nSection, nOrientation, oVariant, nRole*/ ) CLASS hql_atmodel
RETURN .F.  // default to be returned

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
