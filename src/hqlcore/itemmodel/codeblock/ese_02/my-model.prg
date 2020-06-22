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

#include "dbstruct.ch"

/*!

 \brief my_model class definition
 take example from from http://doc.qt.io/qt-5/qtwidgets-itemviews-fetchmore-example.html

*/
CLASS my_model INHERIT hql_atModel

   EXPORTED:
   METHOD init
   METHOD hqlCleaner                      // override
   METHOD setWarea

   PROTECTED:
   VAR nWarea                             INIT 0
   VAR nCounter                           INIT 0
   VAR aList                              INIT {}
   METHOD __isOpen
   METHOD __getHeaderInfo
   METHOD __loadRecords

   METHOD __columnCount                   // override
   METHOD __rowCount                      // override
   METHOD __data                          // override
   METHOD __headerData                    // override
   METHOD __canFetchMore                  // override
   METHOD __fetchMore                     // override

   HIDDEN:
   METHOD __h_hqlCleaner

ENDCLASS

/*!

 \brief initialize object instance

*/
METHOD init( ... ) CLASS my_model

   ::hql_atModel:init( ... )

RETURN Self

/*!

 \brief callable Hql cleaner
 \param[in] none
 \return NIL

*/
METHOD hqlCleaner() CLASS my_model
   ::__h_hqlCleaner()
RETURN NIL

/*!

 \brief set up work area number
 \param[in] numeric
 \return Self

*/
METHOD setWarea( ... ) CLASS my_model

   IF hb_IsNumeric( hb_Pvalue(1) )

      IF hb_Pvalue(1) == 0 // close
         ::beginResetModel()
         ::nWarea := hb_Pvalue(1)
         ::__loadRecords()
         ::endResetModel()
      ELSEIF ! ( ::nWarea == hb_Pvalue(1) )  //different warea
         ::beginResetModel()
         ::nWarea := hb_Pvalue(1)
         ::__loadRecords()
         ::endResetModel()
      ENDIF

   ENDIF

RETURN Self

// ==================== PROTECTED section ====================

METHOD __isOpen() CLASS my_model
RETURN ( ::nWarea > 0 )

METHOD __loadRecords() CLASS my_model

   LOCAL aColumns
   LOCAL nF
   LOCAL dDate
   LOCAL oDate

   ::aList := {}
   ::nCounter := 0

   IF ::__isOpen()
      (::nWarea)->(DBGOTOP())
      DO WHILE ! (::nWarea)->(EOF())
         aColumns := ASIZE( {}, ::__columnCount() )
         FOR nF := 1 TO ::__columnCount()
            aColumns[ nF ] := QStandardItem()
            SWITCH ( ::__getHeaderInfo( nF, DBS_TYPE ) )
            CASE "C"
               aColumns[ nF ]:setText( (::nWarea)->(FIELDGET(nF)) )
               EXIT
            CASE "N"
               aColumns[ nF ]:setText( hb_NtoS( (::nWarea)->(FIELDGET(nF)) ) )
               EXIT
            CASE "D"
               dDate := (::nWarea)->(FIELDGET(nF))
               oDate := QDate( YEAR( dDate ), MONTH( dDate ), DAY( dDate ) )
               aColumns[ nF ]:setText( oDate:toString( QLocale():dateFormat( QLocale_ShortFormat ) ) )
               EXIT
            CASE "L"
               aColumns[ nF ]:setText( hb_ValToExp( (::nWarea)->(FIELDGET(nF)) ) )
               EXIT
            ENDSWITCH
         NEXT nF
         AADD( ::aList, aColumns )
         (::nWarea)->(DBSKIP())
      ENDDO
   ENDIF

hb_trace( HB_TR_ALWAYS, "::nCounter=" + hb_NtoS( ::nCounter ) + " len=" + hb_NtoS( LEN( ::aList ) ) )

RETURN NIL

METHOD __getHeaderInfo( nField, nInfo ) CLASS my_model

   LOCAL aDbStru

   hb_Default( @nField, 0 )
   hb_Default( @nInfo, 0 )

   aDbStru := (::nWarea)->(DBSTRUCT()) // If there is no database file in USE in the current work area, DBSTRUCT() returns an empty array ({})

   SWITCH nInfo
   CASE DBS_NAME
      IF nField > 0 .AND. nField <= LEN( aDbStru )
         RETURN aDbStru[ nField, DBS_NAME ]
      ELSE
         RETURN ""
      ENDIF
   CASE DBS_TYPE
      IF nField > 0 .AND. nField <= LEN( aDbStru )
         RETURN aDbStru[ nField, DBS_TYPE ]
      ELSE
         RETURN ""
      ENDIF
   CASE DBS_LEN
      IF nField > 0 .AND. nField <= LEN( aDbStru )
         RETURN aDbStru[ nField, DBS_LEN ]
      ELSE
         RETURN 0
      ENDIF
   CASE DBS_DEC
      IF nField > 0 .AND. nField <= LEN( aDbStru )
         RETURN aDbStru[ nField, DBS_DEC ]
      ELSE
         RETURN 0
      ENDIF
   ENDSWITCH

RETURN NIL  // never occur

// ==================== PROTECTED section ====================

/*!

 \brief

*/
METHOD __columnCount() CLASS my_model
   IF ::__isOpen()
      RETURN (::nWarea)->(FCOUNT())
   ENDIF
RETURN ::hql_atModel:__columnCount()

/*!

 \brief

*/
METHOD __rowCount() CLASS my_model
RETURN ::nCounter

/*!

 \brief

*/
METHOD __data( nRole, nColumn, nRow ) CLASS my_model

   IF ! ::__isOpen()
      RETURN ::hql_atModel:__data( nRole, nColumn, nRow )
   ENDIF

   IF ( nRow+1 > LEN( ::aList ) .OR. nRow+1 < 1 )
      RETURN ::hql_atModel:__data( nRole, nColumn, nRow )
   ENDIF

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      RETURN ::aList[ nRow+1, nColumn+1 ]:text()

   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
      RETURN ::aList[ nRow+1, nColumn+1 ]:text()

   ENDSWITCH

RETURN ::hql_atModel:__data( nRole, nColumn, nRow )

/*!

 \brief

*/
METHOD __headerData( nRole, nOrientation, nSection ) CLASS my_model

   IF ! ::__isOpen()
      RETURN ::hql_atModel:__headerData( nRole, nOrientation, nSection )
   ENDIF

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN ::__getHeaderInfo( nSection+1, DBS_NAME ) + " (" + ::__getHeaderInfo( nSection+1, DBS_TYPE ) + ")"
      ENDIF
      RETURN hb_NtoS( nSection+1 )

   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN ::__getHeaderInfo( nSection+1, DBS_NAME )
      ENDIF
      RETURN hb_NtoS( nSection+1 )

   CASE Qt_ToolTipRole                 //3.  The data displayed in the item's tooltip. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN "type=" + ::__getHeaderInfo( nSection+1, DBS_TYPE ) + ;
                " length=" + hb_NtoS( ::__getHeaderInfo( nSection+1, DBS_LEN ) ) + ;
                " dec=" + hb_NtoS( ::__getHeaderInfo( nSection+1, DBS_DEC ) )
      ENDIF
      EXIT

   CASE Qt_TextAlignmentRole           //7.  The alignment of the text for items rendered with the default delegate. (Qt::AlignmentFlag)
      IF nOrientation == Qt_Horizontal
         RETURN QVariant( Qt_AlignCenter )
      ENDIF
      EXIT

   ENDSWITCH

RETURN ::hql_atModel:__headerData( nRole, nOrientation, nSection )

/*!

 \brief

*/
METHOD __canFetchMore() CLASS my_model

   IF ::nCounter < LEN( ::aList )
hb_trace( HB_TR_ALWAYS, "ritorna vero" )
      RETURN QVariant( .T. )
   ENDIF

RETURN ::hql_atModel:__canFetchMore()

/*!

 \brief

*/
METHOD __fetchMore() CLASS my_model

   LOCAL nLen
   LOCAL nRemainder
   LOCAL nItemsToFetch

   IF ::__isOpen()

      nLen := LEN( ::aList )
      nRemainder := nLen - ::nCounter
      nItemsToFetch := MIN( 100, nRemainder )

hb_trace( HB_TR_ALWAYS, "nRemainder="+hb_NtoS(nRemainder)+" nItemsToFetch="+hb_NtoS(nItemsToFetch) )

      ::beginInsertRows( QModelIndex(), ::nCounter, ::nCounter+nItemsToFetch-1 )

      ::nCounter += nItemsToFetch

      ::endInsertRows()

   ENDIF

RETURN ::hql_atModel:__fetchMore()

// ==================== HIDDEN section ====================

/*!

 \brief this class cleaner
 \param[in] none
 \return NIL

*/
METHOD __h_hqlCleaner() CLASS my_model

   ::hql_atModel:hqlCleaner()

RETURN NIL
