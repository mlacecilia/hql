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
   METHOD columnCount                     // override
   METHOD data                            // override
   METHOD flags                           // override
   METHOD headerData                      // override
   METHOD rowCount                        // override

   PROTECTED:
   VAR aHeaders                           INIT {}
   VAR aRows                              INIT {}

ENDCLASS

/*!

 \brief initialize object instance

*/
METHOD init( ... ) CLASS my_model

   ::hql_atModel:init( ... )

   AADD( ::aHeaders, "colonna_1" )
   AADD( ::aHeaders, "colonna_2" )
   AADD( ::aHeaders, "colonna_3" )

   AADD( ::aRows, { "R1C1", "R1C2", "R1C3" } )
   AADD( ::aRows, { "R2C1", "R2C2", "R2C3" } )
   AADD( ::aRows, { "R3C1", "R3C2", "R3C3" } )
   AADD( ::aRows, { "R4C1", "R4C2", "R4C3" } )

RETURN Self

/*!

 \brief returns columns number
 \param[in] none
 \return numeric

*/
METHOD columnCount() CLASS my_model
RETURN LEN( ::aHeaders )

/*!

 \brief

*/
METHOD data( oModelIndex, nRole ) CLASS my_model

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
      RETURN ::aRows[ oModelIndex:row()+1, oModelIndex:column()+1 ]

   ENDSWITCH

RETURN NIL

/*!

 \brief

*/
METHOD flags( oModelIndex ) CLASS my_model
RETURN ::hbFlags( oModelIndex )

/*!

 \brief

*/
METHOD headerData( nSection, nOrientation, nRole ) CLASS my_model

   SWITCH nRole

   CASE Qt_DisplayRole                 //0.  The key data to be rendered in the form of text. (QString)
   CASE Qt_EditRole                    //2.  The data in a form suitable for editing in an editor. (QString)
      IF nOrientation == Qt_Horizontal
         RETURN ::aHeaders[ nSection+1 ]
      ENDIF
      IF nOrientation == Qt_Vertical
         RETURN hb_NtoS( nSection+1 )
      ENDIF
      EXIT

   CASE Qt_BackgroundRole              //8.  The background brush used for items rendered with the default delegate. (QBrush)
      IF nOrientation == Qt_Horizontal
         RETURN QBrush( QColor( Qt_cyan ) )
      ENDIF
      EXIT

   ENDSWITCH

RETURN NIL

/*!

 \brief returns rows number
 \param[in] none
 \return numeric

*/
METHOD rowCount() CLASS my_model
RETURN LEN( ::aRows )

// ==================== PROTECTED section ====================

// ==================== HIDDEN section ====================
