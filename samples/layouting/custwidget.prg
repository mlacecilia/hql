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
#include "hqlinclude.ch"

/*!

 \brief

*/
FUNCTION cuswidget( ... )
RETURN custom_widget():new( ... )

/*!

 \brief

*/
CREATE CLASS custom_widget INHERIT hql_frame

   EXPORTED:
   METHOD init

   PROTECTED:
   METHOD __createUi

   HIDDEN:

END CLASS

/*!

 \brief initialize object instance
 \param[in] string name OR NIL (MANDATORY)
 \param[in] other Qt arguments
 \return SELF

*/
METHOD init( ... ) CLASS custom_widget

   ::hql_frame:init( ... )

   ::__createUi()

RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief
 \param[in]
 \return

*/
METHOD __createUi() CLASS custom_widget

   LOCAL oMlayout
   LOCAL oHlayout

   // main layout used for general layouting
   oMlayout := QVBoxLayout( Self )
   oMlayout:setContentsMargins( 0, 0, 0, 0 )
   ::setLayout( oMlayout )

   oMlayout:addWidget( hqlLabel( /*name*/, Self ), 0, Qt_AlignCenter )
   WITH OBJECT oMlayout:itemAt( oMlayout:count()-1 ):widget()
      :setText( "custom widget" )
   END WITH

   oHlayout := QHBoxLayout( Self )
   oHlayout:setContentsMargins( 0, 0, 0, 0 )
   oMlayout:addLayout( oHlayout )

   oHlayout:addWidget( hqlLabel( /*name*/, Self ), 0, hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
   WITH OBJECT oHlayout:itemAt( oHlayout:count()-1 ):widget()
      :setText( "label 1" )
      :setAlignment( Qt_AlignRight )
   END WITH
   oHlayout:addWidget( hqlSpinBox( /*name*/, Self ) )

   oHlayout:addSpacerItem( QSpacerItem( 5, 5, QSizePolicy_Minimum, QSizePolicy_Minimum ) )

   oHlayout:addWidget( hqlLabel( /*name*/, Self ), 0, hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
   WITH OBJECT oHlayout:itemAt( oHlayout:count()-1 ):widget()
      :setText( "label 2" )
      :setAlignment( Qt_AlignRight )
   END WITH
   oHlayout:addWidget( hqlSpinBox( /*name*/, Self ) )

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
