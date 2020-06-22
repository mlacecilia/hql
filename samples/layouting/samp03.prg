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
CREATE CLASS samp03 INHERIT hql_childWindow

   EXPORTED:
   METHOD init
   METHOD hqlCleaner

   PROTECTED:
   METHOD __createUi

   HIDDEN:
   METHOD __h_hqlCleaner

END CLASS

/*!

 \brief initialize object instance
 \param[in] string name OR NIL (MANDATORY)
 \param[in] other Qt arguments
 \return SELF

*/
METHOD init( ... ) CLASS samp03

   ::hql_childWindow:init( ... )

   ::setMaximumSize( ::parent:size() )

   ::__createUi()

   ::adjustSize()

RETURN Self

/*!

 \brief callable Hql cleaner
 \param[in] none
 \return NIL

*/
METHOD hqlCleaner() CLASS samp03
   ::__h_hqlCleaner()
RETURN NIL

// ==================== PROTECTED section ====================

/*!

 \brief
 \param[in]
 \return

*/
METHOD __createUi() CLASS samp03

   LOCAL oCentral
   LOCAL oMlayout
   LOCAL oGlayout
   LOCAL oHlayout

   ::setWindowTitle( "sample 03" )

   WITH OBJECT Self
      WITH OBJECT hqlToolBar()
         :setIconSize( QSize( 32, 32 ) )
         :setContextMenuPolicy( Qt_PreventContextMenu ) // to prevent rightClick to close
         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :setIcon( QIcon( ":/hqlres/txtbold" ) )
         END WITH
         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :setIcon( QIcon( ":/hqlres/txtcenter" ) )
         END WITH
         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :setIcon( QIcon( ":/hqlres/txtcolor" ) )
         END WITH
         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :setIcon( QIcon( ":/hqlres/txtfamily" ) )
         END WITH
      END WITH
   END WITH

   ::setCentralWidget( hqlWidget( /*name*/, Self ) )
   oCentral := ::centralWidget()

   // main layout used for general layouting
   oMlayout := QVBoxLayout( oCentral )
   // avoid it else there aren't margins oMlayout:setContentsMargins( 0, 0, 0, 0 )
   oCentral:setLayout( oMlayout )

   oGlayout := QGridLayout( oCentral )
   oGlayout:setContentsMargins( 0, 0, 0, 0 )

   // is like (+-) :addStretch(); it creates spacer on the right side and push widgets on left :-)
   oGlayout:addItem( QSpacerItem( 5, 5, QSizePolicy_Expanding, QSizePolicy_Expanding ), 0, 2, 1, -1 )

   // row layouting -start-
   oGlayout:addWidget( hqlLabel( /*name*/, oCentral ), 0, 0, hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
   WITH OBJECT oGlayout:itemAt( oGlayout:count()-1 ):widget()
      :setText( "first label" )
      :setAlignment( Qt_AlignRight )
   END WITH
   oGlayout:addWidget( hqlLineEdit( /*name*/, oCentral ), 0, 1 )
   // row layouting -end-

   // row layouting -start-
   oGlayout:addWidget( hqlLabel( /*name*/, oCentral ), 1, 0, hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
   WITH OBJECT oGlayout:itemAt( oGlayout:count()-1 ):widget()
      :setText( "second label" )
      :setAlignment( Qt_AlignRight )
   END WITH
   oGlayout:addWidget( hqlLineEdit( /*name*/, oCentral ), 1, 1 )
   // row layouting -end-

   // row layouting -start-
   oGlayout:addWidget( hqlLabel( /*name*/, oCentral ), 2, 0, hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
   WITH OBJECT oGlayout:itemAt( oGlayout:count()-1 ):widget()
      :setText( "third label" )
      :setAlignment( Qt_AlignRight )
   END WITH
   oGlayout:addWidget( hqlLineEdit( /*name*/, oCentral ), 2, 1 )
   // row layouting -end-

   // row layouting -start-
   oGlayout:addWidget( cuswidget( /*name*/, oCentral ), 3, 0, 1, -1, Qt_AlignLeft )
   // row layouting -end-

   oMlayout:addLayout( oGlayout )

   oMlayout:addSpacerItem( QSpacerItem( 10, 20, QSizePolicy_Expanding, QSizePolicy_Minimum ) )

   // row layouting -start-
   oHlayout := QHBoxLayout( oCentral )
   oHlayout:setContentsMargins( 0, 0, 0, 0 )
   oHlayout:addStretch()   // oHlayout:addStretch( 3 )
   oHlayout:addWidget( hqlPushButton( /*name*/, oCentral ) )
   WITH OBJECT oHlayout:itemAt( oHlayout:count()-1 ):widget()
      :setText( "first button" )
   END WITH
   //oHlayout:addStretch()   // oHlayout:addStretch( 1 )
   oHlayout:addSpacerItem( QSpacerItem( 5, 5, QSizePolicy_Minimum, QSizePolicy_Expanding ) )
   oHlayout:addWidget( hqlPushButton( /*name*/, oCentral ) )
   WITH OBJECT oHlayout:itemAt( oHlayout:count()-1 ):widget()
      :setText( "second button" )
   END WITH
   oMlayout:addLayout( oHlayout )
   // row layouting -end-

   oMlayout:addStretch()

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================

/*!

 \brief this class cleaner
 \param[in] none
 \return NIL

*/
METHOD __h_hqlCleaner() CLASS samp03
   ::hql_childWindow:hqlCleaner()
RETURN NIL
