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
CREATE CLASS samp02 INHERIT hql_childWindow

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
METHOD init( ... ) CLASS samp02

   ::hql_childWindow:init( ... )

   ::setMaximumSize( ::parent:size() )

   ::__createUi()

   ::adjustSize()

RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief
 \param[in]
 \return

*/
METHOD __createUi() CLASS samp02

   LOCAL oCentral
   LOCAL oMlayout
   LOCAL oHlayout
   LOCAL oVlayout

   ::setWindowTitle( "sample 02" )

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

   // row layouting -start-
   oVlayout := QVBoxLayout( oCentral )
   oVlayout:setContentsMargins( 0, 0, 0, 0 )
   oVlayout:addWidget( hqlLabel( /*name*/, oCentral ) )
   WITH OBJECT oVlayout:itemAt( oVlayout:count()-1 ):widget()
      :setText( "first label" )
   END WITH
   oVlayout:addWidget( hqlLineEdit( /*name*/, oCentral ) )

   oHlayout := QHBoxLayout( oCentral )
   oHlayout:setContentsMargins( 0, 0, 0, 0 )
   oHlayout:addLayout( oVlayout )
   oHlayout:addStretch()
   oMlayout:addLayout( oHlayout )
   // row layouting -end-

   // row layouting -start-
   oVlayout := QVBoxLayout( oCentral )
   oVlayout:setContentsMargins( 0, 0, 0, 0 )
   oVlayout:addWidget( hqlLabel( /*name*/, oCentral ) )
   WITH OBJECT oVlayout:itemAt( oVlayout:count()-1 ):widget()
      :setText( "second label" )
   END WITH
   oVlayout:addWidget( hqlLineEdit( /*name*/, oCentral ) )

   oHlayout := QHBoxLayout( oCentral )
   oHlayout:setContentsMargins( 0, 0, 0, 0 )
   oHlayout:addLayout( oVlayout )
   oHlayout:addStretch()
   oMlayout:addLayout( oHlayout )
   // row layouting -end-

   // row layouting -start-
   oVlayout := QVBoxLayout( oCentral )
   oVlayout:setContentsMargins( 0, 0, 0, 0 )
   oVlayout:addWidget( hqlLabel( /*name*/, oCentral ) )
   WITH OBJECT oVlayout:itemAt( oVlayout:count()-1 ):widget()
      :setText( "third label" )
   END WITH
   oVlayout:addWidget( hqlLineEdit( /*name*/, oCentral ) )

   oHlayout := QHBoxLayout( oCentral )
   oHlayout:setContentsMargins( 0, 0, 0, 0 )
   oHlayout:addLayout( oVlayout )
   oHlayout:addStretch()
   oMlayout:addLayout( oHlayout )
   // row layouting -end-

   // row layouting -start-
   oHlayout := QHBoxLayout( oCentral )
   oHlayout:setContentsMargins( 0, 0, 0, 0 )
   oHlayout:addWidget( cuswidget( /*name*/, oCentral ) )
   oHlayout:addStretch()
   oMlayout:addLayout( oHlayout )
   // row layouting -end-

   oMlayout:addSpacerItem( QSpacerItem( 10, 20, QSizePolicy_Expanding, QSizePolicy_Minimum ) )

   // row layouting -start-
   oHlayout := QHBoxLayout( oCentral )
   oHlayout:setContentsMargins( 0, 0, 0, 0 )
   oHlayout:addStretch()
   oHlayout:addWidget( hqlPushButton( /*name*/, oCentral ) )
   WITH OBJECT oHlayout:itemAt( oHlayout:count()-1 ):widget()
      :setText( "first button" )
   END WITH
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
