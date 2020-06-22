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

 \brief define hql_abs0060 class
   inherited by:
      hql_boxLayout,
      hql_formLayout,
      hql_gridLayout,
      hql_hboxLayout,
      hql_layout,
      hql_vboxLayout

*/
CLASS hql_abs0060 INHERIT hql_abs0000

   EXPORTED:
   METHOD hqlAddMeToLayout
   METHOD hqlAddWidget
   METHOD hqlSetLayoutOf

   PROTECTED:

   HIDDEN:

ENDCLASS

/*!

 \brief helper to add object to another layout
 \param(IN) QObject, ...
 \return Self

   PAY ATTENTION to the number of parameters for every Qt object; only QLayout need only one: the widget to be added
      so do not alter IF sequence or at least keep QLayout test as the last.
      - QGridLayout inherits QLayout; QGridLayout:addWidget( oWidget, ... ) see Qt docs for params
      - QHBoxLayout, QVBoxLayout inherits QBoxLayout
      - QBoxLayout inherits QLayout; QBoxLayout:addWidget( oWidget, ... ) see Qt docs for params
      - QLayout:addWidget( oWidget ) see Qt docs for params

      WARNING!!
      - QFormLayout inherits QLayout but QFormLayout:addWidget() it seems not admitted. to be checked

*/
METHOD hqlAddMeToLayout( oObject, ... ) CLASS hql_abs0060
   IF ( oObject:isDerivedFrom("QGridLayout") .OR. oObject:isDerivedFrom("QBoxLayout") )
      oObject:addLayout( Self, ... )
   ELSEIF ( oObject:isDerivedFrom("QLayout") ) // this scenario is not aligned with Qt. I left only for info
      oObject:addLayout( Self )
   ENDIF
RETURN Self

/*!

 \brief helper to add widget
 \param(IN) QWidget derived
 \return self

*/
METHOD hqlAddWidget( oObject, ... ) CLASS hql_abs0060
   IF ( oObject:isDerivedFrom("QWidget") )
      ::addWidget( oObject, ... )
   ENDIF
RETURN Self

/*!

 \brief set current layout as layout of...
 \param[in] object
 \return  object

*/
METHOD hqlSetLayoutOf( oArg1 ) CLASS hql_abs0060
   oArg1 := IIF( hb_IsObject( oArg1 ), oArg1, ::parent() )

   IF ( hb_IsObject(oArg1) )
      oArg1:setLayout( Self )
   ENDIF

RETURN oArg1
// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
