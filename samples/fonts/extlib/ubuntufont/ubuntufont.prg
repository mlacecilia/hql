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

/*!

 \brief [public_function] creates hqlUbuntuFont object
 \param[in] none
 \return hql_ubuntufont object

*/
FUNCTION hqlUbuntuFont()
RETURN hql_ubuntufont():new()

/*!

 \brief hql_ubuntufont class definition

*/
CLASS hql_ubuntufont STATIC

   EXPORTED:
   METHOD init
   ACCESS fontList
   ACCESS rccData                   // override to return hbqtres_xxxxx() data

   PROTECTED:

ENDCLASS

/*!

 \brief initialize object instance
 \param[in] none
 \return SELF

*/
METHOD init() CLASS hql_ubuntufont
RETURN Self

/*!

 \brief returns font list
 \param[in] none
 \return rccData

*/
METHOD fontList() CLASS hql_ubuntufont

   LOCAL aFontList := {}

   // create a list of fonts; the list contains all [only] the fonts in the .qrc file
   // rember to add the path (see qrc) not only the file name
   AADD( aFontList, ":/ubufont/Ubuntu-R.ttf" )
   AADD( aFontList, ":/ubufont/Ubuntu-RI.ttf" )

RETURN aFontList

/*!

 \brief returns content of function hbqtres_hqlubuntufont()
 \param[in] none
 \return rccData

*/
METHOD rccData() CLASS hql_ubuntufont
RETURN hbqtres_ubuntufont()
