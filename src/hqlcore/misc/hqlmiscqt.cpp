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
#include "hbvmint.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbstack.h"
#include "hbqt.h"

#if QT_VERSION <= 0x040900
#include <QtGui/QWidget>
#else
#include <QtWidgets/QWidget>
#endif

HB_FUNC( HQL_DELETE )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   if( pObject )
   {
      hbqt_bindDestroyHbObject( pObject );
   }
}

HB_FUNC( HQL_SETNOPARENT )
{
   if( hbqt_par_isDerivedFrom( 1, "QOBJECT" ) )
   {
      QObject* p = ( QObject * ) hbqt_par_ptr(1) ;
      if(p)
      {
         p->setParent(0);
      }
   }
}
