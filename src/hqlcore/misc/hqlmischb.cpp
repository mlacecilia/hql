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
#include "hbstack.h"

/*

 \brief returns C pointer from given Harbour object
 \param[in] object
 \return pointer

*/
HB_FUNC( HQL_OBJECTID )
{
    PHB_ITEM pItem = hb_param( 1, HB_IT_ARRAY );

    if( pItem && hb_arrayIsObject( pItem ) )
    {
        hb_retptr( hb_arrayId( pItem ) );
    }
}

/*

 \brief returns Harbour object from given C pointer
 \param[in] pointer
 \return object

*/
HB_FUNC( HQL_OBJECTFROMID )
{
    void * pObjectId = hb_parptr( 1 );

    if ( pObjectId ) {
        PHB_ITEM pItem = hb_arrayFromId( NULL, pObjectId );
        if ( hb_arrayIsObject( pItem ) ) {
            hb_itemReturnRelease( pItem );
        }
        else {
            hb_itemRelease( pItem );
        }
    }
}

static PHB_ITEM hb_vmWithObjectItem( HB_ISIZ nLevel )
{
   HB_ISIZ nOffset = hb_stackWithObjectOffset();

   while( nOffset && nLevel > 0 )
   {
      HB_ISIZ * pnOffset = ( HB_ISIZ * ) hb_itemGetPtr( hb_stackItem( nOffset + 1 ) );
      if( !pnOffset )
         break;
      --nLevel;
      nOffset = *pnOffset;
   }

   return ( nOffset && !nLevel ) ? hb_stackItem( nOffset ) : NULL;
}

/*!

 \brief mimic QWITH

*/
HB_FUNC( HQL_QWITH )
{
   hb_itemReturn( hb_vmWithObjectItem( hb_parns( 1 ) ) );
}
