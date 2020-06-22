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
#ifndef HBQT_HQLAbstractTableModel_H
#define HBQT_HQLAbstractTableModel_H

#include "hbqtcore.h"
#include "hbqtgui.h"
#include <QtCore/QAbstractTableModel>
#include <QtCore/QPointer>

#include "hbqtqaim.ch"

class HQLAbstractTableModel : public QAbstractTableModel
{
   Q_OBJECT

public:
   HQLAbstractTableModel( QObject * parent = 0 );
   virtual ~HQLAbstractTableModel( void );

   PHB_ITEM block;

   void hbSetBlock( PHB_ITEM b );
   void hbClearBlock( void );
   QModelIndex   index(int row, int column, const QModelIndex &parent = QModelIndex()) const;

   Qt::ItemFlags flags( const QModelIndex & index ) const;
   QVariant      data( const QModelIndex & index, int role = Qt::DisplayRole ) const;
   bool          setData( const QModelIndex & index, const QVariant & value, int role = Qt::EditRole );
   QVariant      headerData( int section, Qt::Orientation orientation, int role = Qt::DisplayRole ) const;
   bool          setHeaderData( int section, Qt::Orientation orientation, const QVariant &value, int role );
   int           rowCount( const QModelIndex & parent = QModelIndex() ) const;
   int           columnCount( const QModelIndex & parent = QModelIndex() ) const;
   bool          canFetchMore( const QModelIndex &parent ) const;
   void          fetchMore( const QModelIndex &parent );

   void reset();
   void EmitDataChanged( const QModelIndex & topLeft, const QModelIndex & bottomRight );
   void EmitHeaderDataChanged( Qt::Orientation orientation, int first, int last);
   void EmitLayoutAboutToBeChanged();
   void EmitLayoutChanged();
   void beginInsertRows( const QModelIndex & parent, int first, int last );
   void endInsertRows();
   void beginRemoveRows( const QModelIndex & parent, int first, int last );
   void endRemoveRows();
   void beginInsertColumns( const QModelIndex & parent, int first, int last);
   void endInsertColumns();
   void beginRemoveColumns( const QModelIndex & parent, int first, int last);
   void endRemoveColumns();
   void beginResetModel();
   void endResetModel();
};

#endif
