
#ifndef SYMBOLFACTORY_H
#define SYMBOLFACTORY_H

#include "IntegerSymbol.h"
#include "FloatSymbol.h"
#include "StringSymbol.h"
#include "IdentifierSymbol.h"

namespace EpmemNS {
class SymbolFactory
{
	public:
		virtual IntegerSymbol* GetIntegerSymbol( long val ) = 0;
		virtual FloatSymbol* GetFloatSymbol( double val ) = 0;
		virtual StringSymbol* GetStringSymbol( const char* val ) = 0;
		virtual IdentifierSymbol* GetIdentifierSymbol( char letter, long number ) = 0;

		virtual Symbol* GetSymbolByUID( SymbolUID uid ) = 0;

		Symbol* GetSymbol( long val );
		Symbol* GetSymbol( double val );
		Symbol* GetSymbol( const char* val );
		Symbol* GetSymbol( char letter, long number );

	protected:
		IntegerSymbol* NewIntegerSymbol( SymbolUID newUID, long newValue );
		FloatSymbol* NewFloatSymbol( SymbolUID newUID, double newValue );
		StringSymbol* NewStringSymbol( SymbolUID newUID, const char* newValue );
		IdentifierSymbol* NewIdentifierSymbol( SymbolUID newUID, char newLetter, long newNumber );

};
}

#endif
