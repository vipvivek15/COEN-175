#include "Label.h"
#include <iostream>

using namespace std;

unsigned Label::_counter = 0;

Label::Label()
{
	_number = _counter++;
}

unsigned Label::number() const
{
	return _number;
}

ostream& operator<<(ostream& ostr, const Label& label)
{
	ostr << ".L" << label.number();
	return ostr;
}

