#pragma once
#include <iostream>
/// This object class serves basically as dummy for an "any object".
/// Later on we will learn different possibilities how we actually could store "anything" in here.
class Object
{
public:
	Object(const char* name);
	Object(const Object&);
  ~Object();

  /// Returns ID which was passed in the constructor.
  const char* GetName() const { return m_name; };
  /// Returns whether object is const or not
  const char* GetType() const { return "Const object"; };
  const char* GetType() { return "not const"; };
  /// Implementiert den angegebenen und die verwandten Operatoren
  bool operator==(Object& o)
  {
	  return false;
  }
  Object& operator+=(Object& obj);
  bool operator<(Object const& obj);
  bool operator>(Object const& obj);

private:

  char* m_name;

  // Here could be YOUR data!
};
/// Implementiert den angegebenen und die verwandten Operatoren
std::ostream& operator<< (std::ostream& stream, const Object&);

// implement < and > for dynamicobjectlist.sort()
