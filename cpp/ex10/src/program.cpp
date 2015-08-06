#include "hybridarray.hpp"

/// \brief A class which counts the number of existing instances.
/// \details This class has a copy-CTor only to force the solution to not use
///		anything else.
class Watcher
{
public:
	Watcher(const Watcher& _other) : m_me(_other.m_me)
	{
		++m_instances;
		++m_numCopyConstructed;
	}

	~Watcher()
	{
		--m_instances;
	}

	/// \brief Simulating a complex class without standard constructor.
	Watcher(int _id) : m_me(_id)
	{
		++m_instances;
	}

	static int instances() { return m_instances; }
	static int numCopyConstructed() { return m_numCopyConstructed; }
	int me() const { return m_me; }
	void me(int _id) { m_me = _id; }

private:
	static int m_instances;
	static int m_numCopyConstructed;
	int m_me;
};

int Watcher::m_instances;
int Watcher::m_numCopyConstructed;

int main()
{
	// Guaranteed by the standard
	assert(Watcher::instances() == 0);

	{
		HybridArray<Watcher, 8> a;
		assert(Watcher::instances() == 0 && "The CTor of the array should not call any constructor!");

		// Push until resize is required
		for(int i = 0; i < 10; ++i )
			a.pushBack(Watcher(i));
		assert(Watcher::instances() == 10 && "The number of still existing instances is wrong.");
		assert(Watcher::numCopyConstructed() <= 18 && "You used too many/few copies during insertion and resize.");

		// Copy the array and check its consistency later
		HybridArray<Watcher, 8> b(a);

		// Just do some more random work
		a.popBack();
		a.pushBack( Watcher(11) );
		assert(a[9].me() == 11 && "The order of elements is wrong or the data is corrupted.");
		assert(Watcher::instances() == 20 && "a and b should contain 10 instances each. Memory leak?");
		for(int i = 0; i < 9; ++i )
			assert(a[i].me() == i && "The order of elements is wrong or the data is corrupted.");
	
		// Write access
		a[0].me(12);

		// Test the pruning
		a.popBack();
		a.popBack();
		a.reserve(0);
		assert(a.capacity() == 8 && a.size() == 8);
		for(int i = 1; i < 8; ++i )
			assert(a[i].me() == i && "The order of elements is wrong or the data is corrupted.");
		assert(a[0].me() == 12 && "Probably the write access did not succeed or the array was damaged afterwards.");

		// Test if the old copy is untouched
		for(int i = 0; i < 10; ++i )
			assert(b[i].me() == i && "The order of elements is wrong or the data is corrupted.");
	}
	assert(Watcher::instances() == 0 && "Your array implementation has memory leaks.");

	{
		// Test arbitrary capacity initialization
		HybridArray<Watcher, 8> a(12);
		HybridArray<Watcher, 8> b(4);
		assert(Watcher::instances() == 0 && "There should not be any instance inside - only space for up to 12 instances.");
		assert(b.capacity() == 8 && "The local space should always be used. Use max(n, capacity).");

		for(int i = 0; i < 12; ++i )
			a.pushBack(Watcher(i));
		b = a;
		assert(Watcher::instances() == 24 && "There should be 24 different instances in the two arrays.");
	}
}