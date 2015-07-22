#pragma once

#include <cassert>
#include <cstdlib>
#include <memory>

/// \brief A dynamic array which uses local space as long as it isn't too large.
/// \details The array itself contains space for n elements. As long as the
///		number of elements is smaller than this local capacity there is no
///		additional memory block somewhere else. Data and meta information are
///		both part of the same block. This increases speed as long as we are
///		dealing with small arrays (<n). E.g. strings
///
///		In case there is more data the array behaves as std::vector.
///
///		As usual copying arrays is expensive.
template<typename T, unsigned int n = 8>
class HybridArray
{
public:
	/// \brief Standard: create this array with capacity n and size 0.
	/// \details m_data is set to the local storage.
	/// TODO: Write standard CTor which sets m_data, m_capacity and m_size
	HybridArray();

	/// \brief Create a dynamic array with preallocated space. Probably
	///		this disables the advantage of 'hybrid':
	///	\param [in] _capacity Minimum capacity which should be allocated.
	///		The allocated capacity must be �max(_capacity, n)�. So there is
	///		never less than the already existing internal memory block.
	/// TODO: Write custom CTor which sets m_data, m_capacity and m_size.
	///		You might need to allocate memory with malloc!
	HybridArray(unsigned _capacity);

	/// \brief Copy construction (deep).
	/// TODO: Write copy constructor. Use placement new with copy CTor of ElemType
	///		to create copies of the data (not =).
	///		Reason: Any implementation of = must free the old stuff on the left
	///		side before assigning the new one. But during construction there is
	///		nothing to be deleted (left side contains uninitialized memory)!
	HybridArray(const HybridArray<T,n>& _other);

	/// \brief Calls destructor for all contained elements.
	/// TODO: Write destructor.
	~HybridArray();

	/// \brief Deep copying assignment
	/// TODO: Write assignment operator for this array type. Keep in mind: delete
	///		the old content first (otherwise memory leaks)
	HybridArray<T,n>& operator = (const HybridArray<T,n>& _other);

	/// \brief Write-array access.
	/// TODO: Implement [] operator with write access (non const)

	/// \brief Read-array access.
	/// TODO: Implement [] operator with read access (const)

	/// \brief Enlarge or prune the memory.
	/// \details This function is already implemented. You can have a look
	///		to understand the array and to see how std::swap can be used.
	/// \param [in] _capacity New capacity/size. If _capacity is below n
	///		the new capacity is still n.
	///		
	///		A value smaller than size() causes a prune: capacity = size();
	void reserve(unsigned _capacity = 0);

	/// \brief Insert an element copy at the end of the array.
	/// \details This might cause a resize with costs O(n).
	/// TODO: Implement (use placement new to copy the element to the array,
	///	for a reason see Copy-CTor above).
	const T& pushBack(const T& _element);

	/// \brief Delete the last element
	/// TODO: Implement
	void popBack();

	unsigned size() const		{ return m_size; }
	unsigned capacity() const	{ return m_capacity; }

	/// \brief Access first element
	T& front()				{ assert(m_size>0); return *m_data; }
	const T& front() const	{ assert(m_size>0); return *m_data; }

	/// \brief Access last element
	T& back()				{ assert(m_size>0); return m_data[m_size-1]; }
	const T& back() const	{ assert(m_size>0); return m_data[m_size-1]; }
protected:
	T* m_data;		///< Pointer to array memory block. Might be on stack or heap.
	unsigned m_capacity;	///< Maximum number of elements
	unsigned m_size;		///< Current number of elements

	unsigned char m_localStorage[sizeof(T)*n];	///< The local storage on stack or in object heap space.
};






// ********************************************************************* //
//  HybridArray Implementation											 //
// ********************************************************************* //


// ********************************************************************* //
template<typename T, unsigned n>
void HybridArray<T,n>::reserve(unsigned _capacity)
{
	// Resizing without change. Bad Performance!
	assert(m_capacity != _capacity);

	// Prune as much as possible - do not delete data
	if( _capacity <= m_size ) _capacity = m_size;

	unsigned oldCapacity = m_capacity;
	m_capacity = _capacity < n ? n : _capacity;
	// If both old and new capacity are <= n nothing happens otherwise
	// a realloc or copy is necessary
	if( m_capacity > n || oldCapacity > n )
	{
		T* oldData = m_data;

		// Determine target memory
		if( m_capacity <= n )
			m_data = reinterpret_cast<T*>(m_localStorage);
		else
			m_data = (T*)malloc( m_capacity * sizeof(T) );

		// Now keep the old data
		for( unsigned i=0; i<m_size; ++i )
			std::swap(m_data[i], oldData[i]);

		if( oldCapacity > n )
			free(oldData);
	}
}

template<typename T, unsigned int n>
T const &HybridArray::pushBack(T const& _element) {
	if (m_size == m_capacity)
		reserve(m_capacity * 2);
	return *(new(m_data + m_size++) T(_element));
}

template<typename T, unsigned int n>
void HybridArray<T, n>::popBack()
{
	assert(m_size > 0);
	m_data[m_size--].~T();
}

template<typename T, unsigned int n>
HybridArray<T, n> &HybridArray::operator=(const HybridArray<T, n> &_other) {
	return <#initializer#>;
}

template<typename T, unsigned int n>
HybridArray<T,n>::HybridArray()
		: m_size(0),
		  m_capacity(n),
		  m_data(reinterpret_cast<T*>(m_localStorage))
{

}

template<typename T, unsigned int n>
HybridArray<T,n>::HybridArray(unsigned int _capacity)
		: m_size(0),
		  m_capacity(_capacity < n ? n : _capacity)
{
	if (m_capacity > n)
		// Too large for local memory
		m_data = (T*)malloc(m_capacity * sizeof(T));
	else
		m_data = reinterpret_cast<T*>(m_localStorage);
}

template<typename T, unsigned int n>
HybridArray<T,n>::HybridArray(const HybridArray<T, n> &_other)
		: m_size(_other.m_size),
		  m_capacity(_other.m_size < n ? n : _other.m_size)
{
	if (m_capacity > n)
		// Too large for local memory
		m_data = (T*)malloc(m_capacity * sizeof(T));
	else
		m_data = reinterpret_cast<T*>(m_localStorage);

	for (unsigned int i = 0; i < m_size; ++i)
		new (m_data + i) T(_other.m_data[i]);
}

template<typename T, unsigned int n>
HybridArray<T,n>::~HybridArray()
{
	for (unsigned int i = 0; i < m_size; ++i)
		m_data[i].~T();
	if (m_capacity > n)
		free(m_data);

	free(m_data);
}
