#ifndef SRC_SCOPE_HPP_
#define SRC_SCOPE_HPP_

//          Copyright Eric Niebler and Peter Sommerlad 2016 - 2019.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)


#include <utility>
#include <functional>
#include <limits> // for maxint
#include <type_traits>

#define SCOPE_NS std { namespace experimental
#define SCOPE_NS_END }

namespace SCOPE_NS{
namespace _v {
template<typename T>
constexpr auto is_nothrow_move_assignable_v=std::is_nothrow_move_assignable<T>::value;
template<typename T>
constexpr auto is_nothrow_move_constructible_v=std::is_nothrow_move_constructible<T>::value;
template<typename T, typename S>
constexpr auto is_nothrow_constructible_v=std::is_nothrow_constructible<T, S>::value;

template<typename T>
constexpr auto is_copy_constructible_v=std::is_copy_constructible<T>::value;
template<typename T>
constexpr auto is_copy_assignable_v=std::is_copy_assignable<T>::value;
template<typename T>
constexpr auto is_move_constructible_v=is_move_constructible<T>::value;
template<typename T>
constexpr auto is_pointer_v=is_pointer<T>::value;
template<typename T>
constexpr auto is_void_v=is_void<T>::value;

// hack is_incallable that is close enough to is_invocable to be useful
template <typename F, typename... Args>
struct is_invocable :
    std::is_constructible<
        std::function<void(Args ...)>,
        std::reference_wrapper<typename std::remove_reference<F>::type>
    >
{
};

template <typename R, typename F, typename... Args>
struct is_invocable_r :
    std::is_constructible<
        std::function<R(Args ...)>,
        std::reference_wrapper<typename std::remove_reference<F>::type>
    >
{
};
template<typename T>
struct is_callable_impl
{
    typedef char yes_type[1];
    typedef char no_type[2];

    template <typename Q>
    static yes_type& check(decltype(declval<Q>()())*);

    template <typename Q>
    static no_type& check(...);

    static const bool value = sizeof(check<T>(0))==sizeof(yes_type);
};

template<typename R, typename... Args>
struct is_callable_impl<R (*)(Args...)> : std::true_type {};

template<typename R, typename... Args>
struct is_callable_impl<R (&)(Args...)> : std::true_type {};
template<typename T>
using is_callable = std::integral_constant<bool, is_callable_impl<T>::value>;


template<typename T>
constexpr auto is_invocable_v=is_invocable<T>::value;

template<typename T, typename... S>
constexpr auto is_constructible_v=is_constructible<T,S...>::value;
template<typename T, typename S>
constexpr auto is_convertible_v=std::is_convertible<T,S>::value;


}
namespace detail {
namespace hidden{

// TODO: is_nothrow_assignable reimplementation

// should this helper be standardized? // write testcase where recognizable.
template<typename T>
constexpr std::conditional_t<
    _v::is_nothrow_move_assignable_v<T>,
    T &&,
    T const &>
_move_assign_if_noexcept(T &x) noexcept
{
    return std::move(x);
}
template< class T >
constexpr typename std::conditional<
    !_v::is_nothrow_move_constructible_v<T> && _v::is_copy_constructible_v<T>,
    const T&,
    T&&
>::type move_if_noexcept(T& x) noexcept;

}
template<typename T>
class _box
{
    T value;
    _box(T const &t) noexcept(noexcept(T(t)))
      : value(t)
    {}
    _box(T &&t) noexcept(noexcept(T(std::move_if_noexcept(t))))
      : value(std::move_if_noexcept(t))
    {}

public:
    template<typename TT, typename GG,
        typename = std::enable_if_t<_v::is_constructible_v<T, TT>>>
    explicit _box(TT &&t, GG &&guard) noexcept(noexcept(_box((T &&) t)))
      : _box(std::forward<TT>(t))
    {
        guard.release();
    }
    T &get() noexcept
    {
        return value;
    }
    T const &get() const noexcept
    {
        return value;
    }
    T &&move() noexcept
    {
        return std::move(value);
    }
    void reset(T const &t) noexcept(noexcept(value = t))
    {
        value = t;
    }
    void reset(T &&t) noexcept(noexcept(value = hidden::_move_assign_if_noexcept(t)))
    {
        value = hidden::_move_assign_if_noexcept(t);
    }

};

template<typename T>
class _box<T &>
{
    std::reference_wrapper<T> value;
public:
    template<typename TT, typename GG,
        typename = std::enable_if_t<_v::is_convertible_v<TT, T &>>>
    _box(TT &&t, GG &&guard) noexcept(noexcept(static_cast<T &>((TT &&) t)))
      : value(static_cast<T &>(t))
    {
        guard.release();
    }
    T &get() const noexcept
    {
        return value.get();
    }
    T &move() const noexcept
    {
        return get();
    }
    void reset(T &t) noexcept
    {
        value = std::ref(t);
    }
};





// new policy-based exception proof design by Eric Niebler

struct on_exit_policy
{
    bool execute_{ true };

    void release() noexcept
    {
        execute_ = false;
    }

    bool should_execute() const noexcept
    {
        return execute_;
    }
};

// we cheat around by using 0 1 value of uncaught_exception() instead of the 17 uncaught_exceptions
struct on_fail_policy
{
    int ec_ { std::uncaught_exception() }; // just an approximization

    void release() noexcept
    {
        ec_ = std::numeric_limits<int>::max();
    }

    bool should_execute() const noexcept
    {
        return ec_ < std::uncaught_exception();
    }
};

struct on_success_policy
{
    int ec_ { std::uncaught_exception() };

    void release() noexcept
    {
        ec_ = -1;
    }

    bool should_execute() const noexcept
    {
        return ec_ >= std::uncaught_exception() ;
    }
};
}
template<class EF, class Policy = detail::on_exit_policy>
class basic_scope_exit; // silence brain dead clang warning -Wmismatched-tags

//PS: It would be nice if just the following would work in C++17
//PS: however, we need a real class for template argument deduction
//PS: and a deduction guide, because the ctors are partially instantiated
//template<class EF>
//using scope_exit = basic_scope_exit<EF, detail::on_exit_policy>;

template<class EF>
struct [[nodiscard]] scope_exit : basic_scope_exit<EF, detail::on_exit_policy>{
	using basic_scope_exit<EF, detail::on_exit_policy>::basic_scope_exit;
};

template <class EF>
//scope_exit(EF) -> scope_exit<EF>;
inline auto make_scope_exit(EF&&ef){
	return scope_exit<std::decay_t<EF>>(std::forward<EF>(ef));
}

//template<class EF>
//using scope_fail = basic_scope_exit<EF, detail::on_fail_policy>;

template<class EF>
struct scope_fail : basic_scope_exit<EF, detail::on_fail_policy>{
	using basic_scope_exit<EF, detail::on_fail_policy>::basic_scope_exit;
};

template <class EF>
//scope_fail(EF) -> scope_fail<EF>;
inline auto make_scope_fail(EF&&ef){
	return scope_fail<std::decay_t<EF>>(std::forward<EF>(ef));
}

//template<class EF>
//using scope_success = basic_scope_exit<EF, detail::on_success_policy>;

template<class EF>
struct scope_success : basic_scope_exit<EF, detail::on_success_policy>{
	using basic_scope_exit<EF,detail::on_success_policy>::basic_scope_exit;
};

template <class EF>
//scope_success(EF) -> scope_success<EF>;
inline auto make_scope_success(EF&&ef){
	return scope_success<std::decay_t<EF>>(std::forward<EF>(ef));
}


namespace detail{
// DETAIL:
template<class Policy, class EF>
auto _make_guard(EF &&ef)
{
    return basic_scope_exit<std::decay_t<EF>, Policy>(std::forward<EF>(ef));
}
struct _empty_scope_exit
{
    void release() const noexcept
    {}
};

}

// Requires: EF is Callable
// Requires: EF is nothrow MoveConstructible OR CopyConstructible
template<class EF, class Policy /*= on_exit_policy*/>
class [[nodiscard]] basic_scope_exit :  Policy
{
	static_assert(_v::is_invocable_v<EF>,"scope guard must be callable");
    detail::_box<EF> exit_function;

	static auto _make_failsafe(std::true_type, const void *)
    {
        return detail::_empty_scope_exit{};
    }
    template<typename Fn>
    static auto _make_failsafe(std::false_type, Fn *fn)
    {
        return basic_scope_exit<Fn &, Policy>(*fn);
    }
    template<typename EFP>
    using _ctor_from = std::is_constructible<detail::_box<EF>, EFP, detail::_empty_scope_exit>;
    template<typename EFP>
#ifndef _MSC_VER
    using _noexcept_ctor_from = std::integral_constant<bool,noexcept(detail::_box<EF>(std::declval<EFP>(), detail::_empty_scope_exit{}))>;
#else
    // MSVC thinks that it is a function style cast
    using _noexcept_ctor_from = std::integral_constant<bool,noexcept(detail::_box<EF>::_box(std::declval<EFP>(), detail::_empty_scope_exit{}))>;
#endif
public:
    template<typename EFP, typename = std::enable_if_t<_ctor_from<EFP>::value>>
    explicit basic_scope_exit(EFP &&ef) noexcept(_noexcept_ctor_from<EFP>::value)
      : exit_function(std::forward<EFP>( ef), _make_failsafe(_noexcept_ctor_from<EFP>{}, &ef))
    {}
    basic_scope_exit(basic_scope_exit &&that) noexcept(noexcept(detail::_box<EF>(that.exit_function.move(), that)))
      : Policy(that), exit_function(that.exit_function.move(), that)
    {}
    ~basic_scope_exit() noexcept(noexcept(exit_function.get()()))
    {
        if(this->should_execute())
            exit_function.get()();
    }
	basic_scope_exit(const basic_scope_exit &) = delete;
	basic_scope_exit &operator=(const basic_scope_exit &) = delete;
    basic_scope_exit &operator=(basic_scope_exit &&) = delete;

    using Policy::release;

};

template<class EF, class Policy>
void swap(basic_scope_exit<EF, Policy> &, basic_scope_exit<EF, Policy> &) = delete;



template<typename R, typename D>
class unique_resource
{
    static_assert((_v::is_move_constructible_v<R> && _v::is_nothrow_move_constructible_v<R>) ||
                  _v::is_copy_constructible_v<R>,
				  "resource must be nothrow_move_constructible or copy_constructible");
    static_assert((_v::is_move_constructible_v<R> && _v::is_nothrow_move_constructible_v<D>) ||
                  _v::is_copy_constructible_v<D>,
				  "deleter must be nothrow_move_constructible or copy_constructible");

	static const unique_resource &this_; // never ODR used! Just for getting no_except() expr

    detail::_box<R> resource;
    detail::_box<D> deleter;
    bool execute_on_destruction { true };

    static constexpr auto is_nothrow_delete_v=std::integral_constant<bool, noexcept(std::declval<D &>()(std::declval<R &>()))>::value;

public://should be private
    template<typename RR, typename DD,
        typename = std::enable_if_t<_v::is_constructible_v<detail::_box<R>, RR, detail::_empty_scope_exit> &&
                                    _v::is_constructible_v<detail::_box<D>, DD, detail::_empty_scope_exit>>>
    unique_resource(RR &&r, DD &&d, bool should_run)
	noexcept(noexcept(detail::_box<R>(std::forward<RR>(r), detail::_empty_scope_exit {})) &&
			noexcept(detail::_box<D>(std::forward<DD>(d), detail::_empty_scope_exit {})))
      : resource{std::forward<RR>(r), make_scope_exit([&] {if (should_run) d(r);})}
      , deleter{std::forward<DD>(d),  make_scope_exit([&, this] {if (should_run) d(get());})}
	  , execute_on_destruction { should_run }
    {}
    // need help in making the factory a nice friend...
    // the following two ICEs my g++ and gives compile errors about mismatche exception spec on clang7
//    template<typename RM, typename DM, typename S>
//    friend
//    auto make_unique_resource_checked(RM &&r, const S &invalid, DM &&d)
////    noexcept(noexcept(make_unique_resource(std::forward<RM>(r), std::forward<DM>(d))))
//    ;
    // the following as well: complains that its exception specification doesn't match its own exception specification in clang
public:
//    template<typename MR, typename MD, typename S>
////	[[nodiscard]]
//    friend
//	auto make_unique_resource_checked(MR &&r, const S &invalid, MD &&d)
//    noexcept(_v::is_nothrow_constructible_v<std::decay_t<MR>,MR> &&
//    		_v::is_nothrow_constructible_v<std::decay_t<MD>,MD>)
//    ->unique_resource<std::decay_t<MR>,std::decay_t<MD>>
//			{
//				bool const mustrelease(r == invalid);
//				unique_resource resource{std::forward<MR>(r), std::forward<MD>(d),!mustrelease};
//				return resource;
//
//			}
//
//;
public:
    template<typename RR, typename DD,
        typename = std::enable_if_t<_v::is_constructible_v<detail::_box<R>, RR, detail::_empty_scope_exit> &&
                                    _v::is_constructible_v<detail::_box<D>, DD, detail::_empty_scope_exit> >
    >
    unique_resource(RR &&r, DD &&d)
	noexcept(noexcept(detail::_box<R>(std::forward<RR>(r), detail::_empty_scope_exit {})) &&
			 noexcept(detail::_box<D>(std::forward<DD>(d), detail::_empty_scope_exit {})))
      : resource(std::forward<RR>(r), make_scope_exit([&] {d(r);}))
      , deleter(std::forward<DD>(d), make_scope_exit([&, this] {d(get());}))
    {}
	unique_resource(	unique_resource&& that)
			noexcept(noexcept(detail::_box<R>(that.resource.move(), detail::_empty_scope_exit {})) &&
					 noexcept(detail::_box<D>(that.deleter.move(), detail::_empty_scope_exit {})))
		: resource(that.resource.move(), detail::_empty_scope_exit { })
		, deleter(that.deleter.move(), make_scope_exit([&, this] { if (that.execute_on_destruction) that.get_deleter()(get());that.release(); }))
		, execute_on_destruction(std::exchange(that.execute_on_destruction, false))
		{ }

	unique_resource &operator=(unique_resource &&that) noexcept(is_nothrow_delete_v &&
			_v::is_nothrow_move_assignable_v<R> &&
			_v::is_nothrow_move_assignable_v<D>)
	{
		static_assert(_v::is_nothrow_move_assignable_v<R> ||
				_v::is_copy_assignable_v<R>,
				"The resource must be nothrow-move assignable, or copy assignable");
		static_assert(_v::is_nothrow_move_assignable_v<D> ||
				_v::is_copy_assignable_v<D>,
				"The deleter must be nothrow-move assignable, or copy assignable");
		if (&that != this) {
			reset();
			if constexpr (_v::is_nothrow_move_assignable_v<detail::_box<R>>)
				if constexpr (_v::is_nothrow_move_assignable_v<detail::_box<D>>) {
					resource = std::move(that.resource);
					deleter = std::move(that.deleter);
				} else {
					deleter = _as_const(that.deleter);
					resource = std::move(that.resource);
				}
			else if constexpr (_v::is_nothrow_move_assignable_v<detail::_box<D>>) {
				resource = _as_const(that.resource);
				deleter = std::move(that.deleter);
			} else {
				resource = _as_const(that.resource);
				deleter = _as_const(that.deleter);
			}
			execute_on_destruction = std::exchange(that.execute_on_destruction, false);
		}
		return *this;
	}
    ~unique_resource() //noexcept(is_nowthrow_delete_v) // removed deleter must not throw
    {
        reset();
    }

    void reset() noexcept
    {
        if(execute_on_destruction)
        {
            execute_on_destruction = false;
            get_deleter()(get());
        }
    }
    template<typename RR>
    auto reset(RR &&r)
        noexcept(noexcept(resource.reset(std::forward<RR>(r))))
	-> decltype(resource.reset(std::forward<RR>(r)), void())
    {
        auto &&guard = make_scope_fail([&, this]{ get_deleter()(r); }); // -Wunused-variable on clang
        reset();
		resource.reset(std::forward<RR>(r));
        execute_on_destruction = true;
    }
    void release() noexcept
    {
        execute_on_destruction = false;
    }
    decltype(auto) get() const noexcept
    {
        return resource.get();
    }
    decltype(auto) get_deleter() const noexcept
    {
        return deleter.get();
    }
    template<typename RR=R>
    auto operator->() const noexcept//(noexcept(detail::for_noexcept_on_copy_construction(this_.get())))
	-> std::enable_if_t<_v::is_pointer_v<RR>,decltype(get())>
    {
        return get();
    }
    template<typename RR=R>
    auto operator*() const noexcept
    	-> std::enable_if_t<_v::is_pointer_v<RR> && ! _v::is_void_v<std::remove_pointer_t<RR>>,
		std::add_lvalue_reference_t<std::remove_pointer_t<R>>>
    {
        return *get();
    }

	unique_resource& operator=(const unique_resource &) = delete;
	unique_resource(const unique_resource &) = delete;

};

//template<typename R, typename D>
//unique_resource(R, D)
//-> unique_resource<R, D>;

template<typename MR, typename MD>
[[nodiscard]]
auto make_unique_resource(MR&& r, MD&&d)
noexcept(_v::is_nothrow_constructible_v<std::decay_t<MR>,MR> &&
		_v::is_nothrow_constructible_v<std::decay_t<MD>,MD>)
->unique_resource<std::decay_t<MR>,std::decay_t<MD>>
{
	return unique_resource<std::decay_t<MR>,std::decay_t<MD>>
	{std::forward<MR>(r), std::forward<MD>(d)};

}
/*
 template<typename R, typename D>
unique_resource(R, D, bool)
-> unique_resource<R, D>;
*/
template<typename MR, typename MD, typename S>
[[nodiscard]]
auto make_unique_resource_checked(MR &&r, const S &invalid, MD &&d)
noexcept(_v::is_nothrow_constructible_v<std::decay_t<MR>,MR> &&
		_v::is_nothrow_constructible_v<std::decay_t<MD>,MD>)
->unique_resource<std::decay_t<MR>,std::decay_t<MD>>
{
	bool const mustrelease(r == invalid);
	unique_resource<std::decay_t<MR>,std::decay_t<MD>>
	  resource{std::forward<MR>(r), std::forward<MD>(d),!mustrelease};
	return resource;

}

SCOPE_NS_END
}


#endif /* SRC_SCOPE_HPP_ */
