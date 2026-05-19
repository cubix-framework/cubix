module bidder::paginator;

// === imports ===

use std::u64::{Self};
use sui::package::{Self};
use sui::table_vec::{TableVec};

// === structs ===

public struct PAGINATOR has drop {}

// === functions ===

public fun get_page<T: store + copy>(
    items: &TableVec<T>,
    cursor: u64,
    limit: u64,
    ascending: bool,
): (vector<T>, bool, u64)
{
    if (items.length() == 0) {
        (vector::empty(), false, 0)
    } else if (ascending) {
        get_page_ascending(items, cursor, limit)
    } else {
        get_page_descending(items, cursor, limit)
    }
}

fun get_page_ascending<T: store + copy>(
    items: &TableVec<T>,
    cursor: u64,
    limit: u64,
): (vector<T>, bool, u64)
{
    let length = items.length();

    let start = cursor;
    let end = u64::min(length, cursor + limit);

    let mut data = vector<T>[];
    let mut i = start;
    while (i < end) {
        vector::push_back(&mut data, *items.borrow(i));
        i = i + 1;
    };

    let has_more = end < length;
    let next_cursor = end;

    return (data, has_more, next_cursor)
}

fun get_page_descending<T: store + copy>(
    items: &TableVec<T>,
    cursor: u64,
    limit: u64,
): (vector<T>, bool, u64)
{
    let length = items.length();

    let start =
        if (cursor >= length) { length - 1 } // cursor is out of range, start at last item
        else { cursor };

    let end =
        if (limit > start) { 0 } // end at first item
        else { start - limit + 1 };

    let mut data = vector<T>[];
    let mut i = start;
    while (i >= end) {
        vector::push_back(&mut data, *items.borrow(i));
        if (i == 0) { break }  // prevent underflow
        else { i = i - 1; }
    };

    let has_more = start > 0 && end > 0;

    let next_cursor =
        if (data.is_empty()) { start } // if no items are fetched, return start
        else if ( end == 0 ) { end } // prevent underflow
        else { end - 1 }; // otherwise, return the index just before the end index

    return (data, has_more, next_cursor)
}

// === initialization ===

#[allow(lint(share_owned))]
fun init(otw: PAGINATOR, ctx: &mut TxContext)
{
    let publisher = package::claim(otw, ctx);
    transfer::public_transfer(publisher, ctx.sender());
}
