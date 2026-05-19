module goose_bumps::duck {
    use std::string;
    use std::ascii;

    use sui::coin::{Self, Coin, TreasuryCap, CoinMetadata};
    use sui::url;

    public struct DUCK has drop {}

    public struct DuckManager has key {
        id: UID,
        cap: TreasuryCap<DUCK>,
    } 

    #[allow(lint(share_owned))]
    fun init(
        otw: DUCK, 
        ctx: &mut TxContext
    ) {
        let (cap, metadata) = coin::create_currency<DUCK>(
            otw, 
            9, 
            b"DUCK", 
            b"Duck", 
            b"BUCK with a boosted yield that gives you goose bumps",  
            option::some(url::new_unsafe_from_bytes(b"https://twitter.com/goosebumps_farm/photo")), 
            ctx
        );

        transfer::public_share_object(metadata);
        
        transfer::share_object(DuckManager {
            id: object::new(ctx),
            cap,
        });
    }

    // === Friend functions ===

    public(package) fun supply(
        manager: &DuckManager
    ): u64 {
        manager.cap.total_supply()
    }

    public(package) fun cap(
        manager: &mut DuckManager
    ): &mut TreasuryCap<DUCK> {
        &mut manager.cap
    }

    public(package) fun mint(
        treasury_cap: &mut TreasuryCap<DUCK>, 
        amount: u64, ctx: &mut TxContext
    ): Coin<DUCK> {
        treasury_cap.mint(amount, ctx)
    }

    public(package) fun burn(
        treasury_cap: &mut TreasuryCap<DUCK>, 
        coin: Coin<DUCK>
    ) {
        treasury_cap.burn(coin);
    }

    // === Admin only ===

    // TODO: add admin cap
    entry fun update_name(
        manager: &DuckManager, 
        metadata: &mut CoinMetadata<DUCK>, 
        name: string::String
    ) {
        manager.cap.update_name(metadata, name);
    }
    entry fun update_symbol(
        manager: &DuckManager, 
        metadata: &mut CoinMetadata<DUCK>, 
        name: ascii::String
    ) {
        manager.cap.update_symbol(metadata, name);
    }
    entry fun update_description(
        manager: &DuckManager, 
        metadata: &mut CoinMetadata<DUCK>, 
        name: string::String
    ) {
        manager.cap.update_description(metadata, name);
    }
    entry fun update_icon_url(
        manager: &DuckManager, 
        metadata: &mut CoinMetadata<DUCK>, 
        name: ascii::String
    ) {
        manager.cap.update_icon_url(metadata, name);
    }

    // === Test functions ===

    #[test_only]
    public fun init_for_testing(ctx: &mut TxContext) {
        init(DUCK {}, ctx);
    }
}
