<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use \App\Item as Item;

class ItemsController extends Controller
{
    public function index() {
        $inventorys = Item::all();

        return view('items/index', ['items' => $inventorys]);
    }

    public function create() {
        return view('items/create');
    }

    public function store() {
        $validated = request()->validate([
            'name' => 'required',
            'unit_cost_price' => ['required', 'gte:0']
        ]);

        Item::create($validated);

        return redirect('/inventory');
    }

    public function show(Item $inventory){
        return view('items/show', compact('inventory'));
    }

    public function edit(Item $inventory){
        return view('items/edit', compact('inventory'));
    }

    public function update(Item $inventory){

        $validated = request()->validate([
            'name' => 'required',
            'unit_cost_price' => ['required', 'gte:0']
        ]);

        $inventory->update($validated);

        return redirect('/inventory');

    }

    public function destroy(Item $inventory){
        $inventory->delete();
        return redirect('/inventory');
    }
}
