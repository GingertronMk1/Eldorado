<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use \App\Item as Item;

class ItemsController extends Controller
{
    protected function validateItem() {
        return [
            'name' => 'required|min:1',
            'unit_cost_price' => 'required|gte:0',
        ];
    }

    public function index() {
        $inventorys = Item::all();

        return view('items/index', ['items' => $inventorys]);
    }

    public function create() {
        return view('items/create');
    }

    public function store() {
        request()->validate([
            'name' => 'required',
            'unit_cost_price' => ['required', 'gte:0']
        ]);
        Item::create(request(['name', 'description', 'unit_cost_price']));

        return redirect('/inventory');
    }

    public function show(Item $inventory){
        return view('items/show', compact('inventory'));
    }

    public function edit(Item $inventory){
        return view('items/edit', compact('inventory'));
    }

    public function update(Item $inventory){

        request()->validate([
            'name' => 'required',
            'unit_cost_price' => ['required', 'gte:0']
        ]);
        $inventory->update(request(['name', 'description', 'unit_cost_price']));

        return redirect('/inventory');

    }

    public function destroy(Item $inventory){
        $inventory->delete();
        return redirect('/inventory');
    }
}
