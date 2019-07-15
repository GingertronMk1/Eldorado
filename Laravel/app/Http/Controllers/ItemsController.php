<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use \App\Item as Item;

class ItemsController extends Controller
{
    private $validRules = [
        'name' => 'required',
        'description' => ['required', 'min:4'],
        'unit_cost_price' => ['required', 'gte:0']
    ];

    public function index()
    {
        $inventory = Item::all();

        return view('items/index', compact('inventory'));
    }

    public function create()
    {
        return view('items/create');
    }

    public function store()
    {
        $validated = request()->validate($this->validRules);

        Item::create($validated);

        return redirect('/inventory');
    }

    public function show(Item $inventory)
    {
        return view('items/show', compact('inventory'));
    }

    public function edit(Item $inventory)
    {
        return view('items/edit', compact('inventory'));
    }

    public function update(Item $inventory)
    {
        $validated = request()->validate($this->validRules);

        $inventory->update($validated);

        return redirect('/inventory');
    }

    public function destroy(Item $inventory)
    {
        $inventory->delete();
        return redirect('/inventory');
    }
}
