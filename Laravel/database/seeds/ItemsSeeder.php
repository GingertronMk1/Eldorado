<?php

use Illuminate\Support\Str;
use Illuminate\Database\Seeder;
use Illuminate\Support\Facades\DB;

class ItemsSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */
    public function run()
    {
        for ($n = 0; $n <= 10; $n++) {
            DB::table('items')->insert([
                'name' => Str::random(10),
                'description' => Str::random(30),
                'unit_cost_price' => mt_rand(0, 100) / 100
            ]);
        }
    }
}
