<?php

/*
|--------------------------------------------------------------------------
| Web Routes
|--------------------------------------------------------------------------
|
| Here is where you can register web routes for your application. These
| routes are loaded by the RouteServiceProvider within a group which
| contains the "web" middleware group. Now create something great!
|
*/

/*
    GET     /inventory          index
    GET     /inventory/create   create
    POST    /inventory          store
    GET     /inventory/id       show
    GET     /inventory/id/edit  edit
    PATCH   /inventory/id       update
    DELETE  /inventory/id       destroy
*/

Route::resource('/inventory', 'ItemsController');
