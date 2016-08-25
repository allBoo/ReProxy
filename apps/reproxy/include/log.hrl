%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

-define(DBG(F, A), lager:debug(F, A)).
-define(DBG(F), lager:debug(F)).
-define(LOG(F, A), lager:info(F, A)).
-define(LOG(F), lager:info(F)).
-define(WARN(F, A), lager:warning(F, A)).
-define(WARN(F), lager:warning(F)).
-define(ERR(F, A), lager:error(F, A)).
-define(ERR(F), lager:error(F)).
