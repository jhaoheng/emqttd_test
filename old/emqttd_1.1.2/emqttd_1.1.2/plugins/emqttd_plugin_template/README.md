
emqttd_plugin_template
======================

This is a template plugin for emqttd project.

Plugin Config
-------------

Each plugin should have a 'etc/plugin.config' file to store application config.

Authentication and ACL
----------------------

```
emqttd_access_control:register_mod(auth, ?MODULE, Env).
emqttd_access_control:register_mod(acl, ?MODULE, Env).
```

Plugin and Hooks
-----------------

[Plugin Design](http://docs.emqtt.com/en/latest/design.html#plugin-design)

[Hooks Design](http://docs.emqtt.com/en/latest/design.html#hooks-design)

