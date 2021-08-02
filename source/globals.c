#include "globals.h"

WebKitWebPage *PAGE;

GHashTable *EXTENSIONS_DATA;

WebKitWebExtension *EXTENSION;

void
extensions_data_add_from_json_root(JsonNode *root, WebKitWebPage *web_page)
{
        JsonObject *obj = json_node_get_object(root);
        GList *members = json_object_get_members(obj);
        WebKitFrame *frame = webkit_web_page_get_main_frame(web_page);
        for (;members != NULL; members = members->next){
                ExtensionData *extension;
                const char *name = members->data,
                        *permissions = json_node_get_string(
                                json_object_get_member(obj, name));
                extension = malloc(sizeof(ExtensionData));
                extension->name = (char*) name;
                extension->permissions = (char*) permissions;
                extension->world = webkit_script_world_new_with_name(name);
                g_hash_table_insert(EXTENSIONS_DATA, (void*) name, extension);
        }
}

WebKitScriptWorld *
get_extension_world (char* extension_name)
{
        ExtensionData *data = g_hash_table_lookup(EXTENSIONS_DATA, extension_name);
        WebKitFrame *frame = webkit_web_page_get_main_frame(PAGE);
        return data->world;
}

JSCContext *
get_extension_context (char* extension_name)
{
        return webkit_frame_get_js_context_for_script_world(
                frame, get_extension_world(extension_name));
}

void *
empty_constructor_callback (void)
{
        return NULL;
}
