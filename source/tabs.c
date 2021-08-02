#include "globals.h"
#include "tabs.h"

Tabs *TABS;

static void
tabs_query_reply_callback (GObject *web_page,
                           GAsyncResult *res,
                           gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "[]";
        TABS->tabs = (char *) json;
}

static JSCValue *
tabs_query_result_callback (char *extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        return jsc_value_new_from_json(context, TABS->tabs);
}

static void
tabs_query_callback (JSCValue *object)
{
        char *json = jsc_value_to_json(object, 0);
        GVariant *variant = g_variant_new("s", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.queryObject", variant);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_query_reply_callback, NULL);
}

static void
tabs_create_reply_callback (GObject *web_page,
                           GAsyncResult *res,
                           gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "{}";
        TABS->tab = (char *) json;
}

static JSCValue *
tabs_create_result_callback (char *extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        return jsc_value_new_from_json(context, TABS->tab);
}

static void
tabs_create_callback (JSCValue *object)
{
        char *json = jsc_value_to_json(object, 0);
        GVariant *variant = g_variant_new("s", json);
        WebKitUserMessage *message = webkit_user_message_new("tabs.createProperties", variant);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_create_reply_callback, NULL);
}

static void
tabs_get_current_reply_callback (GObject *web_page,
                                 GAsyncResult *res,
                                 gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "{}";
        TABS->tab = (char *) json;
}

static JSCValue *
tabs_get_current_result_callback (char *extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        return jsc_value_new_from_json(context, TABS->tab);
}

static void
tabs_get_current_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("tabs.getCurrent", NULL);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_get_current_reply_callback, NULL);
}

static void
tabs_get_reply_callback (GObject *web_page,
                         GAsyncResult *res,
                         gpointer user_data)
{
        WebKitUserMessage *message =
                webkit_web_page_send_message_to_view_finish((WebKitWebPage *) PAGE, res, NULL);
        GVariant *params = webkit_user_message_get_parameters(message);
        char *json = (char*) g_variant_get_string(params, NULL);
        if (!json)
                json = "null";
        TABS->tab = (char *) json;
}

static JSCValue *
tabs_get_result_callback (char *extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        return jsc_value_new_from_json(context, TABS->tab);
}

static void
tabs_get_callback (int id)
{
        char *num = malloc(sizeof(char) * 1000);
        sprintf(num, "%d", id);
        GVariant *variant = g_variant_new("s", num);
        WebKitUserMessage *message = webkit_user_message_new("tabs.get", variant);
        webkit_web_page_send_message_to_view(
                PAGE, message, NULL, tabs_get_reply_callback, NULL);
}

static void
tabs_print_callback ()
{
        WebKitUserMessage *message = webkit_user_message_new("tabs.print", NULL);
        webkit_web_page_send_message_to_view(PAGE, message, NULL, NULL, NULL);
}

void
inject_tabs_api (char* extension_name)
{
        JSCContext *context = get_extension_context(extension_name);
        JSCValue *tabsQuery = jsc_value_new_function(
                context, "tabsQuery",
                G_CALLBACK(tabs_query_callback), NULL, NULL,
                G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        JSCValue *tabsQueryResult = jsc_value_new_function(
                context, "tabsQueryResult",
                G_CALLBACK(tabs_query_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 1, G_TYPE_STRING);
        JSCValue *tabsCreate = jsc_value_new_function(
                context, "tabsCreate",
                G_CALLBACK(tabs_create_callback), NULL, NULL,
                G_TYPE_NONE, 1, JSC_TYPE_VALUE);
        JSCValue *tabsCreateResult = jsc_value_new_function(
                context, "tabsCreateResult",
                G_CALLBACK(tabs_create_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 1, G_TYPE_STRING);
        JSCValue *tabsGetCurrent = jsc_value_new_function(
                context, "tabsGetCurrent",
                G_CALLBACK(tabs_get_current_callback), NULL, NULL,
                G_TYPE_NONE, 0, G_TYPE_NONE);
        JSCValue *tabsGetCurrentResult = jsc_value_new_function(
                context, "tabsGetCurrentResult",
                G_CALLBACK(tabs_get_current_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 1, G_TYPE_STRING);
        JSCValue *tabsGet = jsc_value_new_function(
                context, "tabsGet",
                G_CALLBACK(tabs_get_callback), NULL, NULL,
                G_TYPE_NONE, 1, G_TYPE_INT);
        JSCValue *tabsGetResult = jsc_value_new_function(
                context, "tabsGetResult",
                G_CALLBACK(tabs_get_result_callback), NULL, NULL,
                JSC_TYPE_VALUE, 1, G_TYPE_STRING);
        JSCValue *print = jsc_value_new_function(
                context, NULL, G_CALLBACK(tabs_print_callback), NULL, NULL,
                G_TYPE_NONE, 0, G_TYPE_NONE);
        JSCClass *Tabs = jsc_context_register_class(context, "Tabs", NULL, NULL, NULL);
        JSCValue *Tabs_constructor = jsc_class_add_constructor(
                Tabs, NULL, G_CALLBACK(empty_constructor_callback),
                NULL, NULL, G_TYPE_NONE, 0, G_TYPE_NONE);
        jsc_context_set_value(context, "Tabs", Tabs_constructor);
        jsc_context_set_value(context, "tabsQuery", tabsQuery);
        jsc_context_set_value(context, "tabsQueryResult", tabsQueryResult);
        jsc_context_set_value(context, "tabsCreate", tabsCreate);
        jsc_context_set_value(context, "tabsCreateResult", tabsCreateResult);
        jsc_context_set_value(context, "tabsGetCurrent", tabsGetCurrent);
        jsc_context_set_value(context, "tabsGetCurrentResult", tabsGetCurrentResult);
        jsc_context_set_value(context, "tabsGet", tabsGet);
        jsc_context_set_value(context, "tabsGetResult", tabsGetResult);
        jsc_context_set_value(context, "tabs", jsc_value_new_object(context, NULL, Tabs));
        char *tabs_query_js = "tabs.query = function (queryObject) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsQuery(queryObject);                                     \
            management.getSelf().then(function (info) {                 \
                setTimeout(() =>                                        \
                    success(tabsQueryResult(info.name)), 0);});        \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.query",
                *tabs_create_js = "tabs.create = function (createProperties) { \
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsCreate(createProperties);                               \
            management.getSelf().then(function (info) {                 \
                setTimeout(() =>                                        \
                    success(tabsCreateResult(info.name)), 0);});       \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.create",
                *tabs_get_current_js = "tabs.getCurrent = function () {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsGetCurrent();                                           \
            management.getSelf().then(function (info) {                 \
                setTimeout(() =>                                        \
                    success(tabsGetCurrentResult(info.name)), 0);});    \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.getCurrent",
                *tabs_get_js = "tabs.get = function (getProperties) {\
    return new Promise(function (success, failure) {                    \
        try {                                                           \
            tabsGet(getProperties);                                     \
            management.getSelf().then(function (info) {\
                setTimeout(() => {                                      \
                    var result = tabsGetResult(info.name);              \
                    if (result)                                         \
                        success(result);                                \
                    else                                                \
                        throw new Error(\"No tab found!\");},           \
                           0);}                                        \
            );                                                          \
        } catch (error) {                                               \
            return failure(error);                                      \
        };                                                              \
    });                                                                 \
};                                                                      \
                                                                        \
tabs.get";
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "tabs", -1),
                "query",
                jsc_context_evaluate(context, tabs_query_js, -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "tabs", -1),
                "create",
                jsc_context_evaluate(context, tabs_create_js, -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "tabs", -1),
                "getCurrent",
                jsc_context_evaluate(context, tabs_get_current_js, -1));
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "tabs", -1),
                "get",
                jsc_context_evaluate(context, tabs_get_js, -1));
        jsc_value_object_set_property(jsc_context_evaluate(context, "tabs", -1),
                                      "print", print);
        jsc_value_object_set_property(
                jsc_context_evaluate(context, "browser", -1), "tabs",
                jsc_context_evaluate(context, "tabs", -1));
}
