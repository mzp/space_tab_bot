#include <jansson.h>

int
stb_json_integer_value(json_t *json) {
  return (int)json_integer_value(json);
}

json_t *
stb_json_integer(int i) {
  return json_integer((json_int_t)i);
}

int
stb_json_integer_set(json_t *json, int i) {
  return json_integer_set(json, (json_int_t)i);
}

json_type
stb_json_typeof(json_t *json) {
  return json_typeof(json);
}

json_t *
stb_json_incref(json_t *json) {
  return json_incref(json);
}

void
stb_json_decref(json_t *json) {
  json_decref(json);
}

int
stb_json_object_set(json_t *object, const char *key, json_t *value) {
  return json_object_set(object, key, value);
}

typedef void (*object_foreach_callback)(const char *, json_t *);

void
stb_json_object_foreach(json_t *obj, object_foreach_callback callback) {
  const char *k;
  json_t *v;

  json_object_foreach(obj, k, v) {
    callback(k, v);
  }
}

typedef void (*array_foreach_callback)(size_t, json_t *);
void
stb_json_array_foreachi(json_t *array, array_foreach_callback callback) {
  size_t i;
  json_t *v;

  json_array_foreach(array, i, v) {
    callback(i, v);
  }
}

int
stb_json_array_set(json_t *array, size_t ind, json_t *value) {
  return json_array_set(array, ind, value);
}

int
stb_json_array_append(json_t *array, json_t *value) {
  return json_array_append(array, value);
}

int
stb_json_array_insert(json_t *array, size_t ind, json_t *value) {
  return json_array_insert(array, ind, value);
}

json_error_t *
stb_json_error_t() {
  return (json_error_t *)malloc(sizeof(json_error_t));
}

int
stb_json_error_t_line(json_error_t *err) {
  return err->line;
}

int
stb_json_error_t_column(json_error_t *err) {
  return err->column;
}

int
stb_json_error_t_position(json_error_t *err) {
  return err->position;
}

const char *
stb_json_error_t_source(json_error_t *err) {
  return err->source;
}

const char *
stb_json_error_t_text(json_error_t *err) {
  return err->text;
}
