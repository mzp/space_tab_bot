#include <stdio.h>
#include <uriparser/Uri.h>

typedef struct {
	int scheme_first;
	int scheme_length;
	int userInfo_first;
	int userInfo_length;
	int host_first;
	int host_length;
	int port_first;
	int port_length;
	int path_first;
	int path_length;
	int query_first;
	int query_length;
	int frag_first;
	int frag_length;
} points;

static points results;

const char* last(UriPathSegmentA *segment)
{
	if (segment->next == NULL) return segment->text.afterLast;
	else last(segment->next);
}

void set(points *buff, const UriUriA uri)
{
	const char* offset = uri.scheme.first;
	/* scheme */
	buff->scheme_first = uri.scheme.first - offset;
	buff->scheme_length = uri.scheme.afterLast - uri.scheme.first;
	/* userInfo */
	if (uri.userInfo.first == NULL){
		buff->userInfo_first = 0;
		buff->userInfo_length = 0;
	} else {
		buff->userInfo_first = uri.userInfo.first - offset;
		buff->userInfo_length =
			uri.userInfo.afterLast - uri.userInfo.first;
	}
	/* host */
	if (uri.hostText.first == NULL){
		buff->host_first = 0;
		buff->host_length = 0;
	} else {
		buff->host_first = uri.hostText.first - offset;
		buff->host_length = uri.hostText.afterLast - uri.hostText.first;
	}
	/* port */
	if (uri.portText.first == NULL){
		buff->port_first = 0;
		buff->port_length = 0;
	} else {
		buff->port_first = uri.portText.first - offset;
		buff->port_length = uri.portText.afterLast - uri.portText.first;
	}
	/* path */
	if (uri.pathHead->text.first == NULL){
		buff->path_first = 0;
		buff->path_length = 0;
	} else {
		buff->path_first = uri.pathHead->text.first - offset;
		buff->path_length =
			last(uri.pathHead) - uri.pathHead->text.first;
	}
	/* query */
	if (uri.query.first == NULL){
		buff->query_first = 0;
		buff->query_length = 0;
	} else {
		buff->query_first = uri.query.first - offset;
		buff->query_length = uri.query.afterLast - uri.query.first;
	}
	/* frag */
	if (uri.fragment.first == NULL){
		buff->frag_first = 0;
		buff->frag_length = 0;
	} else {
		buff->frag_first = uri.fragment.first - offset;
		buff->frag_length = uri.fragment.afterLast - uri.fragment.first;
	}
#ifdef DEBUG
	printf("ext: buff->scheme_first:       %d\n", buff->scheme_first);
	printf("ext: buff->scheme_length:   %d\n", buff->scheme_length);
	printf("ext: buff->userInfo_first:     %d\n", buff->userInfo_first);
	printf("ext: buff->userInfo_length: %d\n", buff->userInfo_length);
	printf("ext: buff->host_first:         %d\n", buff->host_first);
	printf("ext: buff->host_length:     %d\n", buff->host_length);
	printf("ext: buff->port_first:         %d\n", buff->port_first);
	printf("ext: buff->port_length:     %d\n", buff->port_length);
	printf("ext: buff->path_first:         %d\n", buff->path_first);
	printf("ext: buff->path_length:     %d\n", buff->path_length);
	printf("ext: buff->query_first:        %d\n", buff->query_first);
	printf("ext: buff->query_length:    %d\n", buff->query_length);
	printf("ext: buff->frag_first:         %d\n", buff->frag_first);
	printf("ext: buff->frag_length:     %d\n", buff->frag_length);
#endif
}
int uriparser(char *input)
{
	UriParserStateA state;
	UriUriA uri;
	state.uri = &uri;
	if (uriParseUriA(&state, input) != URI_SUCCESS) {
		uriFreeUriMembersA(&uri);
		return 1;
	}
	else {
		set(&results, uri);
		uriFreeUriMembersA(&uri);
		return 0;
	}
}
int get_scheme_first()
{
	return results.scheme_first;
}
int get_scheme_length()
{
	return results.scheme_length;
}
int get_userInfo_first()
{
	return results.userInfo_first;
}
int get_userInfo_length()
{
	return results.userInfo_length;
}
int get_host_first()
{
	return results.host_first;
}
int get_host_length()
{
	return results.host_length;
}
int get_port_first()
{
	return results.port_first;
}
int get_port_length()
{
	return results.port_length;
}
int get_path_first()
{
	return results.path_first;
}
int get_path_length()
{
	return results.path_length;
}
int get_query_first()
{
	return results.query_first;
}
int get_query_length()
{
	return results.query_length;
}
int get_frag_first()
{
	return results.frag_first;
}
int get_frag_length()
{
	return results.frag_length;
}
