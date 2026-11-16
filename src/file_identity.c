#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0602
#endif
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#include <wchar.h>
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32

static int wide_prefix_equal(const WCHAR *value, const WCHAR *prefix) {
  while (*prefix != L'\0') {
    if (*value == L'\0') return 0;
    WCHAR a = *value++;
    WCHAR b = *prefix++;
    if (a >= L'a' && a <= L'z') a = (WCHAR)(a - L'a' + L'A');
    if (b >= L'a' && b <= L'z') b = (WCHAR)(b - L'a' + L'A');
    if (a != b) return 0;
  }
  return 1;
}

static int wide_drive_absolute(const WCHAR *path) {
  return ((path[0] >= L'A' && path[0] <= L'Z') ||
          (path[0] >= L'a' && path[0] <= L'z')) &&
         path[1] == L':' && path[2] == L'\\';
}

static int wide_unc_absolute(const WCHAR *path) {
  return path[0] == L'\\' && path[1] == L'\\' &&
         path[2] != L'\0' && path[2] != L'?' && path[2] != L'.';
}

static int wide_has_dot_component(const WCHAR *path) {
  const WCHAR *component = path;
  for (const WCHAR *cursor = path;; ++cursor) {
    if (*cursor == L'\\' || *cursor == L'/' || *cursor == L'\0') {
      size_t length = (size_t)(cursor - component);
      if ((length == 1 && component[0] == L'.') ||
          (length == 2 && component[0] == L'.' && component[1] == L'.')) {
        return 1;
      }
      if (*cursor == L'\0') break;
      component = cursor + 1;
    }
  }
  return 0;
}

static void wide_normalize_separators(WCHAR *path) {
  for (WCHAR *cursor = path; *cursor != L'\0'; ++cursor) {
    if (*cursor == L'/') *cursor = L'\\';
  }
}

static WCHAR *with_extended_prefix(const WCHAR *absolute_path) {
  size_t length = wcslen(absolute_path);
  if (length > 32760) return NULL;

  if (wide_prefix_equal(absolute_path, L"\\\\?\\")) {
    if (!wide_drive_absolute(absolute_path + 4) &&
        !wide_prefix_equal(absolute_path + 4, L"UNC\\")) {
      return NULL;
    }
    WCHAR *result = (WCHAR *)R_alloc(length + 1, sizeof(WCHAR));
    memcpy(result, absolute_path, (length + 1) * sizeof(WCHAR));
    wide_normalize_separators(result);
    return result;
  }

  if (wide_unc_absolute(absolute_path)) {
    WCHAR *result = (WCHAR *)R_alloc(length + 8, sizeof(WCHAR));
    memcpy(result, L"\\\\?\\UNC\\", 8 * sizeof(WCHAR));
    memcpy(result + 8, absolute_path + 2, (length - 1) * sizeof(WCHAR));
    wide_normalize_separators(result);
    return result;
  }
  if (wide_drive_absolute(absolute_path)) {
    WCHAR *result = (WCHAR *)R_alloc(length + 5, sizeof(WCHAR));
    memcpy(result, L"\\\\?\\", 4 * sizeof(WCHAR));
    memcpy(result + 4, absolute_path, (length + 1) * sizeof(WCHAR));
    wide_normalize_separators(result);
    return result;
  }
  return NULL;
}

static WCHAR *absolute_extended_path(WCHAR *path) {
  wide_normalize_separators(path);
  if (wide_prefix_equal(path, L"\\\\.\\")) return NULL;

  if (wide_prefix_equal(path, L"\\\\?\\")) {
    return with_extended_prefix(path);
  }

  DWORD required = GetFullPathNameW(path, 0, NULL, NULL);
  WCHAR *absolute_path = NULL;
  if (required > 0 && required <= 32767) {
    absolute_path = (WCHAR *)R_alloc((size_t)required + 1, sizeof(WCHAR));
    DWORD written = GetFullPathNameW(path, required + 1, absolute_path, NULL);
    if (written == 0 || written >= required + 1) absolute_path = NULL;
  }

  if (absolute_path == NULL) {
    /* An already absolute, uncomplicated path can still use the extended
       namespace if GetFullPathNameW rejects its length. */
    if ((!wide_drive_absolute(path) && !wide_unc_absolute(path)) ||
        wide_has_dot_component(path)) {
      return NULL;
    }
    absolute_path = path;
  }
  wide_normalize_separators(absolute_path);
  return with_extended_prefix(absolute_path);
}

static WCHAR *utf8_path_to_wide(const char *utf8) {
  if (utf8 == NULL) return NULL;
  size_t length = strlen(utf8);
  if (length == 0 || length > (size_t)INT_MAX) return NULL;

  int required = MultiByteToWideChar(
      CP_UTF8, MB_ERR_INVALID_CHARS, utf8, (int)length, NULL, 0);
  if (required <= 0) return NULL;

  WCHAR *wide = (WCHAR *)R_alloc((size_t)required + 1, sizeof(WCHAR));
  int written = MultiByteToWideChar(
      CP_UTF8, MB_ERR_INVALID_CHARS, utf8, (int)length, wide, required);
  if (written != required) return NULL;
  wide[required] = L'\0';
  return wide;
}

static void format_hex_u64(ULONGLONG value, char output[17]) {
  static const char hex[] = "0123456789abcdef";
  for (int i = 15; i >= 0; --i) {
    output[i] = hex[(unsigned int)(value & 0x0fU)];
    value >>= 4;
  }
  output[16] = '\0';
}

static void format_hex_bytes(const UCHAR *value, size_t length, char *output) {
  static const char hex[] = "0123456789abcdef";
  for (size_t i = 0; i < length; ++i) {
    output[i * 2] = hex[(value[i] >> 4) & 0x0fU];
    output[i * 2 + 1] = hex[value[i] & 0x0fU];
  }
  output[length * 2] = '\0';
}

static SEXP windows_file_identity(const WCHAR *path) {
  HANDLE handle = CreateFileW(
      path,
      0,
      FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
      NULL,
      OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
      NULL);
  if (handle == INVALID_HANDLE_VALUE) return R_NilValue;

  FILE_BASIC_INFO basic_info;
  FILE_STANDARD_INFO standard_info;
  FILE_ID_INFO file_id_info;
  int valid = GetFileType(handle) == FILE_TYPE_DISK &&
      GetFileInformationByHandleEx(
          handle, FileBasicInfo, &basic_info, sizeof(basic_info)) &&
      GetFileInformationByHandleEx(
          handle, FileStandardInfo, &standard_info, sizeof(standard_info)) &&
      GetFileInformationByHandleEx(
          handle, FileIdInfo, &file_id_info, sizeof(file_id_info)) &&
      (basic_info.FileAttributes &
       (FILE_ATTRIBUTE_REPARSE_POINT | FILE_ATTRIBUTE_DEVICE)) == 0 &&
      (((basic_info.FileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0) ==
       (standard_info.Directory != 0));

  char device_id[32];
  char inode_id[48];
  if (valid) {
    char volume_hex[17];
    char file_hex[33];
    format_hex_u64(file_id_info.VolumeSerialNumber, volume_hex);
    format_hex_bytes(file_id_info.FileId.Identifier, 16, file_hex);
    snprintf(device_id, sizeof(device_id), "win-volume:%s", volume_hex);
    snprintf(inode_id, sizeof(inode_id), "win-file-id:%s", file_hex);
  }
  CloseHandle(handle);
  if (!valid) return R_NilValue;

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_VECTOR_ELT(result, 0, Rf_mkString(device_id));
  SET_VECTOR_ELT(result, 1, Rf_mkString(inode_id));
  SET_STRING_ELT(names, 0, Rf_mkChar("device_id"));
  SET_STRING_ELT(names, 1, Rf_mkChar("inode"));
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

#else

#include <inttypes.h>
#include <sys/stat.h>
#include <sys/types.h>

static SEXP posix_file_identity(const char *native_path) {
  size_t length = strlen(native_path);
  size_t trimmed_length = length;
  while (trimmed_length > 0 && native_path[trimmed_length - 1] == '/') {
    --trimmed_length;
  }
  int had_trailing_slash = trimmed_length < length;
  if (trimmed_length == 0) trimmed_length = length;

  char *path = (char *)R_alloc(trimmed_length + 1, sizeof(char));
  memcpy(path, native_path, trimmed_length);
  path[trimmed_length] = '\0';

  struct stat info;
  if (lstat(path, &info) != 0 ||
      (!S_ISREG(info.st_mode) && !S_ISDIR(info.st_mode)) ||
      (had_trailing_slash && !S_ISDIR(info.st_mode))) {
    return R_NilValue;
  }

  char device_id[64];
  char inode_id[64];
  int device_length = snprintf(
      device_id, sizeof(device_id), "posix-device:%" PRIuMAX,
      (uintmax_t)info.st_dev);
  int inode_length = snprintf(
      inode_id, sizeof(inode_id), "posix-inode:%" PRIuMAX,
      (uintmax_t)info.st_ino);
  if (device_length < 0 || (size_t)device_length >= sizeof(device_id) ||
      inode_length < 0 || (size_t)inode_length >= sizeof(inode_id)) {
    return R_NilValue;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_VECTOR_ELT(result, 0, Rf_mkString(device_id));
  SET_VECTOR_ELT(result, 1, Rf_mkString(inode_id));
  SET_STRING_ELT(names, 0, Rf_mkChar("device_id"));
  SET_STRING_ELT(names, 1, Rf_mkChar("inode"));
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

#endif

SEXP file_identity_exact(SEXP path) {
  if (TYPEOF(path) != STRSXP || XLENGTH(path) != 1) return R_NilValue;

  SEXP value = STRING_ELT(path, 0);
  if (value == NA_STRING || Rf_getCharCE(value) == CE_BYTES) {
    return R_NilValue;
  }

#ifdef _WIN32
  const char *utf8 = Rf_translateCharUTF8(value);
  if (utf8 == NULL || utf8[0] == '\0') return R_NilValue;
  WCHAR *wide_path = utf8_path_to_wide(utf8);
  if (wide_path == NULL) return R_NilValue;
  WCHAR *absolute_path = absolute_extended_path(wide_path);
  if (absolute_path == NULL) return R_NilValue;
  return windows_file_identity(absolute_path);
#else
  const char *native_path = Rf_translateChar(value);
  if (native_path == NULL || native_path[0] == '\0') return R_NilValue;
  return posix_file_identity(native_path);
#endif
}

static const R_CallMethodDef CallEntries[] = {
    {"file_identity_exact", (DL_FUNC)&file_identity_exact, 1},
    {NULL, NULL, 0}};

void attribute_visible R_init_dsprrr(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
