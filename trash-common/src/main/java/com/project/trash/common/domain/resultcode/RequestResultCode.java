package com.project.trash.common.domain.resultcode;

import com.project.trash.common.domain.ResultCode;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum RequestResultCode implements ResultCode {

  SUCCESS("REQ000", "성공적으로 수행했습니다."),
  FAIL("REQ001", "요청 처리 중 오류가 발생했습니다."),
  PARAM_INVALID("REQ002", "요청 파라미터가 존재하지 않거나 유효하지 않습니다."),
  UNAUTHORIZED("REQ003", "인증되지 않은 사용자입니다."),
  FORBIDDEN("REQ004", "권한이 존재하지 않습니다.");

  private final String code;
  private final String message;
}
