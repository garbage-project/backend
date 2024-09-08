package com.project.trash.common.domain.resultcode;

import com.project.trash.common.domain.ResultCode;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum AdminResultCode implements ResultCode {

  ADMIN_NOT_FOUND("ADM000", "관리자 정보가 존재하지 않습니다."),
  ADMIN_INFO_NOT_MATCH("ADM001", "관리자 정보가 일치하지 않습니다.");

  private final String code;
  private final String message;
}
