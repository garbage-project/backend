package com.project.trash.common.domain.resultcode;

import com.project.trash.common.domain.ResultCode;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum MemberResultCode implements ResultCode {

  MEMBER_NOT_FOUND("MBR000", "회원 정보가 존재하지 않습니다.");

  private final String code;
  private final String message;
}
