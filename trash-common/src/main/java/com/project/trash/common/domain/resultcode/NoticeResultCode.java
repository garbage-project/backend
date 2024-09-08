package com.project.trash.common.domain.resultcode;

import com.project.trash.common.domain.ResultCode;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum NoticeResultCode implements ResultCode {

  NOTICE_NOT_FOUND("NTC000", "공지 정보가 존재하지 않습니다.");

  private final String code;
  private final String message;
}
