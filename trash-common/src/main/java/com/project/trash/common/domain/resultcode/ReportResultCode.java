package com.project.trash.common.domain.resultcode;

import com.project.trash.common.domain.ResultCode;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ReportResultCode implements ResultCode {

  REPORT_NOT_FOUND("RPW000", "신고 정보가 존재하지 않습니다.");

  private final String code;
  private final String message;
}
