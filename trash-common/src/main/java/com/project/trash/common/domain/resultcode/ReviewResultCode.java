package com.project.trash.common.domain.resultcode;

import com.project.trash.common.domain.ResultCode;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ReviewResultCode implements ResultCode {

  REVIEW_NOT_FOUND("RVW000", "리뷰 정보가 존재하지 않습니다.");

  private final String code;
  private final String message;
}
