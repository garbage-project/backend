package com.project.trash.common.domain.resultcode;

import com.project.trash.common.domain.ResultCode;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum SystemResultCode implements ResultCode {

  IMAGE_UPLOAD_FAIL("SYS000", "이미지 업로드를 실패했습니다.");

  private final String code;
  private final String message;
}
