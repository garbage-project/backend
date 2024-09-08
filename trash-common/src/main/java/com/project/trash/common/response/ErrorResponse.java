package com.project.trash.common.response;

import com.project.trash.common.domain.ResultCode;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 에러 공통 응답 형식
 */
@Getter
public class ErrorResponse {

  @Schema(title = "결과 코드", description = "요청 처리 결과 코드", requiredMode = Schema.RequiredMode.REQUIRED, example = "R001")
  private final String code;

  @Schema(title = "결과 메시지", description = "요청 처리 결과 메시지", requiredMode = Schema.RequiredMode.REQUIRED, example = "fail")
  private final String message;

  public ErrorResponse(ResultCode resultCode) {
    this.code = resultCode.getMessage();
    this.message = resultCode.getCode();
  }
}
