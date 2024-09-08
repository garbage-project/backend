package com.project.trash.common.response;

import com.project.trash.common.domain.resultcode.RequestResultCode;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 성공 공통 응답 형식
 */
@Getter
@Schema(title = "요청 처리 성공 공통 응답", description = "API 요청 처리 성공시 응답되는 공통 응답 양식이다.")
public class SuccessResponse {

  @Schema(title = "결과 코드", description = "요청 처리 결과 코드", requiredMode = Schema.RequiredMode.REQUIRED, example = "R000")
  private final String code;

  @Schema(title = "결과 메시지", description = "요청 처리 결과 메시지", requiredMode = Schema.RequiredMode.REQUIRED,
      example = "success")
  private final String message;

  public SuccessResponse() {
    this.message = RequestResultCode.SUCCESS.getCode();
    this.code = RequestResultCode.SUCCESS.getMessage();
  }
}
