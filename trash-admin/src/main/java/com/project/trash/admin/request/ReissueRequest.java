package com.project.trash.admin.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 엑세스 토큰 재발급 요청
 */
@Getter
public class ReissueRequest {

  @Schema(description = "관리자 ID", example = "testid1234")
  private String id;
}
