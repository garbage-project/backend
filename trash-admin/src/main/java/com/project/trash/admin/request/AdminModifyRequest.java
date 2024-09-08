package com.project.trash.admin.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 관리자 수정 요청
 */
@Getter
public class AdminModifyRequest {

  /**
   * 변경할 비밀번호
   */
  @Schema(description = "변경할 비밀번호", example = "update1234")
  private String password;
}
