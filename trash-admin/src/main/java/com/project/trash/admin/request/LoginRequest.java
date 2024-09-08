package com.project.trash.admin.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 로그인 요청
 */
@Getter
public class LoginRequest {

  /**
   * ID
   */
  @Schema(description = "관리자 ID", example = "id1234")
  private String id;
  /**
   * 비밀번호
   */
  @Schema(description = "관리자 비밀번호", example = "pw1234")
  private String password;
}
