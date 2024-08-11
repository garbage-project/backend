package com.project.trash.auth.request;

import lombok.Getter;

/**
 * 로그인 요청
 */
@Getter
public class LoginRequest {

  /**
   * ID
   */
  private String id;
  /**
   * 비밀번호
   */
  private String password;
}
