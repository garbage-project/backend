package com.project.trash.admin.response;

import lombok.Getter;

/**
 * 토큰 정보 반환
 */
@Getter
public class LoginResponse {

  /**
   * ID
   */
  private final String id;

  public LoginResponse(String id) {
    this.id = id;
  }
}
