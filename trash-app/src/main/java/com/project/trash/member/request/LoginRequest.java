package com.project.trash.member.request;

import lombok.Getter;

/**
 * 로그인 요청
 */
@Getter
public class LoginRequest {

  /**
   * 소셜 타입
   */
  private String socialType;
  /**
   * 엑세스 토큰
   */
  private String accessToken;
  /**
   * 소셜 ID
   */
  private String socialId;
}
