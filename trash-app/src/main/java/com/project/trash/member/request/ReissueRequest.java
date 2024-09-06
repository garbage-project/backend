package com.project.trash.member.request;

import lombok.Getter;

/**
 * 엑세스 토큰 재발급 요청
 */
@Getter
public class ReissueRequest {

  private String socialId;
  private String refreshToken;
}
