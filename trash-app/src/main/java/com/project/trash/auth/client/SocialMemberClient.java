package com.project.trash.auth.client;

import com.project.trash.auth.domain.OAuthMember;
import com.project.trash.member.domain.enums.SocialType;

/**
 * Resource Server 회원 정보 요청
 */
public interface SocialMemberClient {

  /**
   * @return 회원 정보
   */
  OAuthMember fetch(String accessToken);

  /**
   * @return 엑세스 토큰
   */
  String getAccessToken(String authCode);

  /**
   * @return 지원하는 SocialType
   */
  SocialType supportSocial();
}
