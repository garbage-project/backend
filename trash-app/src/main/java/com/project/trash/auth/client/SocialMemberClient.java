package com.project.trash.auth.client;

import com.project.trash.auth.domain.OAuthMember;
import com.project.trash.member.domain.enums.SocialType;

/**
 * Resource Server 정보 요청
 */
public interface SocialMemberClient {

  /**
   * 엑세스 토큰 발급
   */
  String getAccessToken(String authCode);

  /**
   * 엑세스 토큰 정보 확인(검증)
   */
  String getAccessTokenInfo(String accessToken);

  /**
   * 사용자 정보 가져오기
   */
  OAuthMember getMemberInfo(String accessToken);

  /**
   * 지원하는 SocialType
   */
  SocialType supportSocial();
}
