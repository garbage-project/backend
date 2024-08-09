package com.project.trash.auth.kakao.client;

import com.project.trash.auth.client.SocialMemberClient;
import com.project.trash.auth.domain.OAuthMember;
import com.project.trash.auth.kakao.KakaoOAuthConfig;
import com.project.trash.member.domain.enums.SocialType;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;

/**
 * Kakao 회원 정보 요청
 */
@RequiredArgsConstructor
@Component
public class KakaoMemberClient implements SocialMemberClient {

  private final KakaoApiClient kakaoApiClient;
  private final KakaoOAuthConfig kakaoOAuthConfig;

  @Override
  public OAuthMember fetch(String accessToken) {
    return kakaoApiClient.getMember(kakaoOAuthConfig.authorizationPrefix() + accessToken);
  }

  @Override
  public String getAccessToken(String authCode) {
    return kakaoApiClient.getToken(authCode);
  }

  @Override
  public SocialType supportSocial() {
    return SocialType.KAKAO;
  }
}
