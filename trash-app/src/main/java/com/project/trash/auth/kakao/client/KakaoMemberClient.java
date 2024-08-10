package com.project.trash.auth.kakao.client;

import com.project.trash.auth.client.SocialMemberClient;
import com.project.trash.auth.domain.OAuthMember;
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

  @Override
  public String getAccessToken(String authCode) {
    return kakaoApiClient.getToken(authCode);
  }

  @Override
  public String getAccessTokenInfo(String accessToken) {
    return kakaoApiClient.getAccessTokenInfo(accessToken);
  }

  @Override
  public OAuthMember getMemberInfo(String accessToken) {
    return kakaoApiClient.getMemberInfo(accessToken);
  }

  @Override
  public SocialType supportSocial() {
    return SocialType.KAKAO;
  }
}
