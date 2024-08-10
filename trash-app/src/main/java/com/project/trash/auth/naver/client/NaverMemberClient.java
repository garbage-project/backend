package com.project.trash.auth.naver.client;

import com.project.trash.auth.client.SocialMemberClient;
import com.project.trash.auth.domain.OAuthMember;
import com.project.trash.member.domain.enums.SocialType;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;

/**
 * Naver 회원 정보 요청
 */
@RequiredArgsConstructor
@Component
public class NaverMemberClient implements SocialMemberClient {

  private final NaverApiClient naverApiClient;

  @Override
  public String getAccessToken(String authCode) {
    return naverApiClient.getToken(authCode);
  }

  @Override
  public OAuthMember getMemberInfo(String accessToken) {
    return naverApiClient.getMemberInfo(accessToken);
  }

  @Override
  public String getSocialId(String accessToken) {
    return naverApiClient.getSocialId(accessToken);
  }

  @Override
  public SocialType supportSocial() {
    return SocialType.NAVER;
  }
}
