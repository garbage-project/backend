package com.project.trash.auth.service;

import com.project.trash.auth.client.SocialApiClientComposite;
import com.project.trash.auth.provider.AuthCodeRequestUrlProviderComposite;
import com.project.trash.member.domain.enums.SocialType;

import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class AuthService {

  private final AuthCodeRequestUrlProviderComposite authCodeRequestUrlProvider;
  private final SocialApiClientComposite socialMemberClient;

  /**
   * OAuth 엑세스 토큰 발급
   */
  public String getAccessToken(SocialType socialType, String authCode) {
    return socialMemberClient.getAccessToken(socialType, authCode);
  }

  /**
   * 소셜 타입에 따른 AuthCode 요청 Url 반환
   *
   * @param socialType 소셜 타입
   * @return AuthCode 요청 Url
   */
  public String getAuthCodeRequestUrl(SocialType socialType) {
    return authCodeRequestUrlProvider.provide(socialType);
  }
}
