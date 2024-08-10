package com.project.trash.auth.naver;

import com.project.trash.auth.provider.AuthCodeRequestUrlProvider;
import com.project.trash.member.domain.enums.SocialType;

import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

import lombok.RequiredArgsConstructor;

/**
 * Naver AuthCode 요청 Url 제공
 */
@Component
@RequiredArgsConstructor
public class NaverAuthCodeRequestUrlProvider implements AuthCodeRequestUrlProvider {

  private final NaverOAuthConfig naverOAuthConfig;

  @Override
  public String provideUrl() {
    return UriComponentsBuilder.fromUriString(naverOAuthConfig.authorizeUri())
                               .queryParam("response_type", "code")
                               .queryParam("client_id", naverOAuthConfig.clientId())
                               .queryParam("redirect_uri", naverOAuthConfig.redirectUri())
                               .queryParam("scope", String.join(",", naverOAuthConfig.scope()))
                               .toUriString();
  }

  @Override
  public SocialType supportSocial() {
    return SocialType.NAVER;
  }
}
