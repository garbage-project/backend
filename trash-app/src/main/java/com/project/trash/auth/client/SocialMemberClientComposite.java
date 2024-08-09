package com.project.trash.auth.client;

import com.project.trash.auth.domain.OAuthMember;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.member.domain.enums.SocialType;

import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;

/**
 * Resource Server 회원 정보 요청 카카오에서 사용자 정보를 받아오는 클래스 Composite 패턴으로 구현
 */
@Component
public class SocialMemberClientComposite {

  private final Map<SocialType, SocialMemberClient> clientMap;

  public SocialMemberClientComposite(Set<SocialMemberClient> clients) {
    clientMap = clients.stream().collect(toMap(SocialMemberClient::supportSocial, identity()));
  }

  /**
   * 소셜 타입에 맞는 회원 정보 반환
   */
  public OAuthMember fetch(SocialType socialType, String accessToken) {
    return getClient(socialType).fetch(accessToken);
  }

  /**
   * 소셜 타입에 맞는 AccessToken 반환
   */
  public String getAccessToken(SocialType socialType, String authCode) {
    return getClient(socialType).getAccessToken(authCode);
  }

  private SocialMemberClient getClient(SocialType socialType) {
    return Optional.ofNullable(clientMap.get(socialType))
                   .orElseThrow(() -> new ValidationException("auth.param_social_type_invalid"));
  }
}
