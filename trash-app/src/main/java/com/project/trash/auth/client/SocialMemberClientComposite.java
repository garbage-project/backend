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
   * 소셜 타입에 맞는 엑세스 토큰 발급
   */
  public String getAccessToken(SocialType socialType, String authCode) {
    return getClient(socialType).getAccessToken(authCode);
  }

  /**
   * 소셜 타입에 맞는 사용자 정보 가져오기
   */
  public OAuthMember getMemberInfo(SocialType socialType, String accessToken) {
    return getClient(socialType).getMemberInfo(accessToken);
  }

  /**
   * 엑세스 토큰 정보 확인(검증)
   */
  public String getSocialId(SocialType socialType, String accessToken) {
    return getClient(socialType).getSocialId(accessToken);
  }

  private SocialMemberClient getClient(SocialType socialType) {
    return Optional.ofNullable(clientMap.get(socialType))
                   .orElseThrow(() -> new ValidationException("auth.param_social_type_invalid"));
  }
}
