package com.project.trash.auth.service;

import com.project.trash.auth.client.SocialMemberClientComposite;
import com.project.trash.auth.domain.OAuthMember;
import com.project.trash.auth.provider.AuthCodeRequestUrlProviderComposite;
import com.project.trash.auth.request.LoginRequest;
import com.project.trash.auth.response.LoginResponse;
import com.project.trash.member.domain.Member;
import com.project.trash.member.domain.enums.SocialType;
import com.project.trash.member.repository.MemberRepository;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

/**
 * Auth 서비스
 */
@Service
@RequiredArgsConstructor
public class AuthService {

  private final AuthCodeRequestUrlProviderComposite authCodeRequestUrlProvider;
  private final SocialMemberClientComposite socialMemberClient;
  private final JwtService jwtService;
  private final MemberRepository memberRepository;

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

  /**
   * 소셜 로그인, 가입 전이라면 회원가입
   *
   * @param param 요청 파라미터
   */
  @Transactional
  public LoginResponse login(LoginRequest param) {
    OAuthMember memberInfo =
        socialMemberClient.fetch(SocialType.fromCode(param.getSocialType()), param.getAccessToken());

    //    // 소셜 ID 일치여부 검증
    //    if (!memberInfo.socialId().equals(param.getSocialId())) {
    //      throw new ValidationException("auth.not_match_social_id");
    //    }

    if (!memberRepository.existsBySocialId(memberInfo.socialId())) {
      memberRepository.save(
          new Member(memberInfo.email(), memberInfo.name(), memberInfo.gender(), memberInfo.birthday(),
              memberInfo.socialId(), memberInfo.socialType()));
    }

    Pair<String, Integer> accessToken = jwtService.createAccessToken(memberInfo.socialId());
    Pair<String, Integer> refreshToken = jwtService.createRefreshToken(memberInfo.socialId());

    return new LoginResponse(memberInfo.socialId(), accessToken.getLeft(), accessToken.getRight(),
        refreshToken.getLeft(), refreshToken.getRight());
  }
}
