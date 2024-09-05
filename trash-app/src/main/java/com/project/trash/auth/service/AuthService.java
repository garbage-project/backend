package com.project.trash.auth.service;

import com.project.trash.auth.client.SocialMemberClientComposite;
import com.project.trash.auth.domain.OAuthMember;
import com.project.trash.auth.provider.AuthCodeRequestUrlProviderComposite;
import com.project.trash.auth.request.LoginRequest;
import com.project.trash.auth.request.ReissueRequest;
import com.project.trash.auth.response.AccessTokenInfoResponse;
import com.project.trash.auth.response.TokenInfoResponse;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.member.domain.Member;
import com.project.trash.member.domain.enums.SocialType;
import com.project.trash.member.repository.MemberRepository;
import com.project.trash.member.service.MemberQueryService;
import com.project.trash.token.domain.Token;
import com.project.trash.token.repository.TokenRepository;

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

  private final MemberQueryService memberQueryService;
  private final MemberRepository memberRepository;
  private final TokenRepository tokenRepository;

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
   */
  @Transactional
  public TokenInfoResponse login(LoginRequest param) {
    String socialId = param.getSocialId();
    SocialType socialType = SocialType.fromCode(param.getSocialType());
    if (!memberRepository.existsBySocialId(socialId)) {
      // 회원가입
      OAuthMember memberInfo = socialMemberClient.getMemberInfo(socialType, param.getAccessToken());

      // 소셜 ID 일치여부 검증
      if (!socialId.equals(memberInfo.socialId())) {
        throw new ValidationException("auth.not_match_social_id");
      }

      memberRepository.save(
          new Member(memberInfo.email(), memberInfo.name(), memberInfo.gender(), memberInfo.birthday(),
              memberInfo.socialId(), memberInfo.socialType()));
    } else {
      // 엑세스 토큰 유효성 검증
      if (!socialId.equals(socialMemberClient.getSocialId(socialType, param.getAccessToken()))) {
        throw new ValidationException("auth.not_match_social_id");
      }
    }

    Pair<String, Integer> accessToken = jwtService.createAccessToken(socialId);
    Pair<String, Integer> refreshToken = jwtService.createRefreshToken(socialId);

    tokenRepository.save(new Token(socialId, accessToken.getLeft(), refreshToken.getLeft()));

    return new TokenInfoResponse(socialId, accessToken.getLeft(), accessToken.getRight(), refreshToken.getLeft(),
        refreshToken.getRight());
  }

  /**
   * 엑세스 토큰 재발급
   */
  @Transactional
  public AccessTokenInfoResponse reissue(ReissueRequest param) {
    Member member = memberQueryService.getOne(param.getSocialId());

    Token token = tokenRepository.findByMemberId(member.getSocialId())
                                 .orElseThrow(() -> new ValidationException("auth.param_refresh_token_invalid"));

    if (!jwtService.isTokenValid(param.getRefreshToken(), member) ||
        !token.getRefreshToken().equals(param.getRefreshToken())) {
      throw new ValidationException("auth.param_refresh_token_invalid");
    }

    Pair<String, Integer> accessToken = jwtService.createAccessToken(member.getSocialId());

    token.updateAccessToken(accessToken.getLeft());

    return new AccessTokenInfoResponse(accessToken.getLeft(), accessToken.getRight());
  }
}
