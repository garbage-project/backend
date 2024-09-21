package com.project.trash.member.service;

import com.project.trash.auth.client.SocialMemberClientComposite;
import com.project.trash.auth.domain.OAuthMember;
import com.project.trash.auth.service.JwtService;
import com.project.trash.common.domain.enums.Valid;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.member.domain.Member;
import com.project.trash.member.domain.enums.SocialType;
import com.project.trash.member.repository.MemberRepository;
import com.project.trash.member.request.LoginRequest;
import com.project.trash.member.request.ReissueRequest;
import com.project.trash.member.response.AccessTokenInfoResponse;
import com.project.trash.member.response.LoginResponse;
import com.project.trash.token.domain.Token;
import com.project.trash.token.repository.TokenRepository;
import com.project.trash.utils.MemberUtils;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.AuthResultCode.AUTH_OAUTH_ACCESS_TOKEN_INVALID;
import static com.project.trash.common.domain.resultcode.AuthResultCode.AUTH_TOKEN_INVALID;
import static com.project.trash.common.domain.resultcode.AuthResultCode.AUTH_TOKEN_NOT_FOUND;

/**
 * 회원 등록/수정/삭제 서비스
 */
@Service
@RequiredArgsConstructor
public class MemberCommandService {

  private final JwtService jwtService;
  private final SocialMemberClientComposite socialMemberClient;

  private final MemberQueryService memberQueryService;
  private final MemberRepository memberRepository;
  private final TokenRepository tokenRepository;

  /**
   * 소셜 로그인, 가입 전이라면 회원가입
   */
  @Transactional
  public LoginResponse login(LoginRequest param) {
    String socialId = param.getSocialId();
    SocialType socialType = SocialType.fromCode(param.getSocialType());
    Member member;
    if (!memberRepository.existsBySocialId(socialId)) {
      OAuthMember memberInfo = socialMemberClient.getMemberInfo(socialType, param.getAccessToken());

      // 소셜 ID 일치여부 검증
      if (!socialId.equals(memberInfo.socialId())) {
        throw new ValidationException(AUTH_OAUTH_ACCESS_TOKEN_INVALID);
      }

      member = memberRepository.save(
          new Member(memberInfo.email(), memberInfo.gender(), memberInfo.socialId(), memberInfo.socialType()));
    } else {
      // 엑세스 토큰 유효성 검증
      if (!socialId.equals(socialMemberClient.getSocialId(socialType, param.getAccessToken()))) {
        throw new ValidationException(AUTH_OAUTH_ACCESS_TOKEN_INVALID);
      }

      member = memberQueryService.getOne(socialId);
    }

    Pair<String, Integer> accessToken = jwtService.createAccessToken(socialId);
    Pair<String, Integer> refreshToken = jwtService.createRefreshToken(socialId);

    tokenRepository.save(new Token(socialId, accessToken.getLeft(), refreshToken.getLeft()));

    return new LoginResponse(socialId, Valid.convertToCode(member.getAgreementYn()), accessToken.getLeft(), accessToken.getRight(), refreshToken.getLeft(),
        refreshToken.getRight());
  }

  /**
   * 로그아웃
   */
  @Transactional
  public void logout() {
    Token token = memberQueryService.getToken(MemberUtils.getMember().getSocialId())
                                    .orElseThrow(() -> new ValidationException(AUTH_TOKEN_NOT_FOUND));

    tokenRepository.delete(token);
  }

  /**
   * 엑세스 토큰 재발급
   */
  @Transactional
  public AccessTokenInfoResponse reissue(ReissueRequest param) {
    Member member = memberQueryService.getOne(param.getSocialId());

    Token token = tokenRepository.findByMemberId(member.getSocialId())
                                 .orElseThrow(() -> new ValidationException(AUTH_TOKEN_NOT_FOUND));

    if (!jwtService.isTokenValid(param.getRefreshToken(), member) ||
        !token.getRefreshToken().equals(param.getRefreshToken())) {
      throw new ValidationException(AUTH_TOKEN_INVALID);
    }

    Pair<String, Integer> accessToken = jwtService.createAccessToken(member.getSocialId());

    token.updateAccessToken(accessToken.getLeft());

    return new AccessTokenInfoResponse(accessToken.getLeft(), accessToken.getRight());
  }
}
