package com.project.trash.auth.service;

import com.project.trash.admin.domain.Admin;
import com.project.trash.admin.service.AdminQueryService;
import com.project.trash.auth.request.LoginRequest;
import com.project.trash.auth.request.ReissueRequest;
import com.project.trash.auth.response.AccessTokenInfoResponse;
import com.project.trash.auth.response.TokenInfoResponse;
import com.project.trash.common.exception.ValidationException;
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

  private final JwtService jwtService;
  private final AdminQueryService adminQueryService;

  private final TokenRepository tokenRepository;

  /**
   * 로그인
   */
  @Transactional
  public TokenInfoResponse login(LoginRequest param) {
    Admin admin = adminQueryService.getOne(param.getId());

    // 비밀번호 검증
    if (!param.getPassword().equals(admin.getPassword())) {
      throw new ValidationException("auth.not_match_password");
    }

    Pair<String, Integer> accessToken = jwtService.createAccessToken(admin.getId());
    Pair<String, Integer> refreshToken = jwtService.createRefreshToken(admin.getId());

    tokenRepository.save(new Token(admin.getId(), accessToken.getLeft(), refreshToken.getLeft()));

    return new TokenInfoResponse(admin.getId(), accessToken.getLeft(), accessToken.getRight(), refreshToken.getLeft(),
        refreshToken.getRight());
  }

  /**
   * 엑세스 토큰 재발급
   */
  @Transactional
  public AccessTokenInfoResponse reissue(ReissueRequest param) {
    Admin admin = adminQueryService.getOne(param.getId());

    Token token = tokenRepository.findByMemberId(admin.getId())
                                 .orElseThrow(() -> new ValidationException("auth.param_refresh_token_invalid"));

    if (!jwtService.isTokenValid(param.getRefreshToken(), admin) ||
        !token.getRefreshToken().equals(param.getRefreshToken())) {
      throw new ValidationException("auth.param_refresh_token_invalid");
    }

    Pair<String, Integer> accessToken = jwtService.createAccessToken(admin.getId());

    token.updateAccessToken(accessToken.getLeft());

    return new AccessTokenInfoResponse(accessToken.getLeft(), accessToken.getRight());
  }
}
