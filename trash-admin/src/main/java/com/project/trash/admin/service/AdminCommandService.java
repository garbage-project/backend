package com.project.trash.admin.service;

import com.project.trash.admin.domain.Admin;
import com.project.trash.admin.request.AdminModifyRequest;
import com.project.trash.admin.request.LoginRequest;
import com.project.trash.admin.request.ReissueRequest;
import com.project.trash.admin.response.AccessTokenInfoResponse;
import com.project.trash.admin.response.LoginResponse;
import com.project.trash.auth.service.JwtService;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.CookieUtils;
import com.project.trash.token.domain.Token;
import com.project.trash.token.repository.TokenRepository;
import com.project.trash.utils.AdminUtils;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.AdminResultCode.ADMIN_INFO_NOT_MATCH;
import static com.project.trash.common.domain.resultcode.AuthResultCode.AUTH_TOKEN_INVALID;
import static com.project.trash.common.domain.resultcode.AuthResultCode.AUTH_TOKEN_NOT_FOUND;

@Service
@RequiredArgsConstructor
public class AdminCommandService {

  private final AdminQueryService adminQueryService;
  private final JwtService jwtService;

  private final TokenRepository tokenRepository;

  @Transactional
  public LoginResponse login(LoginRequest param, HttpServletResponse response) {
    Admin admin = adminQueryService.getOne(param.getId());

    // 비밀번호 검증
    if (!param.getPassword().equals(admin.getPassword())) {
      throw new ValidationException(ADMIN_INFO_NOT_MATCH);
    }

    Pair<String, Long> accessToken = jwtService.createAccessToken(admin.getId());
    Pair<String, Long> refreshToken = jwtService.createRefreshToken(admin.getId());

    tokenRepository.save(new Token(admin.getId(), accessToken.getLeft(), refreshToken.getLeft()));

    CookieUtils.setCookie("accessToken", accessToken.getLeft(), accessToken.getRight(), response);
    CookieUtils.setCookie("refreshToken", refreshToken.getLeft(), refreshToken.getRight(), response);

    return new LoginResponse(admin.getId());
  }

  @Transactional
  public void logout(HttpServletResponse response) {
    Token token = getToken(AdminUtils.getId());

    tokenRepository.delete(token);

    // 토큰 제거
    CookieUtils.setCookie("accessToken", "", 0L, response);
    CookieUtils.setCookie("refreshToken", "", 0L, response);
  }

  @Transactional
  public void modify(AdminModifyRequest param) {
    Admin admin = adminQueryService.getOne(AdminUtils.getId());

    admin.update(param.getPassword());
  }

  @Transactional
  public AccessTokenInfoResponse reissue(ReissueRequest param, HttpServletRequest request) {
    Admin admin = adminQueryService.getOne(param.getId());

    Token token =
        tokenRepository.findByMemberId(admin.getId()).orElseThrow(() -> new ValidationException(AUTH_TOKEN_NOT_FOUND));

    String refreshToken = CookieUtils.getCookie(request, "refreshToken");

    if (StringUtils.isBlank(refreshToken) || !jwtService.isTokenValid(refreshToken, admin) ||
        !token.getRefreshToken().equals(refreshToken)) {
      throw new ValidationException(AUTH_TOKEN_INVALID);
    }

    Pair<String, Long> accessToken = jwtService.createAccessToken(admin.getId());

    token.updateAccessToken(accessToken.getLeft());

    return new AccessTokenInfoResponse(accessToken.getLeft(), accessToken.getRight());
  }

  private Token getToken(String id) {
    return tokenRepository.findByMemberId(id).orElseThrow(() -> new ValidationException(AUTH_TOKEN_NOT_FOUND));
  }
}
