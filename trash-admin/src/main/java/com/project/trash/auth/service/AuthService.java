package com.project.trash.auth.service;

import com.project.trash.admin.domain.Admin;
import com.project.trash.admin.service.AdminQueryService;
import com.project.trash.auth.request.LoginRequest;
import com.project.trash.auth.response.LoginResponse;
import com.project.trash.common.exception.ValidationException;

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

  /**
   * 로그인
   */
  @Transactional
  public LoginResponse login(LoginRequest param) {
    Admin admin = adminQueryService.getOne(param.getId());

    // 비밀번호 검증
    if (!param.getPassword().equals(admin.getPassword())) {
      throw new ValidationException("auth.not_match_password");
    }

    Pair<String, Integer> accessToken = jwtService.createAccessToken(admin.getId());
    Pair<String, Integer> refreshToken = jwtService.createRefreshToken(admin.getId());

    return new LoginResponse(admin.getId(), accessToken.getLeft(), accessToken.getRight(), refreshToken.getLeft(),
        refreshToken.getRight());
  }
}
