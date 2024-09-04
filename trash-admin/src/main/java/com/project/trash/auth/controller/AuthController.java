package com.project.trash.auth.controller;

import com.project.trash.auth.request.LoginRequest;
import com.project.trash.auth.request.ReissueRequest;
import com.project.trash.auth.service.AuthService;
import com.project.trash.common.response.DataResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;

/**
 * 인증 API
 */
@RestController
@RequestMapping("/auth")
@RequiredArgsConstructor
public class AuthController {

  private final AuthService authService;

  /**
   * 로그인
   */
  @PostMapping("/login")
  public ResponseEntity<?> postLogin(@RequestBody LoginRequest param) {
    AuthValidator.validate(param);

    return ResponseEntity.ok(new DataResponse(authService.login(param)));
  }

  /**
   * 엑세스 토큰 재발급
   */
  @PostMapping("/reissue")
  public ResponseEntity<?> postReissue(@RequestBody ReissueRequest param) {
    AuthValidator.validate(param);

    return ResponseEntity.ok(new DataResponse(authService.reissue(param)));
  }
}
