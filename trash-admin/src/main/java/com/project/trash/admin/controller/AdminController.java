package com.project.trash.admin.controller;

import com.project.trash.admin.request.AdminModifyRequest;
import com.project.trash.admin.request.LoginRequest;
import com.project.trash.admin.request.ReissueRequest;
import com.project.trash.admin.service.AdminCommandService;
import com.project.trash.common.response.DataResponse;
import com.project.trash.common.response.SuccessResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;

/**
 * 관리자 API
 */
@RestController
@RequestMapping("/admins")
@RequiredArgsConstructor
public class AdminController {

  private final AdminCommandService adminCommandService;

  /**
   * 로그인
   */
  @PostMapping("/login")
  public ResponseEntity<?> postLogin(@RequestBody LoginRequest param, HttpServletResponse response) {
    AdminValidator.validate(param);

    return ResponseEntity.ok(new DataResponse(adminCommandService.login(param, response)));
  }

  /**
   * 로그아웃
   */
  @PostMapping("/logout")
  public ResponseEntity<?> postLogout(HttpServletResponse response) {
    adminCommandService.logout(response);
    return ResponseEntity.ok(new SuccessResponse());
  }

  /**
   * 엑세스 토큰 재발급
   */
  @PostMapping("/reissue")
  public ResponseEntity<?> postReissue(@RequestBody ReissueRequest param, HttpServletRequest request) {
    AdminValidator.validate(param);

    return ResponseEntity.ok(new DataResponse(adminCommandService.reissue(param, request)));
  }

  /**
   * 관리자 수정
   */
  @PutMapping
  public ResponseEntity<?> put(@RequestBody AdminModifyRequest param) {
    AdminValidator.validate(param);

    adminCommandService.modify(param);
    return ResponseEntity.ok(new SuccessResponse());
  }
}
