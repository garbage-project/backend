package com.project.trash.admin.controller;

import com.project.trash.admin.request.AdminModifyRequest;
import com.project.trash.admin.service.AdminCommandService;
import com.project.trash.common.response.SuccessResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
   * 관리자 수정
   */
  @PutMapping
  public ResponseEntity<?> put(@RequestBody AdminModifyRequest param) {
    AdminValidator.validate(param);

    adminCommandService.modify(param);
    return ResponseEntity.ok(new SuccessResponse());
  }
}
