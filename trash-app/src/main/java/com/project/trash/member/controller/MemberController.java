package com.project.trash.member.controller;

import com.project.trash.common.response.DataResponse;
import com.project.trash.member.service.MemberCommandService;
import com.project.trash.member.service.MemberQueryService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;

/**
 * 회원 API
 */
@RestController
@RequestMapping("/members")
@RequiredArgsConstructor
public class MemberController {

  private final MemberCommandService memberCommandService;
  private final MemberQueryService memberQueryService;

  /**
   * 로그인 회원 상세 조회
   */
  @GetMapping("/my-info")
  public ResponseEntity<?> getMyInfo() {
    return ResponseEntity.ok(new DataResponse(memberQueryService.getDetail()));
  }
}
