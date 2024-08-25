package com.project.trash.notice.controller;

import com.project.trash.common.response.ListResponse;
import com.project.trash.notice.service.NoticeQueryService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;

/**
 * 공지 API
 */
@RestController
@RequestMapping("/notices")
@RequiredArgsConstructor
public class NoticeController {

  private final NoticeQueryService noticeQueryService;

  /**
   * 공지 목록 조회
   */
  @GetMapping
  public ResponseEntity<?> getList() {
    return ResponseEntity.ok(new ListResponse(noticeQueryService.getList()));
  }
}
