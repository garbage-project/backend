package com.project.trash.notice.controller;

import com.project.trash.common.response.ListResponse;
import com.project.trash.notice.response.NoticeListResponse;
import com.project.trash.notice.service.NoticeQueryService;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@RestController
@RequestMapping("/notices")
@RequiredArgsConstructor
@Tag(name = "공지")
public class NoticeController {

  private final NoticeQueryService noticeQueryService;

  @GetMapping
  @Operation(summary = "공지 목록 조회",
      description = "공지 목록을 조회한다.")
  public ListResponse<NoticeListResponse> getList() {
    return new ListResponse<>(noticeQueryService.getList());
  }
}
