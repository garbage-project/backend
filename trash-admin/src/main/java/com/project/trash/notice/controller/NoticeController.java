package com.project.trash.notice.controller;

import com.project.trash.common.response.PageListResponse;
import com.project.trash.common.response.SuccessResponse;
import com.project.trash.notice.controller.validation.NoticeValidator;
import com.project.trash.notice.request.NoticeEntryRequest;
import com.project.trash.notice.request.NoticeListRequest;
import com.project.trash.notice.request.NoticeModifyRequest;
import com.project.trash.notice.response.NoticeListResponse;
import com.project.trash.notice.service.NoticeCommandService;
import com.project.trash.notice.service.NoticeQueryService;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

import lombok.RequiredArgsConstructor;

/**
 * 공지 API
 */
@RestController
@RequestMapping("/notices")
@RequiredArgsConstructor
public class NoticeController {

  private final NoticeCommandService noticeCommandService;
  private final NoticeQueryService noticeQueryService;

  /**
   * 공지 삭제
   */
  @DeleteMapping("/{noticeSeq}")
  public ResponseEntity<?> delete(@PathVariable Long noticeSeq) {
    noticeCommandService.delete(noticeSeq);
    return ResponseEntity.ok(new SuccessResponse());
  }

  /**
   * 공지 목록 조회
   */
  @GetMapping
  public ResponseEntity<?> getList(@ModelAttribute NoticeListRequest param) {
    NoticeValidator.validate(param);

    Pair<List<NoticeListResponse>, Long> pair = noticeQueryService.getList(param);
    return ResponseEntity.ok(new PageListResponse(param, pair.getLeft(), pair.getRight()));
  }

  /**
   * 공지 등록
   */
  @PostMapping
  public ResponseEntity<?> post(@RequestBody NoticeEntryRequest param) {
    NoticeValidator.validate(param);

    noticeCommandService.entry(param);
    return ResponseEntity.ok(new SuccessResponse());
  }

  /**
   * 공지 수정
   */
  @PutMapping
  public ResponseEntity<?> put(@RequestBody NoticeModifyRequest param) {
    NoticeValidator.validate(param);

    noticeCommandService.modify(param);
    return ResponseEntity.ok(new SuccessResponse());
  }
}
