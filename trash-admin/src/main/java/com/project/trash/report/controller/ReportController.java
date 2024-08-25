package com.project.trash.report.controller;

import com.project.trash.common.response.DataResponse;
import com.project.trash.common.response.PageListResponse;
import com.project.trash.common.response.SuccessResponse;
import com.project.trash.report.controller.validation.ReportValidator;
import com.project.trash.report.request.ReportAnswerRequest;
import com.project.trash.report.request.ReportListRequest;
import com.project.trash.report.response.ReportListResponse;
import com.project.trash.report.service.ReportCommandService;
import com.project.trash.report.service.ReportQueryService;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

import lombok.RequiredArgsConstructor;

/**
 * 신고 API
 */
@RestController
@RequestMapping("/reports")
@RequiredArgsConstructor
public class ReportController {

  private final ReportCommandService reportCommandService;
  private final ReportQueryService reportQueryService;

  /**
   * 신고 상세 조회
   */
  @GetMapping("/{reportSeq}")
  public ResponseEntity<?> getDetail(@PathVariable Long reportSeq) {
    return ResponseEntity.ok(new DataResponse(reportQueryService.getDetail(reportSeq)));
  }

  /**
   * 신고 목록 조회
   */
  @GetMapping
  public ResponseEntity<?> getList(@ModelAttribute ReportListRequest param) {
    ReportValidator.validate(param);

    Pair<List<ReportListResponse>, Long> pair = reportQueryService.getList(param);
    return ResponseEntity.ok(new PageListResponse(param, pair.getLeft(), pair.getRight()));
  }

  /**
   * 신고 답변
   */
  @PutMapping("/answer")
  public ResponseEntity<?> putAnswer(@RequestBody ReportAnswerRequest param) {
    ReportValidator.validate(param);

    reportCommandService.modify(param);
    return ResponseEntity.ok(new SuccessResponse());
  }
}
