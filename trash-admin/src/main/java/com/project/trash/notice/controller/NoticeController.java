package com.project.trash.notice.controller;

import com.project.trash.common.response.DataResponse;
import com.project.trash.common.response.PageListResponse;
import com.project.trash.common.response.SuccessResponse;
import com.project.trash.notice.controller.validation.NoticeValidator;
import com.project.trash.notice.request.NoticeEntryRequest;
import com.project.trash.notice.request.NoticeListRequest;
import com.project.trash.notice.request.NoticeModifyRequest;
import com.project.trash.notice.response.NoticeDetailResponse;
import com.project.trash.notice.response.NoticeListResponse;
import com.project.trash.notice.service.NoticeCommandService;
import com.project.trash.notice.service.NoticeQueryService;

import org.apache.commons.lang3.tuple.Pair;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

/**
 * 공지 API
 */
@RestController
@RequestMapping("/notices")
@RequiredArgsConstructor
@Tag(name = "공지")
public class NoticeController {

  private final NoticeCommandService noticeCommandService;
  private final NoticeQueryService noticeQueryService;

  /**
   * 공지 삭제
   */
  @DeleteMapping("/{noticeSeq}")
  @Operation(summary = "공지 삭제",
      description = "공지를 삭제한다."
          + "\n[에러 코드]"
          + "\n- NTC000 : 공지 정보가 존재하지 않습니다.")
  public SuccessResponse delete(
      @Parameter(description = "삭제할 공지의 일련번호", required = true, example = "1") @PathVariable Long noticeSeq) {
    noticeCommandService.delete(noticeSeq);
    return new SuccessResponse();
  }

  /**
   * 공지 상세 조회
   */
  @GetMapping("/{noticeSeq}")
  @Operation(summary = "공지 상세 조회",
      description = "공지 정보를 상세 조회한다."
          + "\n[에러 코드]"
          + "\n- NTC000 : 공지 정보가 존재하지 않습니다.")
  public DataResponse<NoticeDetailResponse> getDetail(
      @Parameter(description = "조회할 리뷰의 일련번호", required = true, example = "1") @PathVariable Long noticeSeq) {

    return new DataResponse<>(noticeQueryService.getDetail(noticeSeq));
  }

  /**
   * 공지 목록 조회
   */
  @GetMapping
  @Operation(summary = "공지 목록 조회",
      description = "공지 목록을 조회한다.")
  public PageListResponse<NoticeListResponse> getList(@ParameterObject NoticeListRequest param) {
    NoticeValidator.validate(param);

    Pair<List<NoticeListResponse>, Long> pair = noticeQueryService.getList(param);
    return new PageListResponse<>(param, pair.getLeft(), pair.getRight());
  }

  /**
   * 공지 등록
   */
  @PostMapping
  @Operation(summary = "공지 등록",
      description = "공지를 등록한다.")
  public SuccessResponse post(@RequestBody NoticeEntryRequest param) {
    NoticeValidator.validate(param);

    noticeCommandService.entry(param);
    return new SuccessResponse();
  }

  /**
   * 공지 수정
   */
  @PutMapping
  @Operation(summary = "공지 수정",
      description = "공지를 수정한다."
          + "\n[에러 코드]"
          + "\n- NTC000 : 공지 정보가 존재하지 않습니다.")
  public SuccessResponse put(@RequestBody NoticeModifyRequest param) {
    NoticeValidator.validate(param);

    noticeCommandService.modify(param);
    return new SuccessResponse();
  }
}
