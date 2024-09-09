package com.project.trash.facility.controller;

import com.project.trash.common.response.DataResponse;
import com.project.trash.common.response.PageListResponse;
import com.project.trash.common.response.SuccessResponse;
import com.project.trash.facility.controller.validation.FacilityValidator;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.request.FacilityModifyRequest;
import com.project.trash.facility.request.FacilityReviewListRequest;
import com.project.trash.facility.response.FacilityDetailResponse;
import com.project.trash.facility.response.FacilityListResponse;
import com.project.trash.facility.response.FacilityReviewListResponse;
import com.project.trash.facility.service.FacilityCommandService;
import com.project.trash.facility.service.FacilityQueryService;
import com.project.trash.review.service.ReviewCommandService;
import com.project.trash.review.service.ReviewQueryService;

import org.apache.commons.lang3.tuple.Pair;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Set;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

/**
 * 시설물 API
 */
@RestController
@RequestMapping("/facilities")
@RequiredArgsConstructor
@Tag(name = "시설물")
public class FacilityController {

  private final FacilityCommandService facilityCommandService;
  private final FacilityQueryService facilityQueryService;

  private final ReviewQueryService reviewQueryService;
  private final ReviewCommandService reviewCommandService;

  /**
   * 선택한 시설물 목록 삭제
   */
  @DeleteMapping
  @Operation(summary = "선택한 시설물 목록 삭제",
      description = "선택한 시설물들을 삭제한다."
          + "\n[에러 코드]"
          + "\n- FAC000 : 시설물 정보가 존재하지 않습니다.")
  public SuccessResponse delete(
      @Parameter(description = "삭제할 시설물의 일련번호 목록", required = true, example = "[1, 2, 3]") @RequestParam Set<Long> facilitySeqs) {

    facilityCommandService.delete(facilitySeqs);
    return new SuccessResponse();
  }

  /**
   * 시설물 목록 조회
   */
  @GetMapping
  @Operation(summary = "시설물 목록 조회",
      description = "시설물 목록을 조회한다.")
  public PageListResponse<FacilityListResponse> getList(@ParameterObject FacilityListRequest param) {
    FacilityValidator.validate(param);

    Pair<List<FacilityListResponse>, Long> pair = facilityQueryService.getList(param);
    return new PageListResponse<>(param, pair.getLeft(), pair.getRight());
  }

  /**
   * 시설물 상세 조회
   */
  @GetMapping("/{facilitySeq}")
  @Operation(summary = "시설물 상세 조회",
      description = "시설물 정보를 상세 조회한다."
          + "\n[에러 코드]"
          + "\n- FAC000 : 시설물 정보가 존재하지 않습니다.")
  public DataResponse<FacilityDetailResponse> getDetail(
      @Parameter(description = "조회할 시설물의 일련번호", required = true, example = "1") @PathVariable Long facilitySeq) {

    return new DataResponse<>(facilityQueryService.getDetail(facilitySeq));
  }

  /**
   * 시설물 등록
   */
  @PostMapping
  @Operation(summary = "시설물 등록",
      description = "시설물을 등록한다.")
  public SuccessResponse post(@RequestBody FacilityEntryRequest param) {
    FacilityValidator.validate(param);

    facilityCommandService.entry(param);
    return new SuccessResponse();
  }

  /**
   * 시설물 수정
   */
  @PutMapping
  @Operation(summary = "시설물 수정",
      description = "시설물을 수정한다."
          + "\n[에러 코드]"
          + "\n- FAC000 : 시설물 정보가 존재하지 않습니다.")
  public SuccessResponse put(@RequestBody FacilityModifyRequest param) {
    FacilityValidator.validate(param);

    facilityCommandService.modify(param);
    return new SuccessResponse();
  }

  /**
   * 시설물 리뷰 목록 조회
   */
  @GetMapping("/reviews")
  @Operation(summary = "시설물 리뷰 목록 조회",
      description = "시설물의 리뷰 목록을 조회한다.")
  public PageListResponse<FacilityReviewListResponse> getReviewList(@ParameterObject FacilityReviewListRequest param) {
    FacilityValidator.validate(param);

    Pair<List<FacilityReviewListResponse>, Long> pair = reviewQueryService.getList(param);
    return new PageListResponse<>(param, pair.getLeft(), pair.getRight());
  }

  /**
   * 선택한 리뷰 목록 삭제
   */
  @DeleteMapping("/reviews")
  @Operation(summary = "선택한 리뷰 목록 삭제",
      description = "선택한 리뷰들을 삭제한다."
          + "\n[에러 코드]"
          + "\n- RVW000 : 리뷰 정보가 존재하지 않습니다.")
  public SuccessResponse deleteReview(
      @Parameter(description = "삭제할 리뷰들의 일련번호 목록", required = true, example = "[1, 2, 3]") @RequestParam Set<Long> reviewSeqs) {

    reviewCommandService.delete(reviewSeqs);
    return new SuccessResponse();
  }
}
